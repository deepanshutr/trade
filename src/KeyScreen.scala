package desk

import java.io.{IOException, InputStream, PrintStream}
import java.nio.file.Path
import java.security.SecureRandom
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.{ControlThrowable, NonFatal}

// The key screen. It runs before the workspace and has its own input loop, so no feed, event
// queue, fold or diagnostics exist while a seed or a passphrase is typed. Secrets live in byte
// arrays and fixed fields that are zeroed after use and on every way out. The screen shows masks,
// group counts, the address and the approval name, and nothing else about a secret.
object KeyScreen:
  final case class Context(
      network: String,
      keyService: String,
      names: Path,
      active: Path,
      stateDirectory: Path,
      random: SecureRandom,
      now: () => Long,
      clipboard: (Seq[String], Array[Byte]) => ProcessRun,
      helper: (Seq[String], Array[Byte]) => ProcessRun,
      security: Seq[String] => Int,
      env: Map[String, String],
      derive: (Array[Byte], Array[Byte], Int) => Array[Byte] = AgentKey.derive
  )

  private val helperVariables = Set("DESK_PYTHON", "DESK_KEY_HELPER")

  def system(network: String, keyService: String, home: Path, stateDirectory: Path): Context =
    Context(
      network,
      keyService,
      KeyNames.path(home),
      ActiveKey.path(home),
      stateDirectory,
      new SecureRandom(),
      () => System.currentTimeMillis(),
      ProcessRun.system,
      ProcessRun.system,
      KeychainItems.security,
      sys.env.filter((name, _) => helperVariables.contains(name))
    )

  // Runs on the given streams. Returns the active address when the screen is left with a usable
  // key. Ctrl-C and end of input leave with no address.
  def run(context: Context, input: InputStream, output: PrintStream): Option[String] =
    run(context, input, output, _ => ())

  // The same, and the buffer where a paste waits is handed over once, so a test can see that it
  // is wiped at once.
  private[desk] def run(
      context: Context,
      input: InputStream,
      output: PrintStream,
      pasteBuffer: Array[Byte] => Unit
  ): Option[String] =
    runWith(new Cleanup(context), context, input, output, pasteBuffer)

  // Runs on the real terminal. The terminal and the clipboard are put right on every way out,
  // including a kill, which runs the shutdown hook.
  def runOnTerminal(
      context: Context,
      enter: () => String = () => Term.enterRaw(),
      leave: String => Unit = saved => Term.restore(saved),
      addHook: Thread => Unit = hook => Runtime.getRuntime.addShutdownHook(hook),
      removeHook: Thread => Unit = hook => { val _ = Runtime.getRuntime.removeShutdownHook(hook) },
      input: InputStream = System.in,
      output: PrintStream = System.out
  ): Option[String] =
    val cleanup = new Cleanup(context)
    val saved =
      try enter()
      catch
        case NonFatal(error) =>
          leave("")
          throw error
    val hook = new Thread(
      () => {
        cleanup.close()
        leave(saved)
      },
      "key-screen-restore"
    )
    addHook(hook)
    try runWith(cleanup, context, input, output, _ => ())
    finally
      try cleanup.close()
      finally
        leave(saved)
        removeHook(hook)

  private def runWith(
      cleanup: Cleanup,
      context: Context,
      input: InputStream,
      output: PrintStream,
      pasteBuffer: Array[Byte] => Unit
  ): Option[String] =
    val keys = new KeyInput(input)
    pasteBuffer(keys.paste)
    cleanup.keep(keys.paste)
    try new Session(context, keys, output, cleanup).list()
    catch case Interrupted => None
    finally cleanup.close()

  // Ctrl-C and end of input leave the screen. Escape leaves one flow for the list.
  private object Interrupted extends ControlThrowable
  private object Cancelled extends ControlThrowable

  // Everything to put right when the screen closes. It can run from the shutdown hook too.
  private final class Cleanup(context: Context):
    private val wipers = new ConcurrentLinkedQueue[() => Unit]()
    private val copied = new AtomicBoolean(false)

    def keep(bytes: Array[Byte]): Array[Byte] =
      wipers.add(() => java.util.Arrays.fill(bytes, 0.toByte))
      bytes

    def keep(field: SecretField): SecretField =
      wipers.add(() => field.wipe())
      field

    def wipeAll(): Unit = wipers.forEach(wipe => wipe())

    // The seed text is on the clipboard from the copy until a clear works.
    def copying(): Unit = copied.set(true)

    def clearClipboard(): Either[String, Unit] =
      val result = Clipboard.clear(context.clipboard)
      if result.isRight then copied.set(false)
      result

    def clearIfPending(): Unit =
      if copied.get() then
        val _ = clearClipboard()

    def close(): Unit =
      try wipeAll()
      finally clearIfPending()

  // Turns terminal bytes into key codes. A code below 256 is a printable ASCII byte. A paste
  // waits in the paste buffer until the screen takes it and wipes it.
  private object KeyInput:
    val Enter = 256
    val Backspace = 257
    val Escape = 258
    val Up = 259
    val Down = 260
    val ArrowLeft = 261
    val ArrowRight = 262
    val Paste = 263
    val Refused = 264
    val Ignored = 265
    val pasteLimit = 128

  private final class KeyInput(in: InputStream, escapeWaitNanos: Long = Main.escapeWaitNanos):
    import KeyInput.*

    val paste = new Array[Byte](pasteLimit)
    private var pasteLength = 0
    private val none = -2
    private var held = none
    // ESC [ 2 0 1 ~ ends a bracketed paste.
    private val pasteEnd = Array(27, 91, 50, 48, 49, 126)

    def pastedLength: Int = pasteLength

    def wipePaste(): Unit =
      java.util.Arrays.fill(paste, 0.toByte)
      pasteLength = 0

    private def readByte(): Int =
      if held != none then
        val byte = held
        held = none
        byte
      else in.read()

    private def waiting: Boolean = held != none || in.available() > 0

    // After an escape byte a sequence follows at once. When nothing comes it was the Escape key.
    private def follows(): Boolean =
      val deadline = System.nanoTime() + escapeWaitNanos
      while !waiting && System.nanoTime() < deadline do Thread.sleep(2L)
      waiting

    def next(): Int =
      val byte = readByte()
      if byte < 0 || byte == 3 then throw Interrupted
      else if byte == 13 || byte == 10 then Enter
      else if byte == 127 || byte == 8 then Backspace
      else if byte == 27 then escape()
      else if byte >= 32 && byte < 127 then byte
      else Ignored

    private def escape(): Int =
      if !follows() then Escape
      else
        readByte() match
          case 91    => sequence()
          case 79    => arrow(readByte())
          case other =>
            held = other
            Escape

    private def arrow(last: Int): Int = last match
      case 65 => Up
      case 66 => Down
      case 67 => ArrowRight
      case 68 => ArrowLeft
      case 3  => throw Interrupted
      case _  => if last < 0 then throw Interrupted else Ignored

    // ESC [ then parameter bytes, then a final byte. Only the arrows and the paste start count.
    private def sequence(): Int =
      val parameters = new StringBuilder
      var byte = readByte()
      while byte >= 0x20 && byte <= 0x3f do
        if parameters.length < 8 then parameters.append(byte.toChar)
        byte = readByte()
      if byte < 0 then throw Interrupted
      else if byte < 0x40 || byte > 0x7e then
        held = byte
        Ignored
      else if byte == 126 && parameters.toString == "200" then readPaste()
      else if byte >= 65 && byte <= 68 then arrow(byte)
      else Ignored

    private def keep(byte: Int): Boolean =
      if byte < 32 || byte == 127 || pasteLength >= pasteLimit then false
      else
        paste(pasteLength) = byte.toByte
        pasteLength += 1
        true

    // Reads to the end marker. A control byte or more than 128 bytes refuses the whole paste.
    private def readPaste(): Int =
      pasteLength = 0
      var refused = false
      var matched = 0
      var ended = false
      while !ended do
        val byte = readByte()
        if byte < 0 || byte == 3 then
          wipePaste()
          throw Interrupted
        if matched > 0 then
          if byte == pasteEnd(matched) then
            matched += 1
            ended = matched == pasteEnd.length
          else
            refused = true
            matched = if byte == 27 then 1 else 0
        else if byte == 27 then matched = 1
        else if !keep(byte) then refused = true
      if refused then
        wipePaste()
        Refused
      else Paste

  private final case class Row(entry: KeyEntry, state: Either[String, Boolean])
  private type View = Either[String, (Vector[Row], Map[String, String])]

  private enum Drill:
    case Passed, Retry, StartOver

  private val seedGroups = SeedText.groups
  private val seedLength = seedGroups * 5
  private val deriving = "Deriving the key. This takes a few seconds."
  private val nameRule = "Use 1 to 32 characters with no space at either end."
  private val seedHint = "Type or paste the seed from Passwords. Accepted characters show as marks."

  private def storedText(approval: String): String =
    "Stored. Approval at the exchange is your step. Approve this address from the owner wallet " +
      s"as a named API key called $approval. Until then this key cannot trade, and the role " +
      "check must pass."

  private final class Session(
      context: Context,
      input: KeyInput,
      output: PrintStream,
      cleanup: Cleanup
  ):
    private def show(lines: Vector[String]): Unit =
      output.print(Screen.esc + "[2J" + Term.frame(lines))
      output.flush()
      if output.checkError() then throw new IOException("terminal write failed")

    private def notice(text: String): Vector[String] = Option.when(text.nonEmpty)(text).toVector

    // Paste only reaches the caller that asks for it. Anywhere else it is wiped and ignored.
    private def next(paste: Boolean = false): Int =
      val key = input.next()
      if key == KeyInput.Paste && !paste then
        input.wipePaste()
        KeyInput.Ignored
      else key

    private def read(paste: Boolean = false): Int =
      val key = next(paste)
      if key == KeyInput.Escape then throw Cancelled else key

    private def confirm(): Unit =
      while read() != KeyInput.Enter do ()

    private def wipe(bytes: Array[Byte]): Unit = java.util.Arrays.fill(bytes, 0.toByte)

    // The key files can fail on disk. Their reasons are fixed text and never hold a secret.
    private def saving[A](body: => A): Either[String, A] =
      try Right(body)
      catch
        case NonFatal(error) =>
          Left("The key files could not be updated (" + error.getClass.getSimpleName + ")")

    private def present(address: String): Either[String, Boolean] =
      KeychainItems.present(context.keyService, address, context.security)

    private def load(): View =
      val file = (message: String) => message + " (" + context.names.getFileName + ")"
      for
        entries <- KeyNames.load(context.names).left.map(file)
        active <- ActiveKey
          .load(context.active)
          .left
          .map(m => m + " (" + context.active.getFileName + ")")
      yield (entries.map(entry => Row(entry, present(entry.address))), active)

    private def usable(): Option[String] =
      for
        entries <- KeyNames.load(context.names).toOption
        active <- ActiveKey.load(context.active).toOption
        address <- KeySetup.usable(context.network, entries, active, present).toOption
      yield address

    // The list

    private def listLines(view: View, cursor: Int, note: String): Vector[String] =
      val head = Vector("Desk keys on " + context.network, "")
      view match
        case Left(message) =>
          head ++ Vector(message, "Fix or move the file, then run desk --keys again. q leaves.")
        case Right((rows, active)) =>
          val body =
            if rows.isEmpty then Vector("No keys yet.")
            else
              rows.zipWithIndex.flatMap { (row, index) =>
                val entry = row.entry
                val marks =
                  (if active.get(entry.network).contains(entry.address) then "  active" else "") +
                    (row.state match
                      case Right(true)  => ""
                      case Right(false) => "  not in Keychain"
                      case Left(_)      => "  Keychain lookup failed")
                Vector(
                  (if index == cursor then "> " else "  ") + entry.name + marks,
                  s"    ${entry.address}  ${entry.network}  ${AgentKey.approvalName(entry.address)}"
                )
              }
          head ++ body ++ Vector(
            "",
            "Up and Down move. Enter selects. r renames.",
            "n makes a new key. s restores a key from its seed. q leaves."
          ) ++ notice(note)

    def list(): Option[String] =
      var view = load()
      var cursor = 0
      var note = ""
      var result = Option.empty[String]
      var leaving = false
      while !leaving do
        show(listLines(view, cursor, note))
        note = ""
        val key = next()
        view match
          case Left(_) =>
            if key == 'q'.toInt then leaving = true
          case Right((rows, active)) =>
            val row = rows.lift(cursor)
            key match
              case KeyInput.Up    => cursor = (cursor - 1).max(0)
              case KeyInput.Down  => cursor = (cursor + 1).min((rows.size - 1).max(0))
              case KeyInput.Enter =>
                note = row.fold("")(select(_, active))
                view = load()
              case _ =>
                key.toChar match
                  case 'r' =>
                    note = row.fold("")(r => flow(rename(r)))
                    view = load()
                  case 'n' =>
                    note = flow(newKey())
                    view = load()
                  case 's' =>
                    note = flow(restore())
                    view = load()
                  case 'q' =>
                    leaving = true
                  case _ => ()
        if leaving && view.isRight then result = usable()
      result

    // One flow. Escape ends it quietly and whatever it held is wiped.
    private def flow(body: => String): String =
      try body
      catch case Cancelled => ""
      finally cleanup.wipeAll()

    private def select(row: Row, active: Map[String, String]): String =
      val entry = row.entry
      row.state match
        case Left(_)      => "The Keychain lookup failed. Try again."
        case Right(false) => "That key is not in the Keychain. Press s to restore it."
        case Right(true)  =>
          if entry.network != context.network then
            s"That key is for ${entry.network}. Desk is set to ${context.network}."
          else if active.get(context.network).contains(entry.address) then
            "That key is already active."
          else
            switchTo(entry.address).fold(
              _ + ". The current key stays active.",
              _ => "Active key: " + entry.name
            )

    // The one way to change the active key. A saved order or another Desk can refuse it.
    private def switchTo(address: String): Either[String, Unit] =
      ActiveKey.load(context.active).flatMap { active =>
        val current = active.get(context.network)
        if current.contains(address) then Right(())
        else
          KeySetup
            .switchAllowed(context.stateDirectory, context.network, current)
            .flatMap(_ => saving(ActiveKey.select(context.active, context.network, address)))
      }

    private def rename(row: Row): String =
      val name = askName("Rename key", "New name: ", Some(row.entry.name))
      saving(KeyNames.rename(context.names, row.entry.address, name, context.now()))
        .fold(message => message, _ => "")

    // Typing

    private def typeSecret(
        title: String,
        instruction: String,
        label: String,
        field: SecretField,
        first: String
    )(check: => Option[String]): Unit =
      var note = first
      var done = false
      while !done do
        show(
          Vector(title, "", instruction, label + "*" * field.length) ++ notice(note) ++
            Vector("Enter accepts. Esc cancels.")
        )
        note = ""
        read() match
          case KeyInput.Enter =>
            check match
              case None          => done = true
              case Some(message) => note = message
          case KeyInput.Backspace => field.backspace()
          case key if key < 256   =>
            if !field.append(key.toByte) then note = "At most 128 characters."
          case _ => ()

    private def same(a: SecretField, b: SecretField): Boolean =
      var difference = a.length ^ b.length
      var index = 0
      while index < a.length.min(b.length) do
        difference |= a.bytes(index) ^ b.bytes(index)
        index += 1
      difference == 0

    private val passphraseRule = "Use 8 to 128 characters."

    // A passphrase that is valid and typed twice the same.
    private def askPassphrase(title: String): SecretField =
      var chosen = Option.empty[SecretField]
      var note = ""
      while chosen.isEmpty do
        val first = cleanup.keep(new SecretField(128))
        typeSecret(
          title,
          "Choose a passphrase of 8 to 128 printable characters. Spaces are allowed.",
          "Passphrase: ",
          first,
          note
        )(Option.unless(AgentKey.passphraseValid(first.bytes, first.length))(passphraseRule))
        val second = cleanup.keep(new SecretField(128))
        typeSecret(title, "Type the passphrase again.", "Again: ", second, "")(None)
        if same(first, second) then
          second.wipe()
          chosen = Some(first)
        else
          first.wipe()
          second.wipe()
          note = "The two did not match. Start again."
      chosen.get

    // A passphrase typed once. The drill accepts any length and lets the address decide.
    private def askPassphraseOnce(title: String, strict: Boolean): SecretField =
      val field = cleanup.keep(new SecretField(128))
      typeSecret(title, "Type the passphrase.", "Passphrase: ", field, "")(
        Option.when(strict && !AgentKey.passphraseValid(field.bytes, field.length))(passphraseRule)
      )
      field

    private def askName(title: String, label: String, current: Option[String]): String =
      val text = new StringBuilder
      var note = ""
      var done = false
      while !done do
        show(
          Vector(title, "") ++ current.map("Current name: " + _).toVector ++
            Vector(label + text) ++ notice(note) ++ Vector("Enter accepts. Esc cancels.")
        )
        note = ""
        read() match
          case KeyInput.Enter =>
            if KeyNames.validName(text.toString) then done = true else note = nameRule
          case KeyInput.Backspace => if text.nonEmpty then text.setLength(text.length - 1)
          case key if key < 256   => if text.length < 32 then text.append(key.toChar)
          case _                  => ()
      text.toString

    // The seed field keeps normalised characters. A finished group is checked at once.
    private def feed(field: SecretField, byte: Byte): Option[String] =
      SeedText.accept(byte) match
        case None => Option.when(SeedText.refused(byte))("That character is not used in a seed.")
        case Some(normal) =>
          if !field.append(normal) then Some("The seed has no room for more characters.")
          else if field.length % 5 != 0 then None
          else
            val index = field.length / 5 - 1
            val group = java.util.Arrays.copyOfRange(field.bytes, field.length - 5, field.length)
            val valid =
              try SeedText.groupValid(index, group)
              finally wipe(group)
            if valid then None
            else
              for _ <- 0 until 5 do field.backspace()
              Some(s"Group ${index + 1} has a typo. Type it again.")

    // A paste replaces the field. One bad character or group refuses the whole paste.
    private def pasteInto(field: SecretField): Option[String] =
      val trial = new SecretField(seedLength)
      var problem = Option.empty[String]
      var index = 0
      while problem.isEmpty && index < input.pastedLength do
        problem = feed(trial, input.paste(index))
        index += 1
      if problem.isEmpty && trial.length > 0 then
        field.wipe()
        for at <- 0 until trial.length do field.append(trial.bytes(at))
      trial.wipe()
      problem.map("The paste was refused. " + _)

    private def seedLines(title: String, field: SecretField, note: String): Vector[String] =
      val marks = (0 until field.length by 5).map(start => "#" * (field.length - start).min(5))
      val number = (field.length / 5 + 1).min(seedGroups)
      Vector(title, "", seedHint) ++
        Option.when(marks.nonEmpty)("  " + marks.mkString(" ")).toVector ++
        Vector(s"group $number of $seedGroups") ++ notice(note) ++ Vector("Esc cancels.")

    // The seed from the drill or a restore, typed or pasted. Returns the 32 seed bytes.
    private def askSeed(title: String): Array[Byte] =
      val field = cleanup.keep(new SecretField(seedLength))
      var note = ""
      var seed = Option.empty[Array[Byte]]
      while seed.isEmpty do
        show(seedLines(title, field, note))
        note = ""
        read(paste = true) match
          case KeyInput.Backspace => field.backspace()
          case KeyInput.Paste     =>
            val refusal = pasteInto(field)
            input.wipePaste()
            note = refusal.getOrElse("") + afterPaste()
          case KeyInput.Refused =>
            note = "The paste was refused. It was longer than 128 bytes or held a control byte." +
              afterPaste()
          case key if key < 256 => note = feed(field, key.toByte).getOrElse("")
          case _                => ()
        if field.length == seedLength then
          seed = SeedText.decode(field.bytes).map(bytes => cleanup.keep(bytes))
          if seed.isEmpty then
            field.wipe()
            note = "That seed is not valid. Type it again."
      field.wipe()
      seed.get

    // The clipboard may hold the pasted seed, so it is cleared after every paste.
    private def afterPaste(): String =
      cleanup.clearClipboard().fold(reason => " " + reason + ". Clear it yourself.", _ => "")

    // Making a key

    private def derive(title: String, seed: Array[Byte], passphrase: SecretField): Array[Byte] =
      show(Vector(title, "", deriving))
      cleanup.keep(context.derive(seed, passphrase.bytes, passphrase.length))

    private def addressLines(
        title: String,
        address: String,
        extra: Vector[String]
    ): Vector[String] =
      Vector(
        title,
        "",
        "Address: " + address,
        "Approval name: " + AgentKey.approvalName(address)
      ) ++
        extra

    // Makes seeds until one is done or the owner leaves. Start over makes a new seed.
    private def newKey(): String =
      try
        var result = Option.empty[String]
        while result.isEmpty do result = attempt()
        result.get
      catch
        case Cancelled =>
          cleanup.clearIfPending()
          ""

    private def attempt(): Option[String] =
      val seed = cleanup.keep(AgentKey.newSeed(context.random))
      val text = SeedText.encode(seed)
      cleanup.copying()
      val copied =
        try Clipboard.copySecret(text, context.clipboard)
        finally wipe(text)
      copied match
        case Left(reason) =>
          cleanup.wipeAll()
          val _ = cleanup.clearClipboard()
          Some(reason)
        case Right(_) =>
          show(
            Vector(
              "New key",
              "",
              "The seed is on the clipboard. It is not shown here.",
              "Paste it into Passwords and save it there. Then press Enter.",
              "Esc cancels."
            )
          )
          confirm()
          cleanup.clearClipboard() match
            case Left(reason) =>
              cleanup.wipeAll()
              Some(reason)
            case Right(_) => made(seed)

    private def made(seed: Array[Byte]): Option[String] =
      val passphrase = askPassphrase("New key")
      val key = derive("New key", seed, passphrase)
      val address = OrderSigning.address(key)
      show(addressLines("New key", address, Vector("", "Press Enter for the drill. Esc cancels.")))
      confirm()
      var outcome = drill(address)
      while outcome == Drill.Retry do outcome = drill(address)
      wipe(seed)
      passphrase.wipe()
      if outcome == Drill.StartOver then
        cleanup.wipeAll()
        None
      else
        val name = askName("Name this key", "Name: ", None)
        Some(store(key, address, Some(name)))

    // The owner proves the seed and the passphrase were kept by typing them again.
    private def drill(address: String): Drill =
      val seed = askSeed("Drill")
      val passphrase = askPassphraseOnce("Drill", strict = false)
      val matches = AgentKey.passphraseValid(passphrase.bytes, passphrase.length) && {
        val key = derive("Drill", seed, passphrase)
        try OrderSigning.address(key) == address
        finally wipe(key)
      }
      wipe(seed)
      passphrase.wipe()
      if matches then Drill.Passed
      else
        var choice = Option.empty[Drill]
        while choice.isEmpty do
          show(
            Vector(
              "Drill",
              "",
              "The drill did not match this key.",
              "r tries the drill again. s starts over with a new seed. Esc leaves."
            )
          )
          read().toChar match
            case 'r' => choice = Some(Drill.Retry)
            case 's' => choice = Some(Drill.StartOver)
            case _   => ()
        choice.get

    // Writes the name when there is one, stores the key and makes it the active key.
    private def store(key: Array[Byte], address: String, name: Option[String]): String =
      val recorded = name match
        case Some(text) =>
          saving(
            KeyNames.create(
              context.names,
              KeyEntry(address, text, context.network, context.now())
            )
          )
        case None => Right(())
      recorded match
        case Left(message) =>
          wipe(key)
          message
        case Right(_) =>
          val result = KeyStore.store(key, address, context.keyService, context.env, context.helper)
          wipe(key)
          result match
            case StoreResult.Failed(reason) =>
              s"The key was not stored. $reason. Its name is saved. Press s to restore it from its seed."
            case _ =>
              val approval = AgentKey.approvalName(address)
              val refusal = switchTo(address).left.toOption
              show(
                addressLines(
                  "Key stored",
                  address,
                  Vector("") ++ refusal.map(_ + ". The current key stays active.").toVector ++
                    Vector(storedText(approval), "", "Press Enter.")
                )
              )
              confirm()
              ""

    // Restore: the seed, then the passphrase, then the address, then the key is stored.
    private def restore(): String =
      val seed = askSeed("Restore a key")
      val passphrase = askPassphraseOnce("Restore a key", strict = true)
      val key = derive("Restore a key", seed, passphrase)
      wipe(seed)
      passphrase.wipe()
      val address = OrderSigning.address(key)
      KeyNames.load(context.names) match
        case Left(message)  => message
        case Right(entries) =>
          val known = entries.find(_.address == address)
          known match
            case Some(entry) if entry.network != context.network =>
              s"That key is named for ${entry.network}. Desk is set to ${context.network}."
            case _ =>
              show(
                addressLines(
                  "Restore a key",
                  address,
                  known.map("It is already named " + _.name + ".").toVector ++
                    Vector("", "Press Enter to store this key. Esc cancels.")
                )
              )
              confirm()
              val name = Option.when(known.isEmpty)(askName("Name this key", "Name: ", None))
              store(key, address, name)
