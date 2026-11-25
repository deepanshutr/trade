package desk

import java.io.{IOException, InputStream, PrintStream}
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try
import scala.util.control.NonFatal

// The settings screen behind s. It runs on the keys thread while the workspace is paused and
// changes the running Desk only through its context. It never reads a key, a seed or a
// passphrase: those are typed in a child process that owns the terminal and exits.
object SettingsScreen:
  final case class Context(
      network: String,
      settings: () => Either[String, SettingsSnapshot],
      writeSettings: (SettingsSnapshot, TradeTerms) => Either[String, SettingsSnapshot],
      applyTerms: TradeTerms => Unit,
      bindings: () => Bindings,
      writeBindings: Bindings => Either[String, Unit],
      applyBindings: Bindings => Unit,
      keyRow: () => Either[String, String], // the usable-key check rendered
      // The latest status read and its time. Reading it never waits for a read.
      ownerStatus: () => Option[(Either[String, OwnerStatus], Long)],
      refreshOwner: () => Unit, // asks for a read, and does nothing while one runs
      runKeyScreen: () => Either[String, Int], // the whole hand-off
      runOwner: Seq[String] => Either[String, Int],
      copyAddress: String => Either[String, Unit],
      reload: () => Either[String, Unit],
      livePending: () => Boolean,
      now: () => Long,
      size: () => (Int, Int) = () => (100, 30),
      // The --config words the approval step takes, or why it cannot use that file.
      approveConfig: Either[String, Seq[String]] = Right(Nil),
      // The usable key's full address, to check the owner tooling's status against.
      activeKey: () => Option[String] = () => None,
      // The key names file, read again at each approval. No names by default, so the approval
      // step stays shut.
      keyNames: () => Either[String, Vector[KeyEntry]] = () => Right(Vector.empty),
      // Whether the owner trades in prop mode now, and the prop wallet reset and its words.
      prop: () => Boolean = () => false,
      resetProp: () => Either[String, String] = () => Left("Switch to prop first")
  )

  private val help = "Up and Down move. Enter edits or acts. r reads the owner again. Esc leaves."
  private val editing = "Digits and one dot. Enter saves. Esc cancels."
  private val reading = "Reading owner status"
  private val plainRefusal = "The settings file could not be read or written"
  private val amountRule = "Type an amount in dollars with digits and one dot"
  private val copied =
    "Address copied. Fund it by a transfer to this address at the venue from an account you " +
      "control. Desk never sends money."
  private val reloadFailed = "Reload failed"
  private val resetFailed = "Reset failed"
  private val otherKeyText = "The status is for another key. Press r"
  private val statusEveryMs = 15000L

  // Returns true when Control C or the end of input asks Desk to quit.
  def run(context: Context, input: InputStream, output: PrintStream): Boolean =
    run(context, input, output, () => Thread.sleep(25L))

  private[desk] def run(
      context: Context,
      input: InputStream,
      output: PrintStream,
      idle: () => Unit
  ): Boolean =
    new Session(context, new SettingsInput(input), output, idle).loop()

  // Opens the screen over the paused workspace and gives the workspace back with a full repaint.
  // A screen that throws leaves the workspace paused: Desk is stopping and the failure handler
  // restores the terminal.
  private[desk] def around[A](
      pause: Boolean => Unit,
      mouse: Boolean => Unit,
      clear: () => Unit,
      reset: () => Unit,
      invalidate: () => Unit
  )(body: => A): A =
    pause(true)
    mouse(false)
    clear()
    val result = body
    mouse(true)
    reset()
    invalidate()
    pause(false)
    result

  // Shared by every opening of the screen. At most one status read runs. A read asked while one
  // runs waits for it, and asks made meanwhile make one read. The last reader to ask is the one
  // that waits, so an old screen's ask gives way to the current one.
  private[desk] final class ReadGate:
    private var running = false
    private var next = Option.empty[OwnerReads]
    // True when the reader may read now. Otherwise it waits for the read that runs.
    private[SettingsScreen] def ask(reader: OwnerReads): Boolean = synchronized {
      if running then
        next = Some(reader)
        false
      else
        running = true
        true
    }
    // A read ended. The reader that waits, if any, reads next.
    private[SettingsScreen] def done(): Option[OwnerReads] = synchronized {
      val waiting = next
      next = None
      if waiting.isEmpty then running = false
      waiting
    }

  // Reads the owner status off the keys thread, so a slow venue never holds Esc. Each opening has
  // its own reader, so a read still running when its screen closes finishes on its own and is
  // never shown.
  private[desk] final class OwnerReads(
      read: () => Either[String, OwnerStatus],
      now: () => Long,
      spawn: Runnable => Unit,
      gate: ReadGate
  ):
    private val latest = new AtomicReference(Option.empty[(Either[String, OwnerStatus], Long)])
    def status: Option[(Either[String, OwnerStatus], Long)] = latest.get()
    def refresh(): Unit = if gate.ask(this) then start()
    private[SettingsScreen] def start(): Unit =
      try
        spawn(() =>
          // Stamped with the time the read began, so a read begun before a child counts as old.
          val began = now()
          try latest.set(Some((Try(read()).getOrElse(Left(OwnerStatus.unavailable)), began)))
          finally gate.done().foreach(_.start())
        )
      catch case NonFatal(_) => gate.done().foreach(_.start())

  // The terminal hand-off for one child. Live is stopped first and resumed last, whatever happens
  // between. The terminal leaves raw mode only after the Control C guard is on and is back in raw
  // mode before it comes off, so no window opens where Control C stops Desk.
  private[desk] final case class HandOff(
      suspend: () => Either[String, Unit],
      restore: () => Unit,
      start: Seq[String] => Process,
      guard: (() => Unit) => Unit,
      enter: () => Unit,
      enterRaw: () => Unit,
      reload: () => Either[String, Unit],
      resume: () => Unit
  ):
    def run(command: Seq[String]): Either[String, Int] =
      try
        suspend().flatMap { _ =>
          var back = false
          def takeBack(): Unit =
            if !back then
              back = true
              enterRaw()
          val outcome = Child.runOnTerminal(
            command,
            start,
            body =>
              guard(() => {
                restore()
                try body()
                finally takeBack()
              }),
            () => {
              enter()
              takeBack()
            }
          )
          // The child's result is whatever the files say now.
          outcome.flatMap { exit =>
            val reloaded =
              try reload()
              catch case NonFatal(_) => Left(reloadFailed)
            if exit != 0 then Right(exit) else reloaded.map(_ => 0)
          }
        }
      finally resume()

  // What one read of the terminal gave.
  private[desk] enum Read:
    case Key(group: KeyGroup)
    case Quit
    case Skip

  // Reads the terminal one key group at a time: one byte, one UTF-8 character, or one CSI or SS3
  // group, as the bytes a terminal sends for one press. A group cut short by the escape wait comes
  // back as far as it came, and no key rule accepts it. Paste and mouse reports are skipped.
  private[desk] final class SettingsInput(in: InputStream, waitNanos: Long = Main.escapeWaitNanos):
    private val none = -2
    private var held = none
    private val pasteEnd = Vector(27, 91, 50, 48, 49, 126)

    private def ready: Boolean = held != none || in.available() > 0

    private def take(): Int =
      if held != none then
        val byte = held
        held = none
        byte
      else in.read()

    // The next byte when one comes within the escape wait.
    private def within(): Int =
      val deadline = System.nanoTime() + waitNanos
      while !ready && System.nanoTime() < deadline do Thread.sleep(2L)
      if ready then take() else none

    def poll(): Option[Read] = Option.when(ready)(read(take()))

    // Drops every key typed so far, so none acts on a screen it was not typed for. The end of
    // input stays, so it still quits.
    def drain(): Unit =
      held = none
      var byte = 0
      while byte >= 0 && in.available() > 0 do byte = in.read()
      if byte < 0 then held = byte

    private def read(first: Int): Read =
      if first < 0 || first == 3 then Read.Quit
      else if first == 27 then escape()
      else if KeyGroup.utf8Length(first) > 0 then character(Vector(first))
      else Read.Key(KeyGroup(Vector(first)))

    private def character(bytes: Vector[Int]): Read =
      if bytes.size == KeyGroup.utf8Length(bytes.head) then Read.Key(KeyGroup(bytes))
      else
        val byte = within()
        if byte >= 128 && byte <= 191 then character(bytes :+ byte)
        else
          if byte != none then held = byte
          Read.Key(KeyGroup(bytes))

    private def escape(): Read =
      val next = within()
      if next == none then Read.Key(KeyGroup(Vector(27)))
      else if next == 91 || next == 79 then sequence(Vector(27, next))
      else if next == 3 then Read.Quit
      else if next == 27 || next < 0 then
        held = next
        Read.Key(KeyGroup(Vector(27)))
      // What "Use Option as Meta key" sends. No key rule accepts it.
      else Read.Key(KeyGroup(Vector(27, next)))

    private def sequence(bytes: Vector[Int]): Read =
      val byte = within()
      if byte == none then Read.Key(KeyGroup(bytes))
      else if byte < 0 || byte == 3 then Read.Quit
      else if byte >= 64 && byte <= 126 then finish(bytes :+ byte)
      else if byte >= 32 && byte <= 63 && bytes.size < 31 then sequence(bytes :+ byte)
      else
        held = byte
        Read.Key(KeyGroup(bytes))

    private def finish(group: Vector[Int]): Read =
      val text = group.drop(2).map(_.toChar).mkString
      if group(1) == 91 && text == "200~" then
        skipPaste()
        Read.Skip
      else if group(1) == 91 && text.startsWith("<") then Read.Skip
      else if group == Vector(27, 91, 77) then
        for _ <- 1 to 3 do { val _ = within() }
        Read.Skip
      else Read.Key(KeyGroup(group))

    private def skipPaste(): Unit =
      var tail = Vector.empty[Int]
      var byte = within()
      while byte >= 0 && !(tail :+ byte).endsWith(pasteEnd) do
        tail = (tail :+ byte).takeRight(pasteEnd.size - 1)
        byte = within()
      if byte == -1 then held = -1

  private enum Row:
    case Small, Big, Slippage, Reset, TradingKey, Owner, Approval, Funding
    case Action(action: Bind)

  private val rows: Vector[Row] =
    Vector(
      Row.Small,
      Row.Big,
      Row.Slippage,
      Row.Reset,
      Row.TradingKey,
      Row.Owner,
      Row.Approval,
      Row.Funding
    ) ++
      Bind.values.toVector.map(Row.Action(_))

  private def label(row: Row): String = row match
    case Row.Small          => "Small amount"
    case Row.Big            => "Big amount"
    case Row.Slippage       => "Slippage"
    case Row.Reset          => "Reset prop"
    case Row.TradingKey     => "Trading key"
    case Row.Owner          => "Owner"
    case Row.Approval       => "Approval"
    case Row.Funding        => "Funding"
    case Row.Action(action) => action.label

  private def safe(text: String): String = Workspace.safeText(text)

  private final class Session(
      context: Context,
      input: SettingsInput,
      output: PrintStream,
      idle: () => Unit
  ):
    private var cursor = 0
    private var notice = ""
    private var snapshot = settings()
    // When the last child or r ended. A status read begun before it describes the files from
    // before, so the owner rows wait for a newer one.
    private var settled = Long.MinValue
    private var drawn = Vector.empty[String]
    private var quit = false
    // The open amount field: its row and its text.
    private var field = Option.empty[(Row, String)]
    // Reading the size runs stty, so it is read at most every half second, as the workspace does.
    private var size = (0, 0)
    private var sizeAt = Option.empty[Long]

    private def measured(): (Int, Int) =
      val now = context.now()
      if sizeAt.forall(at => now - at >= 500L || now < at) then
        size = context.size()
        sizeAt = Some(now)
      size

    // The status is asked for at open, after a child, on r, and then every 15 seconds. A read
    // already asked for is never asked again before 15 seconds, so a slow one gets no twin.
    private var asked = 0L
    private def ask(): Unit =
      asked = context.now()
      context.refreshOwner()

    def loop(): Boolean =
      // The file is the one source: its sizes and slippage apply from the moment settings open.
      snapshot.foreach(s => context.applyTerms(TradeTerms(s.config.sizes, s.config.slippage)))
      ask()
      var leaving = false
      while !leaving && !quit do
        val now = context.now()
        if now - asked >= statusEveryMs &&
          context.ownerStatus().forall((_, at) => now - at >= statusEveryMs)
        then ask()
        draw()
        input.poll() match
          case None                  => idle()
          case Some(Read.Quit)       => quit = true
          case Some(Read.Skip)       => ()
          case Some(Read.Key(group)) =>
            notice = ""
            leaving = navigate(group)
      quit

    // Waits for the next read, without a time limit.
    private def next(): Read =
      var read = input.poll()
      while read.isEmpty do
        idle()
        read = input.poll()
      read.get

    private def navigate(group: KeyGroup): Boolean = group.bytes match
      case Vector(27)              => true
      case Vector(27, 91 | 79, 65) =>
        cursor = (cursor - 1).max(0)
        false
      case Vector(27, 91 | 79, 66) =>
        cursor = (cursor + 1).min(rows.size - 1)
        false
      case Vector(13) | Vector(10) =>
        act(rows(cursor))
        false
      case Vector(114) =>
        again()
        false
      case _ => false

    private def act(row: Row): Unit = row match
      case Row.Small | Row.Big | Row.Slippage => edit(row)
      case Row.Reset                          => reset()
      case Row.TradingKey                     => child(context.runKeyScreen())
      case Row.Owner                          => owner()
      case Row.Approval                       => approve()
      case Row.Funding                        => fund()
      case Row.Action(action)                 => capture(action)

    // A file nested deep enough by another program can overflow the stack while it is read.
    private def settings(): Either[String, SettingsSnapshot] =
      try context.settings()
      catch
        case _: StackOverflowError => Left(plainRefusal)
        case NonFatal(_)           => Left(plainRefusal)

    // After a child or r the files may have changed, and the screen under the child is gone.
    // Keys typed meanwhile are dropped first, so the next Enter is a press on the redrawn screen.
    private def afterward(): Unit =
      input.drain()
      settled = context.now()
      ask()
      snapshot = settings()
      drawn = Vector.empty

    // A reload can replace the live worker, which an order pending in it must not see.
    private def again(): Unit =
      notice =
        if context.livePending() then "Wait for the pending order"
        else
          Try(context.reload()).toEither.left
            .map(_ => reloadFailed)
            .flatten
            .fold(safe, _ => "Read again")
      afterward()

    // A child needs live suspended, which would retire a worker with an order pending.
    private def child(run: => Either[String, Int]): Unit =
      if context.livePending() then notice = "Wait for the pending order"
      else
        notice = run match
          case Left(reason) => safe(reason)
          case Right(0)     => "Done"
          case Right(exit)  => s"The step did not complete (exit $exit)"
        afterward()

    private def status: Option[Either[String, OwnerStatus]] =
      context.ownerStatus().filter((_, at) => at >= settled).map(_._1)

    // The vault's status describes this Desk only when its owner and key are Desk's own.
    private def ownerDiffers(reply: OwnerStatus): Boolean =
      (reply.owner, snapshot.toOption.flatMap(_.config.owner)) match
        case (Some(vault), Some(file)) => !vault.equalsIgnoreCase(file)
        case _                         => false

    private def otherKey(reply: OwnerStatus): Boolean =
      (reply.key, context.activeKey()) match
        case (Some(vault), Some(active)) => !vault.equalsIgnoreCase(active)
        case _                           => false

    private def words(step: String): Seq[String] =
      Seq("owner", step, "--network", context.network)

    // The vault decides what each phase means. Desk only picks the step.
    private def owner(): Unit = status match
      case None               => notice = reading
      case Some(Left(reason)) => notice = reason
      case Some(Right(reply)) =>
        reply.phase match
          case _ if reply.candidates > 1                 => notice = safe(reply.message)
          case None                                      => child(context.runOwner(words("new")))
          case Some(phase) if phase.startsWith("await-") => child(context.runOwner(words("resume")))
          case Some(_)                                   => notice = safe(reply.message)

    // The approval name in the active key's entry. A file that cannot be read, a key with no
    // entry and an entry with no name all count as none, so the approval step stays shut.
    private def approvalName: Option[String] =
      for
        address <- context.activeKey()
        entries <- context.keyNames().toOption
        entry <- entries.find(_.address.equalsIgnoreCase(address))
        name <- entry.approval
      yield name

    private def approve(): Unit = (context.approveConfig, status) match
      case (Left(note), _)                    => notice = note
      case (_, None)                          => notice = reading
      case (_, Some(Left(reason)))            => notice = reason
      case (Right(extra), Some(Right(reply))) =>
        if otherKey(reply) then notice = otherKeyText
        else if !reply.phase.contains("ready") then notice = "Make the owner first"
        else if context.keyRow() == Left(KeyReload.notRead) then
          notice = "Read the trading key first. Press r"
        else if context.keyRow().isLeft then notice = "Add a trading key first"
        else
          reply.approved match
            case Some(false) =>
              if approvalName.isEmpty then notice = KeyScreen.noApproval
              else child(context.runOwner(words("approve") ++ extra))
            case Some(true) => notice = "The trading key is already approved"
            case None       => notice = "Approval was not checked. Press r to read it again"

    // The address is the owner's own public value. Desk never sends money.
    private def fund(): Unit = status match
      case None               => notice = reading
      case Some(Left(reason)) => notice = reason
      case Some(Right(reply)) =>
        reply.owner match
          case None          => notice = "No owner yet"
          case Some(address) => notice = context.copyAddress(address).fold(identity, _ => copied)

    // The prop balance a reset starts over at, from the settings file.
    private def balance: Option[String] =
      snapshot.toOption.map(s => Trading.format(s.config.propBalance))

    // The owner resets the prop wallet only in prop mode and only by y at the confirm. Enter
    // cancels, so the Enter that opened the confirm, pressed twice or queued, never resets. The
    // router's answer is shown as given, and so is the message of a reset that throws.
    private def reset(): Unit =
      if !context.prop() then notice = "Switch to prop first"
      else
        val wallet = balance.fold("the prop wallet")(b => s"the prop wallet to $$$b")
        group(s"Reset $wallet? y confirms. Any other key cancels.").map(_.bytes) match
          case Some(Vector(121) | Vector(89)) =>
            notice =
              try safe(context.resetProp().merge)
              catch
                case NonFatal(e) =>
                  Option(e.getMessage).map(safe).filter(_.nonEmpty).getOrElse(resetFailed)
          case _ => notice = ""

    // Amounts and slippage. An accepted value is written and applied at once.

    private def edit(row: Row): Unit = snapshot match
      case Left(reason)   => notice = reason
      case Right(current) =>
        val terms = TradeTerms(current.config.sizes, current.config.slippage)
        var text = row match
          case Row.Small => Trading.format(terms.sizes.small)
          case Row.Big   => Trading.format(terms.sizes.big)
          case _         => SettingsFile.percent(terms.slippage)
        notice = editing
        var open = true
        while open && !quit do
          field = Some((row, text))
          draw()
          next() match
            case Read.Quit       => quit = true
            case Read.Skip       => ()
            case Read.Key(group) =>
              group.bytes match
                case Vector(27) =>
                  open = false
                  notice = ""
                case Vector(13) | Vector(10) =>
                  save(row, current, terms, text) match
                    case Left(reason) => notice = reason
                    case Right(saved) =>
                      snapshot = Right(saved)
                      context.applyTerms(TradeTerms(saved.config.sizes, saved.config.slippage))
                      notice = label(row) + " saved. The next order uses it."
                      open = false
                case Vector(127) | Vector(8) =>
                  text = text.dropRight(1)
                  notice = editing
                case Vector(byte)
                    if text.length < 16 && (byte >= 48 && byte <= 57 ||
                      byte == 46 && !text.contains('.')) =>
                  text += byte.toChar
                  notice = editing
                case _ => ()
        field = None

    private def save(
        row: Row,
        current: SettingsSnapshot,
        terms: TradeTerms,
        text: String
    ): Either[String, SettingsSnapshot] =
      val chosen = row match
        case Row.Slippage => SettingsFile.fraction(text).map(f => terms.copy(slippage = f))
        case _            =>
          Option
            .when(text.matches("[0-9]+(\\.[0-9]*)?|\\.[0-9]+"))(BigDecimal(text))
            .toRight(amountRule)
            .map(amount =>
              if row == Row.Small then terms.copy(sizes = terms.sizes.copy(small = amount))
              else terms.copy(sizes = terms.sizes.copy(big = amount))
            )
      chosen.flatMap { next =>
        // A file nested deep enough by another program can overflow the stack while rendering.
        try context.writeSettings(current, next)
        catch
          case _: StackOverflowError => Left(plainRefusal)
          case NonFatal(_)           => Left(plainRefusal)
      }

    // Key bindings. Nothing a capture reads reaches another field.

    private def isEnter(group: KeyGroup): Boolean =
      group.bytes == Vector(13) || group.bytes == Vector(10)

    // Shows the hint and reads one key group. None when Esc cancels or Control C quits.
    private def group(hint: String): Option[KeyGroup] =
      notice = hint
      draw()
      var found = Option.empty[KeyGroup]
      var waiting = true
      while waiting do
        next() match
          case Read.Quit =>
            quit = true
            waiting = false
          case Read.Skip       => ()
          case Read.Key(group) =>
            if group.bytes != Vector(27) then found = Some(group)
            waiting = false
      found

    // One pass: a key, a second key or Enter, then the same again. Left is the reason to start
    // again.
    private def captured(hint: String): Option[Either[String, Vector[KeyGroup]]] =
      group(hint).flatMap { first =>
        KeyGroup.refused(first) match
          case Some(reason) => Some(Left(reason))
          case None         =>
            val more = s"Got ${KeyGroup.name(first)}. Press another key to make a two-key " +
              "sequence, or Enter to keep it."
            group(more).flatMap { second =>
              val keys = if isEnter(second) then Vector(first) else Vector(first, second)
              keys.iterator.flatMap(KeyGroup.refused).nextOption() match
                case Some(reason) => Some(Left(reason))
                case None         =>
                  val seen = keys.indices.iterator
                    .map(_ => group("Press the same again to confirm."))
                    .takeWhile(_.nonEmpty)
                    .flatten
                    .toVector
                  Option.when(seen.size == keys.size)(
                    Either.cond(seen == keys, keys, "That did not match. Start again")
                  )
            }
      }

    private def capture(action: Bind): Unit =
      var hint = s"Press the key or keys for ${action.label}. Esc cancels."
      var done = false
      while !done do
        captured(hint).map(
          _.flatMap(keys => context.bindings().updated(action, Binding(keys)))
        ) match
          case None =>
            notice = ""
            done = true
          case Some(Left(reason)) => hint = reason
          case Some(Right(next))  =>
            notice = context.writeBindings(next) match
              case Left(reason) => reason
              case Right(())    =>
                context.applyBindings(next)
                s"${action.label} is now ${next.map.get(action).fold("")(_.name)}"
            done = true

    // Drawing

    private def shown(row: Row, now: Long): String =
      def vault(text: OwnerStatus => String): String = status match
        case None               => reading
        case Some(Left(reason)) => reason
        case Some(Right(reply)) => safe(text(reply))
      field
        .filter(_._1 == row)
        .fold {
          row match
            case Row.Small =>
              snapshot.fold(_ => "--", s => "$" + Trading.format(s.config.sizes.small))
            case Row.Big => snapshot.fold(_ => "--", s => "$" + Trading.format(s.config.sizes.big))
            case Row.Slippage =>
              snapshot.fold(_ => "--", s => SettingsFile.percent(s.config.slippage) + "%")
            case Row.Reset =>
              if !context.prop() then "switch to prop first"
              else balance.fold("--")("starts over at $" + _)
            case Row.TradingKey => context.keyRow().merge
            case Row.Owner      =>
              vault(reply =>
                OwnerStatus.ownerText(reply) +
                  (if ownerDiffers(reply) then "   differs from the settings file" else "")
              )
            case Row.Approval =>
              context.approveConfig.left.toOption.getOrElse(
                vault(reply =>
                  if otherKey(reply) then otherKeyText else OwnerStatus.approvalText(reply)
                )
              )
            case Row.Funding =>
              vault(reply =>
                OwnerStatus.fundingText(reply, now / 1000) +
                  (if reply.owner.nonEmpty then ". Enter copies the address." else "")
              )
            case Row.Action(action) => context.bindings().map.get(action).fold("unbound")(_.name)
        }((_, text) => text + "_")

    private def page(width: Int, height: Int): Vector[String] =
      val now = context.now()
      val title = s"Desk settings on ${context.network}"
      val head = title + " " * (width - title.length - 10).max(1) + "esc leaves"
      def line(index: Int): String =
        (if index == cursor then "> " else "  ") + label(rows(index)).padTo(21, ' ') +
          shown(rows(index), now)
      val trading = snapshot.fold(reason => "Trading".padTo(23, ' ') + safe(reason), _ => "Trading")
      val keys = context
        .ownerStatus()
        .fold("Keys")((_, at) =>
          "Keys".padTo(23, ' ') + s"owner read ${((now - at) / 1000).max(0)} s ago"
        )
      val body = Vector(trading) ++ (0 to 3).map(line) ++ Vector("", keys) ++ (4 to 7).map(line) ++
        Vector("", "Key bindings") ++ (8 until rows.size).map(line)
      // A long notice wraps rather than losing its end.
      val words = notice.split(" ").toVector
      val said = words.drop(1).foldLeft(Vector(words.head)) { (lines, word) =>
        if lines.last.length + 1 + word.length <= width then lines.init :+ (lines.last + " " + word)
        else lines :+ word
      }
      // The cursor's line in the body, past the section titles and blank lines above it.
      val at = cursor + (if cursor <= 3 then 1 else if cursor <= 7 then 3 else 5)
      val room = (height - 3 - said.size).max(1)
      val offset = (at - room + 1).max(0)
      (Vector(head, "") ++ body.slice(offset, offset + room) ++ Vector(help) ++ said)
        .map(_.take(width))

    private def draw(): Unit =
      val (width, height) = measured() match
        case (w, h) if w > 0 && h > 0 => (w, h)
        case _                        => (100, 30)
      val lines = page(width, height)
      if lines != drawn then
        output.print((if drawn.isEmpty then Screen.esc + "[2J" else "") + Term.frame(lines))
        output.flush()
        if output.checkError() then throw new IOException("terminal write failed")
        drawn = lines
