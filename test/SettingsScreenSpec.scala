package desk

import java.io.{ByteArrayOutputStream, InputStream, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable.ArrayBuffer

class SettingsScreenSpec extends munit.FunSuite:
  private val esc = "\u001b"
  private val up = esc + "[A"
  private val down = esc + "[B"
  private val enter = "\r"
  private val backspace = "\u007f"
  private val interrupt = "\u0003"
  private val owner = "0x" + "1234" * 10
  private val start = 1800000000000L

  // Bytes in chunks. Between two chunks nothing is waiting for a while, which reads as a pause,
  // so an escape at the end of a chunk is a lone Esc. After the last byte the input ends.
  private final class Script(chunks: Vector[String], pauseMs: Long = 80L) extends InputStream:
    private val parts = chunks.map(_.getBytes(UTF_8))
    private var index = 0
    private var offset = 0
    private var resumeAt = 0L
    private def settle(): Unit =
      while index < parts.size && offset >= parts(index).length do
        index += 1
        offset = 0
        resumeAt = System.nanoTime() + pauseMs * 1000000L
    override def available(): Int =
      settle()
      if index >= parts.size then 1
      else if System.nanoTime() < resumeAt then 0
      else parts(index).length - offset
    override def read(): Int =
      settle()
      if index >= parts.size then -1
      else
        while System.nanoTime() < resumeAt do Thread.sleep(1L)
        val byte = parts(index)(offset) & 0xff
        offset += 1
        byte

  private def status(
      phase: Option[String] = Some("ready"),
      approved: Option[Boolean] = Some(false),
      candidates: Int = 1,
      ownerAddress: Option[String] = Some(owner),
      message: String = "ready"
  ): OwnerStatus =
    OwnerStatus(
      ownerAddress,
      phase,
      candidates,
      Some("0x" + "a" * 40),
      approved,
      Some(BigDecimal("12.345")),
      Some(start / 1000 - 12),
      Vector.empty,
      message
    )

  private final class Fixture:
    val events = ArrayBuffer.empty[String]
    var clock = start
    var snapshot: Either[String, SettingsSnapshot] =
      Right(
        SettingsSnapshot(Path.of("/nowhere/config.json"), Array.emptyByteArray, TradingConfig())
      )
    var settingsReads = 0
    val written = ArrayBuffer.empty[TradeTerms]
    var write: (SettingsSnapshot, TradeTerms) => Either[String, SettingsSnapshot] = (s, t) =>
      val next = s.copy(config = s.config.copy(sizes = t.sizes, slippage = t.slippage))
      snapshot = Right(next)
      Right(next)
    val applied = ArrayBuffer.empty[TradeTerms]
    var bindings = Bindings.defaults
    var bindWrite: Bindings => Either[String, Unit] = _ => Right(())
    val bindWrites = ArrayBuffer.empty[Bindings]
    val bound = ArrayBuffer.empty[Bindings]
    var key: Either[String, String] = Right("Main   0xaaaa...aaaa   active, in Keychain")
    var owned: Option[(Either[String, OwnerStatus], Long)] = None
    var refreshes = 0
    var keyScreens = 0
    var keyResult: Either[String, Int] = Right(0)
    val owners = ArrayBuffer.empty[Seq[String]]
    var ownerResult: Either[String, Int] = Right(0)
    val copies = ArrayBuffer.empty[String]
    var copyResult: Either[String, Unit] = Right(())
    var reloads = 0
    var reload: () => Either[String, Unit] = () => Right(())
    var pending = false
    var size = (100, 30)
    var approve: Either[String, Seq[String]] = Right(Nil)
    def context: SettingsScreen.Context = SettingsScreen.Context(
      network = "mainnet",
      settings = () => {
        settingsReads += 1
        snapshot
      },
      writeSettings = (s, t) => {
        written += t
        write(s, t)
      },
      applyTerms = t => { applied += t; () },
      bindings = () => bindings,
      writeBindings = b => {
        bindWrites += b
        bindWrite(b)
      },
      applyBindings = b => {
        bound += b
        bindings = b
      },
      keyRow = () => key,
      ownerStatus = () => owned,
      refreshOwner = () => { refreshes += 1; () },
      runKeyScreen = () => {
        keyScreens += 1
        events += "key screen"
        keyResult
      },
      runOwner = words => {
        owners += words
        events += "owner"
        ownerResult
      },
      copyAddress = address => {
        copies += address
        copyResult
      },
      reload = () => {
        reloads += 1
        reload()
      },
      livePending = () => pending,
      now = () => clock,
      size = () => size,
      approveConfig = approve
    )
    val out = new ByteArrayOutputStream()
    def run(chunks: String*): Boolean =
      SettingsScreen.run(context, new Script(chunks.toVector), new PrintStream(out, true, UTF_8))
    // Every frame drawn, as plain lines.
    def frames: Vector[Vector[String]] =
      out
        .toString(UTF_8)
        .replace(esc + "[2J", "")
        .split(java.util.regex.Pattern.quote(esc + "[H"))
        .toVector
        .drop(1)
        .map(_.split(java.util.regex.Pattern.quote(esc + "[J")).headOption.getOrElse(""))
        .map(_.split("\r\n").toVector.map(_.replace(esc + "[K", "")))
    def last: Vector[String] = frames.last
    // The notice wraps below the help line.
    def notice: String =
      last.drop(last.indexWhere(_.startsWith("Up and Down move")) + 1).mkString(" ")
    def row(label: String): String =
      last
        .find(_.drop(2).startsWith(label))
        .getOrElse(fail(s"no $label row in\n${last.mkString("\n")}"))

  private def value(line: String): String = line.drop(23)

  // The page

  test("the page shows the three sections, their values and the help line") {
    val f = new Fixture
    f.owned = Some((Right(status()), start - 3000L))
    assert(!f.run(esc))
    val page = f.frames.head
    assert(page.head.startsWith("Desk settings on mainnet"), page.head)
    assert(page.head.endsWith("esc leaves"), page.head)
    assertEquals(page.head.length, 100)
    assertEquals(page(1), "")
    assertEquals(page(2), "Trading")
    assertEquals(page(3), "> Small amount         $500")
    assertEquals(page(4), "  Big amount           $1000")
    assertEquals(page(5), "  Slippage             0.10%")
    assert(page(7).startsWith("Keys"), page(7))
    assert(page(7).endsWith("owner read 3 s ago"), page(7))
    assertEquals(page(8), "  Trading key          Main   0xaaaa...aaaa   active, in Keychain")
    assertEquals(page(9), "  Owner                ready   0x1234...1234")
    assertEquals(page(10), "  Approval             not approved")
    assertEquals(
      page(11),
      "  Funding              $12.34 at the venue, read 12 s ago. Enter copies the address."
    )
    assertEquals(page(13), "Key bindings")
    assertEquals(
      page.slice(14, 26).map(_.take(23).trim),
      Bind.values.toVector.map(_.label)
    )
    assertEquals(
      page.slice(14, 26).map(value),
      Vector("q", "p", "l", "m", "c", "v", "1", "2", "3", "4", "x", "s")
    )
    assertEquals(
      page(26),
      "Up and Down move. Enter edits or acts. r reads the owner again. Esc leaves."
    )
    assertEquals(page.size, 28)
  }

  test("Up and Down move the cursor and the page scrolls to keep it visible") {
    val f = new Fixture
    f.size = (60, 12)
    assert(!f.run(up + down * 30, esc))
    val page = f.last
    assertEquals(page.size, 12)
    assert(page.forall(_.length <= 60), page)
    assert(page.exists(_.startsWith("> Settings")), page)
    assert(!page.exists(_ == "Trading"), page)
    assert(f.frames.head(3).startsWith("> Small amount"))
  }

  test("a lone Esc leaves and Control C or the end of input asks Desk to quit") {
    assert(!new Fixture().run(esc))
    assert(new Fixture().run(interrupt))
    assert(new Fixture().run(down))
  }

  // Trading

  test("an amount is edited in place, written and applied at once") {
    val f = new Fixture
    assert(!f.run(enter + backspace * 3 + "25" + enter, esc))
    val terms = TradeTerms(TradeSizes(BigDecimal(25)), Trading.defaultSlippage)
    assertEquals(f.written.toVector, Vector(terms))
    assertEquals(f.applied.toVector, Vector(terms))
    assertEquals(value(f.row("Small amount")), "$25")
    assertEquals(f.notice, "Small amount saved. The next order uses it.")
  }

  test("the big amount and slippage are written as the fraction and applied") {
    val f = new Fixture
    assert(!f.run(down + enter + backspace * 4 + "2000" + enter, esc))
    assert(!f.run(down * 2 + enter + backspace * 4 + "0.25" + enter, esc))
    val big = TradeSizes(big = BigDecimal(2000))
    assertEquals(
      f.applied.toVector,
      Vector(
        TradeTerms(big, Trading.defaultSlippage),
        TradeTerms(big, BigDecimal("0.0025"))
      )
    )
    assertEquals(value(f.row("Slippage")), "0.25%")
  }

  test("a refused amount stays in the field with its reason and never reaches the running terms") {
    val f = new Fixture
    f.write = (_, _) => Left("invalid trading settings: small exceeds big")
    assert(!f.run(enter + backspace * 3 + "5000" + enter, esc, esc))
    assertEquals(f.written.size, 1)
    assertEquals(f.applied.size, 0)
    val refused = f.frames.find(_.last == "invalid trading settings: small exceeds big")
    assert(refused.exists(_.exists(_.startsWith("> Small amount         5000"))), f.frames)
    assertEquals(value(f.row("Small amount")), "$500")
  }

  test("a refused slippage shows the slippage rule and nothing is written") {
    val f = new Fixture
    assert(!f.run(down * 2 + enter + backspace * 4 + "2" + enter, esc, esc))
    assertEquals(f.written.size, 0)
    assert(f.frames.exists(_.last == SettingsFile.slippageRefusal))
  }

  test("an amount field takes digits and one dot only and Esc cancels it") {
    val f = new Fixture
    assert(!f.run(enter + backspace * 3 + "1a.2.3∫" + esc, esc))
    assert(f.frames.exists(_.exists(_.startsWith("> Small amount         1.23_"))), f.frames)
    assertEquals(f.written.size, 0)
    assertEquals(value(f.row("Small amount")), "$500")
  }

  test("an empty amount asks for one and writes nothing") {
    val f = new Fixture
    assert(!f.run(enter + backspace * 3 + enter, esc, esc))
    assertEquals(f.written.size, 0)
    assert(f.frames.exists(_.last == "Type an amount in dollars with digits and one dot"))
  }

  test("a write that overflows the stack shows the plain refusal and the screen keeps running") {
    val f = new Fixture
    f.write = (_, _) => throw new StackOverflowError()
    assert(!f.run(enter + backspace + enter, esc, esc))
    assert(f.frames.exists(_.last == "The settings file could not be read or written"))
    assertEquals(f.applied.size, 0)
  }

  test("an unreadable settings file shows its reason and refuses edits") {
    val f = new Fixture
    f.snapshot = Left("invalid trading settings: settings must be a private regular file")
    assert(!f.run(enter, esc))
    assert(f.last(2).contains("settings must be a private regular file"), f.last(2))
    assertEquals(f.notice, "invalid trading settings: settings must be a private regular file")
    assertEquals(f.written.size, 0)
  }

  // Keys

  test("the owner rows read the status and show its age without waiting for it") {
    val f = new Fixture
    assert(!f.run(esc))
    assertEquals(value(f.row("Owner")), "Reading owner status")
    assertEquals(value(f.row("Approval")), "Reading owner status")
    assertEquals(value(f.row("Funding")), "Reading owner status")
    assert(f.refreshes >= 1)
  }

  test("without the owner tooling the owner rows say so and Enter runs nothing there") {
    val f = new Fixture
    f.owned = Some((Left(Child.notInstalled), start))
    assert(!f.run(down * 4 + enter + down + enter + down + enter, esc))
    for label <- Vector("Owner", "Approval", "Funding") do
      assertEquals(value(f.row(label)), Child.notInstalled)
    assertEquals((f.owners.size, f.copies.size), (0, 0))
    assertEquals(f.notice, Child.notInstalled)
  }

  test("Enter on the owner row picks the step by phase") {
    def owning(phase: Option[String], candidates: Int = 1): Fixture =
      val f = new Fixture
      f.owned =
        Some((Right(status(phase = phase, candidates = candidates, message = "msg")), start))
      assert(!f.run(down * 4 + enter, esc))
      f
    assertEquals(owning(None).owners.toVector, Vector(Seq("owner", "new", "--network", "mainnet")))
    assertEquals(
      owning(Some("await-funding")).owners.toVector,
      Vector(Seq("owner", "resume", "--network", "mainnet"))
    )
    for (phase, candidates) <- Vector((Some("ready"), 1), (Some("odd"), 1), (None, 2)) do
      val f = owning(phase, candidates)
      assertEquals(f.owners.size, 0, phase)
      assertEquals(f.notice, "msg")
  }

  test("Enter on the approval row approves only a ready owner, a usable key and no approval") {
    def approving(
        s: OwnerStatus,
        key: Either[String, String] = Right("Main"),
        extra: Either[String, Seq[String]] = Right(Nil)
    ): Fixture =
      val f = new Fixture
      f.owned = Some((Right(s), start))
      f.key = key
      f.approve = extra
      assert(!f.run(down * 5 + enter, esc))
      f
    val words = Seq("owner", "approve", "--network", "mainnet")
    assertEquals(approving(status()).owners.toVector, Vector(words))
    assertEquals(
      approving(
        status(),
        extra = Right(Seq("--config", "/home/.config/desk/x.json"))
      ).owners.toVector,
      Vector(words ++ Seq("--config", "/home/.config/desk/x.json"))
    )
    val refused = Vector(
      approving(status(approved = Some(true))) -> "The trading key is already approved",
      approving(status(approved = None)) -> "Approval was not checked. Press r to read it again",
      approving(status(phase = Some("await-funding"))) -> "Make the owner first",
      approving(status(), key = Left("No active key")) -> "Add a trading key first",
      approving(status(), key = Left("The key was not read. Press r to ask again")) ->
        "Read the trading key first. Press r"
    )
    for (f, text) <- refused do
      assertEquals(f.owners.size, 0, text)
      assertEquals(f.notice, text)
    val outside = approving(status(), extra = Left("Approval needs the settings in ~/.config/desk"))
    assertEquals(outside.owners.size, 0)
    assertEquals(value(outside.row("Approval")), "Approval needs the settings in ~/.config/desk")
    assertEquals(outside.notice, "Approval needs the settings in ~/.config/desk")
  }

  test("a key that was not read keeps its own text on the trading key row") {
    val f = new Fixture
    f.key = Left("The key was not read. Press r to ask again")
    assert(!f.run(esc))
    assertEquals(value(f.row("Trading key")), "The key was not read. Press r to ask again")
  }

  test("Enter on the funding row copies the owner's address and says Desk never sends money") {
    val f = new Fixture
    f.owned = Some((Right(status()), start))
    assert(!f.run(down * 6 + enter, esc))
    assertEquals(f.copies.toVector, Vector(owner))
    assertEquals(
      f.notice,
      "Address copied. Fund it by a transfer to this address at the venue from an account you " +
        "control. Desk never sends money."
    )
    assert(f.last.forall(_.length <= 100), f.last)
    val none = new Fixture
    none.owned = Some((Right(status(ownerAddress = None, phase = None)), start))
    assert(!none.run(down * 6 + enter, esc))
    assertEquals(none.copies.size, 0)
    assertEquals(value(none.row("Funding")), "no owner yet")
    val failed = new Fixture
    failed.owned = Some((Right(status()), start))
    failed.copyResult = Left("Clipboard copy failed")
    assert(!failed.run(down * 6 + enter, esc))
    assertEquals(failed.notice, "Clipboard copy failed")
  }

  test("Enter on the trading key row runs the key child, then reads the owner and settings again") {
    val f = new Fixture
    f.owned = Some((Right(status()), start))
    assert(!f.run(down * 3 + enter, esc))
    assertEquals(f.keyScreens, 1)
    assertEquals(f.notice, "Done")
    assert(f.refreshes >= 2, f.refreshes)
    assertEquals(f.settingsReads, 2)
    for (result, text) <- Vector(
        Right(3) -> "The step did not complete (exit 3)",
        Left("The step did not start: missing") -> "The step did not start: missing",
        Left("Trading did not stop. Restart Desk") -> "Trading did not stop. Restart Desk"
      )
    do
      val g = new Fixture
      g.keyResult = result
      assert(!g.run(down * 3 + enter, esc))
      assertEquals(g.notice, text)
  }

  test("a pending live order refuses every child") {
    val f = new Fixture
    f.pending = true
    f.owned = Some((Right(status(phase = None)), start))
    assert(!f.run(down * 3 + enter, esc))
    assertEquals(f.notice, "Wait for the pending order")
    assert(!f.run(down * 4 + enter, esc))
    assertEquals(f.notice, "Wait for the pending order")
    // A reload can replace the live worker too, so r reads only the owner status then.
    assert(!f.run("r", esc))
    assertEquals(f.notice, "Wait for the pending order")
    assertEquals((f.keyScreens, f.owners.size, f.reloads), (0, 0, 0))
  }

  test("r reloads, reads the owner again and the settings again") {
    val f = new Fixture
    assert(!f.run("r", esc))
    assertEquals(f.reloads, 1)
    assertEquals(f.settingsReads, 2)
    assert(f.refreshes >= 2, f.refreshes)
    assertEquals(f.notice, "Read again")
    val refused = new Fixture
    refused.reload = () => Left("Restart Desk to change the network")
    assert(!refused.run("r", esc))
    assertEquals(refused.notice, "Restart Desk to change the network")
    val broken = new Fixture
    broken.reload = () => throw new IllegalStateException("keychain")
    assert(!broken.run("r", esc))
    assertEquals(broken.notice, "Reload failed")
  }

  // Key bindings

  private val quitRow = down * 7
  private val marketRow = down * 10

  test("a captured key is confirmed, checked, written and applied, and its row shows its name") {
    val f = new Fixture
    assert(!f.run(quitRow + enter + "\u000b" + enter + "\u000b", esc))
    val expected = Bindings.defaults
      .updated(Bind.Quit, Binding(Vector(KeyGroup(Vector(11)))))
      .toOption
      .get
    assertEquals(f.bindWrites.toVector, Vector(expected))
    assertEquals(f.bound.toVector, Vector(expected))
    assertEquals(value(f.row("Quit")), "Control K")
    assertEquals(f.notice, "Quit is now Control K")
    val prompts = f.frames.map(_.last)
    assert(prompts.contains("Press the key or keys for Quit. Esc cancels."), prompts)
    assert(
      prompts.contains(
        "Got Control K. Press another key to make a two-key sequence, or Enter to keep it."
      ),
      prompts
    )
    assert(prompts.contains("Press the same again to confirm."), prompts)
  }

  test("a two-key sequence and an Option character are stored as the exact bytes") {
    val f = new Fixture
    assert(!f.run(marketRow + enter + "gm" + "gm", esc))
    assertEquals(value(f.row("Market")), "g then m")
    val g = new Fixture
    assert(!g.run(down * 17 + enter + "∫" + enter + "∫", esc))
    assertEquals(
      g.bound.last.command(KeyGroup(Vector(0xe2, 0x88, 0xab))),
      Some(Command.Close)
    )
    assertEquals(value(g.row("Close")), "∫ Option B")
    val h = new Fixture
    assert(!h.run(down * 18 + enter + esc + "[15~" + enter + esc + "[15~", esc))
    assertEquals(value(h.row("Settings")), "F5")
  }

  test("a capture that does not match, conflicts or is reserved starts again and writes nothing") {
    val mismatch = new Fixture
    assert(!mismatch.run(quitRow + enter + "g" + enter + "h", esc, esc))
    assert(mismatch.frames.exists(_.last == "That did not match. Start again"))
    val conflict = new Fixture
    assert(!conflict.run(quitRow + enter + "p" + enter + "p", esc, esc))
    assert(conflict.frames.exists(_.last == "p is already the key for Prop"))
    val reserved = new Fixture
    assert(!reserved.run(quitRow + enter + "\t", esc, esc))
    assert(reserved.frames.exists(_.last == "Control I is Tab"))
    val dead = new Fixture
    assert(!dead.run(quitRow + enter + "é", esc, esc))
    assert(dead.frames.exists(_.last == "That key needs a second press on this layout"))
    for f <- Vector(mismatch, conflict, reserved, dead) do
      assertEquals((f.bindWrites.size, f.bound.size), (0, 0))
      assertEquals(value(f.row("Quit")), "q")
  }

  test("no byte of a capture reaches another field, and mouse reports are skipped") {
    val f = new Fixture
    val mouse = esc + "[<0;10;5M" + esc + "[<0;10;5m"
    assert(!f.run(quitRow + enter + mouse + "r" + mouse + enter + "r", esc))
    assertEquals(value(f.row("Quit")), "r")
    assertEquals((f.reloads, f.written.size, f.keyScreens, f.owners.size), (0, 0, 0, 0))
  }

  test("Esc cancels a capture and Control C quits from inside one") {
    val f = new Fixture
    assert(!f.run(quitRow + enter, esc, esc))
    assertEquals(f.bindWrites.size, 0)
    assert(new Fixture().run(quitRow + enter + "g" + interrupt))
    assert(new Fixture().run(enter + "1" + interrupt))
  }

  test("a refused bindings write keeps the old bindings") {
    val f = new Fixture
    f.bindWrite = _ => Left("local storage must be private (/somewhere/.config/desk)")
    assert(!f.run(quitRow + enter + "k" + enter + "k", esc))
    assertEquals(f.bound.size, 0)
    assertEquals(f.notice, "local storage must be private (/somewhere/.config/desk)")
    assertEquals(value(f.row("Quit")), "q")
  }

  // The owner status read

  test(
    "the status read runs at open, on r, after a child and every 15 seconds, never two at once"
  ) {
    val f = new Fixture
    val reads = new AtomicInteger()
    val running = new AtomicBoolean(false)
    val owner = new SettingsScreen.OwnerReads(
      () => {
        reads.incrementAndGet()
        Right(status())
      },
      () => f.clock,
      _.run(),
      running
    )
    val context =
      f.context.copy(ownerStatus = () => owner.status, refreshOwner = () => owner.refresh())
    var idles = 0
    // Thirty-one idle seconds, then r, then the key child, then Esc.
    val input = new InputStream:
      private val bytes = ("r" + down * 3 + enter + esc).getBytes(UTF_8)
      private var at = 0
      override def available(): Int = if idles < 31 then 0 else bytes.length - at + 1
      override def read(): Int =
        if at < bytes.length then
          at += 1
          bytes(at - 1) & 0xff
        else -1
    assert(
      !SettingsScreen.run(
        context,
        input,
        new PrintStream(f.out, true, UTF_8),
        () => {
          idles += 1
          f.clock += 1000L
        }
      )
    )
    // Open at 0 s, then 15 s and 30 s, then r and the child.
    assertEquals(reads.get(), 5)
    assertEquals(f.keyScreens, 1)
  }

  test("a slow status read never blocks the screen and a second read waits for the first") {
    val release = new CountDownLatch(1)
    val reads = new AtomicInteger()
    val running = new AtomicBoolean(false)
    def reader(): SettingsScreen.OwnerReads = new SettingsScreen.OwnerReads(
      () => {
        reads.incrementAndGet()
        // Bounded, so a read on the screen's own thread fails the timing check instead of hanging.
        val _ = release.await(5L, java.util.concurrent.TimeUnit.SECONDS)
        Right(status())
      },
      () => start,
      task => Threads.daemon("owner-status-test")(task.run()).start(),
      running
    )
    val first = reader()
    val f = new Fixture
    try
      val context =
        f.context.copy(ownerStatus = () => first.status, refreshOwner = () => first.refresh())
      val began = System.nanoTime()
      assert(
        !SettingsScreen.run(context, new Script(Vector(down * 3, esc)), new PrintStream(f.out))
      )
      assert(System.nanoTime() - began < 3000000000L)
      assertEquals(value(f.row("Owner")), "Reading owner status")
      Wait.until("the status read never started")(reads.get() == 1)
      first.refresh()
      val second = reader()
      second.refresh()
      Thread.sleep(50L)
      assertEquals(reads.get(), 1)
      release.countDown()
      Wait.until("the first read never finished")(first.status.nonEmpty)
      // The second screen's reference never takes the first screen's result.
      assertEquals(second.status, None)
      Wait.until("the read flag never cleared")(!running.get())
      second.refresh()
      Wait.until("the second screen never read")(second.status.nonEmpty)
      assertEquals(reads.get(), 2)
    finally release.countDown()
  }

  // The child hand-off

  private final class Exited(code: Int, events: ArrayBuffer[String]) extends Process:
    def getOutputStream: OutputStream = OutputStream.nullOutputStream()
    def getInputStream: InputStream = InputStream.nullInputStream()
    def getErrorStream: InputStream = InputStream.nullInputStream()
    def waitFor(): Int =
      events += "child"
      code
    def exitValue(): Int = code
    def destroy(): Unit = ()
    override def isAlive: Boolean = false

  private final class Handing(
      suspend: Either[String, Unit] = Right(()),
      exit: Int = 0,
      startFails: Boolean = false,
      reload: () => Either[String, Unit] = () => Right(()),
      rawFails: Boolean = false
  ):
    val events = ArrayBuffer.empty[String]
    val handOff = SettingsScreen.HandOff(
      suspend = () => { events += "suspend"; suspend },
      restore = () => { events += "restore"; () },
      start = command => {
        events += "start " + command.mkString(" ")
        if startFails then throw new java.io.IOException("missing program")
        new Exited(exit, events)
      },
      guard = body => {
        events += "handlers on"
        try body()
        finally events += "handlers off"
      },
      enter = () => { events += "enter line"; () },
      enterRaw = () => {
        events += "enter raw"
        if rawFails then throw new java.io.IOException("terminal settings unavailable")
      },
      reload = () => {
        events += "reload"
        reload()
      },
      resume = () => { events += "resume"; () }
    )

  test("the hand-off stops live, gives the terminal to the child and takes it back in order") {
    val h = new Handing()
    assertEquals(h.handOff.run(Seq("desk", "--keys")), Right(0))
    assertEquals(
      h.events.toVector,
      Vector(
        "suspend",
        "handlers on",
        "restore",
        "start desk --keys",
        "child",
        "enter line",
        "enter raw",
        "handlers off",
        "reload",
        "resume"
      )
    )
  }

  test("a child that cannot start gives the reason, takes the terminal back and resumes live") {
    val h = new Handing(startFails = true)
    assertEquals(h.handOff.run(Seq("desk")), Left("The step did not start: missing program"))
    assertEquals(
      h.events.toVector,
      Vector(
        "suspend",
        "handlers on",
        "restore",
        "start desk",
        "enter raw",
        "handlers off",
        "resume"
      )
    )
  }

  test("a retirement that does not finish runs no child and resumes live") {
    val h = new Handing(suspend = Left("Trading did not stop. Restart Desk"))
    assertEquals(h.handOff.run(Seq("desk")), Left("Trading did not stop. Restart Desk"))
    assertEquals(h.events.toVector, Vector("suspend", "resume"))
  }

  test("a nonzero exit says so after the reload") {
    val h = new Handing(exit = 2)
    assertEquals(h.handOff.run(Seq("desk")), Right(2))
    assertEquals(h.events.takeRight(2).toVector, Vector("reload", "resume"))
  }

  test("a reload that throws still resumes live and says Reload failed") {
    val h = new Handing(reload = () => throw new IllegalStateException("keychain"))
    assertEquals(h.handOff.run(Seq("desk")), Left("Reload failed"))
    assertEquals(h.events.last, "resume")
    val refused = new Handing(reload = () => Left("Restart Desk to change the network"))
    assertEquals(refused.handOff.run(Seq("desk")), Left("Restart Desk to change the network"))
  }

  test("a terminal that cannot be taken back stops the hand-off but still resumes live") {
    val h = new Handing(rawFails = true)
    intercept[java.io.IOException](h.handOff.run(Seq("desk")))
    assertEquals(h.events.count(_ == "enter raw"), 1)
    assertEquals(h.events.last, "resume")
    assert(!h.events.contains("reload"))
  }

  // Opening and leaving

  test("the screen pauses the workspace and leaves it with a full repaint") {
    val events = ArrayBuffer.empty[String]
    val result = SettingsScreen.around(
      paused => events += s"paused $paused",
      on => events += s"mouse $on",
      () => events += "clear",
      () => events += "reset",
      () => events += "invalidate"
    ) {
      events += "screen"
      7
    }
    assertEquals(result, 7)
    assertEquals(
      events.toVector,
      Vector(
        "paused true",
        "mouse false",
        "clear",
        "screen",
        "mouse true",
        "reset",
        "invalidate",
        "paused false"
      )
    )
    val failed = ArrayBuffer.empty[String]
    intercept[IllegalStateException](
      SettingsScreen.around(
        paused => failed += s"paused $paused",
        on => failed += s"mouse $on",
        () => failed += "clear",
        () => failed += "reset",
        () => failed += "invalidate"
      )(throw new IllegalStateException("stopped"))
    )
    // Desk is stopping. The failure handler restores the terminal, so nothing is drawn again.
    assertEquals(failed.toVector, Vector("paused true", "mouse false", "clear"))
  }

  test("the terminal size is read at most twice a second") {
    val f = new Fixture
    var sizes = 0
    var idles = 0
    val context = f.context.copy(size = () => { sizes += 1; (100, 30) })
    // Two idle seconds of 25 ms polls, then Esc.
    val input = new InputStream:
      override def available(): Int = if idles < 80 then 0 else 2
      private var sent = false
      override def read(): Int =
        if sent then -1
        else
          sent = true
          27
    assert(
      !SettingsScreen.run(
        context,
        input,
        new PrintStream(f.out, true, UTF_8),
        () => {
          idles += 1
          f.clock += 25L
        }
      )
    )
    assert(sizes >= 4 && sizes <= 6, sizes)
  }

  // The wiring Desk runs

  test("opening settings pauses drawing, turns the mouse off and clears, and leaving repaints") {
    val paused = new AtomicBoolean(false)
    val lock = new Object
    val shown = new ByteArrayOutputStream()
    val frames = new ByteArrayOutputStream()
    val terminal = new Term.Writer(new PrintStream(frames))
    terminal.write(Vector("chart", "buttons"), (100, 30))
    var invalidated = 0
    var inside = ""
    val quit = SettingsWiring.show(
      paused,
      lock,
      terminal,
      () => invalidated += 1,
      new PrintStream(shown, true, UTF_8)
    ) {
      inside = s"paused=${paused.get()} " + shown.toString(UTF_8)
      true
    }
    assert(quit)
    assertEquals(inside, s"paused=true ${esc}[?1006l${esc}[?1000l${esc}[2J")
    assert(shown.toString(UTF_8).endsWith(s"${esc}[?1000h${esc}[?1006h"))
    assert(!paused.get())
    assertEquals(invalidated, 1)
    frames.reset()
    terminal.write(Vector("chart", "buttons"), (100, 30))
    assert(frames.size() > 0, "the first frame after settings was not a full repaint")
  }

  test("the render pass waits while settings are open and drops a frame built as they opened") {
    val paused = new AtomicBoolean(true)
    val lock = new Object
    val log = ArrayBuffer.empty[String]
    SettingsWiring.renderPass(
      paused,
      () => true,
      lock,
      ms => {
        log += s"sleep $ms"
        if log.size == 3 then paused.set(false)
      }
    )(() => log += s"build paused=${paused.get()}")((_, _) => log += "write")
    assertEquals(
      log.toVector,
      Vector("sleep 50", "sleep 50", "sleep 50", "build paused=false", "write")
    )
    val opened = ArrayBuffer.empty[String]
    SettingsWiring.renderPass(paused, () => true, lock, _ => opened += "sleep")(() =>
      paused.set(true)
    )((_, _) => opened += "write")
    assertEquals(opened.toVector, Vector.empty[String])
  }

  test("the keys thread opens settings by the live binding and quits when settings ask") {
    def typed(
        text: String,
        bound: Bindings,
        quit: Boolean,
        take: Boolean = true
    ): (Vector[Command], Int) =
      val handled = ArrayBuffer.empty[Command]
      var opened = 0
      val input = new java.io.ByteArrayInputStream(text.getBytes(UTF_8))
      SettingsWiring.keys(
        input,
        () => input.available() > 0 && !handled.contains(Command.Quit),
        () => false,
        new java.util.concurrent.atomic.AtomicReference(bound),
        command => {
          handled += command
          take
        },
        () => {
          opened += 1
          quit
        }
      )
      (handled.toVector, opened)
    val z = Bindings.defaults
      .updated(Bind.Settings, Binding(Vector(KeyGroup(Vector('z'.toInt)))))
      .toOption
      .get
    assertEquals(typed("sc", z, false), (Vector(Command.ToggleCandles), 0))
    assertEquals(typed("zc", z, false), (Vector(Command.Settings, Command.ToggleCandles), 1))
    assertEquals(typed("zc", z, true), (Vector(Command.Settings, Command.Quit), 1))
    // A command the controller did not take opens nothing.
    assertEquals(typed("z", z, false, take = false), (Vector(Command.Settings), 0))
  }

  test(
    "the settings context keeps the live bindings and controller together and reads off thread"
  ) {
    val controller = new DeskController(
      PanelView(mode = "VIEW"),
      () => start,
      (_, _) => Left("no preview"),
      _ => false,
      _ => Right(())
    )
    val bound = new java.util.concurrent.atomic.AtomicReference(Bindings.defaults)
    val release = new CountDownLatch(1)
    val readers = ArrayBuffer.empty[String]
    val handed = ArrayBuffer.empty[Seq[String]]
    val context = SettingsWiring.context(
      network = "mainnet",
      config = None,
      home = Path.of("/nowhere"),
      bound = bound,
      controller = controller,
      keyRow = () => Right("Main"),
      reload = () => Right(()),
      ownerRead = () => {
        readers.synchronized(readers += Thread.currentThread().getName)
        val _ = release.await(5L, java.util.concurrent.TimeUnit.SECONDS)
        Right(status())
      },
      reading = new AtomicBoolean(false),
      handOff = command => {
        handed += command
        Right(0)
      },
      keyChild = Seq("java", "--keys"),
      env = Map.empty,
      copy = _ => Right(()),
      livePending = () => false,
      size = () => (100, 30),
      now = () => start,
      approve = Right(Nil)
    )
    try
      val chosen =
        Bindings.defaults.updated(Bind.Close, Binding(Vector(KeyGroup(Vector(11))))).toOption.get
      context.applyBindings(chosen)
      assertEquals((bound.get(), controller.panel.bindings), (chosen, chosen))
      val terms = TradeTerms(TradeSizes(BigDecimal(25)), BigDecimal("0.002"))
      context.applyTerms(terms)
      assertEquals(controller.panel.terms, terms)
      val began = System.nanoTime()
      context.refreshOwner()
      assert(System.nanoTime() - began < 1000000000L, "the status read ran on the caller's thread")
      Wait.until("the status read never started")(readers.synchronized(readers.nonEmpty))
      assertEquals(readers.synchronized(readers.toVector), Vector("owner-status"))
      assertEquals(context.ownerStatus(), None)
      release.countDown()
      Wait.until("the status read never finished")(context.ownerStatus().nonEmpty)
      assertEquals(context.runKeyScreen(), Right(0))
      assertEquals(
        context.runOwner(Seq("owner", "new", "--network", "mainnet")),
        Left(Child.notInstalled)
      )
      assertEquals(handed.toVector, Vector(Seq("java", "--keys")))
    finally release.countDown()
  }

  // The workspace side

  test("s closes a mode review and is handled, so the keys thread opens settings") {
    val controller = new DeskController(
      PanelView(mode = "VIEW", modeReview = Some("LIVE")),
      () => start,
      (_, _) => Left("no preview"),
      _ => false,
      _ => Right(())
    )
    val snapshot = Snapshot.initial("XMR", start)
    val handled = controller.handle(
      Command.Settings,
      snapshot,
      TradingView("XMR", message = "View only"),
      (100, 30),
      false
    )
    assert(handled)
    assertEquals(controller.panel.modeReview, None)
    val chosen =
      Bindings.defaults.updated(Bind.Close, Binding(Vector(KeyGroup(Vector(11))))).toOption.get
    controller.setBindings(chosen)
    assertEquals(controller.panel.bindings, chosen)
  }
