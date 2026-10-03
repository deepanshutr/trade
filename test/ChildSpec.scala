package desk

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  IOException,
  InputStream,
  OutputStream,
  PrintStream
}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import sun.misc.{Signal, SignalHandler}

class ChildSpec extends TempDirectorySuite:
  private val java = "/opt/jdk/bin/java"
  private val jar = "/home/test/.local/lib/desk.jar"
  private val python = "/opt/tools/bin/python3"
  // A vault folder with an empty desk_vault.py. Nothing runs it.
  private lazy val folder =
    val directory = suiteDirectory("child-vault")
    Files.write(directory.resolve("desk_vault.py"), Array.emptyByteArray)
    directory.toString
  private lazy val script = folder + "/desk_vault.py"
  private lazy val env = Map("DESK_PYTHON" -> python, "DESK_VAULT" -> folder)
  private val notInstalled = "Owner tooling is not installed"

  private def args(config: Option[String] = None, state: Option[String] = None): Main.Args =
    Main.Args("XMR", mono = false, config = config, stateDirectory = state)

  test("the key child is the same Java and jar with attach off and --keys") {
    assertEquals(
      Child.keyScreen(args(), java, jar),
      Seq(java, "-XX:+DisableAttachMechanism", "-jar", jar, "--keys")
    )
  }

  test("the key child passes the parent's settings and state paths only when it had them") {
    val config = "/home/test/desk/config.json"
    val state = "/home/test/desk/state"
    val base = Seq(java, "-XX:+DisableAttachMechanism", "-jar", jar, "--keys")
    assertEquals(
      Child.keyScreen(args(Some(config), Some(state)), java, jar),
      base ++ Seq("--config", config, "--state", state)
    )
    assertEquals(
      Child.keyScreen(args(config = Some(config)), java, jar),
      base :+ "--config" :+ config
    )
    assertEquals(Child.keyScreen(args(state = Some(state)), java, jar), base :+ "--state" :+ state)
  }

  test("the key child takes no other flag, market or mode from the parent") {
    val parent = Main.Args("BTC", mono = true, trade = true, record = false, audio = "mute")
    assertEquals(
      Child.keyScreen(parent, java, jar),
      Seq(java, "-XX:+DisableAttachMechanism", "-jar", jar, "--keys")
    )
  }

  test("the vault runs the four owner entry points through DESK_PYTHON -B and DESK_VAULT") {
    val steps = Vector(
      Seq("--json", "owner", "status", "--network", "mainnet"),
      Seq("owner", "new", "--network", "testnet"),
      Seq("owner", "resume", "--network", "mainnet"),
      Seq("owner", "approve", "--network", "mainnet"),
      Seq("owner", "approve", "--network", "testnet", "--config", "/home/test/config.json")
    )
    steps.foreach { words =>
      assertEquals(Child.vault(env, words), Right(Seq(python, "-B", script) ++ words))
    }
    // A folder written with a trailing slash names the same script.
    assertEquals(
      Child.vault(env + ("DESK_VAULT" -> (folder + "/")), steps.head),
      Right(Seq(python, "-B", script) ++ steps.head)
    )
  }

  test("nothing runs without an absolute DESK_PYTHON and DESK_VAULT") {
    val words = Seq("owner", "new", "--network", "mainnet")
    Vector(
      Map.empty[String, String],
      Map("DESK_PYTHON" -> python),
      Map("DESK_VAULT" -> folder),
      env + ("DESK_VAULT" -> ""),
      env + ("DESK_PYTHON" -> ""),
      env + ("DESK_VAULT" -> "vault"),
      env + ("DESK_VAULT" -> "./vault"),
      env + ("DESK_PYTHON" -> "python3")
    ).foreach { environment =>
      assertEquals(Child.vault(environment, words), Left(notInstalled), environment.toString)
    }
  }

  test("nothing runs when DESK_VAULT holds no desk_vault.py as a regular file") {
    val words = Seq("owner", "new", "--network", "mainnet")
    val empty = tempDirectory("child-empty")
    val folderNamed = tempDirectory("child-folder-named")
    Files.createDirectory(folderNamed.resolve("desk_vault.py"))
    Vector(empty.toString, folderNamed.toString, empty.resolve("missing").toString).foreach {
      vault =>
        assertEquals(Child.vault(env + ("DESK_VAULT" -> vault), words), Left(notInstalled), vault)
    }
  }

  test("the vault runs no command other than the four owner entry points") {
    Vector(
      Seq.empty[String],
      Seq("status"),
      Seq("owner", "status", "--network", "mainnet"),
      Seq("--json", "owner", "new", "--network", "mainnet"),
      Seq("owner", "rotate", "--network", "mainnet"),
      Seq("owner", "new", "--network", "devnet"),
      Seq("owner", "new", "--network", "mainnet", "--config", "/home/test/config.json"),
      Seq("owner", "approve", "--network", "mainnet", "--config", ""),
      Seq("owner", "approve", "--network", "mainnet", "--state", "/home/test/state"),
      Seq("owner", "new", "--network", "mainnet", "extra"),
      Seq("--json", "owner", "status", "--network")
    ).foreach { words =>
      assert(Child.vault(env, words).isLeft, words.toString)
    }
  }

  private final class FakeProcess(exit: Int, interrupted: Boolean = false) extends Process:
    @volatile var destroyed = false
    def getOutputStream: OutputStream = OutputStream.nullOutputStream()
    def getInputStream: InputStream = InputStream.nullInputStream()
    def getErrorStream: InputStream = InputStream.nullInputStream()
    def waitFor(): Int =
      if interrupted then throw new InterruptedException("stopping")
      exit
    def exitValue(): Int = exit
    def destroy(): Unit = destroyed = true
    override def isAlive: Boolean = interrupted && !destroyed

  private final class Recorder:
    var events = Vector.empty[String]
    def guard(body: () => Unit): Unit =
      events :+= "guard on"
      try body()
      finally events :+= "guard off"
    def start(process: => Process)(command: Seq[String]): Process =
      events :+= "start " + command.mkString(" ")
      process
    def enter(): Unit = events :+= "enter"

  private lazy val command = Seq(python, "-B", script, "owner", "new", "--network", "mainnet")

  test("the child starts and is waited for inside the guard, then Enter, then the guard ends") {
    Vector(0, 3).foreach { exit =>
      val recorder = new Recorder
      val outcome = Child.runOnTerminal(
        command,
        recorder.start(new FakeProcess(exit)),
        recorder.guard,
        () => recorder.enter()
      )
      assertEquals(outcome, Right(exit))
      assertEquals(
        recorder.events,
        Vector("guard on", "start " + command.mkString(" "), "enter", "guard off")
      )
    }
  }

  test("a child that cannot start gives the reason, no Enter line, and the guard still ends") {
    val recorder = new Recorder
    val outcome = Child.runOnTerminal(
      command,
      recorder.start(throw new IOException("Cannot run program: No such file or directory")),
      recorder.guard,
      () => recorder.enter()
    )
    assertEquals(
      outcome,
      Left("The step did not start: Cannot run program: No such file or directory")
    )
    assertEquals(recorder.events, Vector("guard on", "start " + command.mkString(" "), "guard off"))
  }

  test("no child starts when the guard cannot be put in place") {
    val recorder = new Recorder
    val failing: (() => Unit) => Unit = _ => throw new IllegalArgumentException("signal in use")
    intercept[IllegalArgumentException] {
      Child.runOnTerminal(
        command,
        recorder.start(new FakeProcess(0)),
        failing,
        () => recorder.enter()
      )
    }
    assertEquals(recorder.events, Vector.empty)
  }

  test("a guard that never runs its body starts nothing and reports it") {
    val recorder = new Recorder
    val outcome =
      Child.runOnTerminal(
        command,
        recorder.start(new FakeProcess(0)),
        _ => (),
        () => recorder.enter()
      )
    assertEquals(outcome, Left("The step did not start"))
    assertEquals(recorder.events, Vector.empty)
  }

  test("a wait that is interrupted stops the child and still ends the guard") {
    val recorder = new Recorder
    val process = new FakeProcess(0, interrupted = true)
    // munit's intercept passes InterruptedException on, so it is caught here.
    val thrown =
      try
        Child.runOnTerminal(
          command,
          recorder.start(process),
          recorder.guard,
          () => recorder.enter()
        )
        None
      catch case error: InterruptedException => Some(error)
    assert(thrown.nonEmpty, "the interrupt did not reach the caller")
    assert(process.destroyed, "the child outlived the wait")
    assertEquals(recorder.events, Vector("guard on", "start " + command.mkString(" "), "guard off"))
  }

  // A fake signal table: handle installs a handler and returns the one it replaced.
  private final class Table(original: SignalHandler):
    var current = original
    var installed = Vector.empty[SignalHandler]
    def handle(handler: SignalHandler): SignalHandler =
      val previous = current
      current = handler
      installed :+= handler
      previous

  private val original: SignalHandler = _ => fail("the original handler was called")

  test("the guard swaps in a handler that does nothing and puts the original back") {
    val table = new Table(original)
    var during = Option.empty[SignalHandler]
    Child.ignoring(table.handle)(() => during = Some(table.current))
    assert(during.exists(_ ne original), "the body ran with the original handler")
    during.foreach(_.handle(new Signal("INT")))
    assert(table.current eq original, "the original handler was not put back")
    assertEquals(table.installed.size, 2)
  }

  test("the guard puts the original handler back when the body throws") {
    val table = new Table(original)
    intercept[IllegalStateException] {
      Child.ignoring(table.handle)(() => throw new IllegalStateException("child failed"))
    }
    assert(table.current eq original, "the original handler was not put back")
  }

  // Reads the JVM's current SIGINT handler by swapping a probe in and the handler straight back.
  private def currentInt(): SignalHandler =
    val signal = new Signal("INT")
    val probe: SignalHandler = _ => ()
    val found = Signal.handle(signal, probe)
    val _ = Signal.handle(signal, found)
    found

  test("ignoreSignals swaps Desk's SIGINT handler during the body and restores it after") {
    // A JVM handler read twice comes back as two wrappers, so a known handler stands in for it.
    val signal = new Signal("INT")
    val desk: SignalHandler = _ => ()
    val jvm = Signal.handle(signal, desk)
    try
      var during = Option.empty[SignalHandler]
      Child.ignoreSignals(() => during = Some(currentInt()))
      assert(during.exists(_ ne desk), "SIGINT kept Desk's handler while the child ran")
      assert(currentInt() eq desk, "SIGINT was not restored")
      intercept[IllegalStateException] {
        Child.ignoreSignals(() => throw new IllegalStateException("child failed"))
      }
      assert(currentInt() eq desk, "SIGINT was not restored after a throw")
    finally
      val _ = Signal.handle(signal, jvm)
  }

  test("the JVM keeps SIGQUIT for itself, so Control backslash only prints a thread dump") {
    // Reopen trigger: when a JDK lets Java handle SIGQUIT, this fails and the guard can take it.
    val signal = new Signal("QUIT")
    val accepted = scala.util.Try(Signal.handle(signal, _ => ()))
    accepted.foreach(previous => Signal.handle(signal, previous))
    assert(accepted.failed.toOption.exists(_.isInstanceOf[IllegalArgumentException]))
  }

  test("awaitEnter asks for Enter and reads through the first line only") {
    val output = new ByteArrayOutputStream()
    val input = new ByteArrayInputStream("typed\nlater".getBytes(UTF_8))
    Child.awaitEnter(input, new PrintStream(output, true, UTF_8))
    assertEquals(output.toString(UTF_8), "Press Enter to return to Desk.")
    assertEquals(new String(input.readAllBytes(), UTF_8), "later")
  }

  test("awaitEnter returns at the end of input") {
    val output = new ByteArrayOutputStream()
    Child.awaitEnter(InputStream.nullInputStream(), new PrintStream(output, true, UTF_8))
    assertEquals(output.toString(UTF_8), "Press Enter to return to Desk.")
  }
