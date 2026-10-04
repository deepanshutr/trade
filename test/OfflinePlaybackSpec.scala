package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

class OfflinePlaybackSpec extends TempDirectorySuite:
  override val munitTimeout = 90.seconds

  test(
    "run defaults keep the public clients, viewing worker and separate event queues"
  ) {
    def default(number: Int): Any =
      Main.getClass.getMethod(s"run$$default$$$number").invoke(Main)
    val sources = default(2).asInstanceOf[TradingConfig => (Connector, Rest)]
    val (socket, rest) = sources(TradingConfig())
    assertEquals(
      socket.asInstanceOf[SocketConnector].url,
      "wss://api.hyperliquid.xyz/ws"
    )
    assertEquals(
      rest.asInstanceOf[EndpointRest].url,
      "https://api.hyperliquid.xyz/info"
    )
    val first =
      default(4).asInstanceOf[java.util.concurrent.LinkedBlockingQueue[Event]]
    val second =
      default(4).asInstanceOf[java.util.concurrent.LinkedBlockingQueue[Event]]
    first.put(Event.Key(Command.Quit))
    assert(second.isEmpty)
    val create = default(3)
      .asInstanceOf[(TradingConfig, Boolean, String, Path) => TradingWorker]
    val directory = tempDirectory("desk-default-worker")
    val worker = create(TradingConfig(), false, "XMR", directory)
    try
      assertEquals(worker.view.coin, "XMR")
      assertEquals(worker.view.ready, false)
      assertEquals(worker.view.pending, false)
    finally worker.close()
  }

  test(
    "the application uses supplied public interfaces and worker construction"
  ) {
    final class Stop extends RuntimeException
    val connector = new Connector:
      def connect(receive: String => Unit, close: () => Unit): Socket =
        fail("startup reached the public connection before the worker factory")
    val rest = new Rest:
      def post(body: String): Option[String] =
        fail("startup made a public request")
    val directory = tempDirectory("desk-playback-config")
    val settings = directory.resolve("settings.json")
    Files.writeString(settings, "{}")
    Files.setPosixFilePermissions(
      settings,
      java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    )
    var constructed = 0
    intercept[Stop] {
      Main.run(
        Main.Args("ETH", true, config = Some(settings.toString)),
        config => {
          assertEquals(config, TradingConfig())
          (connector, rest)
        },
        (config, enabled, coin, journal) => {
          constructed += 1
          assertEquals(config, TradingConfig())
          assertEquals(enabled, false)
          assertEquals(coin, "ETH")
          assertEquals(journal, directory.resolve(".local/state/desk"))
          throw new Stop
        },
        keys = KeyStartup.Hooks(home = directory, stop = (code, _) => fail(s"Desk stopped ($code)"))
      )
    }
    assertEquals(constructed, 1)
  }

  private def run(
      command: Vector[String],
      directory: Path,
      timeout: FiniteDuration = 75.seconds
  ): Unit =
    val log = directory.resolve("runner.log")
    val process = new ProcessBuilder(command*)
      .redirectErrorStream(true)
      .redirectOutput(log.toFile)
      .start()
    try
      assert(
        process.waitFor(timeout.toNanos, TimeUnit.NANOSECONDS),
        s"playback watchdog, see $log"
      )
      assertEquals(process.exitValue(), 0, Files.readString(log))
    finally
      if process.isAlive then
        val descendants = process.descendants()
        val owned =
          try
            descendants.toArray.map(_.asInstanceOf[ProcessHandle]).toVector :+
              process.toHandle
          finally descendants.close()
        owned.foreach(_.destroyForcibly())
        val deadline = System.nanoTime() + 5.seconds.toNanos
        while owned.exists(_.isAlive) && System.nanoTime() < deadline do Thread.sleep(10)
        assert(owned.forall(!_.isAlive), s"playback cleanup watchdog, see $log")
  test(
    "the capture reader rejects incomplete frames and the held-work controls reject false passes"
  ) {
    run(
      Vector(
        "python3",
        "-m",
        "unittest",
        "discover",
        "-s",
        "test",
        "-p",
        "*playback_spec.py"
      ),
      tempDirectory("desk-capture-check", keepOnFailure = true)
    )
  }

  test(
    "actual terminal input, fold and UTF-8 frames keep working while public work waits"
  ) {
    val directory = tempDirectory("desk-playback-evidence", keepOnFailure = true)
    run(
      Vector(
        "python3",
        "tools/playback.py",
        "--java",
        Path.of(System.getProperty("java.home"), "bin/java").toString,
        "--classpath",
        System.getProperty("java.class.path"),
        "--output",
        directory.resolve("capture").toString
      ),
      directory
    )
  }

  test("the outer watchdog stops a child in a separate session") {
    val directory = tempDirectory("desk-outer-watchdog", keepOnFailure = true)
    val identity = directory.resolve("child.pid")
    val child =
      "import signal,time; signal.signal(signal.SIGTERM, signal.SIG_IGN); " +
        "print('ready', flush=True); time.sleep(60)"
    val driver =
      "import pathlib,subprocess,sys,time; " +
        "child=subprocess.Popen([sys.executable, '-c', sys.argv[2]], " +
        "stdout=subprocess.PIPE, start_new_session=True); " +
        "child.stdout.readline(); pathlib.Path(sys.argv[1]).write_text(str(child.pid)); " +
        "time.sleep(60)"
    try
      val error = intercept[munit.FailException] {
        run(
          Vector("python3", "-c", driver, identity.toString, child),
          directory,
          5.seconds
        )
      }
      assert(error.getMessage.contains("playback watchdog, see"))
      val pid = Files.readString(identity).toLong
      val descendant = ProcessHandle.of(pid)
      assert(descendant.isEmpty || !descendant.get().isAlive)
    finally
      if Files.exists(identity) then
        val descendant = ProcessHandle.of(Files.readString(identity).toLong)
        if descendant.isPresent && descendant.get().isAlive then
          descendant.get().destroyForcibly()
          descendant.get().onExit().get(5, TimeUnit.SECONDS)
  }

  test(
    "six hundred terminal clicks at ten per second record fills or explicit refusals"
      .tag(new munit.Tag("slow"))
  ) {
    val directory = tempDirectory("desk-click-speed-", keepOnFailure = true)
    val output = directory.resolve("capture")
    run(
      Vector(
        "python3",
        "tools/playback.py",
        "--java",
        Path.of(System.getProperty("java.home"), "bin/java").toString,
        "--classpath",
        System.getProperty("java.class.path"),
        "--output",
        output.toString,
        "--mode",
        "speed"
      ),
      directory,
      80.seconds
    )
  }
