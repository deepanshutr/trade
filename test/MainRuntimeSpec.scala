package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import scala.collection.mutable.ArrayBuffer

class MainRuntimeSpec extends TempDirectorySuite:
  test("live one-click submission captures observed leverage and margin") {
    val now = 1800000000000L
    val snapshot = Snapshot.initial("XMR", now)
    val intent = OrderIntent(Side.Buy, OrderKind.Hard)
    val plan = OrderPlan(
      "XMR",
      0,
      Side.Buy,
      OrderKind.Hard,
      BigDecimal(5),
      BigDecimal(100),
      BigDecimal(100),
      BigDecimal(5),
      BigDecimal(500),
      Some(BigDecimal(100)),
      BigDecimal(5),
      false
    )
    val view = TradingView(
      "XMR",
      message = "Ready",
      ready = true,
      rules = Some(MarketRules(0, 2, 5)),
      leverage = Some(3),
      marginMode = Some("isolated")
    )
    var submitted = Option.empty[OrderReview]
    val controller = new DeskController(
      PanelView(mode = "LIVE"),
      () => now,
      _ => Right(plan),
      order => { submitted = Some(order); true },
      _ => Left("unused")
    )
    def draw(): WorkspaceFrame = Workspace.draw(
      snapshot,
      view,
      controller.panel,
      120,
      40,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    controller.publish(draw())
    controller.handle(Command.PlaceOrder(intent), snapshot, view, (120, 40), false)
    val review = submitted.get
    assertEquals(review.leverage, Some(3))
    assertEquals(review.marginMode, Some("isolated"))
    val frame = draw()
  }

  test("prop and recorder modes have explicit independent startup choices") {
    assert(
      Main.parseArgs(Seq("XMR", "--prop", "--state", "/tmp/desk-private")).isRight
    )
    assert(Main.parseArgs(Seq("--record")).isRight)
    assert(Main.parseArgs(Seq("--prop", "--trade")).isLeft)
    assert(Main.parseArgs(Seq("--record", "--trade")).isLeft)
    assert(Main.parseArgs(Seq("--record", "--prop")).isLeft)
    assert(Main.parseArgs(Seq("--state")).isLeft)
  }

  test("prop balance and fee can be configured without live addresses") {
    val directory = tempDirectory("desk-prop-settings")
    val path = directory.resolve("settings.json")
    Files.writeString(path, """{"propBalance":"20000","propFee":"0.001"}""")
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------"))
    assert(TradingConfig.load(Some(path)).isRight)
    Files.writeString(path, """{"propFee":"-1"}""")
    assert(TradingConfig.load(Some(path)).isLeft)
  }

  // Key start-up

  private val keyAddress = "0x" + "a" * 40
  private val ownerAddress = "0x" + "2" * 40
  private val keyBytes = Array.fill[Byte](32)(1)
  private val waiting = "Waiting for the Keychain prompt. Choose Allow, not Always Allow."

  private final class Stopped(val code: Int, val message: Option[String]) extends RuntimeException
  private final class Reached extends RuntimeException

  private final class Startup(settings: String = "{}"):
    val home: Path = tempDirectory("desk-startup")
    val state: Path = home.resolve("state")
    val config: Path = home.resolve("settings.json")
    Files.writeString(config, settings)
    Files.setPosixFilePermissions(config, PosixFilePermissions.fromString("rw-------"))
    val said = ArrayBuffer.empty[String]
    val screens = ArrayBuffer.empty[KeyScreen.Context]
    val reads = ArrayBuffer.empty[(TradingConfig, Int)]
    val securityCalls = ArrayBuffer.empty[Seq[String]]
    val loaders = ArrayBuffer.empty[(TradingConfig, Boolean, String, Path, () => Array[Byte])]
    val created = ArrayBuffer.empty[(TradingConfig, Boolean, String, Path)]
    var sourcesCalled = 0
    val present = scala.collection.mutable.Set.empty[String]
    var onScreen: KeyScreen.Context => Option[String] = _ => None
    var read: () => Array[Byte] = () => keyBytes.clone()

    def makeUsable(network: String = "testnet"): Unit =
      KeyNames.create(KeyNames.path(home), KeyEntry(keyAddress, "Main", network, 1759300000000L))
      ActiveKey.select(ActiveKey.path(home), network, keyAddress)
      present += keyAddress

    def hooks: KeyStartup.Hooks = KeyStartup.Hooks(
      home = home,
      screen = context => {
        screens += context
        onScreen(context)
      },
      security = args => {
        securityCalls += args
        if present.contains(args(args.indexOf("-a") + 1)) then 0 else 44
      },
      readKey = (c, wait) => {
        reads += ((c, wait))
        read()
      },
      worker = (c, enabled, coin, directory, load) => {
        loaders += ((c, enabled, coin, directory, load))
        throw new Reached
      },
      say = text => { said += text; () },
      stop = (code, message) => throw new Stopped(code, message)
    )

    def args(trade: Boolean = false, keys: Boolean = false, record: Boolean = false): Main.Args =
      Main.Args(
        "XMR",
        true,
        trade = trade,
        config = Some(config.toString),
        record = record,
        stateDirectory = Some(state.toString),
        keys = keys
      )

    private val connector = new Connector:
      def connect(onText: String => Unit, onClose: () => Unit): Socket =
        fail("the feed connected")
    private val rest = new Rest:
      def post(body: String): Option[String] = fail("a public request was made")

    def run(args: Main.Args): Unit =
      Main.run(
        args,
        _ => {
          sourcesCalled += 1
          (connector, rest)
        },
        (c, enabled, coin, directory) => {
          created += ((c, enabled, coin, directory))
          throw new Reached
        },
        keys = hooks
      )

  private def withOwner(network: String = "testnet"): String =
    s"""{"network":"$network","owner":"$ownerAddress","keyService":"desk-test"}"""

  test("the keys mode is its own choice and the usage line names it") {
    assertEquals(Main.parseArgs(Seq("--keys")), Right(Main.Args("XMR", false, keys = true)))
    assert(Main.parseArgs(Seq("--keys", "--trade")).isLeft)
    assert(Main.parseArgs(Seq("--prop", "--keys")).isLeft)
    assert(Main.parseArgs(Seq("--keys", "--record")).isLeft)
    assert(Main.usage.contains("--keys"))
  }

  test("--keys opens the key screen with the settings and exits before any feed or state") {
    val s = new Startup(withOwner())
    val stop = intercept[Stopped](s.run(s.args(keys = true)))
    assertEquals(stop.code, 0)
    assertEquals(stop.message, None)
    assertEquals(s.screens.size, 1)
    val context = s.screens.head
    assertEquals(context.network, "testnet")
    assertEquals(context.keyService, "desk-test")
    assertEquals(context.names, KeyNames.path(s.home))
    assertEquals(context.active, ActiveKey.path(s.home))
    assertEquals(context.stateDirectory, s.state.toAbsolutePath.normalize())
    assertEquals(s.sourcesCalled, 0)
    assertEquals(s.created.size, 0)
    assertEquals(s.reads.size, 0)
    assert(!Files.exists(s.state), "the key screen made workspace state")
  }

  test("--trade with no usable key opens the screen and leaves with No trading key") {
    val s = new Startup(withOwner())
    val stop = intercept[Stopped](s.run(s.args(trade = true)))
    assertEquals((stop.code, stop.message), (1, Some("No trading key")))
    assertEquals(s.screens.size, 1)
    assertEquals(s.created.size, 0)
    assertEquals(s.sourcesCalled, 0)
    assertEquals(s.reads.size, 0)
  }

  test("--trade continues into the workspace with a key chosen on the screen") {
    val s = new Startup("""{"network":"testnet"}""")
    s.onScreen = _ =>
      s.makeUsable()
      Some(keyAddress)
    intercept[Reached](s.run(s.args(trade = true)))
    assertEquals(s.screens.size, 1)
    assertEquals(s.created.size, 1)
    val (config, enabled, coin, _) = s.created.head
    assertEquals(config.agent, Some(keyAddress))
    assertEquals((enabled, coin), (true, "XMR"))
    assertEquals(s.reads.size, 0)
    assertEquals(s.said.toVector, Vector.empty[String])
  }

  test("--trade with a usable key does not open the screen") {
    val s = new Startup("""{"network":"testnet"}""")
    s.makeUsable()
    intercept[Reached](s.run(s.args(trade = true)))
    assertEquals(s.screens.size, 0)
    assertEquals(s.created.head._1.agent, Some(keyAddress))
  }

  test("a screen that stops with an error ends the start with its class name only") {
    val s = new Startup()
    s.onScreen = _ => throw new IllegalStateException("secret-looking text")
    val stop = intercept[Stopped](s.run(s.args(trade = true)))
    assertEquals((stop.code, stop.message), (1, Some("Key screen stopped: IllegalStateException")))
  }

  test(
    "an owner and a usable key read the key once, before the workspace, and each worker gets a copy"
  ) {
    val s = new Startup(withOwner())
    s.makeUsable()
    intercept[Reached](s.run(s.args(trade = true)))
    assertEquals(s.said.toVector, Vector(waiting))
    assertEquals(s.reads.size, 1)
    val (readConfig, wait) = s.reads.head
    assertEquals(wait, 120)
    assertEquals(readConfig.agent, Some(keyAddress))
    assertEquals(readConfig.owner, Some(ownerAddress))
    assertEquals(
      s.securityCalls.head,
      Seq("find-generic-password", "-s", "desk-test", "-a", keyAddress)
    )
    assertEquals(s.created.size, 0)
    assertEquals(s.loaders.size, 1)
    val (config, enabled, coin, directory, load) = s.loaders.head
    assertEquals(config.agent, Some(keyAddress))
    assertEquals((enabled, coin, directory), (true, "XMR", s.state.toAbsolutePath.normalize()))
    val one = load()
    val two = load()
    assertEquals(one.toSeq, keyBytes.toSeq)
    assertEquals(two.toSeq, keyBytes.toSeq)
    assert(!(one eq two))
    assertEquals(s.reads.size, 1)
    assertEquals(s.screens.size, 0)
  }

  test("the owner and the key also load for viewing so Live can start without a second prompt") {
    val s = new Startup(withOwner())
    s.makeUsable()
    intercept[Reached](s.run(s.args()))
    assertEquals(s.reads.size, 1)
    assertEquals(s.loaders.head._2, false)
  }

  test("a refused read starts the workspace with no key and asks only once") {
    val s = new Startup(withOwner())
    s.makeUsable()
    s.read = () => throw new IllegalStateException("denied")
    intercept[Reached](s.run(s.args(trade = true)))
    assertEquals(s.reads.size, 1)
    val load = s.loaders.head._5
    intercept[IllegalStateException](load())
    intercept[IllegalStateException](load())
    assertEquals(s.reads.size, 1)
    assertEquals(s.screens.size, 0)
  }

  test("a refused read leaves Live blocked with the existing reason and asks only once") {
    val s = new Startup(withOwner())
    s.makeUsable()
    s.read = () => throw new IllegalStateException("denied")
    val settings =
      TradingConfig(network = "testnet", owner = Some(ownerAddress), keyService = "desk-test")
    val prepared = KeyStartup.prepare(s.args(trade = true), settings, s.state, s.hooks)
    val session = prepared.session.get
    val clock = () => 1000L
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = body("type").str match
        case "meta" =>
          Some(ujson.read("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""))
        case "userRole" =>
          Some(ujson.Obj("role" -> "agent", "data" -> ujson.Obj("user" -> ownerAddress)))
        case _ => None
      def post(body: ujson.Value): Option[ujson.Value] = None
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit = ()
    val worker = new TradingWorker(
      prepared.config,
      true,
      "XMR",
      exchange,
      () => session.copy(),
      () => OrderJournal.open(s.state, prepared.config.identity),
      clock,
      clock,
      warmSigning = () => ()
    )
    try
      worker.start()
      Wait.until("Live never showed why it is blocked")(
        worker.view.block.contains("Protected signing key is unavailable or differs")
      )
      assertEquals(s.reads.size, 1)
    finally worker.close()
  }

  test("viewing with no owner never looks at the key files") {
    val s = new Startup("{}")
    s.makeUsable("mainnet")
    intercept[Reached](s.run(s.args()))
    assertEquals(s.securityCalls.size, 0)
    assertEquals(s.screens.size, 0)
    assertEquals(s.reads.size, 0)
    assertEquals(s.said.toVector, Vector.empty[String])
    assertEquals(s.created.head._1.agent, None)
  }

  test("an owner with no usable key starts the workspace without a key and without the screen") {
    val s = new Startup(withOwner())
    intercept[Reached](s.run(s.args()))
    assertEquals(s.screens.size, 0)
    assertEquals(s.reads.size, 0)
    assertEquals(s.said.toVector, Vector.empty[String])
    assertEquals(s.created.size, 1)
    assertEquals(s.created.head._1.agent, None)
  }

  test("a key chosen for another network is not usable here") {
    val s = new Startup(withOwner("testnet"))
    s.makeUsable("mainnet")
    intercept[Reached](s.run(s.args()))
    assertEquals(s.reads.size, 0)
    assertEquals(s.created.head._1.agent, None)
  }

  test("a start without a session never has a configured account, so no worker asks for a key") {
    for
      trade <- Seq(false, true)
      owner <- Seq(false, true)
      usable <- Seq(false, true)
    do
      val s = new Startup(if owner then withOwner() else """{"network":"testnet"}""")
      if usable then s.makeUsable()
      val settings = TradingConfig(
        network = "testnet",
        owner = Option.when(owner)(ownerAddress),
        keyService = "desk-test"
      )
      val label = s"trade=$trade owner=$owner usable=$usable"
      scala.util.Try(KeyStartup.prepare(s.args(trade = trade), settings, s.state, s.hooks)) match
        case scala.util.Success(prepared) =>
          assertEquals(prepared.session.nonEmpty, owner && usable, label)
          assert(prepared.session.nonEmpty || !prepared.config.configured, label)
        case scala.util.Failure(_: Stopped) => assert(trade && !usable, label)
        case scala.util.Failure(other)      => throw other
  }

  test("the loader of a worker made without a session refuses and never returns a key") {
    for _ <- 1 to 3 do
      val error = intercept[IllegalStateException](Main.noKey())
      assertEquals(error.getMessage, "no key was read at start")
  }

  test("a worker made without a session never gets a key and stays blocked and not ready") {
    val directory = tempDirectory("desk-keyless")
    val config =
      TradingConfig(network = "testnet", owner = Some(ownerAddress), agent = Some(keyAddress))
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = body("type").str match
        case "meta" =>
          Some(ujson.read("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""))
        case "userRole" =>
          Some(ujson.Obj("role" -> "agent", "data" -> ujson.Obj("user" -> ownerAddress)))
        case _ => None
      def post(body: ujson.Value): Option[ujson.Value] = None
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit = ()
    val worker = Main.keyless(config, true, "XMR", directory, exchange)
    try
      worker.start()
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99.99", "20", 1)), Vector(Level("100", "20", 1)), 1000L),
          true
        )
      )
      Wait.until("Live never showed why it is blocked")(
        worker.view.block.contains("Protected signing key is unavailable or differs")
      )
      Thread.sleep(300L)
      assert(!worker.view.ready)
      assertEquals(
        worker.view.block,
        Some("Protected signing key is unavailable or differs")
      )
    finally worker.close()
  }

  test("recording never reaches the key step") {
    val s = new Startup(withOwner())
    s.makeUsable()
    val config = TradingConfig(owner = Some(ownerAddress))
    val prepared = KeyStartup.prepare(s.args(record = true), config, s.state, s.hooks)
    assertEquals(prepared.config, config)
    assertEquals(prepared.session.isEmpty, true)
    assertEquals(s.securityCalls.size + s.screens.size + s.reads.size, 0)
  }

  test("closing the workspace closes the session key after the workers") {
    val order = ArrayBuffer.empty[String]
    def close(name: String): AutoCloseable = () => { order += name; () }
    val held = keyBytes.clone()
    val session = new SessionKey(() => held)
    assertEquals(session.load(), Right(()))
    val closer: AutoCloseable = () => { order += "session"; session.close() }
    WorkspaceApp.closeAll(
      close("outlook"),
      close("announcements"),
      None,
      close("worker"),
      close("diagnostics"),
      Some(closer)
    )
    assertEquals(
      order.toVector,
      Vector("outlook", "announcements", "worker", "diagnostics", "session")
    )
    assert(held.forall(_ == 0))
    intercept[IllegalStateException](session.copy())
  }

  test("live without setup says where to set up") {
    val directory = tempDirectory("desk-router")
    val config = TradingConfig()
    val clock = () => 1000L
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = None
      def post(body: ujson.Value): Option[ujson.Value] = None
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit = ()
    val live = new TradingWorker(
      config,
      false,
      "XMR",
      exchange,
      () => fail("a key was loaded"),
      () => fail("a journal was opened"),
      clock,
      clock
    )
    val rest = new Rest:
      def post(body: String): Option[String] = None
    val router = new ExecutionRouter(
      config,
      "VIEW",
      "XMR",
      live,
      _ => fail("a live worker was made"),
      rest,
      directory,
      clock,
      clock
    )
    try
      assertEquals(
        router.switchMode("LIVE"),
        Left(
          "Live setup required: owner in settings and an active key (desk --keys). See docs/trading.md"
        )
      )
    finally router.close()
  }
