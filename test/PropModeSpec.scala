package desk

import java.nio.file.{Files, Path}
import java.time.ZoneOffset
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

class PropModeSpec extends TempDirectorySuite:
  private val now = 1800000000000L
  private val meta =
    """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""

  private def settings(text: String): Path =
    val path = tempDirectory("prop-settings-").resolve("settings.json")
    Files.writeString(path, text)
    Platform.setMode(path, "rw-------")
    path

  private def fields(value: Product): Map[String, Any] =
    value.productElementNames.zip(value.productIterator).toMap

  test("--prop chooses the prop mode, --paper is an unknown flag and the usage names --prop") {
    assertEquals(
      Main
        .parseArgs(Seq("XMR", "--prop"))
        .map(args => fields(args).collect { case (name, true) => name }.toVector),
      Right(Vector("prop"))
    )
    assertEquals(Main.parseArgs(Seq("XMR", "--paper")), Left("unknown flag --paper"))
    for other <- Vector("--trade", "--record", "--keys") do
      assertEquals(
        Main.parseArgs(Seq("--prop", other)),
        Left("choose one of prop, live, public recording or keys")
      )
    assert(Main.usage.contains("[--prop | --trade | --record]"), Main.usage)
    assert(!Main.usage.toLowerCase.contains("paper"), Main.usage)
  }

  test("--prop starts in PROP, --trade in LIVE and neither in VIEW") {
    assertEquals(Main.parseArgs(Seq("XMR", "--prop")).map(_.mode), Right("PROP"))
    assertEquals(Main.parseArgs(Seq("XMR", "--trade")).map(_.mode), Right("LIVE"))
    assertEquals(Main.parseArgs(Seq("XMR")).map(_.mode), Right("VIEW"))
  }

  test("settings accept propBalance and propFee and refuse paperBalance and paperFee") {
    val loaded = TradingConfig.load(Some(settings("""{"propBalance":"20000","propFee":"0.001"}""")))
    assertEquals(
      loaded.map(config => fields(config).view.filterKeys(_.startsWith("prop")).toMap),
      Right(Map("propBalance" -> BigDecimal("20000"), "propFee" -> BigDecimal("0.001")))
    )
    for old <- Vector("""{"paperBalance":"20000"}""", """{"paperFee":"0.001"}""") do
      assertEquals(
        TradingConfig.load(Some(settings(old))),
        Left("invalid trading settings: unknown setting")
      )
    assertEquals(
      TradingConfig.load(Some(settings("""{"propFee":"-1"}"""))),
      Left("invalid trading settings: invalid prop fee")
    )
    assertEquals(
      TradingConfig.load(Some(settings("""{"propBalance":"0"}"""))),
      Left("invalid trading settings: invalid prop balance")
    )
  }

  private val snapshot = Snapshot.initial("XMR", now)
  private val view = TradingView("XMR", message = "View only", rules = Some(MarketRules(0, 2, 5)))

  private def draw(panel: PanelView, cols: Int = 100, rows: Int = 30): WorkspaceFrame =
    Workspace.draw(snapshot, view, panel, cols, rows, now, ZoneOffset.UTC, Style.mono)

  private def controller: DeskController =
    new DeskController(PanelView(), () => now, (_, _) => Left("unused"), _ => false, _ => Right(()))

  test("key p opens the prop review and Enter confirms it under the PROP XMR heading") {
    val desk = controller
    val (_, command) = Keys.step(Vector.empty, 'p'.toInt, false, bindings = Bindings.defaults)
    assertEquals(command, Some(Command.Mode("PROP")))
    desk.publish(draw(desk.panel))
    desk.handle(command.get, snapshot, view, (100, 30), false)
    assertEquals(desk.panel.modeReview, Some("PROP"))
    desk.publish(draw(desk.panel))
    desk.handle(Command.Confirm, snapshot, view, (100, 30), false)
    assertEquals(desk.panel.mode, "PROP")
    val plain = draw(desk.panel).lines.map(Chart.plain)
    assert(plain.exists(_.contains("PROP XMR")), plain.mkString("\n"))
    assert(!plain.exists(_.toLowerCase.contains("paper")), plain.mkString("\n"))
  }

  test("the bottom row reads p Prop and a click on it opens the prop review") {
    for (cols, rows) <- Vector((100, 30), (120, 40)) do
      val desk = controller
      val frame = draw(desk.panel, cols, rows)
      val prop = frame.buttons.find(_.label == "p Prop")
      assert(prop.nonEmpty, frame.buttons.map(_.label).mkString(", "))
      assertEquals(prop.get.action, DeskAction.Mode("PROP"))
      assertEquals(prop.get.y, rows)
      assert(Chart.plain(frame.lines.last).contains("[p Prop]"))
      desk.publish(frame)
      desk.handle(
        Command.PointerPress(prop.get.x, prop.get.y, 0),
        snapshot,
        view,
        (cols, rows),
        false
      )
      desk.handle(Command.Pointer(prop.get.x, prop.get.y, 0), snapshot, view, (cols, rows), false)
      assertEquals(desk.panel.modeReview, Some("PROP"))
  }

  test("the frame drawn with a PROP mode review reads Use prop trading?") {
    for (cols, rows) <- Vector((100, 30), (120, 40)); mode <- Vector("VIEW", "LIVE") do
      val frame = draw(PanelView(mode = mode, modeReview = Some("PROP")), cols, rows)
      val plain = frame.lines.map(Chart.plain)
      assert(plain.exists(_.contains("Use prop trading?")), plain.mkString("\n"))
      assert(!plain.exists(_.contains("simulated")), plain.mkString("\n"))
      assert(frame.buttons.exists(_.action == DeskAction.ConfirmMode("PROP")))
  }

  test("the diagnostics category for the prop mode is prop") {
    final class Memory extends Diagnostics.Sink:
      private val saved = ArrayBuffer.empty[String]
      def append(line: String): Unit = synchronized { saved += line }
      def records: Vector[ujson.Value] = synchronized(saved.toVector.map(ujson.read(_)))
      def close(): Unit = ()
    val sink = new Memory
    val log = new Diagnostics(() => sink, () => now, 2000L)
    log.execution("PROP", TradingView("XMR", message = "Prop ready", ready = true))
    log.start()
    log.close()
    val events = sink.records.filter(_("event").str == "execution")
    assertEquals(events.map(_("mode").str), Vector("prop"))
    assertEquals(Diagnostics.component("prop-orders"), DiagnosticPart.Account)
  }

  private final class Route(initialMode: String, config: TradingConfig = TradingConfig()):
    val terms = TradeTerms(config.sizes, config.slippage)
    val clock = new AtomicLong(now)
    val directory = tempDirectory("prop-route-")
    val liveCalls = new AtomicInteger()
    private val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] =
        liveCalls.incrementAndGet()
        None
      def post(body: ujson.Value): Option[ujson.Value] =
        liveCalls.incrementAndGet()
        None
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        val _ = liveCalls.incrementAndGet()
    private val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some(meta)
    private val live = new TradingWorker(
      config,
      false,
      "XMR",
      exchange,
      () => fail("prop loaded a key"),
      () => fail("prop opened a live journal"),
      () => clock.get(),
      () => clock.get()
    )
    val route = new ExecutionRouter(
      config,
      initialMode,
      "XMR",
      live,
      _ => fail("prop created a live worker"),
      rest,
      directory,
      () => clock.get(),
      () => clock.get()
    )
    def publish(): Unit =
      val time = clock.incrementAndGet()
      route.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), time),
          true
        )
      )

  private def contents(directory: Path): Map[String, Vector[Byte]] =
    val files = Files.list(directory)
    try
      files.iterator.asScala
        .map(path => path.getFileName.toString -> Files.readAllBytes(path).toVector)
        .toMap
    finally files.close()

  test("prop keeps its wallet under prop/ and leaves an old paper wallet untouched and unread") {
    val loaded = TradingConfig.load(Some(settings("""{"propBalance":"20000","propFee":"0.001"}""")))
    assert(loaded.isRight, loaded.toString)
    val r = new Route("PROP", loaded.toOption.get)
    val oldWallet =
      """{"cash":"123","deposit":"123","realized":"0","fees":"0","positions":{"XMR":{"quantity":"1","entry":"100"}},"orders":[],"count":7}"""
    LocalFiles.write(r.directory.resolve("paper/wallet-mainnet.json"), oldWallet)
    val before = contents(r.directory.resolve("paper"))
    try
      r.route.start()
      r.route.observe(Event.SocketUp)
      r.publish()
      Wait.until("prop did not become ready")(r.route.view.ready)
      assertEquals(r.route.mode, "PROP")
      val intent = Trading.action(0)
      val plan = r.route.preview(intent, r.terms).fold(reason => fail(reason), identity)
      assert(
        r.route.submitReviewed(
          OrderReview(intent, plan, r.terms, r.clock.get(), r.clock.get() + 30000L)
        )
      )
      Wait.until("prop order did not finish")(r.route.view.outcome.nonEmpty)
      val receipt = r.route.view.outcome.get
      assert(receipt.startsWith("PROP XMR buy filled "), receipt)
      val saved = ujson.read(Files.readString(r.directory.resolve("prop/wallet-mainnet.json")))
      assertEquals(BigDecimal(saved("deposit").str), BigDecimal("20000"))
      assertEquals(saved("count").num.toLong, 1L)
      assertEquals(BigDecimal(saved("fees").str), plan.estimatedValue * BigDecimal("0.001"))
      assertEquals(contents(r.directory.resolve("paper")), before)
      assertEquals(r.liveCalls.get(), 0)
    finally r.route.close()
  }

  test("modes switch to prop by the PROP name only") {
    val r = new Route("VIEW")
    try
      assertEquals(r.route.switchMode("PAPER"), Left("Unknown trading mode"))
      assertEquals(r.route.mode, "VIEW")
      assertEquals(r.route.switchMode("PROP"), Right(()))
      assertEquals(r.route.mode, "PROP")
      assert(Files.isDirectory(r.directory.resolve("prop")))
      assert(!Files.exists(r.directory.resolve("paper")))
    finally r.route.close()
  }
