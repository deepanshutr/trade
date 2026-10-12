package desk

class UiBoundarySpec extends munit.FunSuite:
  test("mouse press avoids a process but release rechecks resize before any order") {
    val f = new Fixture
    val buy = f.draw().buttons.find(_.action == DeskAction.Prepare(intent)).get
    f.emit(
      Command.PointerPress(buy.x, buy.y, 0),
      () => (snapshot, f.currentView),
      false,
      () => fail("press measured terminal")
    )
    var checked = false
    f.emit(
      Command.Pointer(buy.x, buy.y, 0),
      () => { assert(checked); (snapshot, f.currentView) },
      false,
      () => { checked = true; (119, 40) }
    )
    assertEquals(f.sent.size, 0)
  }

  test("typing, presses, wheel reports and display toggles do not measure the terminal") {
    for command <- Vector(
        Command.PromptChar('x'),
        Command.PromptBackspace,
        Command.PointerPress(1, 1, 0),
        Command.Pointer(1, 1, 64),
        Command.Pointer(1, 1, 65),
        Command.ToggleMute,
        Command.ToggleCandles
      )
    do
      var measurements = 0
      WorkspaceApp.measureInput(command, () => { measurements += 1; (120, 40) })
      assertEquals(measurements, 0, command.toString)
  }

  test("viewport-dependent input measures once, so a pan after a resize uses the new size") {
    for command <- Vector(
        Command.ZoomIn,
        Command.ZoomOut,
        Command.ScrubForward,
        Command.ScrubBack,
        Command.Pointer(1, 1, 0),
        Command.PlaceOrder(intent),
        Command.Confirm,
        Command.Mode("LIVE")
      )
    do
      var measurements = 0
      WorkspaceApp.measureInput(command, () => { measurements += 1; (120, 40) })
      assertEquals(measurements, 1, command.toString)
  }

  private val now = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val intent = OrderIntent(Side.Buy, OrderKind.Hard)
  private val plan = OrderPlan(
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
  private val view = TradingView(
    "XMR",
    message = "Ready",
    ready = true,
    rules = Some(MarketRules(0, 2, 5)),
    bid = Some(BigDecimal(99)),
    ask = Some(BigDecimal(100)),
    leverage = Some(3),
    marginMode = Some("isolated")
  )
  private val zone = java.time.ZoneOffset.UTC
  private class Fixture(mode: String = "PROP"):
    var currentView = view
    var sent = Vector.empty[OrderReview]
    var previewed = Vector.empty[TradeTerms]
    var modes = Vector.empty[String]
    val controller = new DeskController(
      PanelView(mode = mode),
      () => now,
      (_, terms) => { previewed :+= terms; Right(plan) },
      order => { sent :+= order; true },
      name => { modes :+= name; Right(()) }
    )
    var size = (120, 40)
    def emit(
        command: Command,
        context: () => (Snapshot, TradingView),
        promptOpen: Boolean,
        measure: () => (Int, Int)
    ): Boolean =
      val dimensions = new TerminalSize(
        measure,
        measured => {
          if measured != size then controller.invalidate()
          size = measured
        }
      )
      WorkspaceApp.measureInput(command, () => dimensions.read())
      WorkspaceApp.dispatchInput(command, controller, context, size, promptOpen)
    def draw(cols: Int = 120, rows: Int = 40): WorkspaceFrame =
      val frame =
        Workspace.draw(snapshot, currentView, controller.panel, cols, rows, now, zone, Style.mono)
      controller.publish(frame)
      frame
    def key(command: Command): Unit =
      controller.handle(command, snapshot, currentView, (120, 40), false)

  test("a press previews with the panel terms, its review carries them, and new terms apply next") {
    val f = new Fixture()
    val start = f.controller.panel.terms
    f.draw()
    f.key(Command.PlaceOrder(intent))
    val chosen = TradeTerms(TradeSizes(BigDecimal(20), BigDecimal(100)), BigDecimal("0.005"))
    f.controller.setTerms(chosen)
    assertEquals(f.controller.panel.terms, chosen)
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.sent.size, 1, "a press on a frame drawn with the old sizes was sent")
    val labels = f.draw().buttons.map(_.label)
    assert(labels.contains("$20 *") && labels.contains("1 LONG $20 x3"), labels)
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.previewed, Vector(start, chosen))
    assertEquals(f.sent.map(_.terms), Vector(start, chosen))
  }

  test("one-click live submission captures the current observed leverage and margin") {
    val f = new Fixture("LIVE")
    f.draw()
    f.currentView = view.copy(leverage = Some(2), marginMode = Some("cross"))
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.sent.size, 1)
    assertEquals(f.sent.head.leverage, Some(2))
    assertEquals(f.sent.head.marginMode, Some("cross"))
  }

  for (name, leverage, margin, rules) <- Vector(
      ("missing leverage", None, Some("isolated"), view.rules),
      ("missing margin", Some(3), None, view.rules),
      ("zero leverage", Some(0), Some("cross"), view.rules),
      ("negative leverage", Some(-1), Some("cross"), view.rules),
      ("excess leverage", Some(6), Some("cross"), view.rules),
      ("unknown margin", Some(3), Some("unknown"), view.rules),
      ("missing rules", Some(3), Some("cross"), None),
      ("zero maximum", Some(3), Some("cross"), Some(MarketRules(0, 2, 0)))
    )
  do
    test("invalid current live settings prevent submission: " + name) {
      val f = new Fixture("LIVE")
      f.draw()
      f.currentView = view.copy(leverage = leverage, marginMode = margin, rules = rules)
      f.key(Command.PlaceOrder(intent))
      assertEquals(f.sent.size, 0)
      assert(f.controller.panel.actionNotice.nonEmpty)
    }

  test("Buy and Sell occupy separate complete targets at every supported width") {
    for cols <- Vector(100, 101, 120, 160); style <- Vector(Style.mono, Style.colour) do
      val frame =
        Workspace.draw(snapshot, view, PanelView(mode = "PROP"), cols, 30, now, zone, style)
      val buttons = frame.buttons.filter(_.action.isInstanceOf[DeskAction.Prepare])
      assertEquals(buttons.size, 2)
      assert(buttons.head.x + buttons.head.width <= buttons.last.x)
      for button <- buttons do
        val rendered = frame.lines
          .slice(button.y - 1, button.y + button.height - 1)
          .map(Chart.plain)
          .map(_.slice(button.x - 1, button.x - 1 + button.width))
        assert(rendered.exists(_.contains(button.label)))
        for
          y <- button.y until button.y + button.height; x <- button.x until button.x + button.width
        do assertEquals(frame.buttons.find(_.contains(x, y)).map(_.action), Some(button.action))
      assert(frame.lines.forall(line => Chart.plain(line).length == cols))
  }

  test("terminal size is measured before context and a changed size refuses the old frame") {
    val f = new Fixture
    f.draw()
    var measured = false
    val result = f.emit(
      Command.PlaceOrder(intent),
      () => { assert(measured); (snapshot, f.currentView) },
      false,
      () => { measured = true; (119, 40) }
    )
    assert(result)
    assertEquals(f.size, (119, 40))
    assertEquals(f.sent.size, 0)
    f.draw(119, 40)
    f.emit(
      Command.PlaceOrder(intent),
      () => (snapshot, f.currentView),
      false,
      () => (119, 40)
    )
    assertEquals(f.sent.size, 1)
  }

  test("a market prompt edit avoids a terminal process and never submits an order") {
    val f = new Fixture
    f.draw()
    for command <- Vector(
        Command.PromptChar('1'),
        Command.PromptBackspace
      )
    do
      val result = f.emit(
        command,
        () => (snapshot.copy(prompt = Some("X")), f.currentView),
        true,
        () => fail("text edit requested terminal process")
      )
      assert(!result)
    assertEquals(f.sent.size, 0)
  }
