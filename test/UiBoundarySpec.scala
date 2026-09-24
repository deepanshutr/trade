package desk

class UiBoundarySpec extends munit.FunSuite:
  test("mouse press avoids a process but release rechecks resize before any order") {
    val f = new Fixture
    val buy = f.draw().buttons.find(_.action == DeskAction.Prepare(intent)).get
    WorkspaceApp.dispatchInput(
      Command.PointerPress(buy.x, buy.y, 0),
      f.controller,
      () => (snapshot, f.currentView),
      (120, 40),
      false,
      () => fail("press measured terminal")
    )
    var checked = false
    WorkspaceApp.dispatchInput(
      Command.Pointer(buy.x, buy.y, 0),
      f.controller,
      () => { assert(checked); (snapshot, f.currentView) },
      (120, 40),
      false,
      () => { checked = true; (119, 40) }
    )
    assertEquals(f.sent.size, 0)
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
    BigDecimal(5)
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
  private class Fixture(mode: String = "PAPER"):
    var currentView = view
    var sent = Vector.empty[OrderReview]
    var modes = Vector.empty[String]
    val controller = new DeskController(
      PanelView(mode = mode),
      () => now,
      _ => Right(plan),
      order => { sent :+= order; true },
      _ => fail("removed Ask callback"),
      name => { modes :+= name; Right("selected account") }
    )
    def draw(cols: Int = 120, rows: Int = 40): WorkspaceFrame =
      val frame =
        Workspace.draw(snapshot, currentView, controller.panel, cols, rows, now, zone, Style.mono)
      controller.publish(frame)
      frame
    def key(command: Command): Unit =
      controller.handle(command, snapshot, currentView, (120, 40), false)

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
        Workspace.draw(snapshot, view, PanelView(mode = "PAPER"), cols, 30, now, zone, style)
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
        do assertEquals(frame.hit(x, y, cols, 30, "XMR"), Some(button.action))
      assert(frame.lines.forall(line => Chart.plain(line).length == cols))
  }

  test("gold label and exact paired count survive the equal-width correlation pane") {
    val candles = (0 to 15).toVector.map { i =>
      val price = (i + 10).toDouble
      Candle(now - (16L - i) * 60000L, price, price + 1, price - 1, price, 1)
    }
    val data = snapshot.copy(
      candles = Map("1m" -> candles),
      refs = snapshot.refs.updated("PAXG", Ref.empty.copy(candles = Map("1m" -> candles)))
    )
    val frame = Workspace.draw(data, view, PanelView(), 100, 30, now, zone, Style.mono)
    assert(frame.lines.exists(_.startsWith("PAXG (token tied to gold)")))
    val pane = (Layout.workspaceMarketWidth(100) - 1) / 2
    val row = frame.lines.find(_.startsWith("TAPE")).get
    val correlation = row.slice(pane + 1, 2 * pane + 1).trim
    assert(correlation.startsWith("GOLD 15m"))
    assert(correlation.endsWith("+1.00 15/15"))
    assert(correlation.contains("⣿"))
  }

  test("terminal size is measured before context and a changed size refuses the old frame") {
    val f = new Fixture
    f.draw()
    var measured = false
    val result = WorkspaceApp.dispatchInput(
      Command.PlaceOrder(intent),
      f.controller,
      () => { assert(measured); (snapshot, f.currentView) },
      (120, 40),
      false,
      () => { measured = true; (119, 40) }
    )
    assertEquals(result._2, (119, 40))
    assertEquals(f.sent.size, 0)
    f.draw(119, 40)
    WorkspaceApp.dispatchInput(
      Command.PlaceOrder(intent),
      f.controller,
      () => (snapshot, f.currentView),
      (119, 40),
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
        Command.PromptText("23"),
        Command.PromptBackspace
      )
    do
      val result = WorkspaceApp.dispatchInput(
        command,
        f.controller,
        () => (snapshot.copy(prompt = Some("X")), f.currentView),
        (120, 40),
        true,
        () => fail("text edit requested terminal process")
      )
      assert(!result._1)
    assertEquals(f.sent.size, 0)
  }
