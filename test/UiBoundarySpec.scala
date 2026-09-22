package desk

class UiBoundarySpec extends munit.FunSuite:
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
  private val zone = java.time.ZoneId.of("UTC")

  private class Fixture(mode: String = "PAPER"):
    var currentView =
      if mode == "LIVE" then view else view.copy(leverage = Some(1), marginMode = Some("paper"))
    var sent = Vector.empty[OrderReview]
    var modes = Vector.empty[String]
    var questions = Vector.empty[String]
    var measurements = 0
    val controller = new DeskController(
      PanelView(mode = mode),
      () => now,
      _ => Right(plan),
      review => { sent :+= review; true },
      question => { questions :+= question; true },
      name => { modes :+= name; Right("selected account") }
    )
    def draw(cols: Int = 120, rows: Int = 40): WorkspaceFrame =
      val frame =
        Workspace.draw(snapshot, currentView, controller.panel, cols, rows, now, zone, Style.mono)
      controller.publish(frame)
      frame
    def key(command: Command): Unit =
      val _ = controller.handle(command, snapshot, currentView, (120, 40), false)
    def prepare(): WorkspaceFrame =
      draw()
      key(Command.PlaceOrder(intent))
      draw()
    def dispatch(
        command: Command,
        size: (Int, Int) = (120, 40),
        measured: (Int, Int) = (119, 40)
    ): (Boolean, (Int, Int)) =
      WorkspaceApp.dispatchInput(
        command,
        controller,
        () => (snapshot, currentView),
        size,
        false,
        () => { measurements += 1; measured }
      )

  test("live review retains captured settings after the current view changes") {
    val f = new Fixture("LIVE")
    f.prepare()
    val review = f.controller.panel.review.get
    assertEquals(review.leverage, Some(3))
    assertEquals(review.marginMode, Some("isolated"))
    f.currentView = view.copy(leverage = Some(9), marginMode = Some("cross"))
    val frame = f.draw()
    assert(frame.completeReview)
    assert(frame.lines.exists(_.contains("3x isolated")))
    assert(!frame.lines.exists(_.contains("9x cross")))
  }

  private val incompleteSettings = Vector(
    ("missing leverage", None, Some("isolated")),
    ("missing margin", Some(3), None),
    ("zero leverage", Some(0), Some("cross")),
    ("negative leverage", Some(-1), Some("cross")),
    ("leverage above the market maximum", Some(6), Some("cross")),
    ("unknown margin", Some(3), Some("unknown"))
  )
  for (name, leverage, margin) <- incompleteSettings do
    test("live review with " + name + " is incomplete and cannot claim paper settings") {
      val review = OrderReview(intent, plan, now, now + 30000L, leverage, margin)
      val frame = Workspace.draw(
        snapshot,
        view,
        PanelView(mode = "LIVE", review = Some(review)),
        120,
        40,
        now,
        zone,
        Style.mono
      )
      assert(!frame.lines.exists(_.contains("1x paper")))
      assert(!frame.completeReview)
      assert(!frame.buttons.exists(_.action == DeskAction.Submit))
    }

  for rules <- Vector(None, Some(MarketRules(0, 2, 0))) do
    test("live review requires an available positive market maximum: " + rules) {
      val review = OrderReview(intent, plan, now, now + 30000L, Some(3), Some("isolated"))
      val frame = Workspace.draw(
        snapshot,
        view.copy(rules = rules),
        PanelView(mode = "LIVE", review = Some(review)),
        120,
        40,
        now,
        zone,
        Style.mono
      )
      assert(!frame.completeReview)
      assert(!frame.buttons.exists(_.action == DeskAction.Submit))
    }

  test("live fees stay unknown and paper uses its configured fee") {
    val review = OrderReview(intent, plan, now, now + 30000L, Some(3), Some("isolated"))
    val live = Workspace.draw(
      snapshot,
      view,
      PanelView(mode = "LIVE", review = Some(review), fee = BigDecimal("0.002")),
      120,
      40,
      now,
      zone,
      Style.mono
    )
    assert(live.lines.exists(_.contains("Fee unknown. Value excludes fees")))
    assert(!live.lines.exists(_.contains("Estimated fee")))
    val paper = Workspace.draw(
      snapshot,
      view,
      PanelView(
        mode = "PAPER",
        review = Some(review.copy(leverage = None, marginMode = None)),
        fee = BigDecimal("0.002")
      ),
      120,
      40,
      now,
      zone,
      Style.mono
    )
    assert(paper.lines.exists(_.contains("Estimated fee $1")))
    assert(paper.lines.exists(_.contains("1x paper")))
    assert(paper.completeReview)
  }

  test("review Send and Cancel match separate drawn controls at each supported width") {
    for cols <- Vector(100, 120, 160) do
      val f = new Fixture
      f.prepare()
      val frame = f.draw(cols, 30)
      val send = frame.buttons.find(_.action == DeskAction.Submit).get
      val cancel = frame.buttons.find(_.action == DeskAction.Cancel).get
      assertEquals(send.y, cancel.y)
      assert(send.x + send.width <= cancel.x)
      for button <- Vector(send, cancel) do
        assertEquals(
          frame.lines(button.y - 1).slice(button.x - 1, button.x - 1 + button.width),
          "[" + button.label + "]"
        )
        assertEquals(frame.hit(button.x, button.y, cols, 30, "XMR"), Some(button.action))
      assert(frame.lines.forall(_.length == cols))
  }

  test("the PAXG chart label identifies the gold token without shortening the correlation count") {
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
    val tape = frame.lines.indexWhere(_.startsWith("TAPE"))
    assert(tape >= 0)
    val correlation = frame.lines(tape).take(53).takeRight(25).trim
    assertEquals(correlation, "gold 15m .|⣿ +1.00 15/15")
  }

  test("market-only charts use plain reference labels alongside the workspace correlations") {
    val lines =
      Screen.render(snapshot, 100, 40, now, zone, Style.mono, marketOnly = true, reservedRows = 12)
    for coin <- Vector("BTC", "PAXG") do
      val row = lines.indexWhere(_.startsWith(coin))
      assert(row >= 0, coin)
      assertEquals(
        lines.slice(row, row + Layout.referenceRows).map(_.take(Layout.leftW)),
        Vector(
          Chart.fit(if coin == "PAXG" then "PAXG (token tied to gold)" else coin, Layout.leftW)
        )
          .padTo(Layout.referenceRows, " " * Layout.leftW)
      )
  }

  private val resizeActions = Vector(
    "prepare" -> Command.PlaceOrder(intent),
    "send" -> Command.SubmitTarget,
    "mode" -> Command.Mode("LIVE"),
    "confirm mode" -> Command.SubmitTarget,
    "feedback" -> Command.Feedback(true),
    "target" -> Command.SelectTarget(Side.Buy),
    "ask" -> Command.Ask,
    "scroll" -> Command.ScrollMessages(5)
  )
  for (name, command) <- resizeActions do
    test("runtime remeasures terminal size before keyboard " + name) {
      val f = new Fixture
      f.controller.addMessage("analysis", "visible answer")
      f.draw()
      if name == "send" then f.prepare()
      if name == "confirm mode" then
        f.key(Command.Mode("LIVE"))
        f.draw()
      val (_, measured) = f.dispatch(command)
      name match
        case "prepare"      => assertEquals(f.controller.panel.review, None)
        case "send"         => assertEquals(f.sent, Vector.empty)
        case "mode"         => assertEquals(f.controller.panel.modeReview, None)
        case "confirm mode" => assertEquals(f.modes, Vector.empty)
        case "feedback"     => assert(f.controller.panel.messages.forall(_.feedback.isEmpty))
        case "target"       => assertEquals(f.controller.panel.target, None)
        case "ask"          => assertEquals(f.controller.panel.input, None)
        case "scroll"       => assertEquals(f.controller.panel.scroll, 0)
        case _              => fail(name)
      assertEquals(measured, (119, 40))
      assertEquals(f.measurements, 1)
    }

  test("runtime reads the current context after measuring and allows a newly drawn review") {
    val f = new Fixture
    f.prepare()
    var measured = false
    val result = WorkspaceApp.dispatchInput(
      Command.SubmitTarget,
      f.controller,
      () => { assert(measured); (snapshot, f.currentView) },
      (120, 40),
      false,
      () => { measured = true; (119, 40) }
    )
    assertEquals(result._2, (119, 40))
    assertEquals(f.sent, Vector.empty)
    assert(f.controller.panel.review.nonEmpty)
    f.draw(119, 40)
    f.dispatch(Command.SubmitTarget, (119, 40), (119, 40))
    assertEquals(f.sent.size, 1)
  }

  test("ordinary question text edits do not query terminal size") {
    val f = new Fixture
    f.draw()
    f.key(Command.Ask)
    for command <- Vector(
        Command.PromptChar('1'),
        Command.PromptText("23"),
        Command.PromptBackspace
      )
    do
      WorkspaceApp.dispatchInput(
        command,
        f.controller,
        () => (snapshot, f.currentView),
        (120, 40),
        false,
        () => fail("text edit requested a terminal-size process")
      )
    assertEquals(f.controller.panel.input, Some("12"))
  }
