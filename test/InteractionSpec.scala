package desk

class InteractionSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val intent = OrderIntent(Side.Buy, OrderKind.Hard)
  private val action = DeskAction.Prepare(intent)
  private val initial = Snapshot.initial("XMR", now)
  private val ready = TradingView(
    "XMR",
    message = "Ready",
    ready = true,
    rules = Some(MarketRules(0, 2, 5)),
    bid = Some(BigDecimal(99)),
    ask = Some(BigDecimal(100))
  )
  private class Fixture:
    var snapshot = initial
    var view = ready
    var sent = Vector.empty[OrderReview]
    var modes = Vector.empty[String]
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      order =>
        Trading.plan(
          "XMR",
          ready.rules.get,
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), now),
          ready.reference(order.side).get,
          BigDecimal(0),
          order,
          TradeSizes()
        ),
      order => { sent :+= order; view = view.copy(ready = false, pending = true); true },
      _ => fail("removed Ask callback"),
      mode => { modes :+= mode; Right("selected") }
    )
    def draw(cols: Int = 120, rows: Int = 40): WorkspaceFrame =
      val frame = Workspace.draw(
        snapshot,
        view,
        controller.panel,
        cols,
        rows,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      controller.publish(frame)
      frame
    def key(command: Command, size: (Int, Int) = (120, 40)): Unit =
      controller.handle(command, snapshot, view, size, snapshot.prompt.nonEmpty)
    def press(button: DeskButton): Unit = key(Command.PointerPress(button.x, button.y, 0))
    def release(button: DeskButton): Unit = key(Command.Pointer(button.x, button.y, 0))

  test("a displayed shortcut submits immediately once and Enter cannot repeat it") {
    val f = new Fixture
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.sent.size, 0)
    f.draw()
    f.key(Command.PlaceOrder(intent))
    f.key(Command.PlaceOrder(intent))
    f.key(Command.SubmitTarget)
    assertEquals(f.sent.map(_.intent), Vector(intent))
    assertEquals(f.controller.panel.review, None)
    f.view = ready.copy(outcome = Some("PAPER XMR buy filled 5"), outcomeId = OrderUpdates.nextId())
    f.key(Command.PlaceOrder(intent))
    assertEquals(
      f.sent.size,
      2,
      "a completed order allows the next deliberate shortcut without a redraw"
    )
  }

  test(
    "release without matching press cannot submit, and ordinary redraw preserves a valid gesture"
  ) {
    val f = new Fixture
    val frame = f.draw()
    val buy = frame.buttons.find(_.action == action).get
    f.release(buy)
    assertEquals(f.sent.size, 0)
    f.press(buy)
    f.controller.invalidate()
    f.draw()
    f.release(buy)
    f.release(buy)
    assertEquals(f.sent.size, 1)
  }

  test("changed control context cancels the gesture even if the old frame returns") {
    val changes: Vector[(String, WorkspaceFrame => WorkspaceFrame)] = Vector(
      "columns" -> (f => f.copy(cols = f.cols + 1)),
      "rows" -> (f => f.copy(rows = f.rows + 1)),
      "market" -> (f => f.copy(coin = "BTC")),
      "mode" -> (f => f.copy(mode = "LIVE")),
      "amount" -> (f =>
        f.copy(buttons =
          f.buttons.map(b =>
            if b.action == action then b.copy(action = DeskAction.Prepare(intent.copy(big = true)))
            else b
          )
        )
      ),
      "position" -> (f =>
        f.copy(buttons = f.buttons.map(b => if b.action == action then b.copy(x = b.x + 1) else b))
      ),
      "width" -> (f =>
        f.copy(buttons =
          f.buttons.map(b => if b.action == action then b.copy(width = b.width - 1) else b)
        )
      ),
      "disabled" -> (f => f.copy(buttons = f.buttons.filterNot(_.action == action)))
    )
    changes.foreach { (name, change) =>
      val f = new Fixture
      val frame = f.draw()
      val buy = frame.buttons.find(_.action == action).get
      f.press(buy)
      f.controller.publish(change(frame))
      f.controller.publish(frame)
      f.release(buy)
      assertEquals(f.sent.size, 0, name)
      f.press(buy)
      f.release(buy)
      assertEquals(f.sent.size, 1, name + " accepts a new gesture")
    }
  }

  test("release must hit the pressed button within the current terminal size") {
    val f = new Fixture
    val buy = f.draw().buttons.find(_.action == action).get
    f.press(buy)
    f.key(Command.Pointer(buy.x + buy.width, buy.y, 0))
    f.release(buy)
    f.key(Command.PointerPress(buy.x - 1, buy.y, 0))
    f.release(buy)
    f.press(buy)
    f.key(Command.Pointer(buy.x, buy.y, 0), (119, 40))
    f.release(buy)
    assertEquals(f.sent.size, 0)
  }

  test("keyboard input cancels a pending mouse gesture") {
    val f = new Fixture
    val buy = f.draw().buttons.find(_.action == action).get
    f.press(buy)
    f.key(Command.ZoomIn)
    f.release(buy)
    assertEquals(f.sent.size, 0)
  }

  for pendingFirst <- Vector(false, true) do
    test(
      "changing button readiness during a press reports refusal without sending: " + pendingFirst
    ) {
      val f = new Fixture
      f.view = ready.copy(ready = !pendingFirst, pending = pendingFirst)
      val buy = f.draw().controls.find(_.action == action).get
      f.press(buy)
      f.view = ready.copy(ready = pendingFirst, pending = !pendingFirst)
      f.draw()
      f.release(buy)
      assertEquals(f.sent.size, 0)
      assertEquals(f.controller.panel.actionNotice, Some("Not sent: controls changed"))
      assertEquals(f.controller.panel.actionNumber, 1L)
      f.release(buy)
      assertEquals(f.controller.panel.actionNumber, 1L)
    }

  test("a displayed disabled button remains refused when the worker becomes ready before repaint") {
    val f = new Fixture
    f.view = ready.copy(ready = false, pending = true)
    val button = f.draw().disabledButtons.find(_.action == action).get
    f.view = ready
    f.press(button)
    f.release(button)
    assertEquals(f.sent.size, 0)
    assertEquals(f.controller.panel.actionNotice, Some("Not sent: controls changed"))
  }

  test("market prompts, pending selection and unavailable current quotes block orders") {
    for change <- Vector[Fixture => Unit](
        f => f.snapshot = initial.copy(prompt = Some("BTC")),
        f => f.snapshot = initial.copy(marketPending = Some(1L -> "BTC")),
        f => f.snapshot = initial.copy(coin = "BTC"),
        f => f.view = ready.copy(ready = false, block = Some("stale")),
        f => f.view = ready.copy(pending = true)
      )
    do
      val f = new Fixture
      val buy = f.draw().buttons.find(_.action == action).get
      f.press(buy)
      change(f)
      f.release(buy)
      f.key(Command.PlaceOrder(intent))
      assertEquals(f.sent.size, 0)
  }

  test("mode change requires the displayed confirmation and invalidates old controls") {
    val f = new Fixture
    f.draw()
    f.key(Command.Mode("LIVE"))
    f.key(Command.SubmitTarget)
    assertEquals(f.modes, Vector.empty)
    val confirm = f.draw()
    assert(confirm.lines.exists(_.contains("one click sends real orders")))
    assert(!confirm.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
    f.key(Command.SubmitTarget)
    assertEquals(f.modes, Vector("LIVE"))
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.sent.size, 0)
  }

  test("removed commands have no hidden trading or analysis action") {
    val f = new Fixture
    f.draw()
    Vector(
      Command.Ask,
      Command.Details,
      Command.SelectTarget(Side.Buy),
      Command.Feedback(true),
      Command.SubmitTarget
    ).foreach(command => f.key(command))
    assertEquals(f.sent.size, 0)
    assertEquals(f.controller.panel.input, None)
    assertEquals(f.controller.panel.target, None)
  }
