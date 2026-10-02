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
  private val long = ready.copy(
    position = Some(BigDecimal("0.2")),
    leverage = Some(5),
    marginMode = Some("cross")
  )
  private class Fixture(mode: String = "PROP", start: TradingView = ready):
    var snapshot = initial
    var view = start
    var held = start.position.getOrElse(BigDecimal(0))
    var planned = Vector.empty[OrderIntent]
    var sent = Vector.empty[OrderReview]
    var modes = Vector.empty[String]
    val controller = new DeskController(
      PanelView(mode = mode),
      () => now,
      order => {
        planned :+= order
        Trading.plan(
          "XMR",
          ready.rules.get,
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), now),
          ready.reference(order.side).get,
          held,
          order,
          TradeSizes(),
          start.leverage.getOrElse(1),
          Trading.defaultSlippage
        )
      },
      order => { sent :+= order; view = view.copy(ready = false, pending = true); true },
      mode => { modes :+= mode; Right(()) }
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
    def x(): Unit = key(Keys.step(Vector.empty, 'x'.toInt, snapshot.prompt.nonEmpty)._2.get)
    def notice: Option[String] = controller.panel.actionNotice

  test("a displayed shortcut submits immediately once and Enter cannot repeat it") {
    val f = new Fixture
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.sent.size, 0)
    f.draw()
    f.key(Command.PlaceOrder(intent))
    f.key(Command.PlaceOrder(intent))
    f.key(Command.Confirm)
    assertEquals(f.sent.map(_.intent), Vector(intent))
    f.view = ready.copy(outcome = Some("PROP XMR buy filled 5"), outcomeId = OrderUpdates.nextId())
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
    f.key(Command.Confirm)
    assertEquals(f.modes, Vector.empty)
    val confirm = f.draw()
    assert(confirm.lines.exists(_.contains("one click sends real orders")))
    assert(!confirm.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
    f.key(Command.Confirm)
    assertEquals(f.modes, Vector("LIVE"))
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.sent.size, 0)
  }

  test("removed commands have no hidden trading or analysis action") {
    val f = new Fixture
    f.draw()
    Vector(
      Command.SelectTarget(Side.Buy),
      Command.Confirm
    ).foreach(command => f.key(command))
    assertEquals(f.sent.size, 0)
  }

  test("key x sends a reduce-only close of the whole long or short position") {
    for (position, side) <- Vector("0.2" -> Side.Sell, "-0.2" -> Side.Buy) do
      val f = new Fixture("LIVE", long.copy(position = Some(BigDecimal(position))))
      f.draw()
      f.x()
      val close = OrderIntent(side, OrderKind.Close)
      assertEquals(f.planned, Vector(close), position)
      assertEquals(f.sent.map(_.intent), Vector(close), position)
      val review = f.sent.head
      assertEquals(review.plan.kind, OrderKind.Close)
      assertEquals(review.plan.side, side)
      assertEquals(review.plan.quantity, BigDecimal("0.2"))
      assert(review.plan.reduceOnly)
      assertEquals((review.leverage, review.marginMode), (Some(5), Some("cross")))
      assertEquals(f.notice, Some("Sending"))
      f.draw()
      f.x()
      assertEquals(f.sent.size, 1, "a pending close refuses a second press")
  }

  test("CLOSE ignores the selected amount and a click sends the same close") {
    val f = new Fixture("LIVE", long)
    val size = f.draw().buttons.find(_.action == DeskAction.OrderSize(true)).get
    f.press(size)
    f.release(size)
    assert(f.controller.panel.bigOrder)
    val close = f.draw().buttons.find(_.action == DeskAction.Close).get
    f.press(close)
    f.release(close)
    assertEquals(f.sent.map(_.intent), Vector(OrderIntent(Side.Sell, OrderKind.Close)))
  }

  test("key x while flat answers through the disabled button and sends nothing") {
    val flat = new Fixture("LIVE", long.copy(position = Some(BigDecimal(0))))
    flat.draw()
    flat.x()
    assertEquals((flat.planned, flat.sent), (Vector.empty, Vector.empty))
    assertEquals(flat.notice, Some("Not sent: controls changed"))
    val stale = new Fixture("LIVE", long.copy(ready = false, block = Some("account data stale")))
    stale.draw()
    stale.x()
    assertEquals((stale.planned, stale.sent), (Vector.empty, Vector.empty))
    assertEquals(stale.notice, Some("account data stale"))
  }

  test("a close planned on a flat account reports that there is no position to close") {
    val f = new Fixture("LIVE", long)
    f.held = BigDecimal(0)
    f.draw()
    f.x()
    assertEquals(f.planned, Vector(OrderIntent(Side.Sell, OrderKind.Close)))
    assertEquals(f.sent, Vector.empty)
    assertEquals(f.notice, Some("No position to close"))
  }

  test("key x does nothing in prop or view mode") {
    for (mode, view) <- Vector(
        "PROP" -> long.copy(leverage = None, marginMode = None),
        "VIEW" -> long.copy(ready = false, block = Some("View only"))
      )
    do
      val f = new Fixture(mode, view)
      f.draw()
      f.x()
      assertEquals((f.planned, f.sent), (Vector.empty, Vector.empty), mode)
      assertEquals(f.notice, None, mode)
      assertEquals(f.controller.panel.actionNumber, 0L, mode)
  }

  test("key x sends nothing during a market prompt, a mode review or a pending order") {
    val prompt = new Fixture("LIVE", long)
    prompt.draw()
    prompt.snapshot = initial.copy(prompt = Some("BT"))
    prompt.x()
    prompt.key(Command.Close)
    val review = new Fixture("LIVE", long)
    review.draw()
    review.key(Command.Mode("PROP"))
    review.draw()
    review.x()
    val pending = new Fixture("LIVE", long.copy(ready = false, pending = true))
    pending.draw()
    pending.x()
    for f <- Vector(prompt, review, pending) do
      assertEquals((f.planned, f.sent), (Vector.empty, Vector.empty))
    assertEquals(review.notice, Some("Not sent: controls changed"))
    assertEquals(pending.notice, Some("Order pending"))
  }

  test("key x is refused when the view stops being ready or starts pending before a redraw") {
    for (changed, expected) <- Vector(
        long.copy(ready = false, block = Some("account data stale")) -> "account data stale",
        long.copy(pending = true) -> "Order pending"
      )
    do
      val f = new Fixture("LIVE", long)
      f.draw()
      f.view = changed
      f.x()
      assertEquals((f.planned, f.sent), (Vector.empty, Vector.empty), expected)
      assertEquals(f.notice, Some(expected))
  }

  test("a live close needs the observed margin mode like any live order") {
    val f = new Fixture("LIVE", long.copy(marginMode = None))
    f.draw()
    f.x()
    assertEquals((f.planned, f.sent), (Vector.empty, Vector.empty))
    assertEquals(f.notice, Some("Not sent: price or account changed"))
  }

  test("a position change during a CLOSE press reports changed controls and sends nothing") {
    val f = new Fixture("LIVE", long)
    val close = f.draw().buttons.find(_.action == DeskAction.Close).get
    f.press(close)
    f.view = long.copy(position = Some(BigDecimal("0.3")))
    f.draw()
    f.release(close)
    assertEquals(f.sent, Vector.empty)
    assertEquals(f.notice, Some("Not sent: controls changed"))
  }
