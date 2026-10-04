package desk

// The controller in limit mode, driven with hand built frames. A frame carries the actions a
// drawn screen would offer and the orders axis it drew, so these tests do not depend on how the
// panel is drawn.
class LimitControllerSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val xmr = MarketRules(7, 3, 5)
  private val viewOnly = "View only: p Prop or l Live to trade"
  private def d(text: String): BigDecimal = BigDecimal(text)
  private val buy = OrderIntent(Side.Buy, OrderKind.Hard)
  private val sell = OrderIntent(Side.Sell, OrderKind.Hard)
  private val close = OrderIntent(Side.Sell, OrderKind.Close)
  private val ready = TradingView(
    "XMR",
    position = Some(d("0")),
    message = "Ready",
    ready = true,
    rules = Some(xmr),
    bid = Some(d("546.09")),
    ask = Some(d("546.10")),
    leverage = Some(5),
    marginMode = Some("cross")
  )
  private def order(
      id: String,
      side: Side,
      price: String,
      size: String = "0.2",
      flag: Boolean = false
  ) =
    RestingOrder(id, "XMR", 7, side, d(price), d(size), d(size), flag, now)
  // Long 0.2 from 546.05. From the top the rows are the sells at 552.00 and 548.00, the current
  // price 546.095, the entry 546.05, the buys at 546.00 and the flagged buy at 544.50.
  private val rested = ready.copy(
    position = Some(d("0.2")),
    portfolio = PortfolioView(entry = Some(d("546.05"))),
    resting = Vector(
      order("s1", Side.Sell, "552.00"),
      order("s2", Side.Sell, "548.00"),
      order("b1", Side.Buy, "546.00"),
      order("b2", Side.Buy, "546.00", "0.1"),
      order("b3", Side.Buy, "544.50", flag = true)
    )
  )
  private val trade: Set[DeskAction] = Set(
    DeskAction.Prepare(buy),
    DeskAction.Prepare(sell),
    DeskAction.Close,
    DeskAction.OrderType,
    DeskAction.Orders,
    DeskAction.Place
  )
  // The buttons every frame carries, side by side on row 0.
  private val clicks: Vector[DeskAction] = Vector(
    DeskAction.Mode("PROP"),
    DeskAction.Mode("LIVE"),
    DeskAction.ConfirmMode("PROP"),
    DeskAction.ConfirmMode("LIVE"),
    DeskAction.OrderSize(false),
    DeskAction.OrderSize(true),
    DeskAction.Prepare(buy),
    DeskAction.Place
  )

  // The axis as a tall screen draws it, every mark on its own row, from 540 to 560 on 60 rows.
  private def drawn(view: TradingView): Placed =
    val entry = for
      size <- view.position.filter(_ != 0)
      price <- view.portfolio.entry
    yield (price, size)
    val mid = view.bid.zip(view.ask).map((bid, ask) => (bid + ask) / 2)
    OrdersAxis.marks(mid, entry, view.resting, None, view.leverage, 540, 560, 60)

  private def mark(price: String, kind: AxisKind, row: Int, ids: String*): AxisMark =
    AxisMark(d(price), None, kind, ids.toVector, row, row, ' ', false)

  private class Fixture(mode: String = "LIVE", start: TradingView = ready, big: Boolean = false):
    var snapshot = Snapshot.initial("XMR", now)
    var view = start
    var planned = Vector.empty[OrderIntent]
    var sent = Vector.empty[OrderReview]
    var cancelled = Vector.empty[Vector[String]]
    var accept = true
    var refusal = Option.empty[String]
    var reshape: OrderPlan => OrderPlan = identity
    var switched = Vector.empty[String]
    val controller = new DeskController(
      PanelView(mode = mode, bigOrder = big),
      () => now,
      (intent, terms) => {
        planned :+= intent
        refusal
          .toLeft(())
          .flatMap(_ =>
            Trading
              .plan(
                snapshot.coin,
                xmr,
                Book(Vector(Level("546.09", "20", 1)), Vector(Level("546.10", "20", 1)), now),
                // The book's touch when the view has none.
                view.reference(intent.side).getOrElse(d("546.10")),
                view.position.getOrElse(d("0")),
                intent,
                terms.sizes,
                5,
                terms.slippage
              )
              .map(reshape)
          )
      },
      review => { sent :+= review; accept },
      name => { switched :+= name; Right(()) },
      cancel = ids => { cancelled :+= ids; true }
    )
    private def buttons(actions: Vector[DeskAction], row: Int): Vector[DeskButton] =
      actions.zipWithIndex.map((a, i) => DeskButton(a.toString, a, i * 12, row, 11))
    // A frame drawn from the panel and the view as they are now, as L9 draws one.
    def build(
        shortcuts: Set[DeskAction] = trade,
        disabled: Vector[DeskAction] = Vector.empty,
        axis: Option[Placed] = Some(drawn(view)),
        focus: Option[Int] = controller.panel.focus
    ): WorkspaceFrame =
      WorkspaceFrame(
        Vector.empty,
        buttons(clicks, 0),
        snapshot.coin,
        120,
        40,
        controller.panel.mode,
        disabledButtons = buttons(disabled, 1),
        shortcuts = shortcuts,
        axis = axis,
        focus = focus,
        orderMode = controller.panel.orderMode,
        ticket = controller.panel.ticket,
        bigOrder = controller.panel.bigOrder,
        modeReview = controller.panel.modeReview
      )
    // As the render pass does: the controller sees the view, then the frame drawn from it.
    def frame(
        shortcuts: Set[DeskAction] = trade,
        disabled: Vector[DeskAction] = Vector.empty,
        axis: Option[Placed] = Some(drawn(view)),
        focus: Option[Int] = controller.panel.focus
    ): Unit =
      controller.orders(view)
      controller.publish(build(shortcuts, disabled, axis, focus))
    def key(command: Command): Boolean =
      controller.handle(command, snapshot, view, (120, 40), false)
    def click(action: DeskAction): Unit =
      val x = clicks.indexOf(action) * 12
      key(Command.PointerPress(x, 0, 0))
      key(Command.Pointer(x, 0, 0))
    def panel: PanelView = controller.panel
    def notice: Option[String] = panel.actionNotice
    def ticket: Option[Ticket] = panel.ticket
    // Nothing was submitted or cancelled. A ticket previews its priced press to hold its plan,
    // so every preview that did happen carries a price.
    def nothingSent(): Unit =
      assertEquals((sent, cancelled), (Vector.empty, Vector.empty))
      assert(planned.forall(_.price.nonEmpty), planned)
    // The ticket as opened, without the plan its preview gave.
    def opened: Option[Ticket] = ticket.map(_.copy(plan = None))
    def limit(): Unit =
      if panel.orderMode != OrderMode.Limit then key(Command.OrderType)
      frame()
    def open(intent: OrderIntent = buy): Unit =
      limit()
      key(Command.PlaceOrder(intent))
      frame()

  private val arrows =
    Vector(Command.ZoomIn, Command.ZoomOut, Command.ScrubBack, Command.ScrubForward)

  test("in limit mode a buy opens the ticket at the bid and a sell at the ask, sending nothing") {
    val f = new Fixture
    f.open(buy)
    assertEquals(
      f.opened,
      Some(Ticket("XMR", buy.copy(price = Some(d("546.09"))), d("546.09"), d("0.01"), None, None))
    )
    val g = new Fixture
    g.open(sell)
    assertEquals(
      g.opened,
      Some(Ticket("XMR", sell.copy(price = Some(d("546.10"))), d("546.10"), d("0.01"), None, None))
    )
    f.nothingSent()
    g.nothingSent()
  }

  test("a ticket holds the preview of its press, refreshed by every price move") {
    val f = new Fixture
    f.open(buy)
    def plannedAt(price: String): Boolean =
      f.ticket.flatMap(_.plan).exists(p => p.post && p.limit == d(price))
    assert(plannedAt("546.09"), f.ticket)
    assertEquals(f.ticket.flatMap(_.notice), None)
    f.key(Command.ZoomOut)
    assert(plannedAt("546.08"), f.ticket)
    assertEquals(f.planned.lastOption, Some(buy.copy(price = Some(d("546.08")))))
    // Up twice reaches the ask, where the preview refuses a price that takes liquidity.
    f.key(Command.ZoomIn)
    f.key(Command.ZoomIn)
    assertEquals(f.ticket.map(_.price), Some(d("546.10")))
    assertEquals(f.ticket.flatMap(_.plan), None)
    assertEquals(f.ticket.flatMap(_.notice), Some("Takes liquidity. Move below the ask"))
    assertEquals(f.sent, Vector.empty)
  }

  test("a ticket whose preview fails holds the refusal and no plan") {
    val f = new Fixture
    f.refusal = Some(TradingWorker.openingPausedMessage)
    f.open(buy)
    assertEquals(f.ticket.map(t => (t.plan, t.notice)), Some((None, f.refusal)))
    assertEquals(f.sent, Vector.empty)
  }

  test("the arrows move the ticket price and are consumed, and pass to the chart otherwise") {
    val f = new Fixture
    f.frame()
    arrows.foreach(a => assert(!f.key(a), a.toString))
    f.open(buy)
    def at(command: Command, price: String, step: String): Unit =
      assert(f.key(command), command.toString)
      assertEquals(f.ticket.map(t => (t.price, t.step)), Some((d(price), d(step))))
      assertEquals(f.ticket.flatMap(_.intent.price), Some(d(price)))
    at(Command.ZoomIn, "546.10", "0.01")
    at(Command.ScrubForward, "546.20", "0.1")
    at(Command.ScrubBack, "546.10", "0.1")
    at(Command.ZoomOut, "546.09", "0.01")
    at(Command.ZoomOut, "546.08", "0.01")
    f.nothingSent()
  }

  test("the ticket step is the real move where a tick is larger than the arrow's amount") {
    val f = new Fixture(start = ready.copy(bid = Some(d("1234.5")), ask = Some(d("1234.6"))))
    f.open(buy)
    assertEquals(f.ticket.map(t => (t.price, t.step)), Some((d("1234.5"), d("0.1"))))
    f.key(Command.ZoomIn)
    assertEquals(f.ticket.map(t => (t.price, t.step)), Some((d("1234.6"), d("0.1"))))
    f.key(Command.ScrubForward)
    assertEquals(f.ticket.map(t => (t.price, t.step)), Some((d("1234.7"), d("0.1"))))
  }

  test("Enter previews and submits the priced press once and a sent order closes the ticket") {
    val f = new Fixture
    f.open(buy)
    f.key(Command.ZoomOut)
    // Opening and the move each previewed the ticket. Enter adds its own preview.
    val shown = f.planned.size
    f.key(Command.Confirm)
    assertEquals(f.planned.size, shown, "nothing is sent at a price the ticket did not show")
    assertEquals(f.sent, Vector.empty)
    f.frame()
    f.key(Command.Confirm)
    val priced = OrderIntent(Side.Buy, OrderKind.Hard, price = Some(d("546.08")))
    assertEquals(f.planned.drop(shown), Vector(priced))
    assertEquals(f.sent.map(_.intent), Vector(priced))
    assertEquals(f.sent.map(r => (r.plan.post, r.plan.limit)), Vector((true, d("546.08"))))
    assertEquals(f.sent.map(_.terms), Vector(f.panel.terms))
    assertEquals(f.ticket, None)
    assertEquals(f.notice, Some("Sending"))
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.sent.size, 1)
  }

  test("Enter waits for a frame drawn with the ticket as it is now") {
    val f = new Fixture
    f.limit()
    f.key(Command.PlaceOrder(buy))
    f.key(Command.Confirm)
    assertEquals(f.ticket.map(_.price), Some(d("546.09")))
    f.nothingSent()
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.sent.flatMap(_.intent.price), Vector(d("546.09")))
    // A frame drawn at 546.09 and published after Down moved the price.
    val g = new Fixture
    g.open(buy)
    val before = g.build()
    g.key(Command.ZoomOut)
    g.controller.publish(before)
    g.key(Command.Confirm)
    g.click(DeskAction.Place)
    assertEquals(g.ticket.map(_.price), Some(d("546.08")))
    g.nothingSent()
    g.frame()
    g.key(Command.Confirm)
    assertEquals(g.sent.flatMap(_.intent.price), Vector(d("546.08")))
  }

  test("a press waits for a frame drawn with the order type as it is now") {
    val f = new Fixture
    f.limit()
    f.key(Command.OrderType)
    f.key(Command.PlaceOrder(buy))
    f.nothingSent()
    assertEquals(f.ticket, None)
    // A frame drawn in limit mode and published after t.
    val g = new Fixture
    g.limit()
    val before = g.build()
    g.key(Command.OrderType)
    g.controller.publish(before)
    g.key(Command.PlaceOrder(buy))
    g.click(DeskAction.Prepare(buy))
    g.nothingSent()
    assertEquals(g.ticket, None)
    g.frame()
    g.key(Command.PlaceOrder(buy))
    assertEquals(g.sent.map(_.intent), Vector(buy))
    assertEquals(g.ticket, None)
  }

  test("a press waits for a frame drawn with the size as it is now") {
    val large = buy.copy(big = true)
    val f = new Fixture
    f.frame()
    // A frame drawn with $500 chosen and published after $1000 was chosen.
    val before = f.build()
    f.click(DeskAction.OrderSize(true))
    assertEquals(f.panel.bigOrder, true)
    f.controller.publish(before)
    f.key(Command.PlaceOrder(buy))
    f.key(Command.PlaceOrder(large))
    f.nothingSent()
    assertEquals((f.notice, f.panel.actionNumber), (None, 0L), "the press waits, not refused")
    f.frame(trade + DeskAction.Prepare(large))
    f.key(Command.PlaceOrder(large))
    assertEquals(f.sent.map(_.intent), Vector(large))
  }

  test("Enter waits for a frame drawn with the mode review as it is now") {
    val f = new Fixture
    f.frame()
    // A frame drawn with no review and published after p opened one.
    val before = f.build()
    f.key(Command.Mode("PROP"))
    assertEquals(f.panel.modeReview, Some("PROP"))
    f.controller.publish(before)
    f.key(Command.Confirm)
    assertEquals((f.switched, f.panel.mode), (Vector.empty, "LIVE"))
    f.frame()
    f.key(Command.Confirm)
    assertEquals((f.switched, f.panel.mode), (Vector("PROP"), "PROP"))
    f.nothingSent()
  }

  test("x right after a press opens a ticket waits for the frame that draws the ticket") {
    val f = new Fixture(start = rested)
    f.limit()
    f.key(Command.PlaceOrder(buy))
    assert(f.ticket.nonEmpty)
    val shown = f.planned.size
    f.key(Command.Close)
    f.nothingSent()
    f.frame()
    f.key(Command.Close)
    assertEquals(f.planned.drop(shown), Vector(close))
  }

  test("a second Enter after a refused place waits for the frame that draws the refusal") {
    val f = new Fixture
    f.open(buy)
    f.accept = false
    f.key(Command.Confirm)
    assertEquals(f.ticket.flatMap(_.notice), Some("Not sent: price or account changed"))
    f.key(Command.Confirm)
    assertEquals(f.sent.size, 1, "the frame drew the ticket without the reason")
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.sent.size, 2)
  }

  test("a press waits silently on a frame of another order type that drew it disabled") {
    val f = new Fixture
    f.frame(trade - DeskAction.Prepare(buy), Vector(DeskAction.Prepare(buy)))
    f.key(Command.OrderType)
    f.key(Command.PlaceOrder(buy))
    assertEquals((f.notice, f.panel.actionNumber), (None, 0L))
    f.nothingSent()
    f.frame()
    f.key(Command.PlaceOrder(buy))
    assertEquals(f.ticket.map(_.price), Some(d("546.09")))
  }

  test("only a sized press opens a ticket or sends") {
    val best = OrderIntent(Side.Buy, OrderKind.Best)
    val f = new Fixture
    f.limit()
    f.frame(trade + DeskAction.Prepare(best))
    f.key(Command.PlaceOrder(best))
    assertEquals(f.ticket, None)
    f.nothingSent()
    val g = new Fixture
    g.frame(trade + DeskAction.Prepare(best))
    g.key(Command.PlaceOrder(best))
    g.nothingSent()
  }

  test("Enter on a drawn ticket sends nothing once the touch, the account or the market changed") {
    val changes: Vector[(String, Fixture => Unit)] = Vector(
      "no touch" -> (f => f.view = f.view.copy(bid = None, ask = None)),
      "pending" -> (f => f.view = f.view.copy(pending = true)),
      "not ready" -> (f => f.view = f.view.copy(ready = false)),
      "another market" -> (f => {
        f.snapshot = Snapshot.initial("BTC", now)
        f.view = f.view.copy(coin = "BTC")
      })
    )
    changes.foreach { (name, change) =>
      val f = new Fixture
      f.open(buy)
      val shown = f.planned.size
      change(f)
      f.frame()
      f.key(Command.Confirm)
      assertEquals((f.planned.drop(shown), f.sent), (Vector.empty, Vector.empty), name)
      assert(f.ticket.nonEmpty, name)
    }
  }

  test("a refused order keeps the ticket open with the reason") {
    val f = new Fixture
    f.open(buy)
    f.accept = false
    f.key(Command.Confirm)
    assertEquals(f.sent.size, 1)
    assertEquals(f.notice, Some("Not sent: price or account changed"))
    assertEquals(f.ticket.map(t => (t.price, t.notice)), Some((d("546.09"), f.notice)))
    val g = new Fixture
    g.open(buy)
    val shown = g.planned.size
    g.refusal = Some("Resting orders already reduce the whole position")
    g.key(Command.Confirm)
    assertEquals(g.planned.size, shown + 1)
    assertEquals(g.sent, Vector.empty)
    assertEquals(g.notice, g.refusal)
    assertEquals(g.ticket.flatMap(_.notice), g.refusal)
  }

  test("a preview that is not a post only order at the ticket price is never submitted") {
    val shapes: Vector[OrderPlan => OrderPlan] =
      Vector(_.copy(post = false), _.copy(limit = d("546.10")))
    shapes.foreach { shape =>
      val f = new Fixture
      f.open(buy)
      val shown = f.planned.size
      f.reshape = shape
      f.key(Command.Confirm)
      assertEquals((f.planned.size, f.sent), (shown + 1, Vector.empty))
      assertEquals(f.notice, Some("Not sent: price or account changed"))
      assertEquals(f.ticket.map(_.price), Some(d("546.09")))
    }
  }

  test("a press while a ticket is open sends nothing and keeps the ticket") {
    for orderMode <- Vector(OrderMode.Limit, OrderMode.Market) do
      val f = new Fixture
      f.open(buy)
      if orderMode == OrderMode.Market then f.key(Command.OrderType)
      f.frame()
      assertEquals(f.panel.orderMode, orderMode)
      val before = f.ticket
      f.key(Command.PlaceOrder(sell))
      assertEquals(f.ticket, before, orderMode.toString)
      f.nothingSent()
  }

  test("Enter on a price that would take liquidity refuses with the fixed text and sends nothing") {
    val f = new Fixture
    f.open(buy)
    f.key(Command.ZoomIn)
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.notice, Some("Takes liquidity. Move below the ask"))
    assertEquals(f.ticket.map(_.price), Some(d("546.10")))
    f.nothingSent()
    val g = new Fixture
    g.open(sell)
    g.key(Command.ZoomOut)
    g.frame()
    g.key(Command.Confirm)
    assertEquals(g.notice, Some("Takes liquidity. Move above the bid"))
    g.nothingSent()
  }

  test("Esc closes the ticket, sends nothing and gives the arrows back to the chart") {
    val f = new Fixture
    f.open(buy)
    assert(f.key(Command.Cancel))
    assertEquals(f.ticket, None)
    arrows.foreach(a => assert(!f.key(a), a.toString))
    f.nothingSent()
  }

  test("t toggles the order type in live and in prop and the type survives a switch") {
    val f = new Fixture
    f.frame()
    f.key(Command.OrderType)
    assertEquals(f.panel.orderMode, OrderMode.Limit)
    f.key(Command.OrderType)
    assertEquals(f.panel.orderMode, OrderMode.Market)
    f.key(Command.OrderType)
    f.frame()
    f.key(Command.Mode("PROP"))
    f.frame()
    f.key(Command.Confirm)
    assertEquals((f.panel.mode, f.panel.orderMode), ("PROP", OrderMode.Limit))
    f.frame()
    f.key(Command.OrderType)
    assertEquals(f.panel.orderMode, OrderMode.Market)
    f.nothingSent()
  }

  test("in view every order key answers the view text and reaches no worker") {
    val f = new Fixture("VIEW", rested.copy(ready = false, block = Some("View only")))
    f.frame()
    val keys = Vector(
      Command.OrderType,
      Command.Orders,
      Command.Place,
      Command.PlaceOrder(buy),
      Command.Close,
      Command.Confirm
    )
    keys.zipWithIndex.foreach { (command, i) =>
      assert(f.key(command), command.toString)
      assertEquals(f.notice, Some(viewOnly), command.toString)
      assertEquals(f.panel.actionNumber, i + 1L, command.toString)
    }
    assertEquals(f.panel.orderMode, OrderMode.Market)
    assertEquals((f.panel.focus, f.ticket), (None, None))
    f.nothingSent()
    f.key(Command.Mode("PROP"))
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.panel.mode, "PROP", "Enter still answers a mode review in view")
  }

  test("in view a ready account still keeps every order key from the workers") {
    // Only the view rule keeps a press or x from preview here.
    val f = new Fixture("VIEW", rested)
    f.frame()
    Vector(Command.PlaceOrder(buy), Command.Close, Command.Orders, Command.Confirm).foreach {
      command =>
        f.key(command)
        f.nothingSent()
        assertEquals(f.notice, Some(viewOnly), command.toString)
    }
  }

  test("o focuses the order row nearest the current price, never the entry row") {
    val f = new Fixture(start = rested)
    f.frame()
    assert(f.key(Command.Orders))
    assertEquals(f.panel.focus, Some(3))
    assertEquals(f.notice, Some("opens"))
  }

  test("on an axis drawn with marks left off, Enter acts on the ids the frame drew at the cursor") {
    // Four price rows: the sell at 552.00 is left off above and the buy at 544.50 below.
    val crowded = Placed(
      Vector(
        mark("548.00", AxisKind.Sell, 0, "s2"),
        mark("546.095", AxisKind.Current, 1),
        mark("546.05", AxisKind.Entry, 2),
        mark("546.00", AxisKind.Buy, 3, "b1", "b2")
      ),
      1,
      1
    )
    val f = new Fixture(start = rested)
    f.frame(axis = Some(crowded))
    f.key(Command.Orders)
    assertEquals(f.panel.focus, Some(2))
    f.frame(axis = Some(crowded))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b1", "b2")))
    f.key(Command.ZoomIn)
    f.key(Command.ZoomIn)
    assertEquals(f.panel.focus, Some(0))
    f.frame(axis = Some(crowded))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b1", "b2"), Vector("s2")))
    f.key(Command.Place)
    assertEquals(f.ticket.map(t => (t.intent.side, t.price)), Some((Side.Sell, d("548.00"))))
  }

  test("when a price move shifts the drawn rows, Enter waits for the frame that draws the cursor") {
    // Five price rows. The sell at 552.00 is left off, then after the price moves the buy at
    // 544.50 is, so the buys at 546.00 go from the third selectable row to the fourth.
    val before = Placed(
      Vector(
        mark("548.00", AxisKind.Sell, 0, "s2"),
        mark("546.095", AxisKind.Current, 1),
        mark("546.05", AxisKind.Entry, 2),
        mark("546.00", AxisKind.Buy, 3, "b1", "b2"),
        mark("544.50", AxisKind.Buy, 4, "b3")
      ),
      1,
      0
    )
    val after = Placed(
      Vector(
        mark("552.00", AxisKind.Sell, 0, "s1"),
        mark("548.00", AxisKind.Sell, 1, "s2"),
        mark("546.095", AxisKind.Current, 2),
        mark("546.05", AxisKind.Entry, 3),
        mark("546.00", AxisKind.Buy, 4, "b1", "b2")
      ),
      0,
      1
    )
    val f = new Fixture(start = rested)
    f.frame(axis = Some(before))
    f.key(Command.Orders)
    f.frame(axis = Some(before))
    assertEquals(f.panel.focus, Some(2))
    f.frame(axis = Some(after))
    assertEquals(f.panel.focus, Some(3))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector.empty, "that frame drew the entry row under the cursor")
    f.frame(axis = Some(after))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b1", "b2")))
  }

  test("Enter acts only on a frame drawn with the cursor where the controller holds it") {
    // Flat, so from the top the rows are the sells at 552.00 and 548.00 and the buys at 546.00
    // and 544.50.
    val f = new Fixture(start = rested.copy(position = Some(d("0"))))
    f.frame()
    f.key(Command.Orders)
    assertEquals(f.panel.focus, Some(2))
    f.frame(focus = Some(1))
    f.key(Command.Confirm)
    f.key(Command.Place)
    assertEquals(f.cancelled, Vector.empty, "the frame drew the cursor on the sell at 548.00")
    assertEquals(f.ticket, None)
    f.frame(focus = Some(2))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b1", "b2")))
  }

  test("without a drawn axis the axis keys do nothing") {
    val f = new Fixture(start = rested)
    f.frame(axis = None)
    f.key(Command.Orders)
    assertEquals((f.panel.focus, f.notice), (None, None))
    f.controller.invalidate()
    f.key(Command.Orders)
    assertEquals((f.panel.focus, f.notice), (None, None))
    f.frame()
    f.key(Command.Orders)
    f.frame()
    assertEquals(f.panel.focus, Some(3))
    f.controller.invalidate()
    f.key(Command.ZoomIn)
    f.key(Command.Confirm)
    f.key(Command.Place)
    assertEquals(f.panel.focus, Some(3))
    f.frame(axis = None)
    f.key(Command.Confirm)
    f.key(Command.Place)
    assertEquals(f.ticket, None)
    f.nothingSent()
  }

  test("after an order is added Enter waits for a frame that draws the cursor on its row") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.frame()
    f.view = rested.copy(resting = rested.resting :+ order("s3", Side.Sell, "550.00"))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector.empty, "the frame was drawn before the order came")
    f.frame()
    assertEquals(f.panel.focus, Some(4))
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector.empty, "that frame drew the cursor on the row above")
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b1", "b2")))
  }

  test("o without order rows answers No orders on XMR and does not focus") {
    val f = new Fixture(start = rested.copy(resting = Vector.empty))
    f.frame()
    f.key(Command.Orders)
    assertEquals(f.notice, Some("No orders on XMR"))
    assertEquals(f.panel.focus, None)
    arrows.foreach(a => assert(!f.key(a), a.toString))
  }

  test("o again and Esc drop focus, and o with a ticket open does nothing") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.key(Command.Orders)
    assertEquals(f.panel.focus, None)
    f.key(Command.Orders)
    f.key(Command.Cancel)
    assertEquals(f.panel.focus, None)
    f.open(buy)
    f.key(Command.Orders)
    assertEquals(f.panel.focus, None)
    assert(f.ticket.nonEmpty)
  }

  test("Up and Down move over the selectable rows past the current price, Left and Right do not") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    assert(f.key(Command.ZoomIn))
    assertEquals(f.panel.focus, Some(2))
    assert(f.key(Command.ZoomIn))
    assertEquals(f.panel.focus, Some(1))
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("s2")))
    f.key(Command.ZoomIn)
    f.key(Command.ZoomIn)
    assertEquals(f.panel.focus, Some(0))
    Vector.fill(6)(Command.ZoomOut).foreach(c => assert(f.key(c)))
    assertEquals(f.panel.focus, Some(4))
    assert(!f.key(Command.ScrubBack))
    assert(!f.key(Command.ScrubForward))
    assertEquals(f.panel.focus, Some(4))
  }

  test("the cursor follows its row and moves to the nearest order row when its row is gone") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.view = rested.copy(resting = rested.resting :+ order("s3", Side.Sell, "550.00"))
    f.frame()
    assertEquals(f.panel.focus, Some(4))
    // The entry at 546.05 is nearer, but the cursor only ever lands on an order row by itself.
    f.view = f.view.copy(resting = f.view.resting.filterNot(_.price == d("546.00")))
    f.frame()
    assertEquals(f.panel.focus, Some(4))
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b3")))
    // From 544.50, the sell at 548.00 and the buy at 541.00 are equally near.
    f.view = f.view.copy(resting =
      f.view.resting.filter(_.side == Side.Sell) :+ order("b4", Side.Buy, "541.00")
    )
    f.frame()
    assertEquals(f.panel.focus, Some(2), "the one above on a tie")
    f.view = f.view.copy(resting = Vector.empty)
    f.frame()
    assertEquals(f.panel.focus, None)
    val g = new Fixture(start = rested)
    g.frame()
    g.key(Command.Orders)
    g.key(Command.ZoomIn)
    assertEquals(g.panel.focus, Some(2))
    g.view = rested.copy(resting = Vector.empty)
    g.frame()
    assertEquals(g.panel.focus, None, "the entry row alone keeps no focus")
  }

  test("Enter on the entry row sends the close x sends and is refused the same way") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.key(Command.ZoomIn)
    f.frame()
    f.key(Command.Confirm)
    val g = new Fixture(start = rested)
    g.frame()
    g.key(Command.Close)
    assertEquals(f.planned, Vector(close))
    assertEquals((f.planned, f.sent.map(_.intent)), (g.planned, g.sent.map(_.intent)))
    val off = new Fixture(start = rested)
    val disabled = Vector[DeskAction](DeskAction.Close)
    off.frame(trade - DeskAction.Close, disabled)
    off.key(Command.Orders)
    off.key(Command.ZoomIn)
    off.frame(trade - DeskAction.Close, disabled)
    off.key(Command.Confirm)
    assertEquals(off.notice, Some("Not sent: controls changed"))
    off.key(Command.Close)
    assertEquals(off.notice, Some("Not sent: controls changed"))
    off.nothingSent()
  }

  test("Enter on an order row cancels its ids once drawn, and a gone row answers the fixed text") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector.empty, "a cursor never acts on a row before it is drawn")
    f.frame()
    f.key(Command.Confirm)
    assertEquals(f.cancelled, Vector(Vector("b1", "b2")))
    val g = new Fixture(start = rested)
    g.frame()
    g.key(Command.Orders)
    g.frame()
    g.view = rested.copy(resting = rested.resting.filterNot(_.price == d("546.00")))
    g.key(Command.Confirm)
    assertEquals(g.notice, Some("Nothing to cancel at that price"))
    g.nothingSent()
  }

  test("a on an order row opens a ticket at its side and price and drops focus, not on the entry") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.key(Command.ZoomIn)
    f.frame()
    f.key(Command.Place)
    assertEquals((f.ticket, f.panel.focus), (None, Some(2)))
    f.key(Command.ZoomIn)
    f.key(Command.Place)
    assertEquals(f.ticket, None, "a waits for the frame that draws the cursor")
    f.frame()
    f.key(Command.Place)
    assertEquals(
      f.opened,
      Some(Ticket("XMR", sell.copy(price = Some(d("548.00"))), d("548.00"), d("0.01"), None, None))
    )
    assert(f.ticket.flatMap(_.plan).exists(_.limit == d("548.00")), f.ticket)
    assertEquals(f.panel.focus, None)
    assertEquals(f.panel.orderMode, OrderMode.Market)
    val g = new Fixture(start = rested)
    g.frame()
    g.key(Command.Place)
    assertEquals(g.ticket, None)
    f.nothingSent()
  }

  test("the ticket a opens carries the big size when it is selected") {
    val f = new Fixture(start = rested, big = true)
    f.frame()
    f.key(Command.Orders)
    f.key(Command.ZoomIn)
    f.key(Command.ZoomIn)
    f.frame()
    f.key(Command.Place)
    assertEquals(
      f.ticket.map(_.intent),
      Some(OrderIntent(Side.Sell, OrderKind.Hard, big = true, price = Some(d("548.00"))))
    )
  }

  test("a waits for a frame drawn after the orders changed") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    f.key(Command.ZoomIn)
    f.key(Command.ZoomIn)
    f.frame()
    f.view = rested.copy(resting = rested.resting :+ order("s3", Side.Sell, "550.00"))
    f.key(Command.Place)
    assertEquals(f.ticket, None, "the frame was drawn before the order came")
    f.frame()
    f.frame()
    f.key(Command.Place)
    assertEquals(f.ticket.map(t => (t.intent.side, t.price)), Some((Side.Sell, d("548.00"))))
  }

  test("a ticket, a mode review and the market prompt drop focus, and a mode review the ticket") {
    def focused(): Fixture =
      val f = new Fixture(start = rested)
      f.frame()
      f.key(Command.Orders)
      f.frame()
      assertEquals(f.panel.focus, Some(3))
      f
    val review = focused()
    review.key(Command.Mode("PROP"))
    assertEquals((review.panel.modeReview, review.panel.focus), (Some("PROP"), None))
    val prompt = focused()
    assert(!prompt.key(Command.StartMarketPrompt))
    assertEquals(prompt.panel.focus, None)
    val press = focused()
    press.key(Command.OrderType)
    press.frame()
    press.key(Command.PlaceOrder(buy))
    assertEquals((press.ticket.map(_.price), press.panel.focus), (Some(d("546.09")), None))
    press.frame()
    press.key(Command.Mode("PROP"))
    assertEquals((press.panel.modeReview, press.ticket), (Some("PROP"), None))
  }

  test("resetMarket, Settings, new terms and new bindings close the ticket and drop focus") {
    val resets: Vector[(String, DeskController => Unit)] = Vector(
      "resetMarket" -> (_.resetMarket()),
      "Settings" -> (c => {
        val _ = c.handle(Command.Settings, Snapshot.initial("XMR", now), rested, (120, 40), false)
      }),
      "setTerms" -> (_.setTerms(TradeTerms(TradeSizes(small = d("20")), d("0.002")))),
      "setBindings" -> (_.setBindings(Bindings.defaults))
    )
    resets.foreach { (name, reset) =>
      val f = new Fixture(start = rested)
      f.open(buy)
      assert(f.ticket.nonEmpty, name)
      reset(f.controller)
      assertEquals(f.ticket, None, name)
      val g = new Fixture(start = rested)
      g.frame()
      g.key(Command.Orders)
      assertEquals(g.panel.focus, Some(3), name)
      reset(g.controller)
      assertEquals(g.panel.focus, None, name)
      assert(!g.key(Command.ZoomIn), name)
    }
  }

  test("the effect of the orders under the cursor shows on the status row") {
    val f = new Fixture(start = rested)
    f.frame()
    f.key(Command.Orders)
    assertEquals(f.notice, Some("opens"))
    f.key(Command.ZoomIn)
    assertEquals(f.notice, None)
    f.key(Command.ZoomIn)
    assertEquals(f.notice, Some("reduce"))
    f.key(Command.ZoomIn)
    assertEquals(f.notice, Some("flip $22.08 XMR if filled"))
    Vector.fill(4)(Command.ZoomOut).foreach(f.key)
    assertEquals(f.notice, Some("void"))
  }

  test("CLOSE in limit mode sends an immediate close and opens no ticket") {
    val f = new Fixture(start = rested)
    f.limit()
    assertEquals(f.panel.orderMode, OrderMode.Limit)
    f.key(Command.Close)
    assertEquals(f.planned, Vector(close))
    assertEquals(f.sent.map(_.intent), Vector(close))
    assertEquals(f.ticket, None)
  }

  test("x in prop previews and submits a sell close once, and asks a sell even when flat") {
    val f = new Fixture("PROP", rested.copy(leverage = Some(1), marginMode = None))
    f.frame()
    f.key(Command.Close)
    assertEquals(f.planned, Vector(close))
    assertEquals(f.sent.map(_.intent), Vector(close))
    val flat = new Fixture("PROP", ready.copy(leverage = Some(1)))
    flat.frame()
    flat.key(Command.Close)
    assertEquals(flat.planned, Vector(close))
    assertEquals(flat.sent, Vector.empty)
  }
