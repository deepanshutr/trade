package desk

// Test 16, the parts lane L9 draws: the status row, the panel, the ticket and the orders axis.
class LimitPanelSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val zone = java.time.ZoneOffset.UTC
  private val snapshot = Snapshot.initial("XMR", now)
  private val ready = TradingView("XMR", message = "Ready", ready = true)

  private def text(frame: WorkspaceFrame): String = frame.lines.map(Chart.plain).mkString("\n")

  private def drawn(view: TradingView, panel: PanelView): WorkspaceFrame =
    Workspace.draw(snapshot, view, panel, 100, 30, now, zone, Style.mono)

  // The status row of a notice with its action number, drawn in live mode.
  private def notice(raw: String, number: Long, focus: Option[Int] = None): String =
    text(
      drawn(
        ready,
        PanelView(mode = "LIVE", actionNotice = Some(raw), actionNumber = number)
          .copy(focus = focus)
      )
    )

  test("the status row names Desk's own outcomes with their dollars") {
    for (raw, number, expected) <- Vector(
        ("XMR filled $21.84 at 546.09", 7L, "Filled $21.84 #7"),
        ("XMR part filled $5.46 of $21.84 at 546.09", 8L, "Part filled $5.46 #8"),
        ("XMR resting $19.99 at 546.09", 9L, "Resting $19.99 #9"),
        ("XMR canceled $16.38 at 546.09", 10L, "Canceled $16.38 #10"),
        (
          "XMR resting buy $19.99 at 546.09 canceled by self-trade prevention",
          11L,
          "Canceled $19.99 #11"
        ),
        (
          "XMR canceled by the venue $16.38 at 546.09 (reduceOnlyCanceled)",
          12L,
          "Canceled $16.38 #12"
        ),
        (
          "XMR canceled $16.38 at 546.09 (Prop limit: not enough stables)",
          13L,
          "Canceled $16.38 #13"
        ),
        ("XMR resting $16.38 at 546.09 after release", 15L, "Resting $16.38 #15"),
        ("XMR filled $19.99 at 546.09 after release", 16L, "Filled $19.99 #16")
      )
    do
      val shown = notice(raw, number)
      assert(shown.contains(expected), s"$raw\n$shown")
      for wrong <- Vector("Not sent", "Resting $", "Bought", "Sold", "Waiting for prices")
      do assert(!shown.contains(wrong) || expected.startsWith(wrong), s"$raw\n$shown")
  }

  test("a cancel before sending still reads Not sent") {
    for raw <- Vector("XMR canceled before sending", "XMR canceled $16.38 before sending") do
      val shown = notice(raw, 14L)
      assert(shown.contains("Not sent #14"), shown)
      assert(!shown.contains("Canceled"), shown)
  }

  test("a part fill reads Part filled, never Filled") {
    val shown = notice("XMR part filled $5.46 of $21.84 at 546.09", 3L)
    assert(shown.contains("Part filled $5.46 #3"), shown)
    assert(!shown.contains("Filled $5.46"), shown)
  }

  // A prop market fill as the ledger writes it, at one times leverage.
  private def propFill(side: Side, quantity: String, filled: String, price: String): String =
    val p = BigDecimal(price)
    val q = BigDecimal(filled)
    val plan = OrderPlan(
      "XMR",
      0,
      side,
      OrderKind.Hard,
      BigDecimal(quantity),
      p,
      p,
      q,
      q * p,
      Some(p),
      q,
      false
    )
    PropLedger.fillText(plan, q * p * BigDecimal("0.0005"), 2L, now)

  test("a prop market fill names its side and its dollars, never the fee") {
    for (raw, expected) <- Vector(
        (propFill(Side.Buy, "1", "1", "100"), "Bought $100.00 #2"),
        (propFill(Side.Sell, "0.183", "0.183", "546.09"), "Sold $99.93 #2"),
        (propFill(Side.Buy, "3", "1.5", "100"), "Part filled $150.00 #2")
      )
    do
      val shown = notice(raw, 2L)
      assert(shown.linesIterator.exists(_.take(38).trim == expected), s"$raw\n$shown")
      assert(!shown.contains("Fee"), shown)
  }

  test("refusals from the ticket, the axis and the worker pass through whole on the status row") {
    for raw <- Vector(
        "Takes liquidity. Move below the ask",
        "Takes liquidity. Move above the bid",
        "Nothing to cancel at that price",
        "A limit order takes a sized press",
        "20 orders rest. Cancel one first",
        "Read the trading key first. Press r"
      )
    do
      // Each fits the 38 column status row with its number, so a ticket hides none of it.
      val f = drawn(ready, PanelView(mode = "LIVE", actionNotice = Some(raw), actionNumber = 4))
      assertEquals(f.lines(30 - 12 + 3).take(38).trim, s"$raw #4")
  }

  // The status row of a notice, its panel columns only.
  private def statusOf(raw: String, number: Long): String =
    row(frame(live, limit.copy(ticket = None, actionNotice = Some(raw), actionNumber = number)), 4)

  test("a refused CLOSE names its remedy on the status row, and a plain refusal stays Not sent") {
    val refused = "XMR order rejected. Resting reduce orders may block it, press o"
    assertEquals(statusOf(refused, 5L), "Close refused. Press o for orders #5")
    val blocked =
      frame(live.copy(block = Some(refused)), PanelView(mode = "LIVE"))
    assertEquals(row(blocked, 4), "Close refused. Press o for orders")
    assertEquals(statusOf("XMR order rejected", 6L), "Not sent #6")
  }

  test("a cancel reads Canceling while it runs and Cancel refused when the venue refuses it") {
    val running = live.copy(pending = true, message = "XMR canceling 1 of 3")
    assertEquals(
      row(frame(running, limit.copy(ticket = None, actionNotice = None)), 4),
      "Canceling"
    )
    assertEquals(statusOf("XMR cancel rejected", 7L), "Cancel refused #7")
    assertEquals(statusOf("XMR cancel result unknown", 8L), "Confirming result #8")
    assertEquals(statusOf("XMR canceled $16.38 at 546.09", 9L), "Canceled $16.38 #9")
  }

  test("a CLOSE waiting on a cancel says so on the status row") {
    val waiting = live.copy(pending = true, message = TradingWorker.closeNextMessage)
    assertEquals(
      row(frame(waiting, limit.copy(ticket = None, actionNotice = None)), 4),
      "Close goes after this cancel"
    )
  }

  test("a cancel refused while an order is pending says to wait, not Sending") {
    val shown = text(
      drawn(
        ready.copy(pending = true, message = TradingWorker.cancelPendingMessage),
        PanelView(mode = "LIVE")
      )
    )
    assert(shown.contains(TradingWorker.cancelPendingMessage), shown)
    assert(!shown.contains("Sending"), shown)
  }

  test("the effect under the cursor shows without a number, and only while focused") {
    for raw <- Vector("opens", "reduce", "void", "flip $5.44 XMR if filled") do
      val focused = notice(raw, 5L, focus = Some(0))
      assert(focused.linesIterator.exists(_.contains(raw)), s"$raw\n$focused")
      assert(!focused.contains(s"$raw #5"), s"$raw\n$focused")
      val unfocused = notice(raw, 5L)
      assert(!unfocused.contains(raw), s"$raw\n$unfocused")
  }

  test("view mode reads the view text where a mode has nothing to say") {
    for p <- Vector(
        PanelView(mode = "VIEW"),
        PanelView(mode = "VIEW", actionNotice = Some(DeskController.viewOnly), actionNumber = 3)
      )
    do
      val shown = text(drawn(TradingView("XMR", message = "View only"), p))
      assert(shown.contains(DeskController.viewOnly), shown)
      assert(!shown.contains(DeskController.viewOnly + " #"), shown)
  }

  // The pinned case of lane L5's chart fixtures: the series spans 544.00 to 552.00 and the book
  // holds three levels a side, so the axis is 543.60 to 552.40 and the current price is the mid
  // 546.095. Live is long 0.2 at 546.00 at 5x, with a sell of 0.1 resting at 552.00, a buy of 0.05
  // at 544.00 and a buy on BTC, and a sell ticket at 546.10 from a $20 press.
  private def lv(px: String, sz: String) = Level(px, sz, 1)
  private val pinned: Snapshot =
    val sec = now / 1000
    Snapshot
      .initial("XMR", now)
      .copy(
        feed = Health.Live(now - 300),
        ctx = Some(Ctx("546.09", "546.08", "0.000013", "1200000", "48000000", "539.62", None)),
        book = Some(
          Book(
            Vector(lv("546.09", "0.5"), lv("545.00", "1"), lv("544.00", "3")),
            Vector(lv("546.10", "0.5"), lv("547.00", "1"), lv("549.00", "3")),
            now
          )
        ),
        samples =
          Vector((sec - 40, 548.0), (sec - 30, 552.0), (sec - 20, 544.0), (sec - 1, 546.095)),
        refs = Map(
          "BTC" -> Ref(Vector((sec - 40, 95000.0), (sec - 1, 96000.0)), Map.empty),
          "PAXG" -> Ref(Vector((sec - 40, 2300.0), (sec - 1, 2310.0)), Map.empty)
        )
      )
  private def order(
      id: String,
      side: Side,
      price: String,
      size: String,
      coin: String = "XMR",
      cancelling: Boolean = false
  ): RestingOrder =
    val quantity = BigDecimal(size)
    RestingOrder(id, coin, 0, side, BigDecimal(price), quantity, quantity, false, now, cancelling)
  private val xmr = MarketRules(0, 3, 5)
  private val live = TradingView(
    "XMR",
    position = Some(BigDecimal("0.2")),
    message = "Ready",
    ready = true,
    rules = Some(xmr),
    bid = Some(BigDecimal("546.09")),
    ask = Some(BigDecimal("546.10")),
    leverage = Some(5),
    marginMode = Some("isolated"),
    portfolio = PortfolioView(
      tokenValue = Some(BigDecimal("109.22")),
      stables = Some(BigDecimal("1000")),
      entry = Some(BigDecimal("546.00")),
      openProfit = Some(BigDecimal("0.44"))
    ),
    usableMargin = Some(BigDecimal("880")),
    resting = Vector(
      order("s1", Side.Sell, "552.00", "0.1"),
      order("b1", Side.Buy, "544.00", "0.05"),
      order("b2", Side.Buy, "95000", "0.001", coin = "BTC")
    ),
    fills = Vector(
      Fill("XMR", Side.Buy, now - 11_000, BigDecimal("546.00"), BigDecimal("0.2")),
      Fill("XMR", Side.Sell, now - 4_000, BigDecimal("547.00"), BigDecimal("0.1"))
    )
  )
  private val terms =
    TradeTerms(
      TradeSizes(BigDecimal(20), BigDecimal(100), BigDecimal(100)),
      Trading.defaultSlippage
    )
  // A ticket as the controller opens one. Its plan is the preview the controller holds, a posted
  // order of the given quantity, and None when no quantity is given.
  private def ticketAt(
      price: String,
      side: Side = Side.Sell,
      coin: String = "XMR",
      quantity: Option[String] = None,
      reduce: Boolean = false
  ): Ticket =
    val at = BigDecimal(price)
    val plan = quantity.map(q =>
      val size = BigDecimal(q)
      OrderPlan(coin, 0, side, OrderKind.Hard, size, at, at, 0, 0, None, 0, reduce, post = true)
    )
    val step = BigDecimal("0.01")
    Ticket(coin, OrderIntent(side, OrderKind.Hard, price = Some(at)), at, step, plan, None)
  private val limit = PanelView(
    mode = "LIVE",
    terms = terms,
    actionNotice = Some("XMR filled $21.84 at 546.00"),
    actionNumber = 7,
    orderMode = OrderMode.Limit,
    ticket = Some(ticketAt("546.10", quantity = Some("0.1"), reduce = true))
  )
  private def frame(
      view: TradingView = live,
      panel: PanelView = limit,
      cols: Int = 100,
      rows: Int = 30,
      style: Style = Style.mono,
      s: Snapshot = pinned
  ): WorkspaceFrame = Workspace.draw(s, view, panel, cols, rows, now, zone, style)
  // The panel's twelve rows, plain.
  private def band(f: WorkspaceFrame): Vector[String] =
    f.lines.drop(f.rows - 12).map(line => Chart.plain(line).take(38))
  private def row(f: WorkspaceFrame, number: Int): String = band(f)(number - 1).trim

  test("the left pane and the panel match the hand written fixtures") {
    // Each fixture line is the left 38 columns of one row under the header, then a bar so the
    // trailing spaces stay visible.
    for (name, view, panel, cols, rows) <- Vector(
        ("live-100x30", live, limit, 100, 30),
        ("live-120x40", live, limit, 120, 40),
        ("view-100x30", live, limit.copy(mode = "VIEW", actionNotice = None), 100, 30)
      )
    do
      val f = frame(view, panel, cols, rows)
      val expected = Golden
        .fixture(s"screen-limit-$name.txt")
        .linesIterator
        .toVector
        .map(_.stripSuffix("|"))
      assertEquals(f.lines.drop(1).map(line => Chart.plain(line).take(38)), expected, name)
      assert(f.lines.forall(_.length == cols), name)
      assert(f.lines.drop(rows - 12).forall(_.charAt(38) == '|'), name)
  }

  test("live, prop and view frames of the same data differ only in the panel and view's axis") {
    for (cols, rows) <- Vector((100, 30), (120, 40)) do
      val panel = limit.copy(actionNotice = None, ticket = None)
      val shown = Vector("LIVE", "PROP", "VIEW").map(mode =>
        frame(panel = panel.copy(mode = mode), cols = cols, rows = rows, style = Style.colour)
      )
      val (l, p, v) = (shown(0), shown(1), shown(2))
      // The trace alone, with no fill and no ticket line over it.
      val bare = frame(live.copy(fills = Vector.empty), panel, cols, rows, Style.colour)
      val tradeTop = rows - 12
      val priceRows = (1 to rows - 21).toSet
      // Live and prop draw every row the same but the panel's rows 1 to 7.
      for i <- 0 until rows if i < tradeTop || i >= tradeTop + 7 do
        assertEquals(p.lines(i), l.lines(i), s"prop row $i")
      for i <- 0 until rows do
        if i >= tradeTop && i < tradeTop + 7 then ()
        else if priceRows(i) then
          // View's price rows hold the plain axis and the trace only.
          assertEquals(Chart.plain(v.lines(i)).drop(38), Chart.plain(bare.lines(i)).drop(38))
          assert(!Chart.plain(v.lines(i)).take(17).exists("$',".contains(_)), v.lines(i))
        else assertEquals(v.lines(i), l.lines(i), s"view row $i")
      assertEquals(p.buttons, l.buttons)
      assertEquals(p.disabledButtons, l.disabledButtons)
      assertEquals(p.axis, l.axis)
      assert(v.axis.exists(_.marks.map(_.kind) == Vector(AxisKind.Current)), v.axis)
      assert(l.axis.exists(_.marks.exists(_.kind == AxisKind.Entry)), l.axis)
  }

  test("the view panel reads its own rows and draws its trade rows dim and disabled") {
    val view = limit.copy(mode = "VIEW", actionNotice = None)
    val mono = frame(panel = view)
    assertEquals(
      band(mono).take(4).map(_.trim),
      Vector(
        "VIEW XMR --",
        "Val       -- Open      -- Net      --",
        "Stables       --  Usable       --",
        "View only: p Prop or l Live to trade"
      )
    )
    assertEquals(row(mono, 5), "[$20 *] [$100] [x Close]")
    assert(mono.disabledButtons.exists(_.label == "x Close"), mono.disabledButtons)
    assert(
      !mono.buttons.exists(b =>
        b.action.isInstanceOf[DeskAction.Prepare] || b.action.isInstanceOf[DeskAction.OrderSize] ||
          Set[DeskAction](DeskAction.Close, DeskAction.OrderType, DeskAction.Orders)(b.action)
      ),
      mono.buttons
    )
    assertEquals(mono.shortcuts, Set.empty[DeskAction])
    assertEquals(mono.ticket, None)
    val colour = frame(panel = view, style = Style.colour)
    // The whole mode row is faint in view, the sound with the buttons.
    assertEquals(
      colour.lines(30 - 12 + 5).takeWhile(_ != '|'),
      Screen.dim + "[t Limit] [o Orders] v Sound" + Screen.reset + " " * 10
    )
    for i <- 4 to 6 do
      val line = colour.lines(30 - 12 + i)
      assert(line.startsWith(Screen.dim), line)
      assert(!line.take(line.indexOf('|')).contains(Style.colour.bid), line)
      assert(!line.take(line.indexOf('|')).contains(Style.colour.ask), line)
  }

  test("prop reads PROP at 1x, its balance and usable cash, and closes its long with money") {
    val prop = live.copy(
      leverage = Some(1),
      portfolio = live.portfolio.copy(stables = Some(BigDecimal("10000"))),
      usableMargin = Some(BigDecimal("9880")),
      resting = Vector.empty
    )
    val f = frame(prop, PanelView(mode = "PROP", terms = terms))
    assertEquals(row(f, 1), "PROP XMR 1x LONG")
    assertEquals(row(f, 3), "Prop   $10000.00  Usable $9880.00")
    assertEquals(row(f, 3).split(" +").mkString(" "), "Prop $10000.00 Usable $9880.00")
    assertEquals(row(f, 5), "[$20 *] [$100] [x Close $109.20]")
    assert(f.buttons.exists(b => b.action == DeskAction.Close && b.label == "x Close $109.20"))
    assert(f.shortcuts.contains(DeskAction.Close), f.shortcuts)
    assertEquals(row(f, 7), "[1 LONG $20 x1] [3 REDUCE $20 x1]")
    val flat = frame(prop.copy(position = None, portfolio = PortfolioView()), PanelView("PROP"))
    assertEquals(row(flat, 1), "PROP XMR 1x")
  }

  test("CLOSE names the money the position puts up, -- with the leverage unknown") {
    assertEquals(
      row(frame(panel = limit.copy(ticket = None)), 5),
      "[$20 *] [$100] [x Close $21.84]"
    )
    val unknown = frame(live.copy(leverage = None), limit.copy(ticket = None))
    assertEquals(row(unknown, 5), "[$20 *] [$100] [x Close --]")
    assertEquals(row(unknown, 1), "LIVE XMR LONG")
    val colour = frame(panel = limit.copy(ticket = None), style = Style.colour)
    assert(
      colour.lines(30 - 12 + 4).contains(Style.colour.ask + Screen.bold + "[x Close $21.84]"),
      colour.lines(30 - 12 + 4)
    )
  }

  test("the mode row follows the bindings and drops the count, then the sound, when too wide") {
    assertEquals(row(frame(), 6), "[t Limit] [o Orders] v Sound  1 on BTC")
    val market = frame(panel = limit.copy(orderMode = OrderMode.Market, ticket = None))
    assertEquals(row(market, 6), "[t Market] [o Orders] v Sound 1 on BTC")
    val alone = frame(live.copy(resting = live.resting.filter(_.coin == "XMR")))
    assertEquals(row(alone, 6), "[t Limit] [o Orders] v Sound")
    def bound(action: Bind, text: String): Bindings =
      Bindings.defaults
        .updated(action, Binding(Vector(KeyGroup(text.getBytes("UTF-8").toVector.map(_ & 0xff)))))
        .toOption
        .get
    val longKey = Bindings.defaults
      .updated(Bind.OrderType, Binding(Vector(KeyGroup("\u001b[15;2~".toVector.map(_.toInt)))))
      .toOption
      .get
    val long = frame(panel = limit.copy(bindings = longKey))
    val typeLabel = longKey.map(Bind.OrderType).short + " Limit"
    assert(row(long, 6).startsWith(s"[$typeLabel] [o Orders]"), row(long, 6))
    assert(!row(long, 6).contains("on BTC"), row(long, 6))
    assert(band(long)(5).length <= 38)
    val renamed = frame(panel = limit.copy(bindings = bound(Bind.Orders, "u")))
    assert(row(renamed, 6).startsWith("[t Limit] [u Orders]"), row(renamed, 6))
    assert(renamed.buttons.exists(b => b.action == DeskAction.Orders && b.label == "u Orders"))
    // With the sound key long as well, the row drops the count and then the whole sound label.
    val both = longKey
      .updated(Bind.Sound, Binding(Vector(KeyGroup("\u001b[17;2~".toVector.map(_.toInt)))))
      .toOption
      .get
    assertEquals(row(frame(panel = limit.copy(bindings = both)), 6), s"[$typeLabel] [o Orders]")
  }

  test("the order type and orders buttons are shortcuts in live and prop when they apply") {
    val f = frame(panel = limit.copy(ticket = None))
    assert(f.shortcuts.contains(DeskAction.OrderType), f.shortcuts)
    assert(f.shortcuts.contains(DeskAction.Orders), f.shortcuts)
    val none = frame(live.copy(resting = Vector.empty), limit.copy(ticket = None))
    assert(!none.shortcuts.contains(DeskAction.Orders), none.shortcuts)
    assert(none.disabledButtons.exists(_.action == DeskAction.Orders))
    val open = frame()
    assert(open.shortcuts.contains(DeskAction.Place), open.shortcuts)
    assert(open.disabledButtons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
    assert(!open.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
  }

  test(
    "the trade buttons are one row, bold in their side's colour, and dim while a ticket is open"
  ) {
    val f = frame(panel = limit.copy(ticket = None), style = Style.colour)
    val trades = f.buttons.filter(_.action.isInstanceOf[DeskAction.Prepare])
    assertEquals(
      trades.map(b => (b.label, b.y, b.height)),
      Vector(
        ("1 LONG $20 x5", 30 - 12 + 7, 1),
        ("3 REDUCE $20 x5", 30 - 12 + 7, 1)
      )
    )
    val line = f.lines(30 - 12 + 6)
    assert(line.contains(Style.colour.bid + Screen.bold + "[1 LONG $20 x5]" + Screen.reset), line)
    assert(line.contains(Style.colour.ask + Screen.bold + "[3 REDUCE $20 x5]" + Screen.reset), line)
    val open = frame(style = Style.colour).lines(30 - 12 + 6)
    assert(open.contains(Screen.dim + "[1 LONG $20 x5]" + Screen.reset), open)
    assert(open.contains(Screen.dim + "[3 REDUCE $20 x5]" + Screen.reset), open)
  }

  test("a trade row too wide drops the leverage, then the amount") {
    val wide = Bindings.defaults
      .updated(Bind.LongSmall, Binding(Vector(KeyGroup("\u001b[15;2~".toVector.map(_.toInt)))))
      .toOption
      .get
    val key = wide.map(Bind.LongSmall).short
    val big = terms.copy(sizes = TradeSizes(BigDecimal(2000), BigDecimal(10000), BigDecimal(1)))
    val f = frame(panel = limit.copy(bindings = wide, terms = big, ticket = None))
    val labels = f.buttons.filter(_.action.isInstanceOf[DeskAction.Prepare]).map(_.label)
    assert(labels.head.startsWith(key), labels)
    assert(band(f)(6).trim.length <= 38, band(f)(6))
    assert(!labels.exists(_.endsWith("x5")), labels)
  }

  test("the ticket rows name the side, the price against the touch, the money and the step") {
    assertEquals(
      band(frame()).slice(7, 11).map(_.trim),
      Vector(
        "SELL LIMIT XMR reduce only",
        "Price 546.10  ask 546.10 +0t +0.00%",
        "Size $10.92 of $21.84 held  fee $0.01",
        "[Enter Place] [Esc Cancel] step 0.01"
      )
    )
    val colour = frame(style = Style.colour)
    val top = colour.lines(30 - 12 + 7)
    assert(top.startsWith(Style.colour.ask + Screen.bold + "SELL" + Screen.reset), top)
    val price = colour.lines(30 - 12 + 8)
    assert(price.contains(Style.colour.ask + Screen.bold + "546.10" + Screen.reset), price)
    assert(!colour.lines.exists(_.contains(Style.colour.gold + "SELL")))
    val buy = frame(panel = limit.copy(ticket = Some(ticketAt("546.09", Side.Buy))))
    assertEquals(row(buy, 8), "BUY LIMIT XMR post only")
    assertEquals(row(buy, 9), "Price 546.09  bid 546.09 +0t +0.00%")
  }

  test("ticket row 9 keeps to 38 columns, losing the percent and then the ticks") {
    def sized(
        coin: String,
        rules: MarketRules,
        bid: String,
        ask: String,
        price: String,
        big: Boolean,
        quantity: String
    ): Vector[String] =
      val s = Snapshot
        .initial(coin, now)
        .copy(book = Some(Book(Vector(lv(bid, "1000")), Vector(lv(ask, "1000")), now)))
      val view = TradingView(
        coin,
        position = Some(BigDecimal(0)),
        message = "Ready",
        ready = true,
        rules = Some(rules),
        bid = Some(BigDecimal(bid)),
        ask = Some(BigDecimal(ask)),
        leverage = Some(5)
      )
      val t = ticketAt(price, coin = coin, quantity = Some(quantity))
      val panel = limit.copy(
        ticket = Some(t.copy(intent = t.intent.copy(big = big))),
        bigOrder = big
      )
      band(frame(view, panel, s = s)).slice(7, 11).map(_.trim)
    // The plans are what Trading.plan gives these presses: $100 or $500 at 5x at the price,
    // rounded down to the lot.
    val small = sized("XMR", xmr, "546.09", "546.10", "549.00", big = false, "0.182")
    assertEquals(small(1), "Price 549.00  ask 546.10 +290t +0.53%")
    assertEquals(small(1).length, 37)
    assertEquals(small(2), "Size $19.98  fee $0.01")
    val large = sized("XMR", xmr, "546.09", "546.10", "549.00", big = true, "0.910")
    assertEquals(large(1), "Price 549.00  ask 546.10 +290t +0.53%")
    assertEquals(large(2), "Size $99.92  fee $0.07")
    val btc = sized("BTC", MarketRules(0, 5, 5), "98990", "99040", "99000", big = false, "0.00101")
    assertEquals(btc(1), "Price 99000  ask 99040 -40t -0.04%")
    assertEquals(btc(1).length, 34)
    val tiny =
      sized("TINY", MarketRules(0, 0, 5), "0.01234", "0.012346", "0.012345", big = false, "8100")
    assertEquals(tiny(1), "Price 0.012345  ask 0.012346 -1t")
    assertEquals(tiny(1).length, 32)
    for rows <- Vector(small, large, btc, tiny); text <- rows do assert(text.length <= 38, text)
  }

  test("the status row shows the outcome word and the marks the axis left off") {
    assertEquals(row(frame(), 4), "Filled $21.84 #7")
    // Ten sells above a current price on row 6 of 9: six keep a row and four are left off.
    val sells = (1 to 10).toVector.map(i =>
      order(s"s$i", Side.Sell, (BigDecimal("551.00") + BigDecimal(i) / 10).toString, "0.01")
    )
    val crowded = frame(live.copy(resting = sells), limit.copy(ticket = None))
    assertEquals(band(crowded)(3), "Filled $21.84 #7".padTo(26, ' ') + "4 more above")
    assertEquals(crowded.axis.map(_.above), Some(4))
    val quiet = frame(live.copy(resting = sells), limit.copy(ticket = None, actionNotice = None))
    assertEquals(band(quiet)(3), " " * 26 + "4 more above")
  }

  test("the axis rows take their colours: entry and ticket bold in their side, the price bold") {
    val lines = frame(style = Style.colour).lines
    assert(lines(8).startsWith(Style.colour.bid + Screen.bold + " $21.84   546.00,"), lines(8))
    assert(lines(6).startsWith(Style.colour.ask + Screen.bold + " $10.92   546.10'"), lines(6))
    assert(lines(1).startsWith(Style.colour.ask + " $11.04   552.00 "), lines(1))
    assert(lines(9).startsWith(Style.colour.bid + "  $5.44   544.00 "), lines(9))
    assert(lines(7).startsWith(Screen.bold + "          546.10"), lines(7))
    assertEquals(Chart.plain(lines(4)).slice(17, 38), Chart.FULL * 21)
    assert(!lines.exists(_.contains(Style.colour.gold + " $")))
    assert(!lines.exists(_.contains(Screen.inverse)))
  }

  test("the selected row is inverse, in colour and in mono") {
    for style <- Vector(Style.colour, Style.mono) do
      // The selectable rows are the sell at 552.00, the entry and the buy at 544.00.
      val f = frame(panel = limit.copy(ticket = None, focus = Some(2)), style = style)
      assertEquals(f.focus, Some(2))
      val chosen = f.lines.filter(_.contains(Screen.inverse))
      assertEquals(chosen.size, 1, f.lines.mkString("\n"))
      assert(chosen.head.startsWith(Screen.inverse), chosen.head)
      assert(Chart.plain(chosen.head).startsWith("  $5.44   544.00 "), chosen.head)
      if style == Style.mono then
        assert(f.lines.filterNot(_ == chosen.head).forall(!_.contains(Screen.esc)))
  }

  test("amounts go dim while an order is cancelling and while the account is stale") {
    val cancelling =
      live.copy(resting = Vector(order("s1", Side.Sell, "552.00", "0.1", cancelling = true)))
    val one = frame(cancelling, limit.copy(ticket = None), style = Style.colour).lines(1)
    assert(one.startsWith(Style.colour.ask + Screen.dim + " $11.04" + Screen.reset), one)
    val stale = frame(live.copy(stale = true), limit.copy(ticket = None), style = Style.colour)
    for (i, ink) <- Vector(1 -> Style.colour.ask, 8 -> Style.colour.bid) do
      assert(stale.lines(i).startsWith(ink + Screen.dim), stale.lines(i))
  }

  test("the gauges fill the band's right region with no key row") {
    for (cols, rows, width) <- Vector((100, 30, 61), (120, 40, 81)) do
      val f = frame(cols = cols, rows = rows)
      val right = f.lines.drop(rows - 12).map(_.drop(39))
      val expected = Workspace.indicatorRows(pinned, now, (width - 1) / 2, Style.mono)
      assertEquals(right, expected.map(Chart.fit(_, width)))
      assert(right.take(6).forall(_.startsWith("BOOK")), right)
      assert(right.drop(6).forall(_.startsWith("TAPE")), right)
      assert(right.forall(_.length == width))
  }

  test("the frame records the axis it drew, its cursor, order type and ticket") {
    val f = frame(panel = limit.copy(focus = Some(1)))
    val drawn = f.axis.get
    assertEquals(
      drawn.marks.map(m => (m.price, m.row, m.kind)),
      Vector(
        (BigDecimal("552.00"), 0, AxisKind.Sell),
        (BigDecimal("546.10"), 5, AxisKind.Ticket),
        (BigDecimal("546.095"), 6, AxisKind.Current),
        (BigDecimal("546.00"), 7, AxisKind.Entry),
        (BigDecimal("544.00"), 8, AxisKind.Buy)
      )
    )
    assertEquals(f.focus, Some(1))
    assertEquals(f.orderMode, OrderMode.Limit)
    assertEquals(f.ticket, limit.ticket)
    // A ticket from another market is not drawn, so the frame records none.
    val elsewhere = frame(panel = limit.copy(ticket = Some(ticketAt("95000", coin = "BTC"))))
    assertEquals(elsewhere.ticket, None)
    assert(!band(elsewhere).exists(_.contains("LIMIT")))
    // At nineteen rows the same marks sit on rows 0, 12, 13, 14 and 18.
    val tall = frame(cols = 120, rows = 40).axis.get
    assertEquals(tall.marks.map(_.row), Vector(0, 12, 13, 14, 18))
  }

  test("a sell at the current price keeps its row above it, at the top of the axis") {
    // The series ends at the mid 546.09, where a sell rests. The count above keeps a row for it.
    val sec = now / 1000
    val top = pinned.copy(
      book = Some(Book(Vector(lv("546.08", "1")), Vector(lv("546.10", "1")), now)),
      samples = Vector((sec - 40, 540.0), (sec - 1, 546.09))
    )
    val view = live.copy(
      position = None,
      bid = Some(BigDecimal("546.08")),
      resting = Vector(order("s9", Side.Sell, "546.09", "0.1"))
    )
    val f = frame(view, limit.copy(ticket = None), s = top)
    val drawn = f.axis.get
    assertEquals(drawn.above, 0)
    assertEquals(
      drawn.marks.map(m => (m.kind, m.row)),
      Vector(AxisKind.Sell -> 0, AxisKind.Current -> 1)
    )
  }

  test("own fills and the ticket line are drawn in the plot, never in view") {
    val lines = frame(style = Style.colour).lines
    // Two seconds a cell: the buy 11 s back is in cell 17, the sell 4 s back in cell 20.
    assertEquals(Chart.plain(lines(7)).charAt(38 + 17), '▲')
    assertEquals(Chart.plain(lines(6)).charAt(38 + 20), '▼')
    assert(lines(7).contains(Style.colour.bid + Screen.bold + "▲"), lines(7))
    // The ticket at 546.10 has true row 6, and its dots fill that row's empty cells.
    assert(Chart.plain(lines(7)).drop(38).contains('⠒'), lines(7))
    assert(lines(7).contains(Style.colour.ask + "⠒"), lines(7))
    // View draws the trace alone, as live does with no fill and no ticket.
    def plot(lines: Vector[String]): Vector[String] =
      lines.slice(1, 10).map(line => Chart.plain(line).slice(38, 61))
    val view = frame(panel = limit.copy(mode = "VIEW"), style = Style.colour).lines
    val bare = frame(live.copy(fills = Vector.empty), limit.copy(ticket = None)).lines
    assertEquals(plot(view), plot(bare))
    assert(plot(frame().lines) != plot(bare))
    // While scrubbing ten seconds back, the right edge is ten seconds back.
    val scrubbed = frame(s = pinned.copy(offsetSec = 10)).lines
    assertEquals(Chart.plain(scrubbed(7)).charAt(38 + 22), '▲')
    // Candles take 250 ms a cell for each second of the zoom, 15 s at one minute candles, so a
    // buy 31 s back is two cells back.
    val candles = pinned.copy(chartMode = ChartMode.Candles, zoom = State.candleMinZoom)
    assertEquals(State.zooms(State.candleMinZoom), 60)
    val older = live.copy(fills =
      Vector(Fill("XMR", Side.Buy, now - 31_000, BigDecimal("546.00"), BigDecimal("0.2")))
    )
    assertEquals(Chart.plain(frame(older, s = candles).lines(7)).charAt(38 + 20), '▲')
  }

  test("the drawn frame lets the controller cancel the row it drew and place the ticket") {
    var cancelled = Vector.empty[Vector[String]]
    var sent = Vector.empty[OrderReview]
    val plan = OrderPlan(
      "XMR",
      0,
      Side.Buy,
      OrderKind.Hard,
      BigDecimal("0.183"),
      BigDecimal("546.09"),
      BigDecimal("546.09"),
      BigDecimal(0),
      BigDecimal(0),
      None,
      BigDecimal("0.2"),
      false,
      post = true
    )
    val controller = new DeskController(
      limit.copy(ticket = None, actionNotice = None),
      () => now,
      (_, _) => Right(plan),
      review => { sent :+= review; true },
      _ => Right(()),
      cancel = ids => { cancelled :+= ids; true }
    )
    def draw(): Unit =
      controller.orders(live)
      controller.publish(frame(panel = controller.panel))
    def key(command: Command): Unit =
      val _ = controller.handle(command, pinned, live, (100, 30), false)
    draw()
    key(Command.Orders)
    draw()
    // The order row nearest the current price is the buy at 544.00.
    key(Command.Confirm)
    assertEquals(cancelled, Vector(Vector("b1")))
    key(Command.Orders)
    draw()
    key(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
    // The ticket holds the preview it opened with, and the frame draws exactly that size.
    assertEquals(controller.panel.ticket.flatMap(_.plan), Some(plan))
    draw()
    val shown = frame(panel = controller.panel)
    assertEquals(row(shown, 10), "Size $19.99  fee $0.01")
    assertEquals(shown.ticket.flatMap(_.plan), Some(plan))
    key(Command.Confirm)
    assertEquals(sent.map(_.intent.price), Vector(Some(BigDecimal("546.09"))))
  }

  test("the chart block reaches the band at every market, with no blank row above it") {
    // XMR shows two reference markets and BTC and PAXG one each, so their price rows differ.
    for (coin, priceRows) <- Vector("XMR" -> 21, "BTC" -> 18, "PAXG" -> 18); rows <- Vector(30, 40)
    do
      val f = Workspace.draw(
        Snapshot.initial(coin, now),
        TradingView(coin, message = "Ready"),
        PanelView(),
        100,
        rows,
        now,
        zone,
        Style.mono
      )
      assertEquals(f.lines(rows - priceRows + 1).take(38), " " * 31 + "volume ", coin)
      assertEquals(f.lines(rows - 13).take(38), " " * 24 + "c line 2s/col ", coin)
      assert(f.lines(rows - 12).startsWith("VIEW " + coin), coin)
  }

  test("a long status wraps at spaces onto rows 8 to 11 while no ticket holds them") {
    val mismatch = live.copy(pending = true, message = TradingWorker.ownerMismatchMessage)
    val free = frame(mismatch, limit.copy(ticket = None))
    assertEquals(row(free, 4), "Signer does not match the configured")
    assertEquals(row(free, 8), "owner")
    // A ticket keeps its rows, so the status shows its first line alone.
    val held = frame(mismatch)
    assertEquals(row(held, 4), "Signer does not match the configured")
    assertEquals(row(held, 8), "SELL LIMIT XMR reduce only")
    // The failed order state fits its row whole while a ticket is open.
    val failed = frame(live.copy(block = Some(TradingWorker.storageFailedMessage)))
    assertEquals(row(failed, 4), "Order state failed. Restart to recover")
    assertEquals(row(failed, 8), "SELL LIMIT XMR reduce only")
  }

  test("the worker's two fatal texts show whole on the ticket's size row") {
    for text <- Vector(TradingWorker.storageFailedMessage, TradingWorker.signingFailedMessage) do
      val refused = ticketAt("546.10").copy(notice = Some(text))
      assertEquals(row(frame(panel = limit.copy(ticket = Some(refused))), 10), text)
  }

  test("the ticket shows the size of the plan it holds, or the refusal its preview gave") {
    val held = limit.copy(ticket = Some(ticketAt("546.10", quantity = Some("0.07"), reduce = true)))
    val f = frame(panel = held)
    assertEquals(row(f, 10), "Size $7.65 of $21.84 held  fee $0.01")
    // The ticket's axis row names the same money.
    assert(Chart.plain(f.lines(6)).startsWith("  $7.65   546.10'"), f.lines(6))
    val refused = ticketAt("546.10").copy(notice = Some(TradingWorker.openingPausedMessage))
    val g = frame(panel = limit.copy(ticket = Some(refused)))
    assertEquals(row(g, 10), TradingWorker.openingPausedMessage)
    assert(Chart.plain(g.lines(6)).startsWith("     --   546.10'"), g.lines(6))
    // A refusal is shown as text alone, its first line and no control bytes.
    val hostile = ticketAt("546.10").copy(notice = Some("Refused " + 27.toChar + "[2J\nsecond"))
    val h = frame(panel = limit.copy(ticket = Some(hostile)))
    assertEquals(row(h, 10), "Refused")
    assert(!h.lines.exists(_.contains(27.toChar)))
    // A ticket with neither a plan nor a refusal draws no size of its own.
    val bare = frame(panel = limit.copy(ticket = Some(ticketAt("546.10"))))
    assertEquals(row(bare, 10), "Size --")
  }

  test("every refusal a ticket can show fits the 38 column panel") {
    // The plan's refusals for a priced press, each produced by the plan itself.
    val book = Book(Vector(lv("546.09", "1")), Vector(lv("546.10", "1")), now)
    val sizes = TradeSizes(BigDecimal(20), BigDecimal(100), BigDecimal(100))
    def at(price: String, side: Side = Side.Buy, kind: OrderKind = OrderKind.Hard): OrderIntent =
      OrderIntent(side, kind, price = Some(BigDecimal(price)))
    def refused(
        intent: OrderIntent = at("546.00"),
        coin: String = "XMR",
        rules: MarketRules = xmr,
        quotes: Book = book,
        reference: String = "546.10",
        position: String = "0",
        terms: TradeSizes = sizes,
        leverage: Int = 5,
        reserved: String = "0"
    ): Option[String] =
      Trading
        .plan(
          coin,
          rules,
          quotes,
          BigDecimal(reference),
          BigDecimal(position),
          intent,
          terms,
          leverage,
          Trading.defaultSlippage,
          BigDecimal(reserved)
        )
        .left
        .toOption
    def bids(px: String, sz: String = "1"): Book = book.copy(bids = Vector(lv(px, sz)))
    val planned = Vector(
      refused(coin = ""),
      refused(rules = MarketRules(-1, 3, 5)),
      refused(leverage = 6),
      refused(terms = sizes.copy(small = BigDecimal(0))),
      refused(reference = "546.095"),
      refused(position = "0.0001"),
      refused(quotes = bids("abc")),
      refused(quotes = bids("546.095")),
      refused(quotes = bids("546.09", "0")),
      refused(quotes = bids("546.09", "0.0001")),
      refused(quotes = book.copy(bids = Vector.empty)),
      refused(quotes = bids("546.11")),
      refused(intent = at("546.00", kind = OrderKind.Best)),
      refused(intent = at("546.005")),
      refused(intent = at("546.10")),
      refused(intent = at("546.20", Side.Sell), position = "0.2", reserved = "0.2"),
      refused(terms = sizes.copy(small = BigDecimal("0.001"))),
      refused(terms = sizes.copy(small = BigDecimal(1)))
    )
    assert(planned.forall(_.nonEmpty), planned)
    assertEquals(planned.flatten.distinct.size, planned.size, planned)
    // The workers' and the controller's own refusals of a ticket's preview or its Enter.
    val others = Vector(
      Trading.crossing(Side.Sell, BigDecimal(1), BigDecimal(1), BigDecimal(2)).get,
      Trading.restingCapText,
      TradingWorker.waitingDepthMessage,
      TradingWorker.openingPausedMessage,
      "Position unknown",
      PropLedger.restingNoStables,
      PropLedger.unavailable,
      PropWorker.staleMessage,
      PropWorker.behindMessage,
      "Prop closed. Restart Desk",
      "Reconnecting. Waiting for fresh prices",
      "Prop waiting for market details",
      "Prop waiting for selected-market depth",
      "Prop quote invalid. Waiting for prices",
      "Prop worker is closed",
      "Wait for the pending prop order",
      "No XMR to sell",
      "Trading did not stop. Restart to retry",
      "Order pending",
      "Not sent: price or account changed",
      "Not sent: controls changed",
      "no further step",
      DeskController.orderChanged,
      SessionKey.unavailable,
      TradingWorker.storageFailedMessage,
      TradingWorker.signingFailedMessage,
      Workspace.closeRefused,
      Workspace.canceling,
      Workspace.cancelRefused,
      TradingWorker.closeNextMessage
    )
    // Prop market fills on the status row, at the largest amount a prop wallet can hold, whole.
    val fills = Vector(
      (propFill(Side.Buy, "1000000", "1000000", "1000"), "Bought $"),
      (propFill(Side.Sell, "1000000", "1000000", "1000"), "Sold $"),
      (propFill(Side.Buy, "1000000", "999999.999", "1000"), "Part filled $")
    ).map { (raw, word) =>
      val shown = row(
        frame(live, PanelView(mode = "PROP", actionNotice = Some(raw), actionNumber = 999999L)),
        4
      )
      assert(shown.startsWith(word) && shown.endsWith(" #999999"), shown)
      shown
    }
    for text <- planned.flatten ++ others ++ fills do
      assert(text.length <= 38, s"${text.length}: $text")
  }

  test("no control byte reaches the status row from a pass through, fallback, block or dollar") {
    val clear = 27.toChar.toString + "[2J"
    val frames = Vector(
      drawn(
        ready,
        PanelView(mode = "LIVE", actionNotice = Some(s"Nothing to cancel at that price$clear"))
      ),
      drawn(ready, PanelView(mode = "LIVE", actionNotice = Some(s"Odd $clear text"))),
      drawn(
        ready.copy(block = Some(s"waiting $clear"), blockCode = BlockCode.Market),
        PanelView(mode = "LIVE")
      ),
      drawn(ready, PanelView(mode = "LIVE", actionNotice = Some(s"XMR filled $$21.84$clear at 1")))
    )
    for f <- frames do assert(!f.lines.exists(_.contains(27.toChar)), f.lines.mkString("\n"))
  }

  test("a fill on another market leaves this market's plot as it is") {
    def plot(fills: Vector[Fill]): Vector[String] =
      frame(live.copy(fills = fills), limit.copy(ticket = None)).lines
        .slice(1, 10)
        .map(line => Chart.plain(line).drop(38))
    val btc = Fill("BTC", Side.Buy, now - 11_000, BigDecimal("95000"), BigDecimal("0.001"))
    assertEquals(plot(Vector(btc)), plot(Vector.empty))
  }

  test("a live short draws its liquidation above the range in the buy colour") {
    val short = live.copy(
      position = Some(BigDecimal("-0.2")),
      portfolio = live.portfolio.copy(
        tokenValue = Some(BigDecimal("-109.22")),
        liquidation = Some(BigDecimal("600"))
      ),
      resting = Vector.empty
    )
    val coloured = frame(short, limit.copy(ticket = None), style = Style.colour)
    assert(
      coloured.lines(1).startsWith(Style.colour.bid + "          600.00," + Screen.reset),
      coloured.lines(1)
    )
  }

  test("a live long draws its liquidation price on the axis and the range stays as it was") {
    val plain = live.copy(resting = Vector(order("s1", Side.Sell, "552.00", "0.1")))
    val liquidated =
      plain.copy(portfolio = plain.portfolio.copy(liquidation = Some(BigDecimal("500"))))
    val panel = limit.copy(ticket = None)
    val marked = frame(liquidated, panel)
    val bare = frame(plain, panel)
    // Far below the range, it sits on the bottom price row, shown above its true place.
    assertEquals(Chart.plain(marked.lines(9)).take(17), "          500.00'")
    // The scale, the depth bars and the plot beside every price row are as they were.
    def beside(f: WorkspaceFrame): Vector[String] =
      f.lines.slice(1, 10).map(Chart.plain(_).drop(17))
    assertEquals(beside(marked), beside(bare))
    assertEquals(
      marked.axis.map(OrdersAxis.selectable),
      bare.axis.map(OrdersAxis.selectable)
    )
    assert(marked.axis.exists(_.marks.exists(_.kind == AxisKind.Liquidation)), marked.axis)
    // In colour it takes the sell colour of a long's close, marked no weight.
    val coloured = frame(liquidated, panel, style = Style.colour)
    assert(
      coloured.lines(9).startsWith(Style.colour.ask + "          500.00'" + Screen.reset),
      coloured.lines(9)
    )
    // View draws no liquidation, as it draws no entry.
    val viewed = frame(liquidated, PanelView(mode = "VIEW"))
    assert(!viewed.lines.exists(Chart.plain(_).contains("500.00")), viewed.lines.mkString("\n"))
  }

  test("the ticket's and the entry's prices widen the scale, and the ticket line takes its row") {
    // The plain numbers: a price in columns 8 to 15 with no amount before it and no tie after it.
    def plain(f: WorkspaceFrame): Vector[Double] =
      f.lines.slice(1, 10).map(Chart.plain).collect {
        case line
            if line.take(8).isBlank && line.charAt(16) == ' ' && line.slice(8, 16).trim.nonEmpty =>
          line.slice(8, 16).trim.toDouble
      }
    val high = frame(panel = limit.copy(ticket = Some(ticketAt("560.00", quantity = Some("0.1")))))
    assert(plain(high).exists(_ > 553), plain(high))
    val low = frame(live.copy(portfolio = live.portfolio.copy(entry = Some(BigDecimal("530.00")))))
    assert(plain(low).exists(_ < 543), plain(low))
    // 550.50 is on true row 1 of 543.60 to 552.40, where the rounding row is 2.
    val near = limit.copy(ticket = Some(ticketAt("550.50", quantity = Some("0.1"), reduce = true)))
    val lines = frame(panel = near, style = Style.colour).lines
    assert(lines(2).contains(Style.colour.ask + "⠒"), lines(2))
    assert(!lines(3).contains(Style.colour.ask + "⠒"), lines(3))
  }

  test("the Live label and a disabled CLOSE follow their bindings") {
    def bound(action: Bind, key: String): Bindings =
      Bindings.defaults
        .updated(action, Binding(Vector(KeyGroup(key.getBytes("UTF-8").toVector.map(_ & 0xff)))))
        .toOption
        .get
    val rebound = frame(panel = limit.copy(ticket = None, bindings = bound(Bind.Live, "u")))
    assertEquals(row(rebound, 12), "[p Prop] [u Live] m Market c Chart")
    val view = frame(panel = limit.copy(mode = "VIEW", bindings = bound(Bind.Close, "j")))
    assertEquals(row(view, 5), "[$20 *] [$100] [j Close]")
  }

  test("prop draws the mode row, the trade row and the ticket exactly as live does") {
    val panel = limit.copy(actionNotice = None)
    val l = frame(panel = panel)
    val p = frame(panel = panel.copy(mode = "PROP"))
    assertEquals(band(p).drop(5), band(l).drop(5))
    def lower(f: WorkspaceFrame): Vector[DeskButton] = f.controls.filter(_.y >= 30 - 12 + 6)
    assertEquals(lower(p), lower(l))
    assert(band(l)(5).endsWith("1 on BTC") && band(l)(10).startsWith("[Enter Place]"))
  }

  test("t follows the order type button, off while pending, with a ticket or a confirmation") {
    val calm = limit.copy(ticket = None, actionNotice = None)
    def controller(panel: PanelView): DeskController =
      new DeskController(panel, () => now, (_, _) => Left("unused"), _ => false, _ => Right(()))
    // The key and a click on the button, each on its own controller, from the same frame.
    def both(view: TradingView, panel: PanelView): (PanelView, PanelView) =
      val drawn = frame(view, panel)
      val button = drawn.controls.find(_.action == DeskAction.OrderType).get
      val keyed = controller(panel)
      keyed.publish(drawn)
      val _ = keyed.handle(Command.OrderType, pinned, view, (100, 30), false)
      val clicked = controller(panel)
      clicked.publish(drawn)
      val _ =
        clicked.handle(Command.PointerPress(button.x, button.y, 0), pinned, view, (100, 30), false)
      val _ = clicked.handle(Command.Pointer(button.x, button.y, 0), pinned, view, (100, 30), false)
      (keyed.panel, clicked.panel)
    for (name, view, panel) <- Vector(
        ("pending", live.copy(pending = true), calm),
        ("ticket", live, calm.copy(ticket = limit.ticket)),
        ("confirmation", live, calm.copy(modeReview = Some("PROP")))
      )
    do
      val drawn = frame(view, panel)
      assert(drawn.disabledButtons.exists(_.action == DeskAction.OrderType), name)
      assert(!drawn.shortcuts.contains(DeskAction.OrderType), name)
      val (keyed, clicked) = both(view, panel)
      assertEquals(keyed.orderMode, OrderMode.Limit, name)
      assertEquals(keyed, clicked, name)
    // With the button on, t switches the type, as the click does.
    val (keyed, clicked) = both(live, calm)
    assertEquals(keyed.orderMode, OrderMode.Market)
    assertEquals(keyed, clicked)
  }

  test("t does not switch through a frame drawn before an order went pending") {
    val calm = limit.copy(ticket = None, actionNotice = None)
    val controller =
      new DeskController(calm, () => now, (_, _) => Left("unused"), _ => false, _ => Right(()))
    controller.publish(frame(live, calm))
    val _ =
      controller.handle(Command.OrderType, pinned, live.copy(pending = true), (100, 30), false)
    assertEquals(controller.panel.orderMode, OrderMode.Limit)
    assertEquals(controller.panel.actionNotice, Some("Order pending"))
  }

  test("Orders, the order type and Place light only when they can act") {
    val elsewhere =
      live.copy(resting = Vector(order("b2", Side.Buy, "95000", "0.001", coin = "BTC")))
    val other = frame(elsewhere, limit.copy(ticket = None))
    assert(other.disabledButtons.exists(_.action == DeskAction.Orders), other.buttons)
    assert(!other.buttons.exists(_.action == DeskAction.Orders), other.buttons)
    val pending = frame(live.copy(pending = true), limit.copy(ticket = None))
    assert(pending.disabledButtons.exists(_.action == DeskAction.OrderType), pending.buttons)
    assert(!pending.buttons.exists(_.action == DeskAction.OrderType), pending.buttons)
    val unready = frame(live.copy(ready = false))
    assert(unready.disabledButtons.exists(_.action == DeskAction.Place), unready.buttons)
    assert(!unready.buttons.exists(_.action == DeskAction.Place), unready.buttons)
    assert(!unready.shortcuts.contains(DeskAction.Place))
  }
