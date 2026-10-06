package desk

import java.time.ZoneOffset

class LiveControlsSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val readyView = TradingView(
    "XMR",
    message = "Ready",
    ready = true,
    leverage = Some(5),
    portfolio = PortfolioView(tokenValue = Some(500), stables = Some(1000))
  )
  private def draw(
      view: TradingView,
      panel: PanelView,
      cols: Int = 100,
      rows: Int = 30,
      snapshot: Snapshot = Snapshot.initial("XMR", now)
  ): WorkspaceFrame =
    Workspace.draw(snapshot, view, panel, cols, rows, now, ZoneOffset.UTC, Style.mono)
  private def frame(
      exposure: BigDecimal,
      mode: String,
      cols: Int,
      rows: Int,
      leverage: Option[Int] = Some(5)
  ): WorkspaceFrame =
    draw(
      readyView.copy(
        leverage = leverage,
        portfolio = PortfolioView(tokenValue = Some(exposure), stables = Some(1000))
      ),
      PanelView(mode = mode),
      cols,
      rows
    )
  // The two trade buttons, one row each, left to right.
  private def tiles(frame: WorkspaceFrame): Vector[(String, DeskAction)] =
    frame.controls
      .filter(_.action.isInstanceOf[DeskAction.Prepare])
      .sortBy(_.x)
      .map(b => (b.label, b.action))
  private def hard(side: Side, big: Boolean = false): DeskAction =
    DeskAction.Prepare(OrderIntent(side, OrderKind.Hard, big = big))

  test("live controls name both directions and the observed five-times leverage") {
    for (cols, rows) <- Vector((100, 30), (120, 40), (360, 99)) do
      val live = frame(500, "LIVE", cols, rows)
      val text = live.lines.mkString("\n")
      assert(text.contains("LIVE XMR 5x LONG"), text)
      assert(live.buttons.exists(b => b.label == "1 LONG $500 x5" && b.action == hard(Side.Buy)))
      assert(
        live.buttons.exists(b => b.label == "3 SHORT $500 x5" && b.action == hard(Side.Sell))
      )
      assert(live.lines.forall(_.length == cols))
  }

  test("live labels follow the position and the opposite side reads REDUCE") {
    val cases = Vector(
      None -> ("1 LONG $500 x5", "3 SHORT $500 x5"),
      Some(BigDecimal(0)) -> ("1 LONG $500 x5", "3 SHORT $500 x5"),
      Some(BigDecimal("0.2")) -> ("1 LONG $500 x5", "3 REDUCE $500 x5"),
      Some(BigDecimal("-0.2")) -> ("1 REDUCE $500 x5", "3 SHORT $500 x5")
    )
    for (position, (buy, sell)) <- cases do
      val live = draw(readyView.copy(position = position), PanelView(mode = "LIVE"))
      assertEquals(tiles(live), Vector(buy -> hard(Side.Buy), sell -> hard(Side.Sell)), position)
    val big = draw(
      readyView.copy(position = Some(BigDecimal("0.2"))),
      PanelView(mode = "LIVE", bigOrder = true)
    )
    assertEquals(
      tiles(big),
      Vector(
        "2 LONG $1000 x5" -> hard(Side.Buy, true),
        "4 REDUCE $1000 x5" -> hard(Side.Sell, true)
      )
    )
  }

  test("the leverage suffix appears only when the view has a leverage") {
    val unknown =
      draw(
        readyView.copy(leverage = None, position = Some(BigDecimal(0))),
        PanelView(mode = "LIVE")
      )
    assertEquals(tiles(unknown).map(_._1), Vector("1 LONG $500", "3 SHORT $500"))
  }

  test("short exposure is named without changing the fixed buy and sell targets") {
    val long = frame(500, "LIVE", 100, 30)
    val short = frame(-500, "LIVE", 100, 30)
    assert(short.lines.mkString("\n").contains("LIVE XMR 5x SHORT"))
    assertEquals(short.buttons, long.buttons)
  }

  test("the live heading does not invent leverage and flat exposure is explicit") {
    val flat = frame(0, "LIVE", 100, 30, None)
    val text = flat.lines.mkString("\n")
    assert(text.contains("LIVE XMR FLAT"))
    assert(!text.contains("5x"))
  }

  test("prop controls follow the live rule at the leverage prop reports") {
    val prop = draw(
      readyView.copy(leverage = Some(1), position = Some(BigDecimal("0.2"))),
      PanelView(mode = "PROP")
    )
    assert(prop.lines.mkString("\n").contains("PROP XMR 1x LONG"))
    assertEquals(
      tiles(prop),
      Vector("1 LONG $500 x1" -> hard(Side.Buy), "3 REDUCE $500 x1" -> hard(Side.Sell))
    )
  }

  // A long or short of 0.2 entered at 100 puts up $4.00 at 5x.
  private val entered = readyView.copy(portfolio = readyView.portfolio.copy(entry = Some(100)))

  test("CLOSE names the money the position puts up on the size row and works by key") {
    for position <- Vector("0.2", "-0.2") do
      for (cols, rows) <- Vector((100, 30), (120, 40)) do
        val live = draw(
          entered.copy(position = Some(BigDecimal(position))),
          PanelView(mode = "LIVE"),
          cols,
          rows
        )
        val close = live.buttons.filter(_.action == DeskAction.Close)
        assertEquals(close.map(_.label), Vector("x Close $4.00"), position)
        val big = live.buttons.find(_.action == DeskAction.OrderSize(true)).get
        assertEquals(close.head.y, big.y)
        assertEquals(close.head.y, rows - 12 + 4 + 1)
        assert(close.head.x > big.x + big.width, close)
        assert(live.lines(close.head.y - 1).contains("[x Close $4.00]"))
        assert(live.shortcuts.contains(DeskAction.Close))
  }

  test("CLOSE is dimmed and unnamed unless live or prop, ready, holding and not pending") {
    val long = entered.copy(position = Some(BigDecimal("0.2")))
    val live = PanelView(mode = "LIVE")
    val plain = Snapshot.initial("XMR", now)
    val cases: Vector[(String, TradingView, PanelView, Snapshot)] = Vector(
      ("flat", long.copy(position = Some(BigDecimal(0))), live, plain),
      ("unknown", long.copy(position = None), live, plain),
      ("not ready", long.copy(ready = false), live, plain),
      ("pending", long.copy(pending = true), live, plain),
      ("view", long, PanelView(mode = "VIEW"), plain),
      ("mode review", long, live.copy(modeReview = Some("PROP")), plain),
      ("market prompt", long, live, plain.copy(prompt = Some("BT")))
    )
    for (name, view, panel, snapshot) <- cases do
      val frame = draw(view, panel, snapshot = snapshot)
      assert(!frame.buttons.exists(_.action == DeskAction.Close), name)
      assert(!frame.shortcuts.contains(DeskAction.Close), name)
      assertEquals(
        frame.disabledButtons.filter(_.action == DeskAction.Close).map(_.label),
        Vector("x Close"),
        name
      )
  }

  test("CLOSE shortens its money instead of disappearing when the full label does not fit") {
    val cases = Vector(
      ("0.2", TradeSizes(), "x Close $4.00"),
      // $24691357.80 does not fit after the sizes, so the money takes its short form.
      ("1234567.89", TradeSizes(), "x Close $~2.4e7"),
      ("0.2", TradeSizes(BigDecimal("10000000"), BigDecimal("10000000")), "x Close")
    )
    for (position, sizes, expected) <- cases; big <- Vector(false, true) do
      val live = draw(
        entered.copy(position = Some(BigDecimal(position))),
        PanelView(mode = "LIVE", terms = TradeTerms(sizes, Trading.defaultSlippage), bigOrder = big)
      )
      val close = live.buttons.filter(_.action == DeskAction.Close)
      assertEquals(close.map(_.label), Vector(expected), position)
      assert(live.lines(close.head.y - 1).contains("[" + expected + "]"), position)
      assert(close.head.x + close.head.width - 1 <= 38, position)
      assert(live.shortcuts.contains(DeskAction.Close), position)
  }
