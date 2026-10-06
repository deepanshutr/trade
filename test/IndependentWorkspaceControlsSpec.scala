package desk

import java.time.ZoneOffset

class IndependentWorkspaceControlsSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
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
    leverage = Some(3),
    marginMode = Some("isolated")
  )
  private val review = OrderReview(
    intent,
    plan,
    TradeTerms(TradeSizes(), Trading.defaultSlippage),
    now,
    now + 30000,
    Some(3),
    Some("isolated")
  )
  private val panels = Vector(
    PanelView(mode = "PROP"),
    PanelView(mode = "PROP", bigOrder = true),
    PanelView(mode = "LIVE"),
    PanelView(mode = "PROP", modeReview = Some("LIVE"))
  )

  private def draw(
      panel: PanelView,
      cols: Int,
      rows: Int,
      pending: Boolean = false
  ): WorkspaceFrame =
    Workspace.draw(
      snapshot,
      view.copy(pending = pending),
      panel,
      cols,
      rows,
      now,
      ZoneOffset.UTC,
      Style.mono
    )

  private def tradeAction(action: DeskAction): Boolean = action match
    case DeskAction.Prepare(_) | DeskAction.Mode(_) | DeskAction.ConfirmMode(_) =>
      true
    case _ => false

  test("every enabled action occupies its visible text and hit cells across states and sizes") {
    for
      cols <- Vector(100, 101, 103, 117, 120, 160)
      rows <- Vector(30, 31, 40)
      panel <- panels
      pending <- Vector(false, true)
    do
      val actual = draw(panel, cols, rows, pending)
      assertEquals(actual.lines.size, rows)
      assert(actual.lines.forall(_.length == cols))
      for button <- actual.buttons do
        // Every control sits in the panel, the band's left 38 columns.
        assert(button.x >= 1 && button.x + button.width - 1 <= 38 && button.y > rows - 12)
        assert(button.x + button.width - 1 <= cols && button.y >= 1 && button.y <= rows)
        val text = actual
          .lines(button.y - 1 + button.height / 2)
          .slice(button.x - 1, button.x - 1 + button.width)
        assert(
          text.contains(button.label),
          s"$cols x $rows $panel pending=$pending: $text"
        )
        if button.height == 1 then assertEquals(text, "[" + button.label + "]")
        for
          y <- button.y until button.y + button.height; x <- button.x until button.x + button.width
        do assertEquals(actual.buttons.find(_.contains(x, y)).map(_.action), Some(button.action))
      if pending then assert(!actual.buttons.exists(b => tradeAction(b.action)))
  }

  test("trade buttons keep their row, and their place while pending, across sizes") {
    for cols <- Vector(100, 101, 117, 120, 160); rows <- Vector(30, 31, 40) do
      def trades(f: WorkspaceFrame) = f.controls
        .filter(_.action.isInstanceOf[DeskAction.Prepare])
        .map(b => (b.x, b.y, b.width, b.height))
      val small = draw(PanelView(mode = "PROP"), cols, rows)
      val large = draw(PanelView(mode = "PROP", bigOrder = true), cols, rows)
      val pending = draw(PanelView(mode = "PROP"), cols, rows, pending = true)
      // A wider amount widens the labels along their row. The first button keeps its start.
      assertEquals(trades(small).map(b => (b._2, b._4)), trades(large).map(b => (b._2, b._4)))
      assertEquals(trades(small).head._1, trades(large).head._1)
      assertEquals(trades(small), trades(pending))
  }
