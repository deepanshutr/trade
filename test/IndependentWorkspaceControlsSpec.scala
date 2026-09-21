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
    BigDecimal(5)
  )
  private val view = TradingView(
    "XMR",
    message = "Ready",
    ready = true,
    rules = Some(MarketRules(0, 2, 5)),
    leverage = Some(3),
    marginMode = Some("isolated")
  )
  private val review = OrderReview(intent, plan, now, now + 30000, Some(3), Some("isolated"))
  private val panels = Vector(
    PanelView(mode = "PAPER"),
    PanelView(mode = "PAPER", target = Some(Side.Buy -> BigDecimal("108.5"))),
    PanelView(mode = "PAPER", review = Some(review)),
    PanelView(mode = "LIVE", review = Some(review)),
    PanelView(mode = "PAPER", modeReview = Some("LIVE")),
    PanelView(
      mode = "PAPER",
      detailPage = DetailPage.Answers,
      messages = Vector(DeskMessage(1, now, "analysis", "A visible analysis"))
    ),
    PanelView(mode = "PAPER", input = Some("A question"), detailPage = DetailPage.Answers)
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
    case DeskAction.Prepare(_) | DeskAction.Target(_) | DeskAction.Submit | DeskAction.Mode(_) |
        DeskAction.ConfirmMode(_) =>
      true
    case _ => false

  test("every enabled action occupies its visible text and hit cells across states and sizes") {
    for
      cols <- Vector(100, 101, 103, 117, 120, 160)
      rows <- Vector(30, 31, 40)
      panel <- panels
      details <- Vector(false, true)
      pending <- Vector(false, true)
    do
      val actual = draw(panel.copy(details = details), cols, rows, pending)
      assertEquals(actual.lines.size, rows)
      assert(actual.lines.forall(_.length == cols))
      for button <- actual.buttons do
        assert(button.x > Layout.workspaceMarketWidth(cols) + 1)
        assert(button.x + button.width - 1 <= cols && button.y >= 1 && button.y <= rows)
        assertEquals(
          actual.lines(button.y - 1).slice(button.x - 1, button.x - 1 + button.width),
          "[" + button.label + "]",
          s"$cols x $rows $panel details=$details pending=$pending"
        )
        for x <- button.x until button.x + button.width do
          assertEquals(actual.hit(x, button.y, cols, rows, "XMR"), Some(button.action))
      if pending || panel.input.nonEmpty then
        assert(!actual.buttons.exists(b => tradeAction(b.action)))
  }

  test("opening Details keeps every trade action at the same coordinates") {
    for
      cols <- Vector(100, 101, 117, 120, 160)
      rows <- Vector(30, 31, 40)
      panel <- panels
    do
      val closed = draw(panel, cols, rows)
      val opened = draw(panel.copy(details = true), cols, rows)
      assertEquals(
        closed.buttons.filter(b => tradeAction(b.action)),
        opened.buttons.filter(b => tradeAction(b.action))
      )
      assert(closed.buttons.filter(b => tradeAction(b.action)).forall(_.y >= rows - 13))
      if panel.review.nonEmpty then
        assert(closed.completeReview && opened.completeReview)
        assert(closed.lines(rows - 11).contains("XMR buy Hard"))
        assert(closed.lines(rows - 5).contains("Expires in"))
        assert(closed.buttons.exists(_.action == DeskAction.Submit))
  }
