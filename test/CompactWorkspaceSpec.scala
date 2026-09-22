package desk

import java.time.ZoneOffset

class CompactWorkspaceSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val market = Snapshot.initial("XMR", now)
  private val view = TradingView("XMR", message = "View only")

  test("normal trading view shows three horizons without the transcript or running logs") {
    val panel = PanelView(
      dataStatus = "capture log should stay hidden",
      analysisStatus = "running model log should stay hidden",
      messages = Vector(DeskMessage(1, now, "analysis", "verbose answer should stay hidden"))
    )
    for (cols, rows) <- Vector((100, 30), (120, 40), (160, 45)) do
      val frame = Workspace.draw(market, view, panel, cols, rows, now, ZoneOffset.UTC, Style.mono)
      val text = frame.lines.mkString("\n")
      assert(text.contains("LONG") && text.contains("MID") && text.contains("SHORT"), text)
      assert(!text.contains("should stay hidden"), text)
      assert(!frame.buttons.exists(_.action.isInstanceOf[DeskAction.Feedback]))
      assertEquals(frame.lines.size, rows)
      assert(frame.lines.forall(_.length == cols))
  }

  test("compact view exposes details and mute below the visible trade controls") {
    val frame = Workspace.draw(market, view, PanelView(), 100, 30, now, ZoneOffset.UTC, Style.mono)
    assert(frame.lines.exists(_.contains("Details")))
    assert(frame.lines.exists(_.contains("Mute")))
    val buy = frame.disabledButtons
      .find(_.action == DeskAction.Prepare(OrderIntent(Side.Buy, OrderKind.Hard)))
      .get
    assertEquals(buy.height, 3)
    assert(frame.buttons.find(_.action == DeskAction.Details).get.y >= buy.y + buy.height)
    assert(frame.lines(buy.y).contains("(1 BUY $500)"))
    assert(!frame.lines.exists(_.contains("tokens")))
  }

  test("missing evidence is not presented as zero probability or a trading direction") {
    val frame = Workspace.draw(market, view, PanelView(), 120, 40, now, ZoneOffset.UTC, Style.mono)
    val text = frame.lines.map(_.drop(Layout.workspaceMarketWidth(120) + 1)).mkString("\n")
    assert(text.contains("Waiting") || text.contains("No history"), text)
    assert(!text.contains("0%"), text)
    assert(!text.contains("↑") && !text.contains("↓"), text)
  }
