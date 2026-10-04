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
        )
      )
    do
      val shown = notice(raw, number)
      assert(shown.contains(expected), s"$raw\n$shown")
      for wrong <- Vector("Not sent", "Resting $", "Bought", "Sold", "Waiting for prices")
      do assert(!shown.contains(wrong) || expected.startsWith(wrong), s"$raw\n$shown")
  }

  test("a cancel before sending still reads Not sent") {
    val shown = notice("XMR canceled before sending", 14L)
    assert(shown.contains("Not sent #14"), shown)
    assert(!shown.contains("Canceled"), shown)
  }

  test("a part fill reads Part filled, never Filled") {
    val shown = notice("XMR part filled $5.46 of $21.84 at 546.09", 3L)
    assert(shown.contains("Part filled $5.46 #3"), shown)
    assert(!shown.contains("Filled $5.46"), shown)
  }

  test("a prop market fill keeps its side word and never shows the fee as its amount") {
    val shown = notice("PROP XMR buy filled 1 at 100\nFee $0.05 | #2 2026-09-30T00:00:00Z", 2L)
    assert(shown.contains("Bought #2"), shown)
    assert(!shown.contains("$0.05"), shown)
  }

  test("refusals from the ticket, the axis and the worker pass through unchanged") {
    for raw <- Vector(
        "Price would take liquidity. Move it below the ask",
        "Price would take liquidity. Move it above the bid",
        "Nothing to cancel at that price",
        "A limit order takes a sized press",
        TradingWorker.restingCapMessage,
        "Read the trading key first. Press r"
      )
    do
      val shown = notice(raw, 4L)
      assert(shown.contains(raw.take(30)), s"$raw\n$shown")
      for wrong <- Vector("Waiting for prices", "Not sent", "Sending")
      do assert(!shown.contains(wrong), s"$raw\n$shown")
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
