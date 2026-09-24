package desk

class StatusPanelSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val panel = PanelView(mode = "PAPER", actionNotice = Some("PAPER XMR buy filled 5"))
  private def draw(view: TradingView, p: PanelView = panel, cols: Int = 100): WorkspaceFrame =
    Workspace.draw(snapshot, view, p, cols, 30, now, java.time.ZoneOffset.UTC, Style.mono)

  test("current connection and account blocks replace old success immediately") {
    for (reason, expected) <- Vector(
        "connection disconnected" -> "Reconnecting",
        "selected depth stale" -> "Waiting for prices",
        "account data stale" -> "Account unavailable"
      )
    do
      val frame = draw(TradingView("XMR", message = reason, block = Some(reason)))
      assert(frame.lines.exists(_.contains(expected)))
      assert(!frame.lines.exists(_.contains("Bought")))
      assert(!frame.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
  }

  test("pending progress separates queue, save and unresolved exchange result") {
    for (message, expected) <- Vector(
        "PAPER queued 0s | XMR" -> "Queued 0s",
        "PAPER checking 1s | XMR" -> "Checking 1s",
        "PAPER saving 4s | XMR" -> "Saving 4s",
        "XMR waiting for order result" -> "Confirming result"
      )
    do
      val frame = draw(TradingView("XMR", message = message, pending = true))
      assert(frame.lines.exists(_.contains(expected)), frame.lines.mkString("\n"))
      assert(!frame.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
  }

  test("local storage warnings stay visible outside execution and preserve frame width") {
    for cols <- Vector(100, 120, 160) do
      val frame = draw(
        TradingView("XMR", message = "Ready", ready = true),
        panel.copy(dataStatus = "Logs unavailable", historyFailure = Some("save failed")),
        cols
      )
      assert(frame.lines.head.contains("!logs !save"))
      assert(frame.lines.forall(_.length == cols))
      assert(frame.lines.exists(_.contains("Bought")))
  }

  test("a fill without a reported side cannot be labelled as a sale") {
    for (result, expected) <- Vector(
        "PAPER XMR buy filled 1" -> "Bought",
        "PAPER XMR sell filled 1" -> "Sold",
        "XMR filled 1 @ 100" -> "Filled"
      )
    do
      val frame = draw(
        TradingView("XMR", message = result, ready = true),
        panel.copy(actionNotice = Some(result))
      )
      assert(frame.lines.exists(_.contains(expected)))
  }

  test("a refused stale click does not invent an account outage") {
    val frame = draw(
      TradingView("XMR", message = "ready", ready = true),
      panel.copy(actionNotice = Some("Not sent: price or account changed"))
    )
    assert(frame.lines.exists(_.contains("Not sent")))
    assert(!frame.lines.exists(_.contains("Account unavailable")))
  }
