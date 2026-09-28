package desk

class StatusPanelSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val panel = PanelView(mode = "PAPER", actionNotice = Some("PAPER XMR buy filled 5"))
  private def draw(
      view: TradingView,
      p: PanelView = panel,
      cols: Int = 100,
      logsUnavailable: Boolean = false
  ): WorkspaceFrame =
    Workspace.draw(
      snapshot,
      view,
      p,
      cols,
      30,
      now,
      java.time.ZoneOffset.UTC,
      Style.mono,
      logsUnavailable = logsUnavailable
    )

  test("current connection and account blocks replace old success immediately") {
    for (reason, code, expected) <- Vector(
        ("connection disconnected", BlockCode.Connection, "Reconnecting"),
        ("selected depth stale", BlockCode.Prices, "Waiting for prices"),
        ("account data stale", BlockCode.Account, "Account unavailable")
      )
    do
      val frame = draw(TradingView("XMR", message = reason, block = Some(reason), blockCode = code))
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
        panel,
        cols,
        logsUnavailable = true
      )
      assert(frame.lines.head.contains("!logs"))
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

  test("logs warning follows only the explicit flag, including a mismatched market view") {
    for coin <- Vector("XMR", "BTC"); unavailable <- Vector(false, true) do
      val frame = draw(TradingView(coin, message = "Ready"), logsUnavailable = unavailable)
      assertEquals(frame.lines.head.contains(" !logs"), unavailable)
  }

  test("the status label follows the block code when its wording changes") {
    val text = "Selected information is catching up"
    val frame =
      draw(TradingView("XMR", message = text, block = Some(text), blockCode = BlockCode.Prices))
    assert(frame.lines.exists(_.contains("Waiting for prices")), frame.lines.mkString("\n"))
  }

  test("view-only router copies do not inherit a live block label") {
    val live = TradingView(
      "XMR",
      message = "waiting",
      block = Some("selected depth stale"),
      blockCode = BlockCode.Prices
    )
    val frame = draw(live.copy(block = Some("View only")), panel.copy(mode = "VIEW"))
    assert(frame.lines.exists(_.contains("View only")))
    assert(!frame.lines.exists(_.contains("Waiting for prices")))
  }
