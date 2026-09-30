package desk

class StatusPanelSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val panel = PanelView(mode = "PAPER", actionNotice = Some("PAPER XMR buy filled 5"))
  private def draw(
      view: TradingView,
      p: PanelView = panel,
      cols: Int = 100,
      rows: Int = 30,
      logsUnavailable: Boolean = false
  ): WorkspaceFrame =
    Workspace.draw(
      snapshot,
      view,
      p,
      cols,
      rows,
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

  private def failingReceipt(receipt: String): (DeskController, OrderResultLog.Writer) =
    val failed = new java.util.concurrent.CountDownLatch(1)
    lazy val controller: DeskController = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = (number, text) =>
        writer.append(
          text,
          error => { controller.orderResultNotSaved(number); failed.countDown(); () }
        )
    )
    lazy val writer = new OrderResultLog.Writer(
      _ => throw new java.io.IOException("save failed"),
      _ => ()
    )
    controller.addMessage("order", receipt)
    assert(failed.await(2, java.util.concurrent.TimeUnit.SECONDS))
    writer.close()
    (controller, writer)

  test("failed real order receipts name their own action in the smallest frame") {
    for receipt <- Vector(
        "PAPER XMR buy filled 1 at 100\nFee $0.05 | #1 2026-09-30T00:00:00Z",
        "PAPER XMR sell filled 1 at 100\nFee $0.05 | #1 2026-09-30T00:00:00Z",
        "PAPER XMR buy partial 0.5 at 100\nFee $0.03 | #1 2026-09-30T00:00:00Z",
        "Paper no fill: no depth inside reviewed limit",
        "XMR order rejected"
      )
    do
      val (controller, writer) = failingReceipt(receipt)
      try
        val frame = draw(
          TradingView("XMR", message = "Ready", ready = true),
          controller.panel,
          Layout.minCols,
          Layout.minRows
        )
        assert(
          frame.lines.exists(_.contains("Order result #1 not saved")),
          frame.lines.mkString("\n")
        )
      finally writer.close()
  }

  test("a failed earlier order keeps its number after a newer order notice") {
    val started = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val failed = new java.util.concurrent.CountDownLatch(1)
    lazy val controller: DeskController = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = (number, text) =>
        writer.append(
          text,
          error => { controller.orderResultNotSaved(number); failed.countDown(); () }
        )
    )
    lazy val writer = new OrderResultLog.Writer(
      text => {
        if text.startsWith("PAPER XMR buy filled") then
          started.countDown()
          release.await(2, java.util.concurrent.TimeUnit.SECONDS)
          throw new java.io.IOException("save failed")
        ()
      },
      _ => ()
    )
    try
      controller.addMessage(
        "order",
        "PAPER XMR buy filled 1 at 100\nFee $0.05 | #1 2026-09-30T00:00:00Z"
      )
      assert(started.await(2, java.util.concurrent.TimeUnit.SECONDS))
      controller.addMessage(
        "order",
        "PAPER XMR sell filled 1 at 100\nFee $0.05 | #2 2026-09-30T00:00:00Z"
      )
      val newer = draw(
        TradingView("XMR", message = "Ready", ready = true),
        controller.panel,
        Layout.minCols,
        Layout.minRows
      )
      assert(newer.lines.exists(_.contains("Sold #2")), newer.lines.mkString("\n"))
      release.countDown()
      assert(failed.await(2, java.util.concurrent.TimeUnit.SECONDS))
      val frame = draw(
        TradingView("XMR", message = "Ready", ready = true),
        controller.panel,
        Layout.minCols,
        Layout.minRows
      )
      assert(
        frame.lines.exists(_.contains("Order result #1 not saved")),
        frame.lines.mkString("\n")
      )
      assert(
        !frame.lines.exists(_.contains("Order result #2 not saved")),
        frame.lines.mkString("\n")
      )
    finally
      release.countDown()
      writer.close()
  }

  test("a successful real order receipt adds no failure status") {
    val saved = new java.util.concurrent.CountDownLatch(1)
    lazy val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = (number, text) => writer.append(text)
    )
    lazy val writer = new OrderResultLog.Writer(
      _ => { saved.countDown(); () },
      _ => fail("successful save reported failure")
    )
    try
      controller.addMessage(
        "order",
        "PAPER XMR buy filled 1 at 100\nFee $0.05 | #1 2026-09-30T00:00:00Z"
      )
      assert(saved.await(2, java.util.concurrent.TimeUnit.SECONDS))
      val frame = draw(
        TradingView("XMR", message = "Ready", ready = true),
        controller.panel,
        Layout.minCols,
        Layout.minRows
      )
      assert(!frame.lines.exists(_.contains("not saved")), frame.lines.mkString("\n"))
      assert(frame.lines.exists(_.contains("Bought #1")), frame.lines.mkString("\n"))
    finally writer.close()
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
