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
      logsUnavailable: Boolean = false,
      orderResultsUnavailable: Boolean = false,
      s: Snapshot = snapshot
  ): WorkspaceFrame =
    Workspace.draw(
      s,
      view,
      p,
      cols,
      rows,
      now,
      java.time.ZoneOffset.UTC,
      Style.mono,
      logsUnavailable = logsUnavailable,
      orderResultsUnavailable = orderResultsUnavailable
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

  test("header warnings leave whole snapshot fields visible") {
    val ctx = Ctx("400", "401", "0.0001", "210160.16", "31202156.16", "300", Some("312.452123"))
    val s = snapshot.copy(
      ctx = Some(ctx),
      feed = Health.Live(now - 9_876_500L)
    )
    val blocked = TradingView("XMR", message = "Ready", ready = true)
    val both = draw(
      blocked,
      cols = 100,
      logsUnavailable = true,
      orderResultsUnavailable = true,
      s = s
    )
    val bothHeader = both.lines.head.takeWhile(_ != '|')
    assert(bothHeader.contains("!logs") && bothHeader.contains("!results"), bothHeader)
    assert(bothHeader.contains("connection stale 9876.5s"), bothHeader)
    assert(bothHeader.contains("mid 312.452123"), bothHeader)

    val logsOnly = draw(blocked, logsUnavailable = true, s = snapshot.copy(ctx = Some(ctx)))
    val logsHeader = logsOnly.lines.head.takeWhile(_ != '|')
    assert(logsHeader.contains("!logs"), logsHeader)
    assert(!logsHeader.contains("24h +4") || logsHeader.contains("24h +4.2%"), logsHeader)

    val noWarnings = draw(blocked, s = snapshot.copy(ctx = Some(ctx)))
    val plainHeader = noWarnings.lines.head.takeWhile(_ != '|')
    assert(plainHeader.contains("mid 312.452123"), plainHeader)
  }

  test("the smallest workspace keeps every shown header field whole beside both warnings") {
    val ctx = Ctx("400", "401", "0.0001", "210160.16", "31202156.16", "300", Some("312.452123"))
    val name = "X"
    val s = snapshot.copy(coin = name, ctx = Some(ctx), feed = Health.Live(now))
    val frame = draw(
      TradingView("XMR", message = "Ready", ready = true),
      cols = Layout.minCols,
      logsUnavailable = true,
      orderResultsUnavailable = true,
      s = s
    )
    val header = frame.lines.head.takeWhile(_ != '|')
    val expected = Vector(
      "mid " + Trading.display(BigDecimal("312.452123"), 12),
      "mark 400",
      "oracle 401",
      "24h -100.0%",
      "fund 0.0100%",
      "open interest 210.2k",
      "volume 31.2M"
    )
    assert(header.contains("!logs") && header.contains("!results"), header)
    assert(header.contains("connection active 0.0s"), header)
    assert(header.contains(name), header)
    val withoutWarnings = header.takeWhile(_ != '!').trim
    val stats = withoutWarnings.drop(withoutWarnings.indexOf(name) + name.length).trim
    val shown = if stats.isEmpty then Vector.empty else stats.split("  ").toVector
    assert(shown.forall(expected.contains), s"cut or partial statistics field in: $header")
    assert(
      shown == Vector(expected(0)),
      s"expected only the complete mid before the next field: $header"
    )
  }

  test("the coin remains visible in long and warning constrained headers") {
    val longName = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890ABCD"
    for (name, feed, bad) <- Vector(
        (longName, Health.Live(now), 0),
        ("ABCDEFGHIJKLMNOPQRST", Health.Live(now - 86_400_000L), 12)
      )
    do
      val s = snapshot.copy(coin = name, feed = feed, bad = bad)
      val header = draw(
        TradingView(name, message = "Ready", ready = true),
        cols = Layout.minCols,
        logsUnavailable = true,
        orderResultsUnavailable = true,
        s = s
      ).lines.head.takeWhile(_ != '|')
      val age = (now - State.lastMsgAt(feed)).max(0L) / 1000.0
      val health = if age > 5 then "stale" else "active"
      val badText = if bad > 0 then s" bad $bad" else ""
      val status = f"connection $health $age%.1fs$badText"
      val headerWidth = Layout.workspaceChartWidth(Layout.minCols) - 15
      val marketWidth = (headerWidth - status.length - 2).min(Layout.marketLabelWidth).max(3)
      val shortened =
        if name.length <= marketWidth then name else name.take(marketWidth - 3) + "..."
      assert(header.contains(name) || header.contains(shortened), header)
  }

  test("a market prompt stays visible when status and warnings leave little room") {
    val s = snapshot.copy(
      prompt = Some("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
      feed = Health.Live(now - 86_400_000L),
      bad = 12
    )
    val header = draw(
      TradingView("XMR", message = "Ready", ready = true),
      cols = Layout.minCols,
      logsUnavailable = true,
      s = s
    ).lines.head.takeWhile(_ != '|')
    assert(header.contains("market: "), header)
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

  test("unsaved order notices yield to pending progress and current blocks") {
    val failed = panel.copy(actionNotice = Some("Order result #1 not saved"), actionNumber = 1)
    val unsaved = draw(
      TradingView("XMR", message = "Ready", ready = true),
      failed.copy(mode = "VIEW"),
      Layout.minCols,
      Layout.minRows
    )
    assert(
      unsaved.lines.exists(_.contains("Order result #1 not saved")),
      unsaved.lines.mkString("\n")
    )
    assert(!unsaved.lines.exists(_.contains("not saved #1")), unsaved.lines.mkString("\n"))

    val pending = draw(
      TradingView("XMR", message = "PAPER saving 3s | XMR", pending = true),
      failed,
      Layout.minCols,
      Layout.minRows,
      orderResultsUnavailable = true
    )
    assert(pending.lines.exists(_.contains("Saving 3s")), pending.lines.mkString("\n"))
    assert(
      !pending.lines.exists(_.contains("Order result #1 not saved")),
      pending.lines.mkString("\n")
    )
    assert(pending.lines.head.contains("!results"), pending.lines.head)

    val blocked = draw(
      TradingView("XMR", message = "Trading stopped", block = Some("Trading stopped")),
      failed,
      Layout.minCols,
      Layout.minRows,
      orderResultsUnavailable = true
    )
    assert(blocked.lines.exists(_.contains("Trading stopped")), blocked.lines.mkString("\n"))
    assert(
      !blocked.lines.exists(_.contains("Order result #1 not saved")),
      blocked.lines.mkString("\n")
    )
    assert(blocked.lines.head.contains("!results"), blocked.lines.head)

    val healthy = draw(TradingView("XMR", message = "Ready", ready = true), panel)
    assert(!healthy.lines.head.contains("!results"), healthy.lines.head)
  }

  test("an unsaved notice does not change a block status line") {
    val view = TradingView(
      "XMR",
      message = "Trading stopped",
      block = Some("Trading stopped\nextra detail"),
      blockCode = BlockCode.Blocked
    )
    val failed = panel.copy(actionNotice = Some("Order result #7 not saved"), actionNumber = 7)
    val frame = draw(view, failed)
    assert(frame.lines.exists(_.contains("Trading stopped #7")), frame.lines.mkString("\n"))
    assert(!frame.lines.exists(_.contains("extra detail")), frame.lines.mkString("\n"))
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
