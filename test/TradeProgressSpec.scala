package desk

import java.nio.file.Files
import java.time.ZoneId
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong

class TradeProgressSpec extends TempDirectorySuite:
  private val terms = TradeTerms(TradeSizes(), Trading.defaultSlippage)
  private val start = 1800000000000L

  private class Prop(balance: BigDecimal = BigDecimal(10000), fee: BigDecimal = BigDecimal(0)):
    val clock = new AtomicLong(start)
    val ledger =
      PropLedger.open(tempDirectory("trade-progress-"), "mainnet", balance, fee)
    val worker = new PropWorker(
      TradingConfig(),
      "XMR",
      ledger,
      new Rest:
        def post(body: String): Option[String] =
          Some(
            """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5},{"name":"BTC","szDecimals":2,"maxLeverage":40}]}"""
          )
      ,
      () => clock.get(),
      () => clock.get()
    )
    var book = Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), start)
    def publish(depth: String = "50"): Unit =
      book = book.copy(asks = Vector(Level("100", depth, 1)), time = clock.incrementAndGet())
      worker.observe(Event.BookUpdate("XMR", book, true))
    def open(): Unit =
      worker.start()
      worker.observe(Event.SocketUp)
      publish()
      Wait.until("metadata unavailable")(worker.view.ready)
    def review(): OrderReview =
      val intent = Trading.action(0)
      OrderReview(
        intent,
        worker.preview(intent, terms).toOption.get,
        terms,
        clock.get(),
        clock.get() + 30000
      )
    def close(): Unit = worker.close()

  test("fees that exceed prop capacity are explained before Send without any write") {
    val f = new Prop(BigDecimal(500), BigDecimal("0.001"))
    try
      f.open()
      val before = f.ledger.account
      val result = f.worker.preview(Trading.action(0), terms)
      assert(result.isLeft, s"unaffordable order reached review: $result")
      assert(result.swap.toOption.get.toLowerCase.contains("limit"))
      assertEquals(f.ledger.account, before)
    finally f.close()
  }

  test("a repeated capacity refusal gives a distinct acknowledgement on the main screen") {
    val f = new Prop(BigDecimal(200))
    val c = new DeskController(
      PanelView(mode = "PROP"),
      () => f.clock.get(),
      f.worker.preview,
      f.worker.submitReviewed,
      _ => Left("disabled")
    )
    try
      f.open()
      draw(c, f.worker.view)
      c.handle(Command.PlaceOrder(Trading.action(0)), snapshot, f.worker.view, (100, 30), false)
      val first = draw(c, f.worker.view)
      capture("100x30-mono-repeated-first", first)
      assert(first.lines.exists(_.contains("Insufficient funds")))
      c.handle(Command.PlaceOrder(Trading.action(0)), snapshot, f.worker.view, (100, 30), false)
      val second = draw(c, f.worker.view)
      capture("100x30-mono-repeated-second", second)
      assert(first.lines != second.lines, "the repeated refusal looked like no response")
      assertEquals(c.panel.actionNumber, 2L)
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  test("a flat account below every preset gets an achievable settings route") {
    val f = new Prop(BigDecimal(200))
    try
      f.open()
      val reason = f.worker.preview(Trading.action(0), terms).swap.toOption.get
      assert(reason.contains("settings"), reason)
      assert(reason.contains("applies to the next order"), reason)
      assert(!reason.contains("restart"), reason)
      assert(!reason.contains("reduce the position"), "flat account has no position to reduce")
    finally f.close()
  }

  test("prop receipts retain exact time in a human-readable form") {
    val f = new Prop()
    try
      f.open()
      assert(f.worker.submitReviewed(f.review()))
      Wait.until("receipt missing")(!f.worker.view.pending)
      val result = f.worker.view.outcome.get
      assert(result.contains(java.time.Instant.ofEpochMilli(f.clock.get()).toString), result)
      assert(!result.contains(f.clock.get().toString), result)
      assert(result.contains("\nFee"), result)
    finally f.close()
  }

  test("wallet refusals and a losing sale preserve the remaining spendable cash") {
    val f = new Prop(BigDecimal(200))
    def one(side: Side, price: BigDecimal): OrderPlan =
      OrderPlan(
        "XMR",
        0,
        side,
        OrderKind.Hard,
        BigDecimal(1),
        price,
        price,
        BigDecimal(1),
        price,
        Some(price),
        BigDecimal(0),
        false
      )
    try
      f.open()
      val initial = f.ledger.account
      assert(f.ledger.fill(one(Side.Sell, BigDecimal(100)), start).isLeft)
      assertEquals(f.ledger.account, initial)
      assert(f.ledger.fill(one(Side.Buy, BigDecimal(200)), start + 1).isRight)
      assertEquals(f.ledger.account.cash, BigDecimal(0))
      val spent = f.ledger.account
      assert(f.ledger.fill(one(Side.Buy, BigDecimal(1)), start + 2).isLeft)
      assertEquals(f.ledger.account, spent)
      assert(f.ledger.fill(one(Side.Sell, BigDecimal(50)), start + 3).isRight)
      val remaining = f.ledger.account
      assertEquals(remaining.cash, BigDecimal(50))
      assertEquals(remaining.realized, BigDecimal(-150))
      assert(remaining.positions.isEmpty)
      val reason = f.worker.preview(Trading.action(0), terms).swap.toOption.get
      assert(reason.contains("settings"), reason)
      assertEquals(f.ledger.account, remaining)
      assertEquals(f.ledger.account.count, 2L)
    finally f.close()
  }

  test("capacity uses actual partial depth and checks larger depth again after review") {
    val f = new Prop(BigDecimal(200))
    try
      f.open()
      f.publish("1")
      val review = f.review()
      assertEquals(review.plan.estimatedQuantity, BigDecimal(1))
      f.publish("50")
      assert(f.worker.submitReviewed(review))
      Wait.until("capacity result absent")(!f.worker.view.pending)
      assert(f.worker.view.outcome.exists(_.toLowerCase.contains("limit")))
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  test("a newer quote excludes stale depth from prop capacity preview") {
    val f = new Prop(BigDecimal(200))
    try
      f.open()
      val depth = f.book.copy(
        asks = Vector(Level("100", "4", 1), Level("100.05", "1", 1)),
        time = f.clock.incrementAndGet()
      )
      f.worker.observe(Event.BookUpdate("XMR", depth, true))
      f.worker.observe(
        Event.BboUpdate(
          "XMR",
          Level("100", "1", 1),
          Level("100.05", "1", 1),
          f.clock.incrementAndGet()
        )
      )
      val result = f.worker.preview(Trading.action(0), terms)
      assert(result.isRight, s"affordable fresh partial fill rejected: $result")
      val plan = result.toOption.get
      assertEquals(plan.estimatedQuantity, BigDecimal(1))
      assertEquals(plan.estimatedValue, BigDecimal("100.05"))
      assert(
        f.worker.submitReviewed(
          OrderReview(Trading.action(0), plan, terms, f.clock.get(), f.clock.get() + 30000)
        )
      )
      Wait.until("partial result absent")(!f.worker.view.pending)
      assertEquals(f.ledger.account.positions("XMR").quantity, BigDecimal(1))
    finally f.close()
  }

  test("every identical completed refusal is delivered even when no pending frame is sampled") {
    val f = new Prop(BigDecimal(200))
    val updates = new OrderUpdates
    var received = Vector.empty[String]
    try
      f.open()
      for _ <- 1 to 2 do
        f.publish("1")
        val review = f.review()
        f.publish("50")
        assert(f.worker.submitReviewed(review))
        Wait.until("refusal absent")(!f.worker.view.pending)
        updates.collect(f.worker.view)(text => received :+= text)
        updates.collect(f.worker.view)(text => received :+= text)
      assertEquals(received.size, 2)
      assertEquals(received.distinct.size, 1)
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  private val rules = MarketRules(0, 2, 5)
  private val book = Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), start)
  private val snapshot = Snapshot.initial("XMR", start).copy(book = Some(book))
  private val ready = TradingView(
    "XMR",
    Some(BigDecimal(0)),
    "Prop ready",
    ready = true,
    rules = Some(rules),
    bid = Some(BigDecimal("99.99")),
    ask = Some(BigDecimal(100))
  )
  private def plan(intent: OrderIntent, terms: TradeTerms): Either[String, OrderPlan] =
    Trading.plan(
      "XMR",
      rules,
      book,
      ready.reference(intent.side).get,
      BigDecimal(0),
      intent,
      terms.sizes,
      1,
      terms.slippage
    )
  private def draw(
      c: DeskController,
      view: TradingView = ready,
      now: Long = start,
      cols: Int = 100,
      rows: Int = 30
  ): WorkspaceFrame =
    val frame =
      Workspace.draw(snapshot, view, c.panel, cols, rows, now, ZoneId.of("UTC"), Style.mono)
    c.publish(frame)
    frame

  for mouse <- Vector(false, true) do
    test(s"unavailable order explains its reason for mouse=$mouse") {
      val c = new DeskController(
        PanelView(mode = "PROP"),
        () => start,
        (_, _) => fail("unavailable control prepared an order"),
        _ => fail("sent unavailable order"),
        _ => Left("disabled")
      )
      val stale = ready.copy(
        ready = false,
        message = "Depth stale",
        block = Some("Depth stale. Waiting for fresh prices")
      )
      val frame = draw(c, stale)
      if mouse then
        val b = frame.disabledButtons.find(_.action == DeskAction.Prepare(Trading.action(0))).get
        c.handle(Command.PointerPress(b.x, b.y, 0), snapshot, stale, (100, 30), false)
        c.handle(Command.Pointer(b.x, b.y, 0), snapshot, stale, (100, 30), false)
      else c.handle(Command.PlaceOrder(Trading.action(0)), snapshot, stale, (100, 30), false)
      assert(c.panel.actionNotice.exists(_.toLowerCase.contains("stale")), c.panel.toString)
    }

  test("progress remains readable beside a long position at all supported sizes") {
    val pending = ready.copy(
      position = Some(BigDecimal("1234567890" * 8)),
      ready = false,
      pending = true,
      message = "PROP saving 4s | XMR"
    )
    val c = new DeskController(
      PanelView(mode = "PROP"),
      () => start,
      plan,
      _ => false,
      _ => Left("disabled")
    )
    for (cols, rows) <- Vector((100, 30), (120, 40), (160, 45)) do
      val frame = draw(c, pending, cols = cols, rows = rows)
      assert(frame.lines.exists(_.contains("Saving 4s")), frame.lines.mkString("\n"))
  }

  private def capture(label: String, frame: WorkspaceFrame): Unit =
    sys.props.get("desk.ux.frames").foreach { directory =>
      val root = java.nio.file.Path.of(directory)
      Files.createDirectories(root)
      Files.writeString(
        root.resolve(label + ".txt"),
        frame.lines.map(Chart.plain).mkString("\n") + "\n"
      )
      Files.writeString(root.resolve(label + ".ansi"), frame.lines.mkString("\n") + "\n")
    }

  for
    (cols, rows) <- Vector((100, 30), (120, 40), (160, 45))
    style <- Vector(Style.mono, Style.colour)
    scenario <- Vector("filled", "partial", "no-fill", "capacity")
  do
    test(s"visible one-click prop $scenario pipeline at $cols x$rows colour=" + style.colour) {
      val f = new Prop(if scenario == "capacity" then BigDecimal(200) else BigDecimal(10000))
      val c = new DeskController(
        PanelView(mode = "PROP"),
        () => f.clock.get(),
        f.worker.preview,
        f.worker.submitReviewed,
        _ => Left("disabled")
      )
      val updates = new OrderUpdates
      val prefix = cols.toString + "x" + rows + "-" + (if style.colour then "colour"
                                                       else "mono") + "-" + scenario
      def update(label: String): WorkspaceFrame =
        val began = System.nanoTime()
        val view = f.worker.view
        updates.collect(view)(c.addMessage("order", _))
        val frame = Workspace.draw(
          snapshot.copy(book = Some(f.book), feed = Health.Live(f.clock.get())),
          view,
          c.panel,
          cols,
          rows,
          f.clock.get(),
          ZoneId.of("UTC"),
          style
        )
        c.publish(frame)
        assert(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began) <= 250L)
        assert(frame.lines.forall(line => Chart.plain(line).length == cols))
        assertEquals(frame.lines.size, rows)
        capture(prefix + "-" + label, frame)
        frame
      def key(command: Command): Unit =
        c.handle(command, snapshot, f.worker.view, (cols, rows), false)
      try
        f.open()
        if scenario == "partial" || scenario == "capacity" then f.publish("1")
        val before = f.worker.preview(Trading.action(0), terms).toOption.get
        val first = update("ready")
        val buy = first.buttons.find(_.action == DeskAction.Prepare(Trading.action(0))).get
        f.worker.synchronized {
          if style.colour then
            key(Command.PointerPress(buy.x, buy.y, 0))
            key(Command.Pointer(buy.x, buy.y, 0))
          else key(Command.PlaceOrder(Trading.action(0)))
          assert(f.worker.view.pending)
          val pending = update("queued")
          assert(pending.lines.exists(_.contains("Queued 0s")))
          assertEquals(f.worker.view.position, Some(BigDecimal(0)))
          assert(!pending.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
          if scenario == "capacity" then f.publish("50")
          if scenario == "no-fill" then
            f.worker.observe(
              Event.BboUpdate(
                "XMR",
                Level("100.99", "1", 1),
                Level("101", "1", 1),
                f.clock.incrementAndGet()
              )
            )
        }
        Wait.until("prop completion missing")(!f.worker.view.pending)
        val completed = update("result")
        val expected = scenario match
          case "no-fill"  => "No fill"
          case "capacity" => "Insufficient funds"
          case "partial"  => "Part filled"
          case _          => "Bought"
        assert(completed.lines.exists(_.contains(expected)), completed.lines.mkString("\n"))
        val expectedPosition = if scenario == "filled" then BigDecimal(5)
        else if scenario == "partial" then BigDecimal(1)
        else BigDecimal(0)
        assertEquals(f.worker.view.position, Some(expectedPosition))
        assertEquals(c.panel.actionNumber, 1L)
        assertEquals(
          f.worker.view.lastPlan.map(p => (p.quantity, p.limit)),
          Some((before.quantity, before.limit))
        )
        assertEquals(f.ledger.account.count, if expectedPosition == 0 then 0L else 1L)
      finally f.close()
    }
