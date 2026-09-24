package desk

import java.nio.file.Files
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

class PaperBoundaryReviewSpec extends munit.FunSuite:
  private val start = 1800000000000L
  private def waitFor(label: String)(condition: => Boolean): Unit =
    val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < end do Thread.sleep(2)
    assert(condition, label)

  private class Fixture(val held: BigDecimal = BigDecimal(0)):
    val clock = new AtomicLong(start)
    val directory = Files.createTempDirectory("boundary-")
    val config = TradingConfig()
    if held > 0 then
      LocalFiles.write(
        directory.resolve("wallet-mainnet.json"),
        s"""{"cash":"${config.paperBalance}","deposit":"${config.paperBalance + held * 100}","realized":"0","fees":"0","orders":[],"count":"0","positions":{"XMR":{"quantity":"$held","entry":"100"}}}"""
      )
    val ledger = PaperLedger.open(directory, "mainnet", config.paperBalance, config.paperFee)
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some(
          """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5},{"name":"BTC","szDecimals":2,"maxLeverage":40}]}"""
        )
    val worker = new PaperWorker(config, "XMR", ledger, rest, () => clock.get(), () => clock.get())
    var latest = Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), start)
    def publish(book: Book = latest, coin: String = "XMR"): Unit =
      latest = book.copy(time = clock.incrementAndGet())
      worker.observe(Event.BookUpdate(coin, latest, true))
    def open(): Unit =
      worker.start()
      worker.observe(Event.SocketUp)
      publish()
      waitFor("paper did not become ready")(worker.view.ready)
    def review(intent: OrderIntent = Trading.action(0)): OrderReview =
      val plan = worker.preview(intent).toOption.get
      OrderReview(intent, plan, clock.get(), clock.get() + 30000)
    def settle(): Unit =
      waitFor("paper order remained pending")(!worker.view.pending && worker.view.outcome.nonEmpty)
    def alterRules(rules: MarketRules): Unit = worker.synchronized {
      // Simulate only the periodic metadata result. This avoids a minute-long
      // sleep and adds no production hook. All submission uses the public API.
      val field = classOf[PaperWorker].getDeclaredFields.find(_.getName == "rules").get
      field.setAccessible(true)
      field.set(worker, Some(rules))
    }
    def close(): Unit = worker.close()

  test("only the exact prepared intent and plan can be submitted") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      val plan = review.plan
      val changedPlans = Vector(
        plan.copy(coin = "BTC"),
        plan.copy(asset = plan.asset + 1),
        plan.copy(side = Side.Sell),
        plan.copy(kind = OrderKind.Best),
        plan.copy(quantity = plan.quantity + 1),
        plan.copy(limit = plan.limit + 1),
        plan.copy(reference = plan.reference + 1),
        plan.copy(estimatedQuantity = plan.estimatedQuantity - 1),
        plan.copy(estimatedValue = plan.estimatedValue - 1),
        plan.copy(average = plan.average.map(_ + 1)),
        plan.copy(positionAfter = plan.positionAfter + 1)
      )
      changedPlans.foreach(plan =>
        assert(!f.worker.submitReviewed(review.copy(plan = plan)), plan.toString)
      )
      val changedIntents = Vector(
        review.intent.copy(side = Side.Sell),
        review.intent.copy(kind = OrderKind.Best),
        review.intent.copy(big = true),
        review.intent.copy(target = Some(plan.limit))
      )
      changedIntents.foreach(intent =>
        assert(!f.worker.submitReviewed(review.copy(intent = intent)), intent.toString)
      )
      assert(!f.worker.submitReviewed(review.copy(leverage = Some(1))))
      assert(!f.worker.submitReviewed(review.copy(marginMode = Some("cross"))))
      assertEquals(f.ledger.account.count, 0L)
      assert(f.worker.submitReviewed(review))
      f.settle()
      assertEquals(f.ledger.account.count, 1L)
    finally f.close()
  }

  test("a newer preview supersedes an older visible plan") {
    val f = new Fixture
    try
      f.open()
      val first = f.review()
      f.clock.incrementAndGet()
      val second = f.review(Trading.action(1))
      assert(!f.worker.submitReviewed(first))
      assert(f.worker.submitReviewed(second))
      f.settle()
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.ledger.account.positions("XMR").quantity, second.plan.positionAfter)
    finally f.close()
  }

  test("a newer identical preview still supersedes the earlier review timestamp") {
    val f = new Fixture
    try
      f.open()
      val first = f.review()
      f.clock.incrementAndGet()
      val second = f.review()
      assertEquals(first.plan, second.plan)
      assert(!f.worker.submitReviewed(first))
      assert(f.worker.submitReviewed(second))
      f.settle()
      assertEquals(f.ledger.account.count, 1L)
    finally f.close()
  }

  test("an accepted review is consumed once and a pending preview cannot replace it") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      f.worker.synchronized {
        assert(f.worker.submitReviewed(review))
        assert(f.worker.view.pending)
        assert(f.worker.preview(Trading.action(3)).isLeft)
        assert(!f.worker.submitReviewed(review))
      }
      f.settle()
      assertEquals(f.ledger.account.count, 1L)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.ledger.account.count, 1L)
    finally f.close()
  }

  test("a normal review timestamp can follow preview computation without immediate rejection") {
    val f = new Fixture
    try
      f.open()
      val plan = f.worker.preview(Trading.action(0)).toOption.get
      f.clock.addAndGet(5)
      val review = OrderReview(Trading.action(0), plan, f.clock.get(), f.clock.get() + 30000)
      assert(f.worker.submitReviewed(review))
      f.settle()
      assertEquals(f.ledger.account.count, 1L)
    finally f.close()
  }

  test("expired, future, unbounded and timestamp-renewed reviews cannot submit") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      assert(!f.worker.submitReviewed(review.copy(createdAt = f.clock.get() + 1)))
      assert(!f.worker.submitReviewed(review.copy(createdAt = f.clock.get() - 1)))
      assert(!f.worker.submitReviewed(review.copy(expiresAt = f.clock.get())))
      assert(!f.worker.submitReviewed(review.copy(expiresAt = review.createdAt + 30001)))
      assert(
        !f.worker.submitReviewed(
          review.copy(createdAt = Long.MinValue + 1, expiresAt = Long.MaxValue)
        )
      )
      f.clock.addAndGet(30000)
      f.publish()
      assert(f.worker.view.ready)
      assert(!f.worker.submitReviewed(review))
      assert(
        !f.worker.submitReviewed(
          review.copy(createdAt = f.clock.get(), expiresAt = f.clock.get() + 30000)
        )
      )
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  test("a changed ledger count invalidates a preview even when the market remains fresh") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      val external = review.plan.copy(
        quantity = BigDecimal(1),
        estimatedQuantity = BigDecimal(1),
        estimatedValue = BigDecimal(100)
      )
      assert(f.ledger.fill(external, f.clock.get()).isRight)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.ledger.account.positions("XMR").quantity, BigDecimal(1))
    finally f.close()
  }

  test("an account change after queue acceptance cancels before another fill") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      f.worker.synchronized {
        assert(f.worker.submitReviewed(review))
        val external = review.plan.copy(
          quantity = BigDecimal(1),
          estimatedQuantity = BigDecimal(1),
          estimatedValue = BigDecimal(100)
        )
        assert(f.ledger.fill(external, f.clock.get()).isRight)
      }
      f.settle()
      assert(f.worker.view.outcome.exists(_.contains("canceled")))
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.ledger.account.positions("XMR").quantity, BigDecimal(1))
    finally f.close()
  }

  test("changed market rules invalidate the prepared order") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      f.alterRules(MarketRules(0, 3, 5))
      assert(f.worker.view.ready)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  test("changing away from and back to the same market does not revive the earlier review") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      f.worker.select("BTC")
      f.publish(coin = "BTC")
      assert(!f.worker.submitReviewed(review))
      f.worker.select("XMR")
      f.publish()
      assert(f.worker.view.ready)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  for side <- Vector(Side.Buy, Side.Sell) do
    for scenario <- Vector("improved", "adverse within limit", "partial", "outside limit") do
      test(s"$side uses fresh $scenario depth at the captured quantity and limit") {
        val f = new Fixture(if side == Side.Sell then BigDecimal(10) else BigDecimal(0))
        try
          f.open()
          val review = f.review(OrderIntent(side, OrderKind.Hard))
          val buy = side == Side.Buy
          val fillPrice = BigDecimal((buy, scenario) match
            case (true, "improved")       => "99.9"
            case (false, "improved")      => "100.05"
            case (true, "outside limit")  => "100.2"
            case (false, "outside limit") => "99.8"
            case (true, _)                => "100.05"
            case (false, _)               => "99.95")
          val quantity = if scenario == "partial" then "2" else "50"
          val depth = Vector(Level(fillPrice.toString, quantity, 1)) ++
            Option
              .when(scenario == "partial")(
                Level(if buy then "100.2" else "99.8", "50", 1)
              )
              .toVector
          val book = if buy then
            Book(
              Vector(Level((fillPrice - BigDecimal("0.01")).toString, "50", 1)),
              depth,
              f.clock.get()
            )
          else
            Book(
              depth,
              Vector(Level((fillPrice + BigDecimal("0.01")).toString, "50", 1)),
              f.clock.get()
            )
          f.publish(book)
          assert(f.worker.view.ready)
          assert(f.worker.submitReviewed(review))
          f.settle()
          if scenario == "outside limit" then
            assertEquals(f.ledger.account.count, 0L)
            assertEquals(f.ledger.account.cash, f.config.paperBalance)
            assertEquals(f.worker.view.position, Some(f.held))
            assert(
              f.worker.view.outcome.exists(text =>
                text.contains("no valid fill") || text.contains("no fill")
              )
            )
          else
            val filled = if scenario == "partial" then BigDecimal(2) else review.plan.quantity
            assertEquals(f.ledger.account.count, 1L)
            assertEquals(
              f.ledger.account.positions("XMR").quantity,
              if buy then filled else f.held - filled
            )
            assertEquals(
              f.ledger.account.positions("XMR").entry,
              if buy then fillPrice else BigDecimal(100)
            )
            assertEquals(f.ledger.account.fees, filled * fillPrice * f.config.paperFee)
            assertEquals(f.worker.view.position, Some(if buy then filled else f.held - filled))
            assert(f.worker.view.outcome.exists(_.contains(if scenario == "partial" then "partial"
            else "filled")))
          assert(!f.worker.submitReviewed(review), "consumed review was replayed")
        finally f.close()
      }

  test("expired depth, disconnect and malformed depth cannot fill a captured review") {
    val f = new Fixture
    try
      f.open()
      val review = f.review()
      f.clock.addAndGet(1001)
      assert(!f.worker.submitReviewed(review))
      f.publish()
      f.worker.observe(Event.SocketDown)
      assert(!f.worker.submitReviewed(review))
      f.worker.observe(Event.SocketUp)
      f.publish()
      val current = f.review()
      f.publish(
        Book(
          Vector(Level("99.99", "50", 1)),
          Vector(Level("100", "not-a-number", 1)),
          f.clock.get()
        )
      )
      if f.worker.submitReviewed(current) then f.settle()
      assertEquals(f.ledger.account.count, 0L)
      assertEquals(f.worker.view.position, Some(BigDecimal(0)))
    finally f.close()
  }
