package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class PaperUxIndependentSpec extends munit.FunSuite:
  private val epoch = 1800000000000L

  private def openLedger(
      initial: PaperAccount,
      fee: BigDecimal,
      save: PaperAccount => Unit
  ): PaperLedger =
    val constructor =
      classOf[PaperLedger].getDeclaredConstructors.find(_.getParameterCount == 4).get
    constructor.setAccessible(true)
    constructor
      .newInstance(initial, fee, save, new AutoCloseable { def close(): Unit = () })
      .asInstanceOf[PaperLedger]

  private class Fixture(
      initial: PaperAccount,
      fee: BigDecimal = BigDecimal("0.001"),
      size: BigDecimal = BigDecimal(500),
      save: PaperAccount => Unit = _ => ()
  ):
    val clock = new AtomicLong(epoch)
    val writes = new AtomicInteger(0)
    val ledger = openLedger(initial, fee, next => { save(next); writes.incrementAndGet(); () })
    val config = TradingConfig().copy(sizes = TradeSizes(size, size, size))
    val worker = new PaperWorker(
      config,
      "XMR",
      ledger,
      new Rest:
        def post(body: String): Option[String] =
          require(ujson.read(body) == ujson.Obj("type" -> "meta"))
          Some(
            """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5},{"name":"BTC","szDecimals":2,"maxLeverage":5}]}"""
          )
      ,
      () => clock.get(),
      () => clock.get()
    )
    def depth(
        bid: String,
        ask: String,
        bidSize: String = "50",
        askSize: String = "50",
        coin: String = "XMR"
    ): Unit =
      publish(
        Book(
          Vector(Level(bid, bidSize, 1)),
          Vector(Level(ask, askSize, 1)),
          clock.incrementAndGet()
        ),
        coin
      )
    def publish(book: Book, coin: String = "XMR"): Unit =
      worker.observe(Event.BookUpdate(coin, book, true))
    def open(bid: String = "99.99", ask: String = "100"): Unit =
      worker.start()
      worker.observe(Event.SocketUp)
      depth(bid, ask)
      Wait.until("synthetic metadata did not arrive")(worker.view.ready)
    def review(side: Side): OrderReview =
      val intent = OrderIntent(side, OrderKind.Hard)
      val candidate = worker.preview(intent)
      assert(candidate.isRight, candidate.toString)
      OrderReview(intent, candidate.toOption.get, clock.get(), clock.get() + 30000L)
    def send(review: OrderReview): Unit =
      assert(worker.submitReviewed(review))
      Wait.until("synthetic order did not finish")(
        !worker.view.pending && worker.view.outcome.nonEmpty
      )
    def orderThread: Thread =
      val field = classOf[PaperWorker].getDeclaredFields.find(_.getName == "threads").get
      field.setAccessible(true)
      field.get(worker).asInstanceOf[Vector[Thread]].find(_.getName == "paper-orders").get
    def close(): Unit = worker.close()

  for side <- Vector(Side.Buy, Side.Sell) do
    test(side.toString + " cannot review an order without the required wallet balance") {
      val f = new Fixture(PaperAccount(BigDecimal(500), deposit = BigDecimal(500)))
      try
        if side == Side.Buy then f.open() else f.open("100", "100.01")
        val result = f.worker.preview(OrderIntent(side, OrderKind.Hard))
        assert(result.isLeft, result.toString)
        assert(result.swap.toOption.get.contains(if side == Side.Buy then "limit" else "No XMR"))
        assertEquals(f.ledger.account, PaperAccount(BigDecimal(500), deposit = BigDecimal(500)))
        assertEquals(f.writes.get(), 0)
      finally f.close()
    }

    test(side.toString + " affordable partial uses actual fees and preserves requested quantity") {
      val initial = PaperAccount(
        BigDecimal(100),
        deposit = if side == Side.Buy then BigDecimal(100) else BigDecimal(600),
        positions =
          if side == Side.Buy then Map.empty
          else Map("XMR" -> PaperPosition(BigDecimal(5), BigDecimal(100)))
      )
      val f = new Fixture(initial)
      try
        if side == Side.Buy then
          f.open()
          f.depth("99.99", "100", askSize = "0.99")
        else
          f.open("100", "100.01")
          f.depth("100", "100.01", bidSize = "0.99")
        val review = f.review(side)
        assertEquals(review.plan.quantity, BigDecimal(5))
        assertEquals(review.plan.estimatedQuantity, BigDecimal("0.99"))
        assertEquals(f.writes.get(), 0)
        f.send(review)
        val account = f.ledger.account
        assertEquals(account.cash, BigDecimal(if side == Side.Buy then "0.901" else "198.901"))
        assertEquals(account.fees, BigDecimal("0.099"))
        assertEquals(account.realized, BigDecimal(0))
        assertEquals(
          account.positions("XMR"),
          PaperPosition(
            if side == Side.Buy then BigDecimal("0.99") else BigDecimal("4.01"),
            BigDecimal(100)
          )
        )
        assertEquals(account.count, 1L)
        assert(f.worker.view.outcome.exists(_.contains("partial")))
      finally f.close()
    }

    test(side.toString + " fresh best quote excludes stale capacity and stale preview estimates") {
      val initial = PaperAccount(
        BigDecimal(200),
        deposit = if side == Side.Buy then BigDecimal(200) else BigDecimal(700),
        positions =
          if side == Side.Buy then Map.empty
          else Map("XMR" -> PaperPosition(BigDecimal(5), BigDecimal(100)))
      )
      val f = new Fixture(initial)
      try
        f.open()
        val book = if side == Side.Buy then
          Book(
            Vector(Level("99", "50", 1)),
            Vector(Level("100", "4", 1), Level("100.05", "1", 1)),
            f.clock.incrementAndGet()
          )
        else
          Book(
            Vector(Level("100", "4", 1), Level("99.95", "1", 1)),
            Vector(Level("101", "50", 1)),
            f.clock.incrementAndGet()
          )
        f.publish(book)
        val (bid, ask) = if side == Side.Buy then ("100", "100.05") else ("99.95", "100")
        f.worker.observe(
          Event.BboUpdate("XMR", Level(bid, "1", 1), Level(ask, "1", 1), f.clock.incrementAndGet())
        )
        val review = f.review(side)
        val expectedPrice = BigDecimal(if side == Side.Buy then "100.05" else "99.95")
        assertEquals(review.plan.estimatedQuantity, BigDecimal(1))
        assertEquals(review.plan.estimatedValue, expectedPrice)
        assertEquals(review.plan.average, Some(expectedPrice))
        f.send(review)
        assertEquals(
          f.ledger.account.positions("XMR"),
          PaperPosition(
            if side == Side.Buy then BigDecimal(1) else BigDecimal(4),
            if side == Side.Buy then expectedPrice else BigDecimal(100)
          )
        )
        assertEquals(
          f.ledger.account.cash,
          BigDecimal(if side == Side.Buy then "99.84995" else "299.85005")
        )
        assert(f.worker.view.outcome.exists(_.contains("partial")))
      finally f.close()
    }

  test("a wallet with no stables can sell held tokens after a loss, but cannot spend absent cash") {
    val initial = PaperAccount(
      BigDecimal(0),
      deposit = BigDecimal(1000),
      positions = Map(
        "XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100))
      )
    )
    val f = new Fixture(initial, size = BigDecimal(80))
    try
      f.open("80", "80.01")
      assert(f.worker.view.ready, "absent cash must not block selling held tokens")
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard)).isLeft)
      assertEquals(f.writes.get(), 0)
      val review = f.review(Side.Sell)
      assertEquals(review.plan.quantity, BigDecimal(1))
      f.send(review)
      val account = f.ledger.account
      assertEquals(
        account.positions("XMR"),
        PaperPosition(BigDecimal(9), BigDecimal(100))
      )
      assertEquals(account.cash, BigDecimal("79.92"))
      assertEquals(account.realized, BigDecimal(-20))
      assertEquals(account.fees, BigDecimal("0.08"))
      val increase =
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard))
      assert(increase.isRight, "sale proceeds can fund a later buy: " + increase.toString)
      assertEquals(f.ledger.account, account)
      assertEquals(f.writes.get(), 1)
    finally f.close()
  }

  test("an oversized selected sell is capped at the held tokens") {
    val initial = PaperAccount(
      BigDecimal(100),
      deposit = BigDecimal(1100),
      positions = Map(
        "XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100))
      )
    )
    val f = new Fixture(initial, size = BigDecimal(1900))
    try
      f.open("100", "100.01")
      val review = f.review(Side.Sell)
      assertEquals(review.plan.quantity, BigDecimal(10))
      f.send(review)
      assertEquals(f.ledger.account.positions, Map.empty[String, PaperPosition])
      assertEquals(f.ledger.account.cash, BigDecimal(1099))
      assertEquals(f.writes.get(), 1)
    finally f.close()
  }

  test("closing a losing wallet holding returns its sale proceeds and removes its entry") {
    val initial = PaperAccount(
      BigDecimal(100),
      deposit = BigDecimal(1100),
      positions = Map(
        "XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100))
      )
    )
    val price = BigDecimal(80)
    val ledger = openLedger(initial, BigDecimal("0.001"), _ => ())
    val plan = OrderPlan(
      "XMR",
      0,
      Side.Sell,
      OrderKind.Hard,
      BigDecimal(10),
      price,
      price,
      BigDecimal(10),
      price * 10,
      Some(price),
      BigDecimal(0)
    )
    try
      assert(ledger.check(plan, initial).isRight)
      assertEquals(ledger.account, initial)
      assert(ledger.fill(plan, epoch, Some(0L)).isRight)
      assertEquals(ledger.account.positions, Map.empty[String, PaperPosition])
      assertEquals(ledger.account.cash, BigDecimal("899.2"))
      assertEquals(ledger.account.realized, BigDecimal(-200))
    finally ledger.close()
  }

  test("another held market is named before review and returning to it preserves the account") {
    val initial = PaperAccount(
      BigDecimal(1000),
      deposit = BigDecimal(1200),
      positions = Map("XMR" -> PaperPosition(BigDecimal(2), BigDecimal(100)))
    )
    val f = new Fixture(initial)
    try
      f.open()
      f.worker.select("BTC")
      f.depth("99.99", "100", coin = "BTC")
      val result = f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard))
      assert(result.isLeft)
      assert(result.swap.toOption.get.contains("XMR"))
      assertEquals(f.ledger.account, initial)
      f.worker.select("XMR")
      f.depth("99.99", "100")
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard)).isRight)
      assertEquals(f.writes.get(), 0)
    finally f.close()
  }

  test("an intervening writer invalidates the captured account revision after fill calculation") {
    val f =
      new Fixture(PaperAccount(BigDecimal(600), deposit = BigDecimal(600)), fee = BigDecimal(0))
    try
      f.open()
      val review = f.review(Side.Buy)
      val one = review.plan.copy(
        quantity = BigDecimal(1),
        estimatedQuantity = BigDecimal(1),
        estimatedValue = BigDecimal(100),
        average = Some(BigDecimal(100)),
        positionAfter = BigDecimal(1)
      )
      f.ledger.synchronized {
        assert(f.worker.submitReviewed(review))
        Wait.until("order did not reach the ledger writer lock")(
          f.orderThread.getState == Thread.State.BLOCKED &&
            f.orderThread.getStackTrace.exists(e =>
              e.getClassName == "desk.PaperLedger" && e.getMethodName == "fill"
            )
        )
        assert(f.ledger.fill(one, epoch).isRight)
      }
      Wait.until("revision outcome missing")(!f.worker.view.pending)
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.ledger.account.positions("XMR").quantity, BigDecimal(1))
      assert(f.worker.view.outcome.exists(_.contains("account changed")))
    finally f.close()
  }

  test("identical completed rejections remain distinct across worker lifetimes") {
    val updates = new OrderUpdates
    var messages = Vector.empty[String]
    var identities = Vector.empty[Long]
    for _ <- 1 to 3 do
      val f =
        new Fixture(PaperAccount(BigDecimal(200), deposit = BigDecimal(200)), fee = BigDecimal(0))
      try
        f.open()
        f.depth("99.99", "100", askSize = "1")
        val review = f.review(Side.Buy)
        f.depth("99.99", "100")
        f.send(review)
        val view = f.worker.view
        identities :+= view.outcomeId
        updates.collect(view)(text => messages :+= text)
        updates.collect(view)(text => messages :+= text)
        assertEquals(f.ledger.account.count, 0L)
      finally f.close()
    assertEquals(messages.size, 3)
    assertEquals(messages.distinct.size, 1)
    assertEquals(identities.distinct.size, 3)
    assert(identities.forall(_ > 0L))
  }

  for fail <- Vector(false, true) do
    test("held paper save publishes only after success, failure=" + fail) {
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val read = new CountDownLatch(1)
      val observed = new AtomicReference[TradingView]()
      val f = new Fixture(
        PaperAccount(BigDecimal(1000), deposit = BigDecimal(1000)),
        save = _ => {
          entered.countDown()
          if !release.await(5, TimeUnit.SECONDS) then
            throw new IllegalStateException("synthetic save held too long")
          if fail then throw new java.io.IOException("synthetic save failure")
        }
      )
      val reader =
        new Thread(() => { observed.set(f.worker.view); read.countDown() }, "paper-ux-read")
      try
        f.open()
        assert(f.worker.submitReviewed(f.review(Side.Buy)))
        assert(entered.await(2, TimeUnit.SECONDS))
        reader.start()
        assert(read.await(300, TimeUnit.MILLISECONDS), "view blocked on paper storage")
        assert(observed.get().pending)
        assertEquals(observed.get().position, Some(BigDecimal(0)))
        assertEquals(observed.get().outcome, None)
        assert(observed.get().message.contains("saving"))
        assertEquals(f.ledger.account.count, 0L)
        release.countDown()
        Wait.until("save result missing")(!f.worker.view.pending)
        if fail then
          assertEquals(f.ledger.account, PaperAccount(BigDecimal(1000), deposit = BigDecimal(1000)))
          assert(f.worker.view.outcome.exists(_.contains("storage failed")))
        else
          assertEquals(f.ledger.account.cash, BigDecimal("499.5"))
          assertEquals(f.worker.view.position, Some(BigDecimal(5)))
          assertEquals(f.ledger.account.count, 1L)
      finally
        release.countDown()
        reader.join(1000L)
        f.close()
    }
