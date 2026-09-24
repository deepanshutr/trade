//> using target.scope test
package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.time.ZoneId

class PaperExecutionDepthSpec extends munit.FunSuite:
  private val epoch = 1800000000000L
  private def await(label: String)(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < until do Thread.sleep(2)
    assert(condition, label)

  private final class Fixture(
      initial: PaperAccount = PaperAccount(BigDecimal(10000), deposit = BigDecimal(10000)),
      persist: PaperAccount => Unit = _ => ()
  ):
    val clock = new AtomicLong(epoch)
    val writes = new AtomicInteger()
    val closedLeases = new AtomicInteger()
    val constructor =
      classOf[PaperLedger].getDeclaredConstructors.find(_.getParameterCount == 4).get
    constructor.setAccessible(true)
    val ledger = constructor
      .newInstance(
        initial,
        BigDecimal("0.001"),
        ((next: PaperAccount) => { persist(next); val _ = writes.incrementAndGet(); () }),
        new AutoCloseable { def close(): Unit = { val _ = closedLeases.incrementAndGet(); () } }
      )
      .asInstanceOf[PaperLedger]
    val rest = new Rest:
      def post(body: String): Option[String] =
        require(ujson.read(body) == ujson.Obj("type" -> "meta"))
        Some(
          """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5},{"name":"BTC","szDecimals":2,"maxLeverage":5}]}"""
        )
    val worker =
      new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => clock.get(), () => clock.get())
    def book(at: Long = clock.get(), coin: String = "XMR"): Book =
      Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), at)
    def publish(at: Long = clock.get(), coin: String = "XMR"): Unit =
      publishBook(book(at, coin), coin)
    def publishBook(b: Book, coin: String = "XMR"): Unit =
      val payload = ujson.Obj(
        "channel" -> "l2Book",
        "data" -> ujson.Obj(
          "coin" -> coin,
          "time" -> ujson.Num(b.time.toDouble),
          "levels" -> ujson.Arr.from(
            Vector(b.bids, b.asks).map(side =>
              ujson.Arr.from(side.map(l => ujson.Obj("px" -> l.px, "sz" -> l.sz, "n" -> l.n)))
            )
          )
        )
      )
      val parsed = Events.parse(ujson.write(payload)).get
      assert(parsed.isInstanceOf[Event.BookUpdate], s"bad synthetic book: $payload -> $parsed")
      worker.observe(parsed)
    def quote(size: String, time: Long = clock.get()): Unit =
      val payload = ujson.Obj(
        "channel" -> "bbo",
        "data" -> ujson.Obj(
          "coin" -> "XMR",
          "time" -> ujson.Num(time.toDouble),
          "bbo" -> ujson.Arr(
            ujson.Obj("px" -> "99.99", "sz" -> size, "n" -> 1),
            ujson.Obj("px" -> "100", "sz" -> size, "n" -> 1)
          )
        )
      )
      val parsed = Events.parse(ujson.write(payload)).get
      assert(parsed.isInstanceOf[Event.BboUpdate])
      worker.observe(parsed)
    def start(): Unit =
      worker.start()
      worker.observe(Event.SocketUp)
      await("metadata never loaded")(worker.view.rules.nonEmpty)
    def open(): Unit =
      start()
      publish()
      assert(worker.view.ready)
    def review(side: Side = Side.Buy): OrderReview =
      val intent = OrderIntent(side, OrderKind.Hard)
      val plan = worker.preview(intent)
      assert(plan.isRight, plan.toString)
      OrderReview(intent, plan.toOption.get, clock.get(), clock.get() + 30000)
    def settled(): Unit =
      await("order did not settle")(!worker.view.pending && worker.view.outcome.nonEmpty)
    def close(): Unit = worker.close()

  for offset <- Vector(-60000L, 60000L) do
    test(s"stream clock offset $offset retains receipt-age expiry") {
      val f = new Fixture()
      try
        f.start()
        f.publish(f.clock.get() + offset)
        assert(f.worker.view.ready)
        f.clock.addAndGet(1001L)
        assert(!f.worker.view.ready)
        assert(f.worker.preview(Trading.action(0)).isLeft)
        f.publish(f.clock.get() + offset)
        assert(f.worker.view.ready)
        assertEquals(f.ledger.account.count, 0L)
      finally f.close()
    }

  for
    side <- Vector(Side.Buy, Side.Sell)
    queued <- Vector(false, true)
    moved <- Vector(false, true)
  do
    test(s"$side a newer best quote caps the committed top quantity queued=$queued moved=$moved") {
      val initial = if side == Side.Buy then
        PaperAccount(BigDecimal(10000), deposit = BigDecimal(10000))
      else
        PaperAccount(
          BigDecimal(9000),
          deposit = BigDecimal(10000),
          positions = Map("XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100)))
        )
      val f = new Fixture(initial)
      try
        f.open()
        val depth = if moved then
          f.book()
            .copy(
              bids =
                Vector(Level("99.99", "50", 1), Level("99.94", "50", 1), Level("99.8", "50", 1)),
              asks =
                Vector(Level("100", "50", 1), Level("100.05", "50", 1), Level("100.2", "50", 1))
            )
        else f.book()
        f.publishBook(depth)
        val bbo = ujson.Obj(
          "channel" -> "bbo",
          "data" -> ujson.Obj(
            "coin" -> "XMR",
            "time" -> ujson.Num(f.clock.incrementAndGet().toDouble),
            "bbo" -> ujson.Arr(
              ujson.Obj("px" -> (if moved then "99.94" else "99.99"), "sz" -> "0.01", "n" -> 1),
              ujson.Obj("px" -> (if moved then "100.05" else "100"), "sz" -> "0.01", "n" -> 1)
            )
          )
        )
        val update = Events.parse(ujson.write(bbo)).get
        assert(update.isInstanceOf[Event.BboUpdate])
        val shown = State.reduce(
          Snapshot.initial("XMR", epoch).copy(book = Some(depth)),
          update,
          f.clock.get()
        )
        assertEquals(shown.book.get.asks.head.sz, "0.01")
        assertEquals(shown.book.get.bids.head.sz, "0.01")
        f.worker.synchronized {
          if queued then
            assert(f.worker.submitReviewed(f.review(side)))
            f.worker.observe(update)
          else
            f.worker.observe(update)
            assert(f.worker.submitReviewed(f.review(side)))
        }
        f.settled()
        assertEquals(f.ledger.account.count, 1L)
        println(
          s"newest visible quantity=0.01 side=$side queued=$queued moved=$moved receipt=${f.worker.view.outcome}"
        )
        assertEquals(
          f.ledger.account.positions("XMR").quantity,
          if side == Side.Buy then BigDecimal("0.01") else BigDecimal("9.99"),
          "Committed more than the current visible best quote"
        )
      finally f.close()
    }

  for side <- Vector(Side.Buy, Side.Sell) do
    test(s"$side a delayed full book cannot revive capacity removed by a newer quote") {
      val f = new Fixture(
        PaperAccount(
          BigDecimal(9000),
          deposit = BigDecimal(10000),
          positions = Map("XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100)))
        )
      )
      try
        f.open()
        f.clock.addAndGet(2L)
        f.quote("0.01")
        assertEquals(f.review(side).plan.estimatedQuantity, BigDecimal("0.01"))
        f.publish(epoch + 1L)
        assert(f.worker.submitReviewed(f.review(side)))
        f.settled()
        println(s"delayed full book side=$side receipt=${f.worker.view.outcome}")
        assertEquals(
          f.ledger.account.positions("XMR").quantity,
          if side == Side.Buy then BigDecimal("10.01") else BigDecimal("9.99")
        )
      finally f.close()
    }

    test(s"$side an older best quote cannot shrink newer full depth") {
      val f = new Fixture(
        PaperAccount(
          BigDecimal(9000),
          deposit = BigDecimal(10000),
          positions = Map("XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100)))
        )
      )
      try
        f.open()
        f.clock.addAndGet(20L)
        f.publish()
        f.quote("0.01", f.clock.get() - 1L)
        assertEquals(f.review(side).plan.estimatedQuantity, BigDecimal(5))
        assertEquals(f.ledger.account.count, 0L)
      finally f.close()
    }

    test(s"$side a best quote increase cannot inflate known full depth") {
      val f = new Fixture(
        PaperAccount(
          BigDecimal(9000),
          deposit = BigDecimal(10000),
          positions = Map("XMR" -> PaperPosition(BigDecimal(10), BigDecimal(100)))
        )
      )
      try
        f.open()
        f.clock.incrementAndGet()
        f.publishBook(
          f.book()
            .copy(
              bids = Vector(Level("99.99", "0.02", 1)),
              asks = Vector(Level("100", "0.02", 1))
            )
        )
        f.clock.incrementAndGet()
        f.quote("50")
        assertEquals(f.review(side).plan.estimatedQuantity, BigDecimal("0.02"))
        assertEquals(f.ledger.account.count, 0L)
      finally f.close()
    }

  test("best quote updates cannot renew the expired full-depth receipt") {
    val f = new Fixture()
    try
      f.open()
      f.clock.addAndGet(1001L)
      f.quote("0.01", epoch + 500L)
      assert(!f.worker.view.ready)
      assert(f.worker.preview(Trading.action(0)).isLeft)
      assertEquals(f.ledger.account.count, 0L)
    finally f.close()
  }

  test("a later full book can restore capacity after a smaller best quote") {
    val f = new Fixture()
    try
      f.open()
      f.clock.incrementAndGet()
      f.quote("0.01")
      f.clock.incrementAndGet()
      f.publish()
      assertEquals(f.review().plan.estimatedQuantity, BigDecimal(5))
    finally f.close()
  }

  test("switching market while queued cancels the captured order") {
    val f = new Fixture()
    try
      f.open()
      f.worker.synchronized {
        assert(f.worker.submitReviewed(f.review()))
        f.worker.select("BTC")
        f.clock.incrementAndGet()
        f.publish(coin = "BTC")
      }
      f.settled()
      assertEquals(f.ledger.account.count, 0L)
      assertEquals(f.worker.view.coin, "BTC")
      assert(f.worker.view.outcome.exists(_.contains("canceled")))
    finally f.close()
  }

  for failSave <- Vector(false, true) do
    test(
      s"switching while saving preserves the prior committed view and old market attribution failure=$failSave"
    ) {
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val f = new Fixture(persist = _ => {
        entered.countDown()
        if !release.await(5, TimeUnit.SECONDS) then
          throw new IllegalStateException("save not released")
        if failSave then throw new java.io.IOException("synthetic failure")
      })
      try
        f.open()
        assert(f.worker.submitReviewed(f.review()))
        assert(entered.await(2, TimeUnit.SECONDS))
        f.worker.select("BTC")
        f.clock.incrementAndGet()
        f.publish(coin = "BTC")
        val during = f.worker.view
        assert(during.pending)
        assert(during.message.contains("XMR"))
        assertEquals(during.position, Some(BigDecimal(0)))
        assertEquals(during.portfolio.stables, Some(BigDecimal(10000)))
        release.countDown()
        f.settled()
        assertEquals(f.worker.view.coin, "BTC")
        assertEquals(f.worker.view.position, Some(BigDecimal(0)))
        if failSave then
          assertEquals(
            f.ledger.account,
            PaperAccount(BigDecimal(10000), deposit = BigDecimal(10000))
          )
          assert(!f.worker.view.ready)
          assert(f.worker.view.portfolio.stables.isEmpty)
        else
          assertEquals(f.ledger.account.positions.keySet, Set("XMR"))
          assertEquals(f.ledger.account.cash, BigDecimal("9499.5"))
          assert(f.worker.view.outcome.exists(_.contains("XMR buy")))
          assert(f.worker.preview(Trading.action(0)).isLeft)
      finally
        release.countDown()
        f.close()
    }

  test("rapid successive controller actions retain every completed receipt") {
    val f = new Fixture()
    val c = new DeskController(
      PanelView(mode = "PAPER"),
      () => f.clock.get(),
      f.worker.preview,
      f.worker.submitReviewed,
      _ => false,
      _ => Left("disabled")
    )
    def snapshot: Snapshot = Snapshot
      .initial("XMR", f.clock.get())
      .copy(book = Some(f.book()), feed = Health.Live(f.clock.get()), workspace = true)
    def draw(): Unit = c.publish(
      Workspace.draw(
        snapshot,
        f.worker.view,
        c.panel,
        120,
        40,
        f.clock.get(),
        ZoneId.of("UTC"),
        Style.mono
      )
    )
    try
      f.open()
      for i <- 0 until 12 do
        draw()
        val side = if i % 2 == 0 then Side.Buy else Side.Sell
        assert(
          c.handle(
            Command.PlaceOrder(OrderIntent(side, OrderKind.Hard)),
            snapshot,
            f.worker.view,
            (120, 40),
            false
          )
        )
        f.settled()
        c.orders(f.worker.view)
      assertEquals(f.ledger.account.count, 12L)
      assertEquals(c.panel.messages.count(_.kind == "order"), 12)
      assertEquals(f.ledger.account.positions.size, 0)
      assert(f.ledger.account.cash < BigDecimal(10000))
    finally
      c.close()
      f.close()
  }

  test(
    "closing during an interruptible save preserves the prior wallet and releases its lease once"
  ) {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(persist = _ => {
      entered.countDown()
      if !release.await(5, TimeUnit.SECONDS) then
        throw new IllegalStateException("unreleased synthetic save")
    })
    try
      f.open()
      assert(f.worker.submitReviewed(f.review()))
      assert(entered.await(2, TimeUnit.SECONDS))
      f.close()
      assertEquals(f.ledger.account, PaperAccount(BigDecimal(10000), deposit = BigDecimal(10000)))
      assertEquals(f.writes.get(), 0)
      assertEquals(f.closedLeases.get(), 1)
      assert(!f.ledger.available)
      assert(f.worker.view.outcome.exists(_.contains("storage failed")))
      assert(!f.worker.view.pending)
      assert(!f.worker.view.ready)
      f.close()
      assertEquals(f.closedLeases.get(), 1)
    finally
      release.countDown()
      f.close()
  }
