package desk

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

class PropStatusSpec extends TempDirectorySuite:
  test("prop status follows fresh data, expiry, disconnect and market selection") {
    val clock = new AtomicLong(1800000000000L)
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PropLedger.open(
      tempDirectory("desk-prop-status"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val worker =
      new PropWorker(TradingConfig(), "XMR", ledger, rest, () => clock.get(), () => clock.get())
    def publish(): Unit = worker.observe(
      Event.BookUpdate(
        "XMR",
        Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), clock.get()),
        true
      )
    )
    def blocked(code: BlockCode): Unit =
      val view = worker.view
      assert(!view.ready)
      assertEquals(view.block, Some(view.message))
      assertEquals(view.blockCode, code)
    try
      blocked(BlockCode.Connection)
      worker.start()
      worker.observe(Event.SocketUp)
      publish()
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !worker.view.ready && System.nanoTime() < deadline do Thread.sleep(5)
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Prop ready")
      clock.addAndGet(1001)
      blocked(BlockCode.Prices)
      publish()
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Prop ready")
      worker.observe(Event.SocketDown)
      blocked(BlockCode.Connection)
      worker.observe(Event.SocketUp)
      blocked(BlockCode.Prices)
      clock.incrementAndGet()
      publish()
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Prop ready")
      worker.select("BTC")
      blocked(BlockCode.Market)
      assertEquals(ledger.account.count, 0L)
    finally worker.close()
  }

  test("prop pending status and retained result follow current readiness") {
    val clock = new AtomicLong(1800000000000L)
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PropLedger.open(
      tempDirectory("desk-prop-status-result"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val worker =
      new PropWorker(TradingConfig(), "XMR", ledger, rest, () => clock.get(), () => clock.get())
    def publish(): Unit = worker.observe(
      Event.BookUpdate(
        "XMR",
        Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), clock.get()),
        true
      )
    )
    try
      worker.start()
      worker.observe(Event.SocketUp)
      publish()
      Wait.until("PropStatusSpec condition")(worker.view.ready)
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val review = OrderReview(
        intent,
        worker.preview(intent).toOption.get,
        clock.get(),
        clock.get() + 30000
      )
      worker.synchronized {
        assert(worker.submitReviewed(review))
        val pending = worker.view
        assert(pending.pending)
        assert(!pending.ready)
        assertEquals(pending.message, "PROP queued 0s | XMR")
        assertEquals(pending.outcome, None)
      }
      Wait.until("PropStatusSpec condition")(!worker.view.pending)
      assertEquals(ledger.account.count, 1L)
      val result = Some(ledger.account.orders.last)
      def completed(ready: Boolean): Unit =
        val view = worker.view
        assertEquals(view.ready, ready)
        assert(!view.pending)
        if ready then assertEquals(view.message, "Prop ready")
        else assertEquals(view.block, Some(view.message))
        assertEquals(view.outcome, result)
      completed(true)
      clock.addAndGet(1001)
      completed(false)
      publish()
      completed(true)
      worker.observe(Event.SocketDown)
      completed(false)
      worker.observe(Event.SocketUp)
      completed(false)
      clock.incrementAndGet()
      publish()
      completed(true)
      worker.select("BTC")
      completed(false)
    finally worker.close()
  }
