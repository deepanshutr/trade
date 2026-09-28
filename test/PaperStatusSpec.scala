package desk

import java.nio.file.Files
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

class PaperStatusSpec extends munit.FunSuite:
  test("paper status follows fresh data, expiry, disconnect and market selection") {
    val clock = new AtomicLong(1800000000000L)
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PaperLedger.open(
      Files.createTempDirectory("desk-paper-status"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val worker =
      new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => clock.get(), () => clock.get())
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
      assertEquals(worker.view.message, "Paper ready")
      clock.addAndGet(1001)
      blocked(BlockCode.Prices)
      publish()
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Paper ready")
      worker.observe(Event.SocketDown)
      blocked(BlockCode.Connection)
      worker.observe(Event.SocketUp)
      blocked(BlockCode.Prices)
      clock.incrementAndGet()
      publish()
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Paper ready")
      worker.select("BTC")
      blocked(BlockCode.Market)
      assertEquals(ledger.account.count, 0L)
    finally worker.close()
  }

  test("paper pending status and retained result follow current readiness") {
    val clock = new AtomicLong(1800000000000L)
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PaperLedger.open(
      Files.createTempDirectory("desk-paper-status-result"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val worker =
      new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => clock.get(), () => clock.get())
    def publish(): Unit = worker.observe(
      Event.BookUpdate(
        "XMR",
        Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), clock.get()),
        true
      )
    )
    def waitFor(condition: => Boolean): Unit =
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !condition && System.nanoTime() < deadline do Thread.sleep(5)
      assert(condition)
    try
      worker.start()
      worker.observe(Event.SocketUp)
      publish()
      waitFor(worker.view.ready)
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
        assertEquals(pending.message, "PAPER queued 0s | XMR")
        assertEquals(pending.outcome, None)
      }
      waitFor(!worker.view.pending)
      assertEquals(ledger.account.count, 1L)
      val result = Some(ledger.account.orders.last)
      def completed(ready: Boolean): Unit =
        val view = worker.view
        assertEquals(view.ready, ready)
        assert(!view.pending)
        if ready then assertEquals(view.message, "Paper ready")
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
