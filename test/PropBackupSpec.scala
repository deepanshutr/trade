package desk

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

class PropBackupSpec extends TempDirectorySuite:
  private val terms = TradeTerms(TradeSizes(), Trading.defaultSlippage)

  private final class Fixture:
    val now = new AtomicLong(1800000000000L)
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PropLedger.open(
      tempDirectory("backup-"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val worker =
      new PropWorker(TradingConfig(), "XMR", ledger, rest, () => now.get(), () => now.get())
    worker.start()
    Wait.until("metadata was not loaded")(worker.view.rules.nonEmpty)
    def book(time: Long = now.get(), market: String = "XMR", socket: Boolean = false): Unit =
      worker.observe(
        Event.BookUpdate(
          market,
          Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), time),
          socket
        )
      )

  test("fresh backup depth permits reviewed simulated fills while the stream reconnects") {
    val f = new Fixture
    try
      f.worker.observe(Event.SocketDown)
      assert(!f.worker.view.ready)
      f.book()
      assert(f.worker.view.ready, "fresh backup depth was blocked by the stream connection flag")
      assert(f.worker.view.message.toLowerCase.contains("backup"))
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val review =
        OrderReview(
          intent,
          f.worker.preview(intent, terms).toOption.get,
          terms,
          f.now.get(),
          f.now.get() + 30000
        )
      assert(f.worker.submitReviewed(review))
      Wait.until("the simulated fill did not complete")(!f.worker.view.pending)
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.worker.view.position, Some(BigDecimal(25)))
      f.now.addAndGet(1001)
      assert(!f.worker.view.ready)
      f.book()
      assert(f.worker.view.ready)
    finally f.worker.close()
  }

  test("backup readiness rejects delayed, future and wrong-market depth") {
    val f = new Fixture
    try
      f.book(f.now.get() - 1001)
      assert(!f.worker.view.ready)
      f.book(f.now.get() + 1001)
      assert(!f.worker.view.ready)
      f.book(market = "BTC")
      assert(!f.worker.view.ready)
      f.book(socket = true)
      assert(!f.worker.view.ready, "an obsolete stream update cannot restore connection readiness")
      f.now.incrementAndGet()
      f.book()
      assert(f.worker.view.ready)
      assertEquals(f.ledger.account.count, 0L)
    finally f.worker.close()
  }

  test("backup depth expires by its market timestamp even before receipt age expires") {
    val f = new Fixture
    try
      f.book(f.now.get() - 750)
      assert(f.worker.view.ready)
      f.now.addAndGet(251)
      assert(!f.worker.view.ready)
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), terms).isLeft)
      assertEquals(f.ledger.account.count, 0L)
    finally f.worker.close()
  }

  test("readiness names backup prices until fresh stream depth replaces them") {
    val f = new Fixture
    try
      f.book()
      assertEquals(f.worker.view.message, "Prop ready: backup prices")
      f.worker.observe(Event.SocketUp)
      assert(f.worker.view.ready)
      assertEquals(f.worker.view.message, "Prop ready: backup prices")
      f.now.incrementAndGet()
      f.book(socket = true)
      assertEquals(f.worker.view.message, "Prop ready")
      f.book(f.now.get() - 1)
      assertEquals(f.worker.view.message, "Prop ready")
      f.now.incrementAndGet()
      f.book()
      assertEquals(f.worker.view.message, "Prop ready: backup prices")
      f.worker.observe(Event.SocketDown)
      assert(f.worker.view.ready)
      assertEquals(f.worker.view.message, "Prop ready: backup prices")
      assertEquals(f.ledger.account.count, 0L)
    finally f.worker.close()
  }

  test("failed background retries preserve a reviewed order backed by current fresh depth") {
    val f = new Fixture
    try
      f.book()
      assert(f.worker.view.ready)
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val review =
        OrderReview(
          intent,
          f.worker.preview(intent, terms).toOption.get,
          terms,
          f.now.get(),
          f.now.get() + 30000
        )
      f.worker.observe(Event.SocketDown)
      f.worker.observe(Event.SocketDown)
      assert(f.worker.view.ready)
      assert(
        f.worker.submitReviewed(review),
        "a reconnect attempt invalidated the fresh backup review"
      )
      Wait.until("the reviewed fill did not complete")(!f.worker.view.pending)
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.worker.view.position, Some(BigDecimal(25)))
      assert(!f.worker.submitReviewed(review))
    finally f.worker.close()
  }

  test("preview reads a backup block once across a backward wall-clock step") {
    val now = 1800000000000L
    val config = TradingConfig()
    var previewing = false
    var reads = 0
    def wall(): Long =
      if !previewing then now
      else
        reads += 1
        if reads == 1 then now - 1 else now
    val ledger = PropLedger.open(
      tempDirectory("backup-clock-step"),
      "mainnet",
      config.propBalance,
      config.propFee
    )
    val rest = new Rest:
      def post(body: String): Option[String] = fail("unexpected metadata request")
    val metadata = ujson.read(
      """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""
    )
    val worker = new PropWorker(
      config,
      "XMR",
      ledger,
      rest,
      () => wall(),
      () => 0L,
      Some((metadata, 0L))
    )
    try
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("99.99", "50", 1)),
            Vector(Level("100", "50", 1)),
            now + config.quoteAgeMs
          ),
          false
        )
      )
      assert(worker.view.ready)
      previewing = true
      assertEquals(
        worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), terms),
        Left("Prop prices stale. Waiting for depth")
      )
      assertEquals(reads, 1)
      assertEquals(ledger.account.count, 0L)
    finally worker.close()
  }
