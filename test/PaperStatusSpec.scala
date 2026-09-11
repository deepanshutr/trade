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
    def blocked(): Unit =
      val view = worker.view
      assert(!view.ready)
      assertEquals(view.message, "Paper needs fresh depth and available storage")
    try
      blocked()
      worker.start()
      worker.observe(Event.SocketUp)
      publish()
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !worker.view.ready && System.nanoTime() < deadline do Thread.sleep(5)
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Paper ready")
      clock.addAndGet(1001)
      blocked()
      publish()
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Paper ready")
      worker.observe(Event.SocketDown)
      blocked()
      worker.observe(Event.SocketUp)
      blocked()
      clock.incrementAndGet()
      publish()
      assert(worker.view.ready)
      assertEquals(worker.view.message, "Paper ready")
      worker.select("BTC")
      blocked()
      assertEquals(ledger.account.count, 0L)
    finally worker.close()
  }
