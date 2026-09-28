package desk

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

class PaperFreshTouchReviewSpec extends TempDirectorySuite:
  private val start = 1800000000000L

  private class Fixture(side: Side):
    val buy = side == Side.Buy
    val clock = new AtomicLong(start)
    val config = TradingConfig()
    val directory = tempDirectory("touch-")
    val held = if buy then BigDecimal(0) else BigDecimal(10)
    if held > 0 then
      LocalFiles.write(
        directory.resolve("wallet-mainnet.json"),
        s"""{"cash":"${config.paperBalance}","deposit":"${config.paperBalance + held * 100}","realized":"0","fees":"0","orders":[],"count":"0","positions":{"XMR":{"quantity":"$held","entry":"100"}}}"""
      )
    val ledger = PaperLedger.open(
      directory,
      "mainnet",
      config.paperBalance,
      config.paperFee
    )
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val worker = new PaperWorker(config, "XMR", ledger, rest, () => clock.get(), () => clock.get())
    val offers = if buy then
      Vector(
        Level("100", "1", 1),
        Level("100.05", "1", 1),
        Level("100.1", "2", 1),
        Level("100.2", "50", 1)
      )
    else
      Vector(
        Level("100", "1", 1),
        Level("99.95", "1", 1),
        Level("99.9", "2", 1),
        Level("99.8", "50", 1)
      )
    val book = if buy then Book(Vector(Level("99.99", "50", 1)), offers, start)
    else Book(offers, Vector(Level("100.01", "50", 1)), start)
    def open(): Unit =
      worker.start()
      worker.observe(Event.SocketUp)
      worker.observe(Event.BookUpdate("XMR", book, true))
      Wait.until("paper did not become ready")(worker.view.ready)
    def review(): OrderReview =
      val intent = OrderIntent(side, OrderKind.Hard)
      val plan = worker.preview(intent).toOption.get
      assertEquals(plan.quantity, BigDecimal(5))
      assertEquals(plan.estimatedQuantity, BigDecimal(4))
      OrderReview(intent, plan, clock.get(), clock.get() + 30000)
    def touch(price: String): Unit =
      val value = BigDecimal(price)
      val bid = if buy then value - BigDecimal("0.01") else value
      val ask = if buy then value else value + BigDecimal("0.01")
      worker.observe(
        Event.BboUpdate(
          "XMR",
          Level(bid.toString, "1", 1),
          Level(ask.toString, "1", 1),
          clock.incrementAndGet()
        )
      )
    def settle(): Unit =
      Wait.until("order remained pending")(!worker.view.pending && worker.view.outcome.nonEmpty)
    def filled(quantity: BigDecimal, value: BigDecimal): Unit =
      assertEquals(ledger.account.count, 1L)
      assertEquals(
        ledger.account.positions("XMR").quantity,
        if buy then quantity else held - quantity
      )
      assertEquals(
        ledger.account.positions("XMR").entry,
        if buy then value / quantity else BigDecimal(100)
      )
      assertEquals(ledger.account.fees, value * config.paperFee)
      assertEquals(worker.view.position, Some(if buy then quantity else held - quantity))
    def close(): Unit = worker.close()

  for side <- Vector(Side.Buy, Side.Sell) do
    for queued <- Vector(false, true) do
      test(
        s"$side newer best quote outside the reviewed limit cannot fill old levels, queued=$queued"
      ) {
        val f = new Fixture(side)
        try
          f.open()
          val review = f.review()
          def move(): Unit = f.touch(if f.buy then "100.2" else "99.8")
          f.worker.synchronized {
            if queued then
              assert(f.worker.submitReviewed(review))
              move()
            else
              move()
              assert(f.worker.submitReviewed(review))
          }
          f.settle()
          assertEquals(f.ledger.account.count, 0L)
          assertEquals(f.worker.view.position, Some(f.held))
          assertEquals(f.ledger.account.cash, f.config.paperBalance)
          assert(
            f.worker.view.outcome.exists(text =>
              text.contains("no fill") || text.contains("not filled")
            )
          )
          assert(!f.worker.submitReviewed(review))
        finally f.close()
      }

      test(
        s"$side newer best quote discards only stale better levels and retains known deeper partial depth, queued=$queued"
      ) {
        val f = new Fixture(side)
        try
          f.open()
          val review = f.review()
          def move(): Unit = f.touch(if f.buy then "100.05" else "99.95")
          f.worker.synchronized {
            if queued then
              assert(f.worker.submitReviewed(review))
              move()
            else
              move()
              assert(f.worker.submitReviewed(review))
          }
          f.settle()
          // One unit at the newer quote plus two at the next known level.
          val value = if f.buy then BigDecimal("100.05") + 2 * BigDecimal("100.1")
          else BigDecimal("99.95") + 2 * BigDecimal("99.9")
          f.filled(BigDecimal(3), value)
          assert(f.worker.view.outcome.exists(_.contains("partial")))
        finally f.close()
      }

    test(
      s"$side improved best quote uses known depth without inventing liquidity at the new quote"
    ) {
      val f = new Fixture(side)
      try
        f.open()
        val review = f.review()
        f.touch(if f.buy then "99.9" else "100.1")
        assert(f.worker.submitReviewed(review))
        f.settle()
        val value = if f.buy then BigDecimal(100) + BigDecimal("100.05") + 2 * BigDecimal("100.1")
        else BigDecimal(100) + BigDecimal("99.95") + 2 * BigDecimal("99.9")
        f.filled(BigDecimal(4), value)
        assert(f.worker.view.outcome.exists(_.contains("partial")))
      finally f.close()
    }

    test(
      s"$side fresh deeper book can fill the captured quantity beyond the original partial estimate"
    ) {
      val f = new Fixture(side)
      try
        f.open()
        val review = f.review()
        val deeper = if f.buy then
          f.book.copy(asks = Vector(Level("100", "50", 1)), time = f.clock.incrementAndGet())
        else f.book.copy(bids = Vector(Level("100", "50", 1)), time = f.clock.incrementAndGet())
        f.worker.observe(Event.BookUpdate("XMR", deeper, true))
        assert(f.worker.submitReviewed(review))
        f.settle()
        f.filled(BigDecimal(5), BigDecimal(500))
        assert(f.worker.view.outcome.exists(_.contains("filled")))
      finally f.close()
    }
