package desk

import java.util.concurrent.TimeUnit

class PaperReviewSpec extends TempDirectorySuite:
  test("paper review lifetime is bounded without timestamp overflow") {
    val now = 1800000000000L
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PaperLedger.open(
      tempDirectory("desk-paper-review-expiry"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val worker = new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => now, () => now)
    try
      worker.start()
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), now),
          true
        )
      )
      val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !worker.view.ready && System.nanoTime() < until do Thread.sleep(5)
      assert(worker.view.ready)
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val review = OrderReview(intent, worker.preview(intent).toOption.get, now, now + 30000)
      assert(!worker.submitReviewed(review.copy(expiresAt = now + 30001)))
      assert(
        !worker.submitReviewed(
          review.copy(createdAt = Long.MinValue + 1, expiresAt = Long.MaxValue)
        )
      )
      assert(!worker.submitReviewed(review.copy(expiresAt = now)))
      assert(!worker.submitReviewed(review.copy(createdAt = now + 1)))
      assert(!worker.submitReviewed(review.copy(leverage = Some(1))))
      assert(!worker.submitReviewed(review.copy(marginMode = Some("cross"))))
      assertEquals(ledger.account.count, 0L)
      assert(worker.submitReviewed(review))
    finally worker.close()
  }
