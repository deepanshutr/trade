package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}

class RuntimeSpec extends munit.FunSuite:
  private def awaitReady(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < until do Thread.sleep(10)
    assert(condition)

  test("paper execution uses public data, requires reviewed plan and refuses stale books") {
    var time = 1800000000000L
    val requested = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requested.add(ujson.read(body)("type").str)
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PaperLedger.open(
      Files.createTempDirectory("desk-paper-execution"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal("0.0005")
    )
    val worker = new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => time, () => time)
    try
      worker.start()
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), time),
          true
        )
      )
      awaitReady(worker.view.ready)
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val plan = worker.preview(intent).toOption.get
      val review = OrderReview(intent, plan, time, time + 30000)
      assert(!worker.submitReviewed(review.copy(plan = plan.copy(quantity = plan.quantity + 1))))
      assertEquals(ledger.account.count, 0L)
      assert(worker.submitReviewed(review))
      awaitReady(ledger.account.count == 1)
      assertEquals(ledger.account.positions("XMR").quantity, plan.estimatedQuantity)
      time += 1001
      assert(!worker.view.ready)
      assert(!worker.submitReviewed(review))
      assertEquals(requested.toArray.toVector, Vector("meta"))
    finally worker.close()
  }

  test("background recorder saves queued public events and a restart can load its checkpoint") {
    val directory = Files.createTempDirectory("desk-recorder-worker")
    val time = 1800000000000L
    val recorder = new PublicRecorder(MarketArchive.open(directory), () => time)
    recorder.start()
    recorder.observe(Event.CandleUpdate("XMR", "1m", Candle(time - 60000, 100, 101, 99, 100, 5)))
    recorder.observe(Event.SocketDown)
    recorder.close()
    val recovered = MarketArchive.restore(directory, Snapshot.initial("XMR", time))
    assertEquals(recovered.candles("1m").size, 1)
    assert(recovered.feed.isInstanceOf[Health.Down])
    assert(recorder.status.contains("saved"))
  }

  test("saved messages preserve acceptance across restart without restoring an order") {
    val directory = Files.createTempDirectory("desk-message-history")
    val history = MessageHistory.open(directory)
    try history.save(Vector(DeskMessage(1, 123L, "analysis", "reading", Some(false))))
    finally history.close()
    val again = MessageHistory.open(directory)
    try
      assertEquals(again.messages, Vector(DeskMessage(1, 123L, "analysis", "reading", Some(false))))
    finally again.close()
  }

  test("saved news messages survive loading and a new order") {
    val directory = Files.createTempDirectory("desk-legacy-news")
    val path = directory.resolve("messages.json")
    try
      LocalFiles.write(path, """[{"id":1,"at":123,"kind":"news","text":"old headline"}]""")
      val old = Vector(DeskMessage(1, 123L, "news", "old headline"))
      val next = old :+ DeskMessage(2, 124L, "order", "PAPER XMR buy filled 5")
      val history = MessageHistory.open(directory)
      try
        assertEquals(history.messages, old)
        history.save(next)
      finally history.close()
      val reopened = MessageHistory.open(directory)
      try assertEquals(reopened.messages, next)
      finally reopened.close()
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory.resolve("messages.lock"))
      Files.deleteIfExists(directory)
  }
