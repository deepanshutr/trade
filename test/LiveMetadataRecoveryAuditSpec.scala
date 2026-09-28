package desk

import java.util.concurrent.atomic.AtomicInteger

class LiveMetadataRecoveryAuditSpec extends munit.FunSuite:
  private val valid = ujson.Obj(
    "universe" -> ujson.Arr(
      ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5)
    )
  )
  Vector(
    "null" -> ujson.Null,
    "malformed universe" -> ujson.Obj("universe" -> "unavailable"),
    "missing market precision" -> ujson.Obj("universe" -> ujson.Arr(ujson.Obj("name" -> "XMR")))
  ).foreach { (label, invalid) =>
    test("live metadata retries after " + label) {
      val attempts = new AtomicInteger()
      val exchange = new Exchange:
        def info(body: ujson.Value): Option[ujson.Value] =
          assertEquals(body("type").str, "meta")
          Some(if attempts.incrementAndGet() == 1 then invalid else valid)
        def post(body: ujson.Value): Option[ujson.Value] = fail("no order in metadata check")
        def watch(
            coin: String,
            owner: String,
            generation: Long,
            receive: Option[ujson.Value] => Unit
        ): Unit =
          fail("no account in metadata check")
      val worker = new TradingWorker(
        TradingConfig(),
        false,
        "XMR",
        exchange,
        () => fail("no protected key in metadata check"),
        () => fail("no order journal in metadata check"),
        () => 10000L,
        () => 10000L
      )
      val refresh = classOf[TradingWorker].getDeclaredMethods.find(_.getName == "refresh").get
      refresh.setAccessible(true)
      try
        refresh.invoke(worker)
        val acceptedInvalid = worker.acceptedMetadata.nonEmpty
        assertEquals(worker.view.rules, None)
        refresh.invoke(worker)
        assertEquals(attempts.get(), 2, "one malformed result suppressed the next metadata read")
        assertEquals(worker.view.rules, Some(MarketRules(0, 3, 5)))
        assert(!acceptedInvalid, "malformed metadata was published as an accepted snapshot")
      finally worker.close()
    }
  }

  test("switching to malformed cached market limits retries metadata immediately") {
    val attempts = new AtomicInteger()
    val malformed = ujson.Obj(
      "universe" -> ujson.Arr(
        ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5),
        ujson.Obj("name" -> "BTC")
      )
    )
    val corrected = ujson.Obj(
      "universe" -> ujson.Arr(
        ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5),
        ujson.Obj("name" -> "BTC", "szDecimals" -> 5, "maxLeverage" -> 40)
      )
    )
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] =
        assertEquals(body("type").str, "meta")
        Some(if attempts.incrementAndGet() == 1 then malformed else corrected)
      def post(body: ujson.Value): Option[ujson.Value] = fail("no order in metadata check")
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        fail("no account in metadata check")
    val worker = new TradingWorker(
      TradingConfig(),
      false,
      "XMR",
      exchange,
      () => fail("no protected key in metadata check"),
      () => fail("no order journal in metadata check"),
      () => 10000L,
      () => 10000L
    )
    val refresh = classOf[TradingWorker].getDeclaredMethods.find(_.getName == "refresh").get
    refresh.setAccessible(true)
    try
      refresh.invoke(worker)
      worker.select("BTC")
      refresh.invoke(worker)
      assertEquals(attempts.get(), 2, "malformed selected limits stayed cached")
      assertEquals(worker.view.rules, Some(MarketRules(1, 5, 40)))
    finally worker.close()
  }
