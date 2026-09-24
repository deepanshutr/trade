package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

class LiveMetadataBoundaryFinderSpec extends munit.FunSuite:
  private val valid = ujson.read(
    """{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5},{"name":"BTC","szDecimals":5,"maxLeverage":40}]}"""
  )
  private class Fixture(reply: Int => ujson.Value):
    val attempts = new AtomicInteger()
    val time = new AtomicLong(10000L)
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] =
        require(body("type").str == "meta")
        Some(reply(attempts.incrementAndGet()))
      def post(body: ujson.Value): Option[ujson.Value] = throw new AssertionError(
        "No order permitted"
      )
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        throw new AssertionError("No account subscription permitted")
    val worker = new TradingWorker(
      TradingConfig(),
      false,
      "XMR",
      exchange,
      () => throw new AssertionError("No key permitted"),
      () => throw new AssertionError("No order journal permitted"),
      time.get,
      time.get
    )
    private val refreshMethod =
      classOf[TradingWorker].getDeclaredMethods.find(_.getName == "refresh").get
    refreshMethod.setAccessible(true)
    def refresh(): Unit = { refreshMethod.invoke(worker); () }

  test("confirmed absence or delisting is accepted while selected rules are removed") {
    val missing = ujson.read("""{"universe":[{"name":"BTC","szDecimals":5,"maxLeverage":40}]}""")
    val delisted = ujson.read("""{"universe":[{"name":"XMR","isDelisted":true}]}""")
    for replacement <- Vector(missing, delisted) do
      val f = new Fixture(n => if n == 1 then valid else replacement)
      try
        f.refresh()
        assert(f.worker.view.rules.nonEmpty)
        f.time.set(70001L)
        f.refresh()
        assertEquals(f.worker.view.rules, None)
        assertEquals(f.worker.acceptedMetadata.map(_._1), Some(replacement))
        f.refresh()
        assertEquals(f.attempts.get(), 2, "Confirmed removal caused an immediate retry loop")
      finally f.worker.close()
  }

  test("a malformed live refresh cannot renew the old snapshot acceptance time") {
    val f = new Fixture(n => if n == 2 then ujson.Null else valid)
    try
      f.refresh()
      assertEquals(f.worker.acceptedMetadata.map(_._2), Some(10000L))
      f.time.set(70001L)
      f.refresh()
      assertEquals(f.worker.acceptedMetadata, None, "Malformed response renewed accepted metadata")
      f.refresh()
      assertEquals(f.attempts.get(), 3)
      assertEquals(f.worker.acceptedMetadata.map(_._2), Some(70001L))
      assertEquals(f.worker.view.rules, Some(MarketRules(0, 3, 5)))
    finally f.worker.close()
  }

  test("a delayed reply validated for the old selection cannot publish for its replacement") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val partial =
      ujson.read("""{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5},{"name":"BTC"}]}""")
    val f = new Fixture(n =>
      if n == 1 then
        entered.countDown()
        require(release.await(3L, TimeUnit.SECONDS))
        partial
      else valid
    )
    val thread = new Thread(() => f.refresh())
    thread.setDaemon(true)
    try
      thread.start()
      assert(entered.await(1L, TimeUnit.SECONDS))
      f.worker.select("BTC")
      release.countDown()
      thread.join(1000L)
      assert(!thread.isAlive)
      assertEquals(f.worker.view.coin, "BTC")
      assertEquals(f.worker.view.rules, None)
      assertEquals(
        f.worker.acceptedMetadata,
        None,
        "Old selection published a newly accepted snapshot"
      )
      f.refresh()
      assertEquals(f.attempts.get(), 2)
      assertEquals(f.worker.view.rules, Some(MarketRules(1, 5, 40)))
    finally
      release.countDown()
      thread.interrupt()
      thread.join(1000L)
      f.worker.close()
  }
