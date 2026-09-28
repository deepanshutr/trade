package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

class MetadataReadinessSpec extends munit.FunSuite:
  private val at = 1800000000000L
  private val valid =
    """{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5},{"name":"BTC","szDecimals":4,"maxLeverage":10}]}"""

  private final class Fixture(initial: Option[(ujson.Value, Long)], respond: () => Option[String]):
    val time = new AtomicLong(at)
    val calls = new AtomicInteger(0)
    val rest = new Rest:
      def post(body: String): Option[String] =
        calls.incrementAndGet()
        respond()
    val config = TradingConfig()
    val path = Files.createTempDirectory("desk-metadata-readiness-")
    val ledger = PaperLedger.open(path, "mainnet", config.paperBalance, config.paperFee)
    val worker =
      new PaperWorker(config, "XMR", ledger, rest, () => time.get(), () => time.get(), initial)
    def quote(coin: String = "XMR"): Unit =
      worker.observe(
        Event.BookUpdate(
          coin,
          Book(Vector(Level("99", "20", 1)), Vector(Level("101", "20", 1)), time.get()),
          true
        )
      )
    worker.observe(Event.SocketUp)
    quote()
    def close(): Unit =
      worker.close()
      val files = Files.walk(path)
      try files.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
      finally files.close()

  test("a syntactically valid but malformed first response does not suppress retries") {
    val count = new AtomicInteger(0)
    val f = new Fixture(None, () => Some(if count.incrementAndGet() == 1 then "null" else valid))
    try
      f.worker.start()
      Wait.until(s"No recovery from malformed first response, calls=${f.calls.get()}")(
        f.worker.view.ready
      )
      assert(f.calls.get() >= 2)
    finally f.close()
  }

  test("a malformed inherited snapshot is immediately replaced, not treated as recently accepted") {
    val f = new Fixture(Some(ujson.Null -> at), () => Some(valid))
    try
      assert(!f.worker.view.ready)
      f.worker.start()
      Wait.until("Malformed inherited metadata suppressed a valid fetch")(f.worker.view.ready)
      assert(f.calls.get() >= 1)
    finally f.close()
  }

  test("old and future inherited metadata cannot enable an order") {
    for stamp <- Vector(at - 60001L, at + 1L, Long.MinValue, Long.MaxValue) do
      val f = new Fixture(Some(ujson.read(valid) -> stamp), () => None)
      try
        assert(!f.worker.view.ready, s"Metadata timestamp $stamp was accepted")
        assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard)).isLeft)
      finally f.close()
  }

  test("inherited metadata retains its exact original expiry boundary") {
    val f = new Fixture(Some(ujson.read(valid) -> (at - 60000L)), () => None)
    try
      assert(f.worker.view.ready)
      f.time.incrementAndGet()
      f.quote()
      assert(!f.worker.view.ready)
    finally f.close()
  }

  test("failed refresh does not renew accepted metadata age") {
    val originalAt = at - 31000L
    val f = new Fixture(Some(ujson.read(valid) -> originalAt), () => None)
    try
      f.worker.start()
      Wait.until("Refresh not attempted")(f.calls.get() >= 1)
      assert(f.worker.view.ready)
      f.time.set(originalAt + 60000L)
      f.quote()
      assert(f.worker.view.ready)
      f.time.incrementAndGet()
      f.quote()
      assert(!f.worker.view.ready, "Failure incorrectly renewed metadata age")
    finally f.close()
  }

  test("malformed refresh retains the valid snapshot only until its original expiry") {
    val originalAt = at - 31000L
    val returned = new CountDownLatch(1)
    val f = new Fixture(
      Some(ujson.read(valid) -> originalAt),
      () => { returned.countDown(); Some("null") }
    )
    try
      f.worker.start()
      assert(returned.await(2L, TimeUnit.SECONDS))
      Thread.sleep(40L)
      assert(f.worker.view.ready, "Malformed refresh erased accepted rules")
      f.time.set(originalAt + 60001L)
      f.quote()
      assert(!f.worker.view.ready, "Malformed refresh renewed metadata age")
    finally f.close()
  }

  test("delayed refresh cannot hide expiry and binds replacement rules to the current market") {
    val originalAt = at - 31000L
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(
      Some(ujson.read(valid) -> originalAt),
      () =>
        entered.countDown()
        require(release.await(3L, TimeUnit.SECONDS))
        Some(valid)
    )
    try
      f.worker.start()
      assert(entered.await(2L, TimeUnit.SECONDS))
      f.worker.select("BTC")
      f.time.set(originalAt + 60001L)
      f.quote("BTC")
      assert(!f.worker.view.ready, "Pending refresh kept expired metadata tradable")
      release.countDown()
      Wait.until("Successful delayed refresh never enabled current market")(f.worker.view.ready)
      assertEquals(f.worker.view.coin, "BTC")
      assertEquals(f.worker.view.rules.map(_.szDecimals), Some(4))
    finally
      release.countDown()
      f.close()
  }

  test("confirmed market removal or delisting disables old accepted rules") {
    val replies = Vector(
      """{"universe":[{"name":"BTC","szDecimals":4,"maxLeverage":10}]}""",
      """{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5,"isDelisted":true}]}"""
    )
    for reply <- replies do
      val f = new Fixture(Some(ujson.read(valid) -> (at - 31000L)), () => Some(reply))
      try
        assert(f.worker.view.ready)
        f.worker.start()
        Wait.until("Removal or delisting left stale rules tradable")(!f.worker.view.ready)
        assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard)).isLeft)
      finally f.close()
  }

  test("selecting malformed cached rules triggers a replacement fetch promptly") {
    val partial = ujson.read(valid)
    partial("universe")(1)("szDecimals") = ujson.Str("bad")
    val f = new Fixture(Some(partial -> at), () => Some(valid))
    try
      assert(f.worker.view.ready)
      f.worker.select("BTC")
      f.quote("BTC")
      assert(!f.worker.view.ready)
      f.worker.start()
      Wait.until("Malformed newly selected metadata suppressed refetch")(f.worker.view.ready)
      assertEquals(f.worker.view.rules.map(_.szDecimals), Some(4))
    finally f.close()
  }
