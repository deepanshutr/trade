package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class LiveMetadataWorkerFinderSpec extends munit.FunSuite:
  test("the actual public worker loop recovers promptly from malformed initial metadata") {
    val attempts = new AtomicInteger()
    val first = new CountDownLatch(1)
    val valid = ujson.read("""{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5}]}""")
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] =
        require(body("type").str == "meta")
        val count = attempts.incrementAndGet()
        first.countDown()
        Some(if count == 1 then ujson.Null else valid)
      def post(body: ujson.Value): Option[ujson.Value] =
        throw new AssertionError("No order is permitted in the metadata check")
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        throw new AssertionError("No account subscription is permitted in the metadata check")
    val worker = new TradingWorker(
      TradingConfig(),
      false,
      "XMR",
      exchange,
      () => throw new AssertionError("No key is permitted in the metadata check"),
      () => throw new AssertionError("No journal is permitted in the metadata check"),
      () => 10000L,
      () => 10000L,
      warmSigning = () => throw new AssertionError("No signer warmup is permitted")
    )
    worker.start()
    try
      assert(first.await(1L, TimeUnit.SECONDS))
      val deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(2500L)
      while worker.view.rules.isEmpty && System.nanoTime() < deadline do Thread.sleep(5L)
      assert(
        worker.view.rules.nonEmpty,
        s"Actual refresh loop kept malformed metadata, attempts=${attempts.get()}"
      )
      assert(attempts.get() >= 2)
    finally worker.close()
    assert(worker.isRetired, "Metadata-only worker did not retire")
  }
