package desk

import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class AnalysisWorkerSpec extends munit.FunSuite:
  test("a manual question interrupts an automatic summary and does not wait for its reply") {
    val now = 1800000000000L
    val bars = Vector.tabulate(50)(i => Candle(now - (50 - i) * 60000L, 100, 101, 99, 100, 1))
    val snapshot = Snapshot.initial("XMR", now).copy(candles = Map("1m" -> bars))
    val began = new CountDownLatch(1)
    val answered = new CountDownLatch(1)
    val calls = new AtomicInteger(0)
    val emitted = new CopyOnWriteArrayList[String]()
    val contexts = new CopyOnWriteArrayList[String]()
    val request = (context: String, canceled: () => Boolean, study: Boolean) =>
      if calls.incrementAndGet() == 1 then
        began.countDown()
        val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
        while !canceled() && System.nanoTime() < deadline do Thread.sleep(10)
        Left("Analysis canceled")
      else
        contexts.add(context)
        Right(ModelReply("manual answer", None))
    val worker = new AnalysisWorker(
      () => snapshot,
      () => "calendar unavailable",
      request,
      (kind, text) => {
        emitted.add(text); if text.contains("manual answer") then answered.countDown()
      },
      () => now
    )
    worker.start()
    try
      assert(began.await(5, TimeUnit.SECONDS))
      assert(worker.ask("my question"))
      assert(
        answered.await(5, TimeUnit.SECONDS),
        s"${worker.status}, calls=${calls.get()}, emitted=$emitted"
      )
      assertEquals(calls.get(), 2)
      assert(contexts.get(0).contains("my question"))
      assert(!emitted.toArray.exists(_.toString.contains("Analysis canceled")))
    finally worker.close()
  }
