package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ArrayBuffer

class ExecutionDiagnosticsFinderSpec extends munit.FunSuite:
  private class Memory extends Diagnostics.Sink:
    private val data = ArrayBuffer.empty[String]
    def append(line: String): Unit = synchronized { data += line }
    def close(): Unit = ()
    def execution: Vector[ujson.Value] = synchronized {
      data.toVector.map(ujson.read(_)).filter(_("event").str == "execution")
    }

  test("execution repeat timing uses monotonic time and includes the exact boundary") {
    val monotonic = new AtomicLong(1000L)
    val wall = new AtomicLong(90000L)
    val saved = new Memory
    val log = new Diagnostics(() => saved, wall.get, 2000L, monotonic = monotonic.get)
    val waiting =
      TradingView("XMR", message = "waiting", block = Some("Paper waiting for market details"))
    log.execution("PAPER", waiting)
    wall.set(-100000L)
    monotonic.set(30999L)
    log.execution("PAPER", waiting)
    monotonic.incrementAndGet()
    log.execution("PAPER", waiting)
    monotonic.set(61001L)
    log.execution("PAPER", waiting.copy(ready = true, block = None))
    log.start()
    log.close()
    val events = saved.execution
    assertEquals(events.size, 3)
    assertEquals(events.map(_("state_ms").str.toLong), Vector(0L, 30000L, 0L))
    assertEquals(events.map(_("previous_ms").str.toLong), Vector(0L, 0L, 60001L))
  }

  test("unknown execution data cannot enlarge or add fields to readiness records") {
    val saved = new Memory
    val log = new Diagnostics(() => saved, () => 123L, 2000L)
    val privateValue = "PRIVATE_SENTINEL_ALPHA_" * 1000
    val view = TradingView(
      privateValue,
      message = privateValue,
      block = Some(privateValue),
      outcome = Some(privateValue),
      portfolio = PortfolioView(tokenValue = Some(BigDecimal("8642097531.123")))
    )
    for _ <- 1 to 1000 do log.execution(privateValue, view)
    log.execution(privateValue, view.copy(pending = true))
    log.start()
    log.close()
    val events = saved.execution
    assertEquals(events.size, 2)
    val fields = Set("at", "event", "mode", "state", "state_ms", "previous_ms")
    events.foreach { event =>
      assertEquals(event.obj.keySet.toSet, fields)
      val text = ujson.write(event)
      assert(text.length < 180)
      assert(!text.contains("SENTINEL") && !text.contains("8642097531"))
      assertEquals(event("mode").str, "unknown")
    }
    assertEquals(events.map(_("state").str), Vector("blocked", "pending"))
  }

  test("a full diagnostic queue does not hold the execution caller") {
    val saved = new Memory
    val log = new Diagnostics(() => saved, () => 123L, 2000L, capacity = 1)
    val finished = new CountDownLatch(1)
    val thread = new Thread(() =>
      try
        for i <- 0 until 10000 do
          log.execution("PAPER", TradingView("XMR", message = "waiting", ready = i % 2 == 0))
      finally finished.countDown()
    )
    thread.setDaemon(true)
    thread.start()
    try
      assert(finished.await(2L, TimeUnit.SECONDS), "Full logging queue held execution caller")
      assertEquals(log.droppedCount, 9999L)
    finally
      thread.interrupt()
      thread.join(1000L)
      log.close()
      assert(!thread.isAlive)
  }
