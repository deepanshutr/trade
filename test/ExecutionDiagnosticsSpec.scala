package desk

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ArrayBuffer

class ExecutionDiagnosticsSpec extends munit.FunSuite:
  private class Memory extends Diagnostics.Sink:
    private val saved = ArrayBuffer.empty[String]
    def append(line: String): Unit = synchronized { saved += line }
    def records: Vector[ujson.Value] = synchronized(saved.toVector.map(ujson.read(_)))
    def close(): Unit = ()

  test("readiness changes record elapsed waits without recording every frame") {
    val time = new AtomicLong(10000L)
    val saved = new Memory
    val log = new Diagnostics(() => saved, () => 10000L, 2000L, monotonic = time.get)
    val waiting =
      TradingView("XMR", message = "waiting", block = Some("Paper waiting for market details"))
    log.execution("PAPER", waiting)
    for _ <- 1 to 100 do log.execution("PAPER", waiting)
    time.addAndGet(1250L)
    log.execution("PAPER", waiting.copy(ready = true, block = None))
    time.addAndGet(30000L)
    log.execution("PAPER", waiting.copy(ready = true, block = None))
    log.start()
    log.close()
    val events = saved.records.filter(_("event").str == "execution")
    assertEquals(events.size, 3)
    assertEquals(events.map(_("state").str), Vector("market", "ready", "ready"))
    assertEquals(events(1)("previous_ms").str.toLong, 1250L)
    assertEquals(events(2)("state_ms").str.toLong, 30000L)
    assert(events.forall(_("mode").str == "simulated"))
  }

  test("readiness records keep fixed categories and exclude private values") {
    val saved = new Memory
    val log = new Diagnostics(() => saved, () => 10000L, 2000L)
    val secret = "PRIVATE_SENTINEL_01923"
    val base = TradingView(
      secret,
      message = secret,
      block = Some(secret),
      portfolio = PortfolioView(tokenValue = Some(BigDecimal("987654.321")))
    )
    val cases = Vector(
      ("LIVE", base.copy(block = Some("account data stale")), "account"),
      ("LIVE", base.copy(block = Some("Set XMR leverage to 5x")), "leverage"),
      ("PAPER", base.copy(block = Some("Paper prices stale. Waiting for fresh depth")), "prices"),
      (
        "PAPER",
        base.copy(block = Some("Paper storage unavailable. Restart to recover")),
        "storage"
      ),
      ("LIVE", base.copy(pending = true), "pending"),
      ("VIEW", base, "view"),
      (secret, base, "blocked")
    )
    cases.foreach((mode, view, _) => log.execution(mode, view))
    log.start()
    log.close()
    val events = saved.records.filter(_("event").str == "execution")
    assertEquals(events.map(_("state").str), cases.map(_._3))
    val text = events.map(ujson.write(_)).mkString
    assert(!text.contains(secret))
    assert(!text.contains("987654"))
    assertEquals(events.last("mode").str, "unknown")
  }
