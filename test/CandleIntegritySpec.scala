package desk

import java.nio.file.Files

class CandleIntegritySpec extends munit.FunSuite:
  private val step = 60000L
  private val bars = Vector.tabulate(50)(i => Candle(i * step, 100, 102, 98, 101, 1))
  private val invalid = Vector(
    bars(25).copy(o = 103),
    bars(25).copy(o = 97),
    bars(25).copy(c = 103),
    bars(25).copy(c = 97),
    bars(25).copy(l = 104)
  )
  private def json(c: Candle): ujson.Value = ujson.Obj(
    "t" -> ujson.Num(c.t.toDouble),
    "o" -> c.o.toString,
    "h" -> c.h.toString,
    "l" -> c.l.toString,
    "c" -> c.c.toString,
    "v" -> c.v.toString
  )

  test("inconsistent candle ranges are rejected at both public input paths") {
    invalid.foreach { c =>
      assertEquals(Events.parseRestCandles(ujson.Arr(json(c)).render()), None)
      val data = json(c)
      data("s") = "XMR"
      data("i") = "1m"
      assert(
        Events
          .parse(ujson.Obj("channel" -> "candle", "data" -> data).render())
          .exists(_.isInstanceOf[Event.Bad])
      )
    }
    val flat = Candle(0, 100, 100, 100, 100, 0)
    assertEquals(Events.parseRestCandles(ujson.Arr(json(flat)).render()), Some(Vector(flat)))
  }

  test("conflicting history duplicates cannot silently choose one price") {
    val first = bars.head
    val other = first.copy(c = 102)
    for order <- Vector(Vector(first, other), Vector(other, first)) do
      assertEquals(Events.parseRestCandles(ujson.Arr.from(order.map(json)).render()), None)
    assert(Events.parseRestCandles(ujson.Arr(json(first), json(first)).render()).nonEmpty)
  }

  test("legacy measurements preserve an invalid or conflicting bar as a gap") {
    val malformed = invalid.map(c => bars.updated(25, c)) :+
      (bars :+ bars(25).copy(c = 102)) :+
      bars.updated(25, bars(25).copy(v = -1))
    malformed.foreach { cs =>
      val measured = Quant.measure(cs, "1m", 50 * step)
      assertEquals(measured.status, "partial")
      assertEquals(measured.slow, None)
      assertEquals(measured.support, None)
      assertEquals(measured.resistance, None)
    }
    assertEquals(Quant.measure(bars :+ bars(25), "1m", 50 * step).status, "complete")
  }

  test("restored history rejects impossible ranges and conflicting duplicates") {
    import MarketArchive.given
    val directory = Files.createTempDirectory("desk-candle-integrity")
    try
      for cs <- invalid.map(c => bars.updated(25, c)) :+ (bars :+ bars(25).copy(c = 102)) do
        val saved = CandleCheckpoint(50 * step, Map("XMR" -> Map("1m" -> cs)))
        LocalFiles.write(directory.resolve("candles.json"), upickle.default.write(saved))
        intercept[IllegalArgumentException] {
          MarketArchive.restore(directory, Snapshot.initial("XMR", 50 * step))
        }
    finally
      Files.deleteIfExists(directory.resolve("candles.json"))
      Files.deleteIfExists(directory)
  }
