package desk

class QuantWindowCostSpec extends munit.FunSuite:
  private val step = 60000L
  private val now = 6000L * step
  private val history = Vector.tabulate(State.seriesCap) { index =>
    val price = 100.0 + (index % 31) / 10.0
    Candle(now - (State.seriesCap - index) * step, price, price + 1, price - 1, price, 1)
  }

  test("a current comparison does not allocate for every retained candle") {
    val tail = history.takeRight(61)
    assertEquals(
      Quant.compare(history, history, "1m", 60, now),
      Quant.compare(tail, tail, "1m", 60, now)
    )
    val small = DisplayCost.warmedBytes(Quant.compare(tail, tail, "1m", 60, now))
    val retained = DisplayCost.warmedBytes(Quant.compare(history, history, "1m", 60, now))
    assert(
      retained <= small * 4 + 131072L,
      s"Retained history allocated $retained bytes versus $small for the visible window"
    )
  }

  test("history outside the selected window preserves stale and unavailable distinctions") {
    val old = history.take(2)
    assertEquals(Quant.compare(old, old, "1m", 60, now).status, "stale")
    val invalid = old.map(c => c.copy(h = c.l - 1))
    assertEquals(Quant.compare(invalid, invalid, "1m", 60, now).status, "unavailable")
    val conflicts = old ++ old.map(c => c.copy(v = 2))
    assertEquals(Quant.compare(conflicts, conflicts, "1m", 60, now).status, "unavailable")
  }

  test("a conflicting duplicate remains a gap in an unordered current window") {
    val conflict = history(history.size - 30).copy(v = 2)
    val result = Quant.compare((history :+ conflict).reverse, history, "1m", 60, now)
    assertEquals(result.status, "partial")
    assertEquals(result.value, None)
  }
