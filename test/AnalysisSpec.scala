package desk

class AnalysisSpec extends munit.FunSuite:
  private def series(values: Vector[Double]): Vector[Candle] =
    values.zipWithIndex.map((c, i) => Candle(i * 60000L, c, c + 2, c - 2, c, 10))

  test("analysis uses closed contiguous bars and states the lookback") {
    val cs = series(Vector.tabulate(51)(i => 100.0 + i))
    val result = Quant.measure(cs, "1m", 51 * 60000L - 1)
    assertEquals(result.count, 50)
    assertEquals(result.expected, 50)
    assertEquals(result.fast, Some(139.5))
    assertEquals(result.slow, Some(124.5))
    assertEquals(result.support, Some(98.0))
    assertEquals(result.resistance, Some(151.0))
    assertEquals(result.status, "complete")
    val missing = Quant.measure(cs.filterNot(_.t == 25 * 60000L), "1m", 51 * 60000L - 1)
    assertEquals(missing.status, "partial")
    assertEquals(missing.slow, None)
    assertEquals(Quant.measure(cs, "1m", 60 * 60000L).status, "stale")
  }

  test("crossing study waits for a later open and complete outcome, charging both fees") {
    val cs = series(Vector(10, 9, 8, 9, 11, 12, 14, 13, 12))
    val result = Quant.crossings(cs, "1m", 2, 3, 2, 9 * 60000L, 0.001)
    assertEquals(result.samples, 1)
    // Up-cross at bar 4. Enter next open 12, exit close two bars later 13.
    assertEqualsDouble(result.mean.get, (13.0 - 12) / 12 - 0.001 * (1 + 13.0 / 12), 1e-12)
    assertEquals(Quant.crossings(cs.take(7), "1m", 2, 3, 2, 7 * 60000L, 0.001).samples, 0)
    assertEquals(Quant.crossings(cs, "1m", 2, 3, 2, 8 * 60000L - 1, 0.001).samples, 0)
    assertEquals(Quant.crossings(cs, "1m", 2, 3, 2, 6 * 60000L, 0.001).samples, 0)
    assertEquals(Quant.crossings(cs, "1m", 2, 3, 2, 8 * 60000L, 0.001).samples, 1)
    val openGap = cs.updated(5, cs(5).copy(o = 20, h = 22))
    val openResult = Quant.crossings(openGap, "1m", 2, 3, 2, 9 * 60000L, 0.001)
    assertEquals(openResult.samples, 1)
    assertEqualsDouble(openResult.mean.get, -0.35165, 1e-12)
    val gapped = cs.filterNot(_.t == 5 * 60000L)
    assertEquals(Quant.crossings(gapped, "1m", 2, 3, 2, 9 * 60000L, 0.001).samples, 0)
  }

  test("correlation coverage excludes unfinished and stale candles") {
    val cs = series(Vector.tabulate(20)(i => 100.0 + i % 7))
    val result = Quant.compare(cs, cs, "1m", 15, 20 * 60000L - 1)
    assertEquals(result.count, 15)
    assertEquals(result.status, "complete")
    assertEqualsDouble(result.value.get, 1.0, 1e-12)
    val missing = Quant.compare(cs, cs.filterNot(_.t == 10 * 60000L), "1m", 15, 20 * 60000L - 1)
    assertEquals(missing.count, 13)
    assertEquals(missing.value, None)
    assertEquals(missing.status, "partial")
    assertEquals(Quant.compare(cs, cs, "1m", 15, 30 * 60000L).status, "stale")
  }

  test("correlation uses logarithmic returns from both series") {
    val values = Vector(10.0, 15.0, 12.0, 30.0, 18.0, 63.0, 42.0)
    val result = Quant.compare(series(values), series(values.map(1000.0 / _)), "1m", 5, 7 * 60000L)
    assertEquals(result.count, 5)
    assertEquals(result.status, "complete")
    // Reciprocal prices have equal and opposite log returns, while their price correlation differs.
    assertEqualsDouble(result.value.get, -1.0, 1e-12)
  }

  test("crossing study can signal at the previous exit close") {
    val cs = series(Vector(10, 9, 8, 9, 11, 12, 8, 7, 6))
    val result = Quant.crossings(cs, "1m", 2, 3, 1, 9 * 60000L, 0.001)
    // Long at bar 5 open 12, exit bar 6 close 8. Short at bar 7 open 7, exit bar 8 close 6.
    assertEquals(result.samples, 2)
    assertEqualsDouble(result.mean.get, -0.097, 1e-12)
    assertEquals(result.positive, 1)
  }

  test("crossing study skips signals while a position is open") {
    val cs = series(Vector(10, 9, 8, 11, 9, 8, 12, 7, 13, 6, 14, 5))
    val result = Quant.crossings(cs, "1m", 2, 3, 2, 12 * 60000L, 0.001)
    // Long from bar 4 open 9 to bar 6 close 12, then bar 7 open 7 to bar 9 close 6.
    val first = (12.0 - 9) / 9 - 0.001 * (1 + 12.0 / 9)
    val second = (6.0 - 7) / 7 - 0.001 * (1 + 6.0 / 7)
    assertEquals(result.samples, 2)
    assertEqualsDouble(result.mean.get, (first + second) / 2, 1e-12)
  }
