package desk

class AnalysisSpec extends munit.FunSuite:
  private def series(values: Vector[Double]): Vector[Candle] =
    values.zipWithIndex.map((c, i) => Candle(i * 60000L, c, c + 2, c - 2, c, 10))

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
