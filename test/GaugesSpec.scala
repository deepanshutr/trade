package desk

class GaugesSpec extends munit.FunSuite:
  private def lvl(px: String, sz: String): Level = Level(px, sz, 1)
  private def book(bidSz: String, askSz: String, t: Long = 1L): Book =
    Book(Vector(lvl("100", bidSz)), Vector(lvl("101", askSz)), t)
  test("imbalance keeps the part one formulas") {
    val (q, d) = Gauges.imbalance(book("1", "3"))
    assertEqualsDouble(q, -0.5, 1e-9)
    assertEqualsDouble(d, -0.5, 1e-9)
    assertEquals(Gauges.weights.size, 10)
    assertEqualsDouble(Gauges.weights(1), 0.7, 1e-9)
  }

  test("pearson of returns and its edge cases") {
    assertEquals(Gauges.pearson(Vector(1, 2, 3), Vector(2, 4, 6)).map(_.round), Some(1L))
    assertEqualsDouble(Gauges.pearson(Vector(1, 2, 3), Vector(3, 2, 1)).get, -1.0, 1e-9)
    assertEquals(Gauges.pearson(Vector(1, 1, 1), Vector(1, 2, 3)), None)
    assertEquals(Gauges.pearson(Vector(1, 2), Vector(1, 2)), None)
  }
