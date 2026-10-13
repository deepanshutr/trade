package desk

class DisplayCacheSpec extends munit.FunSuite:
  test("populated tape and sample volume retain the recorded quantities") {
    val s = Golden.snapshot
    val strings = s.tape.map(t => (t.px, t.sz))
    assert(s.tape.map(_.sz).distinct.size > 20)
    assertEquals(s.tape.map(_.side).toSet, Set(Side.Buy, Side.Sell))
    val first = s.tape.map(_.time / 1000).min
    val last = s.tape.map(_.time / 1000).max
    val columns = Chart.fromSamples(
      Vector(first -> 1.0),
      s.tape,
      first,
      1,
      (last - first + 1).toInt
    )
    val total = s.tape.map(t => BigDecimal(t.sz)).sum.toDouble
    assert(total > 0)
    assertEqualsDouble(columns.flatten.map(_.volume).sum, total, 1e-9)
    assertEquals(s.tape.map(t => (t.px, t.sz)), strings)
  }

  test("warmed retained volume allocates less than one million bytes per call") {
    val tape = DisplayCost.tape(6, 200000)
    assertEquals(tape.size, 200000)
    assert(tape.map(_.sz).distinct.size > 20)
    val first = Math.floorDiv(Golden.now / 1000 - 14400, 14400L)
    val samples = Vector(first * 14400 -> 1.0)
    val volumeBytes = DisplayCost.warmedBytes {
      Chart.fromSamples(samples, tape, first, 14400, 2)
    }
    assert(volumeBytes < 1000000, s"Volume allocated $volumeBytes bytes per call")
  }
