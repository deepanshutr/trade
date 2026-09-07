package desk

import java.nio.file.{Files, Path}

class DisplayCacheSpec extends munit.FunSuite:
  test("populated tape gauges and sample volume retain the recorded quantities") {
    val s = Golden.snapshot
    val strings = s.tape.map(t => (t.px, t.sz))
    assert(s.tape.map(_.sz).distinct.size > 20)
    assertEquals(s.tape.map(_.side).toSet, Set(Side.Buy, Side.Sell))
    val actual = Gauges.tapeStats(s.tape, Golden.now, DisplayCost.lenses)
    DisplayCost.lenses.zip(actual).foreach { (seconds, value) =>
      val inside = s.tape.filter(t => t.time > Golden.now - seconds * 1000)
      val buy = inside.filter(_.side == Side.Buy).map(t => BigDecimal(t.sz)).sum
      val sell = inside.filter(_.side == Side.Sell).map(t => BigDecimal(t.sz)).sum
      assert(inside.nonEmpty)
      assertEquals(value._2, inside.size)
      assertEqualsDouble(value._1.get, ((buy - sell) / (buy + sell)).toDouble, 1e-12)
    }
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

  test("warmed retained tape and volume allocate less than one million bytes per call") {
    val tape = DisplayCost.tape(6, 200000)
    assertEquals(tape.size, 200000)
    assert(tape.map(_.sz).distinct.size > 20)
    val tapeBytes = DisplayCost.warmedBytes {
      Gauges.tapeStats(tape, Golden.now, DisplayCost.lenses)
    }
    assert(tapeBytes < 1000000, s"Tape allocated $tapeBytes bytes per call")
    val first = Math.floorDiv(Golden.now / 1000 - 14400, 14400L)
    val samples = Vector(first * 14400 -> 1.0)
    val volumeBytes = DisplayCost.warmedBytes {
      Chart.fromSamples(samples, tape, first, 14400, 2)
    }
    assert(volumeBytes < 1000000, s"Volume allocated $volumeBytes bytes per call")
  }

  test("three retained sizes preserve twenty-four recorded frame hashes") {
    val expected =
      Files.readString(Path.of("golden", "display_frames.sha256")).linesIterator.toVector
    val actual = DisplayCost.frameHashes
    assertEquals(actual.size, 24)
    assertEquals(actual, expected)
  }
