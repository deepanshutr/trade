package desk

import java.time.ZoneOffset

class ChartSpec extends munit.FunSuite:
  private def br(bits: Int): String = (0x2800 + bits).toChar.toString
  private def lvl(px: String, sz: String = "1"): Level = Level(px, sz, 1)
  private def col(v: Double, vol: Double = 0.0): Option[Column] = Some(Column(v, v, v, v, vol))
  private val t0 = 1_700_000_000_000L
  private def s0: Snapshot = Snapshot.initial("XMR", t0)
  private val FULL = (0x28ff).toChar.toString
  private val LHALF = (0x2847).toChar.toString
  private val RHALF = (0x28b8).toChar.toString

  test("a canvas maps dots to braille bits") {
    val cv = Canvas(1, 1)
    cv.dot(0, 0)
    assertEquals(cv.lines, Vector(br(0x01)))
    cv.dot(1, 3)
    assertEquals(cv.lines, Vector(br(0x81)))
    for x <- 0 to 1; y <- 0 to 3 do cv.dot(x, y)
    assertEquals(cv.lines, Vector(br(0xff)))
    cv.dot(5, 5)
    assertEquals(cv.lines, Vector(br(0xff)))
  }

  test("dot bounds-checks x independently of y") {
    val cv = Canvas(2, 1)
    cv.dot(-1, 1)
    cv.dot(4, 1)
    assertEquals(cv.lines, Vector("  "))
  }

  test("a run fills every dot between its ends in either order") {
    val cv = Canvas(1, 2)
    cv.run(0, 6, 1)
    assertEquals(cv.lines, Vector(br(0x46), br(0x07)))
    assertEquals(Canvas(2, 1).lines, Vector("  "))
  }

  test("a canvas carries a style per cell and wraps it on output") {
    val cv = Canvas(2, 1)
    cv.dot(0, 0, "X")
    cv.dot(2, 0)
    assertEquals(cv.lines, Vector("X" + br(0x01) + Screen.reset + br(0x01)))
    cv.restyle(1, 0, "Y")
    assertEquals(cv.lines, Vector("X" + br(0x01) + Screen.reset + "Y" + br(0x01) + Screen.reset))
    assertEquals(
      Chart.styled(Vector(('a', "S"), ('b', "S"), ('c', ""))),
      "S" + "ab" + Screen.reset + "c"
    )
  }

  test("a style turns its colours off for greyscale") {
    assertEquals(Style.colour.bid, Screen.esc + "[32m")
    assertEquals(Style.colour.ask, Screen.esc + "[31m")
    assertEquals(Style.mono.bid, "")
    assertEquals(
      Style.colour.wrap(Style.colour.bid, "ab"),
      Screen.esc + "[32m" + "ab" + Screen.reset
    )
    assertEquals(Style.mono.wrap(Style.mono.bid, "ab"), "ab")
    assertEquals(Style.colour.wrap(Style.colour.bid, "  "), "  ")
  }

  test("the window ends at the column holding the right edge second") {
    val now = 1_000_000L
    assertEquals(Chart.window(s0, 4, now), (997L, 1))
    assertEquals(Chart.window(s0.copy(zoom = 3), 4, now), (97L, 10))
    assertEquals(Chart.window(s0.copy(offsetSec = 25), 4, now), (972L, 1))
  }

  test(
    "sample columns take first low high last, sum the volume and repeat the last value in gaps"
  ) {
    val samples = Vector((996L, 1.0), (997L, 3.0), (999L, 2.0), (1002L, 5.0))
    val tape = Vector(
      Trade(996_000L, Side.Buy, "1", "1"),
      Trade(997_500L, Side.Sell, "1", "2"),
      Trade(1_003_000L, Side.Buy, "1", "4")
    )
    val cs = Chart.fromSamples(samples, tape, 498L, 2, 4)
    assertEquals(
      cs,
      Vector(
        Some(Column(1.0, 1.0, 3.0, 3.0, 3.0)),
        Some(Column(2.0, 2.0, 2.0, 2.0, 0.0)),
        Some(Column(2.0, 2.0, 2.0, 2.0, 0.0)),
        Some(Column(5.0, 5.0, 5.0, 5.0, 4.0))
      )
    )
  }

  test("columns before the first sample stay blank and a sample before the window feeds the fill") {
    assertEquals(
      Chart.fromSamples(Vector.empty, Vector.empty, 498L, 2, 3),
      Vector(None, None, None)
    )
    assertEquals(
      Chart.fromSamples(Vector((1000L, 4.0)), Vector.empty, 498L, 2, 3),
      Vector(None, None, col(4.0))
    )
    assertEquals(
      Chart.fromSamples(Vector((990L, 7.0)), Vector.empty, 498L, 2, 2),
      Vector(col(7.0), col(7.0))
    )
  }

  test("candle columns merge whole minutes and leave holes blank") {
    val cs = (100 to 104).toVector.map(k => Candle(k * 60_000L, k, k + 2, k - 2, k + 1, 10))
    assertEquals(
      Chart.fromCandles(cs, 50L, 120, 3),
      Vector(
        Some(Column(100.0, 98.0, 103.0, 102.0, 20.0)),
        Some(Column(102.0, 100.0, 105.0, 104.0, 20.0)),
        Some(Column(104.0, 102.0, 106.0, 105.0, 10.0))
      )
    )
    assertEquals(Chart.fromCandles(cs, 48L, 120, 2), Vector(None, None))
  }

  test("columns pick samples below sixty seconds and candles from sixty") {
    val s =
      s0.copy(
        samples = Vector((999L, 1.0)),
        candles = Map("1m" -> Vector(Candle(0L, 2, 2, 2, 2, 1)))
      )
    assertEquals(Chart.columns(s, 2, 1_000_000L), Vector(col(1.0), col(1.0)))
    assertEquals(
      Chart.columns(s.copy(zoom = 5), 2, 119_000L),
      Vector(Some(Column(2.0, 2.0, 2.0, 2.0, 1.0)), None)
    )
  }

  test("columns pick the zoom series and references retain the same complete candle") {
    val candle = Candle(0L, 2, 3, 1, 2.5, 1)
    val s = s0.copy(
      candles = Map("5m" -> Vector(candle), "1m" -> Vector(Candle(0L, 9, 9, 9, 9, 9))),
      zoom = State.zooms.indexOf(300)
    )
    val expected = Vector(Some(Column(2.0, 1.0, 3.0, 2.5, 1.0)), None)
    assertEquals(Chart.columns(s, 2, 599_000L), expected)
    val ref = Ref(Vector.empty, Map("5m" -> Vector(candle)))
    assertEquals(Chart.referenceColumns(ref, 2, s, 599_000L), expected)
    val sampled = Ref(Vector(999L -> 1.0), Map.empty)
    assertEquals(Chart.referenceColumns(sampled, 2, s0, 1_000_000L), Vector(col(1), col(1)))
  }

  test("the axis pads five percent around the columns and the book and maps rows and dots") {
    val book = Book(Vector(lvl("99")), Vector(lvl("112")), 1L)
    val a = Chart.axis(Vector(Some(Column(100, 100, 110, 110, 0)), None), Some(book), 14).get
    assertEqualsDouble(a.lo, 98.35, 1e-9)
    assertEqualsDouble(a.hi, 112.65, 1e-9)
    assertEquals(a.row(112.65), 0)
    assertEquals(a.row(98.35), 13)
    assertEquals(a.row(105.0), 7)
    assertEquals(a.dot(98.35), 55)
    assertEquals(a.row(500.0), 0)
    assertEquals(a.dot(0.0), 55)
  }

  test("a flat axis pads one tenth of a percent and no data gives no axis") {
    val a = Chart.axis(Vector(col(200.0)), None, 14).get
    assertEqualsDouble(a.lo, 199.8, 1e-9)
    assertEqualsDouble(a.hi, 200.2, 1e-9)
    assertEquals(Chart.axis(Vector(None), None, 14), None)
  }

  test("decimals follow the part one rule") {
    assertEquals(Chart.decimals(112.65), 2)
    assertEquals(Chart.decimals(0.5), 5)
    assertEquals(Chart.decimals(123456.0), 0)
  }

  test("the line joins closing points and bolds the newest cell") {
    val axis = Axis(1.0, 3.0, 1)
    val lines = Chart.line(Vector(col(1.0), col(3.0)), axis)
    assertEquals(lines, Vector(Screen.bold + br(0x5c) + Screen.reset))
    assertEquals(Chart.line(Vector(None, None), axis), Vector(" "))
  }

  test("a bucket width follows the significant figures") {
    assertEqualsDouble(Chart.width(530.0, 4), 0.1, 1e-9)
    assertEqualsDouble(Chart.width(530.0, 3), 1.0, 1e-9)
    assertEqualsDouble(Chart.width(530.0, 2), 10.0, 1e-9)
    assertEqualsDouble(Chart.width(81010.0, 3), 100.0, 1e-9)
    assertEqualsDouble(Chart.width(0.0523, 3), 0.0001, 1e-12)
  }

  test("coarse buckets join the spans only beyond the finer book's outermost level") {
    val full = Book(Vector(lvl("530.5", "1"), lvl("530.4", "1")), Vector(lvl("530.7", "1")), 1L)
    val sf3 =
      Book(Vector(lvl("530", "10"), lvl("529", "20")), Vector(lvl("531", "5"), lvl("532", "7")), 1L)
    val sf2 = Book(
      Vector(lvl("530", "100"), lvl("520", "200")),
      Vector(lvl("540", "50"), lvl("550", "60")),
      1L
    )
    val spans = Chart.spans(full, Map(3 -> sf3, 2 -> sf2))
    assertEquals(
      spans.take(3),
      Vector(
        Span(530.5, 530.5, 1.0, true),
        Span(530.4, 530.4, 1.0, true),
        Span(530.7, 530.7, 1.0, false)
      )
    )
    assertEquals(
      spans.drop(3),
      Vector(
        Span(529.0, 530.0, 20.0, true),
        Span(531.0, 532.0, 7.0, false),
        Span(540.0, 550.0, 60.0, false)
      )
    )
    assertEquals(Chart.spans(full, Map.empty).size, 3)
  }

  test("a bid bucket exactly touching the finer book's lowest bid is joined, not excluded") {
    val full = Book(Vector(lvl("101", "1")), Vector(lvl("102", "1")), 1L)
    val sf3 = Book(Vector(lvl("100", "5")), Vector.empty, 1L)
    val spans = Chart.spans(full, Map(3 -> sf3))
    assertEquals(
      spans,
      Vector(
        Span(101.0, 101.0, 1.0, true),
        Span(102.0, 102.0, 1.0, false),
        Span(100.0, 101.0, 5.0, true)
      )
    )
  }

  test("a span spreads its size evenly over the rows it covers") {
    val a = Axis(500.0, 560.0, 7)
    val (depth, isBid) = Chart.depthRows(
      Vector(
        Span(529.0, 530.0, 20.0, true),
        Span(540.0, 550.0, 60.0, false),
        Span(600.0, 610.0, 5.0, false)
      ),
      a
    )
    assertEquals(depth.toVector, Vector(0.0, 30.0, 30.0, 20.0, 0.0, 0.0, 0.0))
    assertEquals(isBid.toVector, Vector(false, false, false, true, false, false, false))
  }

  test("a span partly outside the axis contributes only its visible fraction") {
    val a = Axis(526.3058, 531.02, 14)
    val (depth, isBid) = Chart.depthRows(Vector(Span(531.0, 532.0, 500.0, false)), a)
    assertEqualsDouble(depth(0), 10.0, 1e-9)
    assertEqualsDouble(depth.toVector.sum, 10.0, 1e-9)
    assertEquals(isBid.toVector.count(identity), 0)
    val inside = Chart.depthRows(Vector(Span(529.0, 530.0, 20.0, true)), Axis(500.0, 560.0, 7))
    assertEquals(inside._1.toVector, Vector(0.0, 0.0, 0.0, 20.0, 0.0, 0.0, 0.0))
    val level = Chart.depthRows(Vector(Span(530.0, 530.0, 7.0, true)), Axis(500.0, 560.0, 7))
    assertEquals(level._1.toVector, Vector(0.0, 0.0, 0.0, 7.0, 0.0, 0.0, 0.0))
  }

  test("bars are braille with a half cell for the odd step and the clamp mark outside") {
    assertEquals(Chart.bar(3, bid = true, clamp = false), FULL + LHALF + " " * 19)
    assertEquals(Chart.bar(3, bid = false, clamp = false), " " * 19 + RHALF + FULL)
    assertEquals(Chart.bar(1, bid = false, clamp = false), " " * 20 + RHALF)
    assertEquals(Chart.bar(42, bid = true, clamp = true), FULL * 20 + ">")
    assertEquals(Chart.bar(42, bid = false, clamp = true), ">" + FULL * 20)
    assertEquals(Chart.bar(42, bid = true, clamp = false), FULL * 21)
  }

  test("depth bars put bids from the label side and asks from the chart side") {
    val book = Book(Vector(lvl("100", "1")), Vector(lvl("110", "100"), lvl("107.5", "10")), 1L)
    val a = Axis(100.0, 110.0, 5)
    val rows = Chart.depthBars(Some(book), Map.empty, Some(a), 5, Style.mono)
    assertEquals(rows.size, 5)
    val plain = rows.map(Chart.plain)
    assert(plain.forall(_.length == 21), plain)
    assertEquals(plain(0), FULL * 21)
    assertEquals(plain(1), " " * 15 + FULL * 6)
    assertEquals(plain(2), " " * 21)
    assertEquals(plain(3), " " * 21)
    assertEquals(plain(4), FULL + " " * 20)
    assert(!rows.exists(_.contains(Screen.bold)))
  }

  test("depth bars use the log rule with the clamp mark at the outer end") {
    val bids = (1 to 20).toVector.map(i => Level((200 - i).toString, (i * i).toString, 1))
    val book = Book(bids, Vector.empty, 1L)
    val a = Axis(180.0, 199.0, 20)
    val plain =
      Chart.depthBars(Some(book), Map.empty, Some(a), 20, Style.mono).map(Chart.plain)
    assertEquals(plain(19), FULL * 20 + ">")
    assertEquals(plain(0), LHALF + " " * 20)
    assert(plain.forall(_.length == 21))
    assertEquals(
      Chart.depthBars(None, Map.empty, None, 2, Style.mono),
      Vector(" " * 21, " " * 21)
    )
  }

  test("depth bars scale by depth when the reference ties the lower median") {
    val book = Book(Vector(lvl("100", "1"), lvl("99", "5"), lvl("98", "5")), Vector.empty, 1L)
    val a = Axis(98.0, 100.0, 3)
    val plain =
      Chart.depthBars(Some(book), Map.empty, Some(a), 3, Style.mono).map(Chart.plain)
    assertEquals(plain(0), FULL * 5 + LHALF + " " * 15)
    assertEquals(plain(1), FULL * 21)
    assertEquals(plain(2), FULL * 21)
  }

  test("depth bars draw far depth from the coarse books in colour") {
    val full = Book(Vector(lvl("530.5", "1")), Vector(lvl("530.7", "1")), 1L)
    val sf2 = Book(Vector.empty, Vector(lvl("550", "60")), 1L)
    val a = Axis(500.0, 560.0, 7)
    val rows = Chart.depthBars(Some(full), Map(2 -> sf2), Some(a), 7, Style.colour)
    val plain = rows.map(Chart.plain)
    assert(plain.forall(_.length == 21), plain)
    assert(plain(1).trim.nonEmpty && plain(2).trim.nonEmpty, plain)
    assert(rows(1).contains(Style.colour.ask), rows(1))
    assert(rows(3).contains(Style.colour.bid), rows.mkString("\n"))
    assert(!plain.exists(_.contains("#")))
    assert(
      !Chart
        .depthBars(Some(full), Map(2 -> sf2), Some(a), 7, Style.mono)
        .exists(_.contains(Screen.esc + "[3"))
    )
  }

  test("depth is placed on the true row, so a bid never sits above the current price") {
    // At nine rows the mid 547.47 has true row 5. The touch either side shares that row, where
    // the bid wins, and the rounded row of the old ladder would have put both on row 4.
    val a = Axis(543.6, 552.4, 9)
    val book = Book(
      Vector(lvl("547.46"), lvl("545.00")),
      Vector(lvl("547.48"), lvl("549.00")),
      1L
    )
    val (depth, isBid) = Chart.depthRows(Chart.spans(book, Map.empty), a)
    assertEquals(depth.toVector, Vector(0.0, 0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 1.0, 0.0))
    assertEquals(isBid.toVector.zipWithIndex.collect { case (true, i) => i }, Vector(5, 7))
    assertEquals(a.trueRow(547.47), 5)
    assertEquals(a.trueRow(552.4), 0)
    assertEquals(a.trueRow(543.6), 8)
    assertEquals(a.trueRow(600.0), 0)
    assertEquals(a.trueRow(500.0), 8)
  }

  test("scale rows print the top, the bottom and the current price in bold with its marker") {
    val a = Axis(98.35, 112.65, 14)
    val rows = Chart.scaleRows(Some(a), 14, Some(7), Some("105.5"), 8)
    assertEquals(rows(0), " 112.65 ")
    assertEquals(rows(7), Screen.bold + "  105.5>" + Screen.reset)
    assertEquals(rows(13), "  98.35 ")
    assertEquals(rows(1), " " * 8)
    val lower = Chart.scaleRows(Some(a), 14, Some(8), Some("105.5"), 8)
    assertEquals(lower(7), " 104.95 ")
    assertEquals(lower(8), Screen.bold + "  105.5>" + Screen.reset)
    assertEquals(Chart.scaleRows(None, 2, None, None, 8), Vector(" " * 8, " " * 8))
    // A price too wide for its column is shortened, never cut.
    val wide = Chart.scaleRows(Some(Axis(1.0e8, 3.0e8, 3)), 3, None, None, 8)
    assertEquals(wide, Vector("   ~3e8 ", "   ~2e8 ", "   ~1e8 "))
  }

  test("one volume row scales to the largest column with at least one dot") {
    val cs = Vector(col(1.0, 0.0), col(1.0, 7.0), col(1.0, 1.0), col(1.0, 14.0))
    val (lines, vmax) = Chart.volumeRows(cs, Style.mono)
    assertEquals(vmax, 14.0)
    assertEquals(lines, Vector(br(0xa0) + br(0xf0)))
    assertEquals(Chart.volumeRows(Vector(None, None), Style.mono), (Vector(" "), 0.0))
  }

  test("volume colour remains available to its renderer but the screen uses plain ink") {
    val up = Some(Column(1.0, 1.0, 2.0, 2.0, 5.0))
    val down = Some(Column(2.0, 1.0, 2.0, 1.0, 5.0))
    val (lines, _) = Chart.volumeRows(Vector(up, up, down, down), Style.colour)
    assert(lines.head.contains(Style.colour.bid) && lines.head.contains(Style.colour.ask))
    val (mono, _) = Chart.volumeRows(Vector(up, up, down, down), Style.mono)
    assert(!mono.head.contains(Screen.esc))
  }

  test("held prices widen the axis before the five percent pad") {
    val series = Vector(
      Some(Column(548, 546, 552, 547, 0)),
      None,
      Some(Column(547, 544, 549, 546, 0))
    )
    val plain = Chart.axis(series, None, 9).get
    assertEqualsDouble(plain.lo, 543.60, 1e-9)
    assertEqualsDouble(plain.hi, 552.40, 1e-9)
    val held = Chart.axis(series, None, 9, Vector(500.0)).get
    assert(held.lo < 500.0, held)
    assertEqualsDouble(held.lo, 497.40, 1e-9)
    assertEqualsDouble(held.hi, 554.60, 1e-9)
    val above = Chart.axis(series, None, 9, Vector(560.0, 546.0)).get
    assertEqualsDouble(above.lo, 543.20, 1e-9)
    assertEqualsDouble(above.hi, 560.80, 1e-9)
  }

  // The orders axis places the current price on its true row, the prices above it upward nearest
  // first, each on its true row or the row above its lower neighbour, and the prices below it
  // downward the same way. A row under 0 or past the last is a mark left off the axis.
  private def placed(a: Axis, current: Double, held: Vector[Double]): Vector[Int] =
    val row = a.trueRow(current)
    val up = held
      .filter(_ > current)
      .distinct
      .sorted
      .scanLeft(row)((last, p) => a.trueRow(p).min(last - 1))
      .tail
    val down = held
      .filter(_ < current)
      .distinct
      .sorted
      .reverse
      .scanLeft(row)((last, p) => a.trueRow(p).max(last + 1))
      .tail
    up ++ down

  // The touch is 546.08 and 546.10, so the current price is 546.09.
  private val touch = Book(Vector(lvl("546.08")), Vector(lvl("546.10")), 1L)

  test("a current price on the top row leaves a row for each held price above it") {
    // The plotted range ends at the current price, with a sell resting one tick above it.
    val series = Vector(Some(Column(540.0, 540.0, 546.09, 546.09, 0)))
    val held = Vector(546.09, 546.10)
    for rows <- Vector(9, 20) do
      val a = Chart.axis(series, Some(touch), rows, held).get
      assertEquals(a.trueRow(546.09), 1, a)
      assert(placed(a, 546.09, held).forall(r => r >= 0 && r < rows), (a, placed(a, 546.09, held)))
      assertEqualsDouble(a.lo, 539.695, 1e-9)
      assert(a.hi > 546.405, a)
  }

  test("three sells just above a current price on the top rows each keep a row at 50 rows") {
    // At 50 rows the five percent pad leaves the current price on row 2, two rows for three.
    val series = Vector(Some(Column(540.0, 540.0, 546.09, 546.09, 0)))
    val three = Vector(546.09, 546.10, 546.11, 546.12)
    val wide = Chart.axis(series, Some(touch), 50, three).get
    assertEquals(wide.trueRow(546.09), 3, wide)
    assertEquals(placed(wide, 546.09, three).sorted, Vector(0, 1, 2))
    assertEqualsDouble(wide.lo, 539.694, 1e-9)
  }

  test("a current price on the bottom row leaves a row for each held price below it") {
    // The plotted range starts at the current price, with a buy ticket one tick under it.
    val series = Vector(Some(Column(546.09, 546.09, 552.0, 552.0, 0)))
    val held = Vector(546.09, 546.08)
    for rows <- Vector(9, 20) do
      val a = Chart.axis(series, Some(touch), rows, held).get
      assertEquals(a.trueRow(546.09), rows - 2, a)
      assert(placed(a, 546.09, held).forall(r => r >= 0 && r < rows), (a, placed(a, 546.09, held)))
      assertEqualsDouble(a.hi, 552.296, 1e-9)
      assert(a.lo < 545.784, a)
  }

  test("held prices that cannot all fit keep the five percent pad and are left off as before") {
    // Three prices above and the current price need four rows of three.
    val series = Vector(Some(Column(540.0, 540.0, 546.09, 546.09, 0)))
    val held = Vector(546.09, 546.10, 546.11, 546.12)
    val a = Chart.axis(series, Some(touch), 3, held).get
    assertEqualsDouble(a.lo, 539.694, 1e-9)
    assertEqualsDouble(a.hi, 546.426, 1e-9)
    assert(placed(a, 546.09, held).exists(_ < 0), placed(a, 546.09, held))
  }

  test("held prices with room either side of the current price keep the five percent pad") {
    // The plan's pinned case: the current price 546.095 keeps row 6 of nine and row 13 of 19.
    val series = Vector(Some(Column(548.0, 544.0, 552.0, 546.0, 0)))
    val book = Book(Vector(lvl("546.09")), Vector(lvl("546.10")), 1L)
    val held = Vector(552.0, 546.10, 546.095, 546.0, 544.0)
    for (rows, row) <- Vector((9, 6), (19, 13)) do
      val a = Chart.axis(series, Some(book), rows, held).get
      assertEqualsDouble(a.lo, 543.60, 1e-9)
      assertEqualsDouble(a.hi, 552.40, 1e-9)
      assertEquals(a.trueRow(546.095), row)
      assert(placed(a, 546.095, held).forall(r => r >= 0 && r < rows))
  }

  test(
    "plain labels put a faint row middle price on every third row from the second and the last"
  ) {
    val rows = Chart.plainLabels(Axis(543.6, 552.4, 9))
    assertEquals(rows.size, 9)
    val priced = Map(1 -> "550.93", 4 -> "548.00", 7 -> "545.07", 8 -> "544.09")
    rows.zipWithIndex.foreach { (row, i) =>
      priced.get(i) match
        case Some(price) =>
          assertEquals(row, Screen.dim + " " * 8 + Chart.rjust(price, 8) + " " + Screen.reset)
          assertEquals(Chart.plain(row).indexOf(price), 16 - price.length)
        case None => assertEquals(row, " " * 17)
    }
    val btc = Chart.plainLabels(Axis(98000.0, 100000.0, 4)).map(Chart.plain)
    assertEquals(btc, Vector(" " * 17, " " * 8 + "   99250 ", " " * 17, " " * 8 + "   98250 "))
    val huge = Chart.plainLabels(Axis(1e9, 2e9, 2)).map(Chart.plain)
    assert(huge.forall(_.length == 17), huge)
    assertEquals(huge(1).trim, Trading.display(BigDecimal(1.25e9), 8))
  }

  test("fill marks land at their time column and true row and a past fill drops off the left") {
    val a = Axis(543.6, 552.4, 9)
    def fill(side: Side, time: Long, price: String, quantity: String): Fill =
      Fill("XMR", side, time, BigDecimal(price), BigDecimal(quantity))
    def marks(fills: Fill*): Vector[PlotMark] =
      Chart.fillMarks(fills.toVector, a, 1_000_000L, 2000L, 23, Style.colour)
    val buy = Style.colour.bid + Screen.bold
    val sell = Style.colour.ask + Screen.bold
    assertEquals(
      marks(fill(Side.Buy, 999_000L, "546.00", "0.2")),
      Vector(PlotMark(22, 6, '▲', buy))
    )
    assertEquals(
      marks(fill(Side.Sell, 1_001_000L, "549.00", "0.2")),
      Vector(PlotMark(22, 3, '▼', sell))
    )
    assertEquals(marks(fill(Side.Buy, 950_000L, "546.00", "0.2")), Vector.empty)
    assertEquals(marks(fill(Side.Buy, 955_000L, "546.00", "0.2")), Vector(PlotMark(0, 6, '▲', buy)))
    assertEquals(marks(fill(Side.Buy, 954_000L, "546.00", "0.2")), Vector.empty)
    assertEquals(
      Chart.fillMarks(
        Vector(fill(Side.Sell, 999_000L, "546.00", "1")),
        a,
        1_000_000L,
        2000L,
        23,
        Style.mono
      ),
      Vector(PlotMark(22, 6, '▼', Screen.bold))
    )
    // A mono buy carries no colour either.
    assertEquals(
      Chart.fillMarks(
        Vector(fill(Side.Buy, 999_000L, "546.00", "0.2")),
        a,
        1_000_000L,
        2000L,
        23,
        Style.mono
      ),
      Vector(PlotMark(22, 6, '▲', Screen.bold))
    )
    // 547.47 is on true row 5, where the rounded row would be 4.
    assertEquals(
      marks(fill(Side.Buy, 999_000L, "547.47", "0.2")),
      Vector(PlotMark(22, 5, '▲', buy))
    )
  }

  test("two fills in one cell show the larger dollar fill, the later on a tie") {
    val a = Axis(543.6, 552.4, 9)
    def fill(side: Side, time: Long, price: String, quantity: String): Fill =
      Fill("XMR", side, time, BigDecimal(price), BigDecimal(quantity))
    def marks(fills: Fill*): Vector[PlotMark] =
      Chart.fillMarks(fills.toVector, a, 1_000_000L, 2000L, 23, Style.colour)
    val sell = PlotMark(20, 6, '▼', Style.colour.ask + Screen.bold)
    val small = fill(Side.Buy, 995_000L, "546.00", "0.2")
    val large = fill(Side.Sell, 996_000L, "546.05", "1")
    assertEquals(marks(small, large), Vector(sell))
    assertEquals(marks(large, small), Vector(sell))
    val early = fill(Side.Buy, 995_000L, "546.00", "1")
    val late = fill(Side.Sell, 996_000L, "546.00", "1")
    assertEquals(marks(late, early), Vector(sell))
    assertEquals(marks(early, late), Vector(sell))
    val apart = marks(small, fill(Side.Sell, 999_000L, "549.00", "1"))
    assertEquals(apart.map(m => (m.col, m.row)).toSet, Set((20, 6), (22, 3)))
  }

  test("the time row ticks a round time with its label after it") {
    // Two seconds per cell picks thirty second ticks. 08:20:00 falls in cell 10.
    assertEquals(Chart.timeRow(29_980L, 2L, 23, ZoneOffset.UTC), " " * 10 + "|08:20:00" + " " * 4)
    // At cell 14 of 23 the label has its nine cells.
    assertEquals(Chart.timeRow(29_972L, 2L, 23, ZoneOffset.UTC), " " * 14 + "|08:20:00")
    // At cell 15 only the tick is drawn.
    assertEquals(
      Chart.timeRow(29_970L, 2L, 23, ZoneOffset.UTC),
      "|08:19:30" + " " * 6 + "|" + " " * 7
    )
  }

  test("the time row picks the smallest round interval that keeps fourteen cells") {
    // Four seconds per cell needs 56 seconds, so the ticks are whole minutes.
    assertEquals(
      Chart.timeRow(10L, 4L, 30, ZoneOffset.UTC),
      " " * 12 + "|00:01:00" + " " * 6 + "|" + "  "
    )
    // The clock is local: one round minute in a zone half an hour ahead.
    assertEquals(
      Chart.timeRow(10L, 4L, 30, ZoneOffset.ofHoursMinutes(9, 30)),
      " " * 12 + "|09:31:00" + " " * 6 + "|" + "  "
    )
  }

  test("the time row never ticks inside fourteen cells of the last tick") {
    // Two days per cell holds a midnight in every cell. Day ticks carry the date.
    assertEquals(
      Chart.timeRow(0L, 172_800L, 30, ZoneOffset.UTC),
      "|01-01" + " " * 8 + "|01-29" + " " * 8 + "|" + " "
    )
  }

  test("rjust pads on the left and cuts on the right") {
    assertEquals(Chart.rjust("ab", 4), "  ab")
    assertEquals(Chart.rjust("abcdef", 4), "abcd")
  }
