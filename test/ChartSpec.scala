package desk

import java.time.ZoneOffset

class ChartSpec extends munit.FunSuite:
  private def br(bits: Int): String = (0x2800 + bits).toChar.toString
  private def lvl(px: String, sz: String = "1"): Level = Level(px, sz, 1)
  private def col(v: Double, vol: Double = 0.0): Option[Column] = Some(Column(v, v, v, v, vol))
  private val t0 = 1_700_000_000_000L
  private def s0: Snapshot = Snapshot.initial("XMR", t0)

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
    val s = s0.copy(samples = Vector((999L, 1.0)), candles = Vector(Candle(0L, 2, 2, 2, 2, 1)))
    assertEquals(Chart.columns(s, 2, 1_000_000L), Vector(col(1.0), col(1.0)))
    assertEquals(
      Chart.columns(s.copy(zoom = 5), 2, 119_000L),
      Vector(Some(Column(2.0, 2.0, 2.0, 2.0, 1.0)), None)
    )
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

  test("the line joins consecutive columns and bolds the newest cell") {
    val a = Axis(1.0, 3.0, 1)
    val lines = Chart.line(Vector(col(1.0), col(3.0)), a)
    assertEquals(lines, Vector(Screen.bold + br(0xf8) + Screen.reset))
    assertEquals(Chart.line(Vector(None, None), a), Vector(" "))
  }

  test("ladder rows put bids from the label side and asks from the chart side") {
    val book = Book(Vector(lvl("100", "1")), Vector(lvl("110", "100"), lvl("107.5", "10")), 1L)
    val a = Axis(100.0, 110.0, 5)
    val rows = Chart.ladderRows(Some(book), Some(a), 5, Some(2))
    assertEquals(rows.size, 5)
    val plain = rows.map(Chart.plain)
    assert(plain.forall(_.length == 30), plain)
    assertEquals(plain(0), "  110.00 " + "#" * 21)
    assertEquals(plain(1), "  107.50 " + " " * 15 + "######")
    assertEquals(plain(2), "  105.00>" + " " * 21)
    assertEquals(plain(3), "  102.50 " + " " * 21)
    assertEquals(plain(4), "  100.00 " + "#" + " " * 20)
    assert(rows(0).startsWith(Screen.bold) && rows(4).startsWith(Screen.bold))
    assert(!rows(1).contains(Screen.bold))
    assert(rows(2).contains(Screen.bold + "  105.00>" + Screen.reset))
  }

  test("ladder bars use the log rule with the clamp mark at the outer end") {
    val bids = (1 to 20).toVector.map(i => Level((200 - i).toString, (i * i).toString, 1))
    val book = Book(bids, Vector.empty, 1L)
    val a = Axis(180.0, 199.0, 20)
    val plain = Chart.ladderRows(Some(book), Some(a), 20, None).map(Chart.plain)
    assertEquals(plain(19).drop(9), "#" * 20 + ">")
    assertEquals(plain(0).drop(9).count(_ == '#'), 1)
    assert(plain.forall(_.length == 30))
    assertEquals(Chart.ladderRows(None, None, 2, None), Vector(" " * 30, " " * 30))
  }

  test("ladder bars scale by depth when the reference ties the lower median") {
    val book = Book(Vector(lvl("100", "1"), lvl("99", "5"), lvl("98", "5")), Vector.empty, 1L)
    val a = Axis(98.0, 100.0, 3)
    val plain = Chart.ladderRows(Some(book), Some(a), 3, None).map(Chart.plain)
    assertEquals(plain(0).drop(9), "######" + " " * 15)
    assertEquals(plain(1).drop(9), "#" * 21)
    assertEquals(plain(2).drop(9), "#" * 21)
  }

  test("scale rows label the top middle and bottom and print the mid bold") {
    val a = Axis(98.35, 112.65, 14)
    val rows = Chart.scaleRows(Some(a), 14, Some(7), Some("105.5"))
    assertEquals(rows(0), "   112.65")
    assertEquals(rows(7), Screen.bold + "   <105.5" + Screen.reset)
    assertEquals(rows(13), "    98.35")
    assertEquals(rows(1), " " * 9)
    val lower = Chart.scaleRows(Some(a), 14, Some(8), Some("105.5"))
    assertEquals(lower(7), "   104.95")
    assertEquals(lower(8), Screen.bold + "   <105.5" + Screen.reset)
    assertEquals(Chart.scaleRows(None, 2, None, None), Vector(" " * 9, " " * 9))
  }

  test("the middle scale label is the price of the middle ladder row") {
    val a = Axis(98.35, 112.65, 14)
    val book = Book(Vector(lvl("100")), Vector(lvl("110")), 1L)
    val ladder = Chart.ladderRows(Some(book), Some(a), 14, None).map(Chart.plain)
    val scale = Chart.scaleRows(Some(a), 14, None, None)
    assertEquals(scale(7).trim, ladder(7).take(Layout.labelW).trim)
    assertEquals(scale(0).trim, ladder(0).take(Layout.labelW).trim)
    assertEquals(scale(13).trim, ladder(13).take(Layout.labelW).trim)
  }

  test("volume rows scale to the largest column with at least one dot") {
    val cs = Vector(col(1.0, 0.0), col(1.0, 7.0), col(1.0, 1.0), col(1.0, 14.0))
    val (lines, vmax) = Chart.volumeRows(cs)
    assertEquals(vmax, 14.0)
    assertEquals(lines, Vector(" " + br(0xb0), br(0xb8) + br(0xf8)))
    assertEquals(Chart.volumeRows(Vector(None, None)), (Vector(" ", " "), 0.0))
  }

  test("the axis row prints three clocks at the first middle and last column") {
    val fine = Chart.axisRow(0L, 1, 122, ZoneOffset.UTC)
    assertEquals(fine.length, 61)
    assertEquals(fine.take(8), "00:00:00")
    assertEquals(fine.slice(26, 34), "00:01:01")
    assertEquals(fine.drop(53), "00:02:01")
    val coarse = Chart.axisRow(0L, 60, 122, ZoneOffset.UTC)
    assertEquals(coarse.take(5), "00:00")
    assertEquals(coarse.slice(28, 33), "01:01")
    assertEquals(coarse.drop(56), "02:01")
  }

  test("rjust pads on the left and cuts on the right") {
    assertEquals(Chart.rjust("ab", 4), "  ab")
    assertEquals(Chart.rjust("abcdef", 4), "abcd")
  }
