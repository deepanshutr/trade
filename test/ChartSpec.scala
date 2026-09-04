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

  test("a style turns its colours off for greyscale and keeps the references dim") {
    assertEquals(Style.colour.bid, Screen.esc + "[32m")
    assertEquals(Style.colour.ask, Screen.esc + "[31m")
    assertEquals(Style.colour.btc, Screen.esc + "[38;5;208m")
    assertEquals(Style.colour.gold, Screen.esc + "[38;5;178m")
    assertEquals(Style.mono.bid, "")
    assertEquals(Style.mono.btc, Screen.dim)
    assertEquals(Style.mono.gold, Screen.dim)
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

  test("columns pick the series of the zoom and reference columns take the last value") {
    val c5 = Candle(0L, 2, 3, 1, 2.5, 1)
    val s = s0.copy(
      candles = Map("5m" -> Vector(c5), "1m" -> Vector(Candle(0L, 9, 9, 9, 9, 9))),
      zoom = State.zooms.indexOf(300)
    )
    assertEquals(Chart.columns(s, 2, 599_000L), Vector(Some(Column(2.0, 1.0, 3.0, 2.5, 1.0)), None))
    val r = Ref(Vector((999L, 1.0)), Map.empty)
    assertEquals(Chart.refColumns(r, 2, s0, 1_000_000L), Vector(Some(1.0), Some(1.0)))
    assertEquals(
      Chart.refColumns(Ref(Vector.empty, Map("5m" -> Vector(c5))), 2, s, 599_000L),
      Vector(Some(2.5), None)
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
    val lines = Chart.line(Vector(col(1.0), col(3.0)), a, Vector.empty)
    assertEquals(lines, Vector(Screen.bold + br(0xf8) + Screen.reset))
    assertEquals(Chart.line(Vector(None, None), a, Vector.empty), Vector(" "))
  }

  test("a reference line normalizes to the first visible main value and keeps its pattern") {
    val a = Axis(90.0, 120.0, 1)
    val main = Vector(col(100.0), col(100.0), col(100.0), col(100.0))
    val ref = Vector(Some(50.0), Some(55.0), Some(60.0), None)
    val lines = Chart.line(main, a, Vector(RefLine(ref, Chart.dash, Style.colour.btc)))
    val cells = Chart.plain(lines.head)
    assertEquals(cells.length, 2)
    assertEquals(a.dot(110.0), 1)
    assertEquals(a.dot(100.0), 2)
    assertEquals(lines, Vector(br(0x34) + Screen.bold + br(0x24) + Screen.reset))
    assertEquals(Chart.dash(0) && Chart.dash(1) && !Chart.dash(2) && !Chart.dash(3), true)
    assertEquals(
      (0 to 6).map(Chart.dotted).toVector,
      Vector(true, false, false, true, false, false, true)
    )
    val alone = Chart.line(main, a, Vector.empty)
    assert(Chart.plain(alone.head) != cells)
  }

  test("the main line clears the style of every cell it inks, the reference keeps the rest") {
    val a = Axis(90.0, 120.0, 1)
    val main = Vector(col(100.0), col(100.0), None, None, col(100.0), col(100.0))
    val ref = RefLine(Vector.fill(6)(Some(50.0)), _ => true, "S")
    val lines = Chart.line(main, a, Vector(ref))
    assertEquals(
      lines,
      Vector(
        br(0x24) + "S" + br(0x24) + Screen.reset + Screen.bold + br(0x24) + Screen.reset
      )
    )
  }

  test("a reference value outside the axis draws nothing") {
    val a = Axis(90.0, 120.0, 1)
    val main = Vector(col(100.0), None, None, None)
    def drawn(v: Double): Vector[String] =
      Chart.line(main, a, Vector(RefLine(Vector(Some(50.0), None, Some(v), None), _ => true, "S")))
    val onlyMain = Screen.bold + br(0x04) + Screen.reset + " "
    assertEquals(drawn(70.0), Vector(onlyMain))
    assertEquals(drawn(20.0), Vector(onlyMain))
    assertEquals(
      drawn(55.0),
      Vector(Screen.bold + br(0x04) + Screen.reset + "S" + br(0x02) + Screen.reset)
    )
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

  test("ladder rows put bids from the label side and asks from the chart side") {
    val book = Book(Vector(lvl("100", "1")), Vector(lvl("110", "100"), lvl("107.5", "10")), 1L)
    val a = Axis(100.0, 110.0, 5)
    val rows = Chart.ladderRows(Some(book), Map.empty, Some(a), 5, Some(2), Style.mono)
    assertEquals(rows.size, 5)
    val plain = rows.map(Chart.plain)
    assert(plain.forall(_.length == 30), plain)
    assertEquals(plain(0), "  110.00 " + FULL * 21)
    assertEquals(plain(1), "  107.50 " + " " * 15 + FULL * 6)
    assertEquals(plain(2), "  105.00>" + " " * 21)
    assertEquals(plain(3), "  102.50 " + " " * 21)
    assertEquals(plain(4), "  100.00 " + FULL + " " * 20)
    assert(rows(0).startsWith(Screen.bold) && rows(4).startsWith(Screen.bold))
    assert(!rows(1).contains(Screen.bold))
    assert(rows(2).contains(Screen.bold + "  105.00>" + Screen.reset))
  }

  test("ladder bars use the log rule with the clamp mark at the outer end") {
    val bids = (1 to 20).toVector.map(i => Level((200 - i).toString, (i * i).toString, 1))
    val book = Book(bids, Vector.empty, 1L)
    val a = Axis(180.0, 199.0, 20)
    val plain =
      Chart.ladderRows(Some(book), Map.empty, Some(a), 20, None, Style.mono).map(Chart.plain)
    assertEquals(plain(19).drop(9), FULL * 20 + ">")
    assertEquals(plain(0).drop(9), LHALF + " " * 20)
    assert(plain.forall(_.length == 30))
    assertEquals(
      Chart.ladderRows(None, Map.empty, None, 2, None, Style.mono),
      Vector(" " * 30, " " * 30)
    )
  }

  test("ladder bars scale by depth when the reference ties the lower median") {
    val book = Book(Vector(lvl("100", "1"), lvl("99", "5"), lvl("98", "5")), Vector.empty, 1L)
    val a = Axis(98.0, 100.0, 3)
    val plain =
      Chart.ladderRows(Some(book), Map.empty, Some(a), 3, None, Style.mono).map(Chart.plain)
    assertEquals(plain(0).drop(9), FULL * 5 + LHALF + " " * 15)
    assertEquals(plain(1).drop(9), FULL * 21)
    assertEquals(plain(2).drop(9), FULL * 21)
  }

  test("ladder rows draw far depth from the coarse books in colour") {
    val full = Book(Vector(lvl("530.5", "1")), Vector(lvl("530.7", "1")), 1L)
    val sf2 = Book(Vector.empty, Vector(lvl("550", "60")), 1L)
    val a = Axis(500.0, 560.0, 7)
    val rows = Chart.ladderRows(Some(full), Map(2 -> sf2), Some(a), 7, None, Style.colour)
    val plain = rows.map(Chart.plain)
    assert(plain.forall(_.length == 30), plain)
    assert(plain(1).drop(9).trim.nonEmpty && plain(2).drop(9).trim.nonEmpty, plain)
    assert(rows(1).contains(Style.colour.ask), rows(1))
    assert(
      rows(3).contains(Style.colour.bid) || rows(4).contains(Style.colour.bid),
      rows.mkString("\n")
    )
    assert(!plain.exists(_.contains("#")))
    assert(
      !Chart
        .ladderRows(Some(full), Map(2 -> sf2), Some(a), 7, None, Style.mono)
        .exists(_.contains(Screen.esc + "[3"))
    )
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
    val ladder =
      Chart.ladderRows(Some(book), Map.empty, Some(a), 14, None, Style.mono).map(Chart.plain)
    val scale = Chart.scaleRows(Some(a), 14, None, None)
    assertEquals(scale(7).trim, ladder(7).take(Layout.labelW).trim)
    assertEquals(scale(0).trim, ladder(0).take(Layout.labelW).trim)
    assertEquals(scale(13).trim, ladder(13).take(Layout.labelW).trim)
  }

  test("volume rows scale to the largest column with at least one dot") {
    val cs = Vector(col(1.0, 0.0), col(1.0, 7.0), col(1.0, 1.0), col(1.0, 14.0))
    val (lines, vmax) = Chart.volumeRows(cs, Style.mono)
    assertEquals(vmax, 14.0)
    assertEquals(lines, Vector(" " + br(0xb0), br(0xb8) + br(0xf8)))
    assertEquals(Chart.volumeRows(Vector(None, None), Style.mono), (Vector(" ", " "), 0.0))
  }

  test("volume dots colour by column direction") {
    val up = Some(Column(1.0, 1.0, 2.0, 2.0, 5.0))
    val down = Some(Column(2.0, 1.0, 2.0, 1.0, 5.0))
    val (lines, _) = Chart.volumeRows(Vector(up, up, down, down), Style.colour)
    assert(lines(1).contains(Style.colour.bid) && lines(1).contains(Style.colour.ask), lines)
    val (mono, _) = Chart.volumeRows(Vector(up, up, down, down), Style.mono)
    assert(!mono(1).contains(Screen.esc))
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

  test("the axis row picks its clock pattern by zoom") {
    assertEquals(Chart.axisRow(0L, 3600, 122, ZoneOffset.UTC).take(11), "01-01 00:00")
    assertEquals(Chart.axisRow(0L, 86400, 122, ZoneOffset.UTC).take(10), "1970-01-01")
    assertEquals(Chart.axisRow(0L, 60, 122, ZoneOffset.UTC).take(5), "00:00")
    assertEquals(Chart.axisRow(0L, 1, 122, ZoneOffset.UTC).take(8), "00:00:00")
  }

  test("rjust pads on the left and cuts on the right") {
    assertEquals(Chart.rjust("ab", 4), "  ab")
    assertEquals(Chart.rjust("abcdef", 4), "abcd")
  }
