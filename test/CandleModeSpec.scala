package desk

import java.time.ZoneOffset

class CandleModeSpec extends munit.FunSuite:
  private def market(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): Vector[String] =
    // cols is the chart area here, so the terminal is that plus the outlook and its bar.
    val r = Layout.regions(cols + Layout.outlookWidth + 1, rows)
    val band = Vector.fill(r.band)(" " * (r.left + r.plot))
    Screen.renderMarket(s, now, zone, style, r, AxisDecor.plain, band).lines
  private val now = 1_700_000_000_000L
  private def press(s: Snapshot, command: Command): Snapshot =
    State.reduce(s, Event.Key(command), now)
  // The plot as the chart draws it, with no ticket row and no marks.
  private def candleRows(columns: Vector[Option[Column]], a: Axis, cells: Int) =
    Chart.decorated(columns, a, cells, ChartMode.Candles, None, Vector.empty)
  private def line(columns: Vector[Option[Column]], a: Axis) =
    Chart.decorated(columns, a, columns.size / 2, ChartMode.Line, None, Vector.empty)
  private def bits(ch: Char): Int = if ch == ' ' then 0 else ch.toInt - 0x2800
  private def dots(lines: Vector[String]): Set[(Int, Int)] =
    val masks = Vector(Vector(1, 8), Vector(2, 16), Vector(4, 32), Vector(64, 128))
    lines
      .map(Chart.plain)
      .zipWithIndex
      .flatMap { (line, row) =>
        line.zipWithIndex.flatMap { (ch, cell) =>
          for
            y <- 0 until 4
            x <- 0 until 2
            if (bits(ch) & masks(y)(x)) != 0
          yield (cell * 2 + x, row * 4 + y)
        }
      }
      .toSet

  test("candle mode starts at a minute and returning to lines restores the previous zoom") {
    val line = Snapshot.initial("XMR", now).copy(zoom = 2, offsetSec = 120)
    val candles = press(line, Command.ToggleCandles)
    assertEquals(candles.chartMode, ChartMode.Candles)
    assertEquals(State.zooms(candles.zoom), 60)
    assertEquals(candles.lineZoom, 2)
    assertEquals(candles.offsetSec, 120L)
    assertEquals(press(candles, Command.ZoomIn).zoom, State.candleMinZoom)
    val wider = press(press(candles, Command.ZoomOut), Command.ZoomOut)
    val back = press(wider, Command.ToggleCandles)
    assertEquals(back.chartMode, ChartMode.Line)
    assertEquals(back.zoom, 2)
    assertEquals(back.offsetSec, 120L)
  }

  test("a coarse line keeps its chosen duration when entering candles") {
    val line = Snapshot.initial("XMR", now).copy(zoom = State.zooms.indexOf(86400))
    val candles = press(line, Command.ToggleCandles)
    assertEquals(candles.zoom, line.zoom)
    assertEquals(press(candles, Command.ToggleCandles).zoom, line.zoom)
  }

  test("c switches the chart outside the market prompt and types inside it") {
    assertEquals(
      Keys.step(Vector.empty, 99, false, bindings = Bindings.defaults),
      (Vector.empty, Some(Command.ToggleCandles))
    )
    assertEquals(
      Keys.step(Vector.empty, 99, true, bindings = Bindings.defaults),
      (Vector.empty, Some(Command.PromptChar('c')))
    )
  }

  test("a market switch retains chart mode and the saved line zoom") {
    val candles = press(Snapshot.initial("XMR", now).copy(zoom = 3), Command.ToggleCandles)
    val switched = press(candles.copy(prompt = Some("BTC")), Command.PromptSubmit)
    assertEquals(switched.chartMode, ChartMode.Candles)
    assertEquals(switched.lineZoom, 3)
    assertEquals(switched.zoom, State.candleMinZoom)
  }

  test("the candle window and scrub step use the visible candle count") {
    val s = press(Snapshot.initial("XMR", now), Command.ToggleCandles)
    val count = Layout.columnsIn(Layout.regions(100, 30).plot, s.chartMode)
    assertEquals(count, 5)
    assertEquals(Layout.columnsIn(Layout.regions(100, 30).plot, ChartMode.Line), 46)
    val (first, z) = Chart.window(s, count, now)
    assertEquals(first + count - 1, Math.floorDiv(now / 1000, 60L))
    assertEquals(z, 60)
    assertEquals(press(s, Command.ScrubBack).offsetSec, 60L)
    val wide = s.copy(cols = 120)
    assertEquals(Layout.columnsIn(Layout.regions(120, 40).plot, ChartMode.Candles), 10)
    assertEquals(press(wide, Command.ScrubBack).offsetSec, 120L)
  }

  test("candle columns use stored trade candles rather than fine midpoint samples") {
    val s = press(Snapshot.initial("XMR", 119_000L), Command.ToggleCandles).copy(
      samples = Vector(0L -> 999.0, 59L -> 1001.0),
      candles = Map("1m" -> Vector(Candle(0L, 10, 13, 8, 12, 7)))
    )
    assertEquals(Chart.columns(s, 2, 119_000L), Vector(Some(Column(10, 8, 13, 12, 7)), None))
    assertEquals(Chart.columns(s.copy(candles = Map.empty), 2, 119_000L), Vector(None, None))
  }

  test("rising bodies are hollow and falling bodies are filled with separate wicks") {
    val rising = Some(Column(2, 0, 6, 4, 1))
    val falling = Some(Column(4, 0, 6, 2, 1))
    val rows = candleRows(Vector(rising, falling), Axis(0, 6, 7), 8)
    assertEquals(
      rows,
      Vector(
        " │   │  ",
        " │   │  ",
        "┌─┐ ███ ",
        "│ │ ███ ",
        "└─┘ ███ ",
        " │   │  ",
        " │   │  "
      )
    )
    assert(!rows.exists(_.contains("#")))
    assert(!rows.exists(_.exists(c => c >= 0x2800 && c <= 0x28ff)))
  }

  test("flat and collapsed bodies remain distinct and absent candles stay blank") {
    val flat = Some(Column(3, 0, 6, 3, 1))
    val rising = Some(Column(3, 0, 6, 3.1, 1))
    val falling = Some(Column(3.1, 0, 6, 3, 1))
    val rows = candleRows(Vector(flat, rising, falling, None), Axis(0, 6, 3), 16)
    assertEquals(rows(1), "───  □  ███     ")
    assert(rows.forall(_.drop(12) == " " * 4))
    assert(rows.head.startsWith(" │   │   │  "))
    assert(rows.last.startsWith(" │   │   │  "))
  }

  test("body height follows open and close while wick height follows high and low") {
    val axis = Axis(0, 10, 11)
    val first = candleRows(Vector(Some(Column(3, 1, 9, 6, 1))), axis, 4)
    val changedBody = candleRows(Vector(Some(Column(2, 1, 9, 7, 1))), axis, 4)
    assertEquals(first(1), " │  ")
    assertEquals(first(9), " │  ")
    assertEquals(first(4), "┌─┐ ")
    assertEquals(first(7), "└─┘ ")
    assertEquals(changedBody(3), "┌─┐ ")
    assertEquals(changedBody(8), "└─┘ ")
    assertEquals(changedBody(1), first(1))
    assertEquals(changedBody(9), first(9))
  }

  test("a segment includes both ends and connects both rising and falling slopes") {
    val rising = Canvas(3, 2)
    rising.segment(0, 7, 5, 0)
    val actual = dots(rising.lines)
    assert(actual.contains((0, 7)) && actual.contains((5, 0)))
    assertEquals(actual.map(_._1), (0 to 5).toSet)
    assertEquals(actual.map(_._2), (0 to 7).toSet)
    val reverse = Canvas(3, 2)
    reverse.segment(5, 0, 0, 7)
    assertEquals(dots(reverse.lines), actual)
    val falling = Canvas(3, 2)
    falling.segment(0, 0, 5, 7)
    assertEquals(dots(falling.lines).map((x, y) => (x, 7 - y)), actual)
    val horizontal = Canvas(3, 2)
    horizontal.segment(0, 3, 5, 3)
    assertEquals(dots(horizontal.lines), (0 to 5).map(_ -> 3).toSet)
  }

  test("a line connects adjacent closing prices and leaves a missing column unbridged") {
    val columns = Vector(
      Some(Column(2, 0, 8, 2, 0)),
      None,
      Some(Column(6, 0, 8, 6, 0)),
      Some(Column(4, 0, 8, 4, 0))
    )
    val axis = Axis(0, 8, 3)
    val rows = line(columns, axis)
    val ink = dots(rows)
    assertEquals(ink.filter(_._1 == 0), Set((0, axis.dot(2))))
    assert(!ink.exists(_._1 == 1))
    assert(ink.contains((2, axis.dot(6))) && ink.contains((3, axis.dot(4))))
    assert(rows.exists(_.contains(Screen.bold)))
    val changedWicks = columns.map(_.map(_.copy(low = 1, high = 7)))
    assertEquals(line(changedWicks, axis), rows)
  }

  test("candle volume is centred beneath each candle and keeps its total") {
    val columns = Vector(Some(Column(1, 1, 1, 1, 7)), None, Some(Column(1, 1, 1, 1, 14)))
    val (rows, maximum) = Chart.volumeRows(columns, Style.mono, ChartMode.Candles, 12)
    assertEquals(maximum, 14.0)
    assertEquals(rows.size, 1)
    assertEquals(rows.head.length, 12)
    assertEquals(rows.head.zipWithIndex.collect { case (ch, i) if ch != ' ' => i }, Vector(1, 9))
    assertEquals(dots(rows).map(_._1), Set(2, 3, 18, 19))
  }

  test("candle ticks sit on the first cell of the candle that opens at the round time") {
    // One minute candles of four cells give fifteen seconds a cell and five minute ticks, so the
    // ticks fall on candles 0, 5, 10 and 15 at cells 0, 20, 40 and 60.
    val row = Chart.timeRow(0L, 15L, 61, ZoneOffset.UTC)
    assertEquals(
      row,
      "|00:00:00" + " " * 11 + "|00:05:00" + " " * 11 + "|00:10:00" + " " * 11 + "|"
    )
  }

  test("both modes preserve all screen rows with no magenta") {
    for
      mode <- Vector(ChartMode.Line, ChartMode.Candles)
      (cols, rows) <- Vector((100, 30), (101, 31), (120, 40))
    do
      val s = Golden.snapshot.copy(
        chartMode = mode,
        zoom = if mode == ChartMode.Candles then State.candleMinZoom else 0
      )
      val frame = market(s, cols, rows, Golden.now, ZoneOffset.UTC, Style.colour)
      assertEquals(frame.size, rows - 12)
      assert(frame.map(Chart.plain).forall(_.length == cols))
      assert(!frame.exists(_.contains(Screen.esc + "[35m")))
      val axis = frame(frame.size - 1)
      assert(axis.contains(if mode == ChartMode.Candles then "1m/candle" else "2s/col"))
      if mode == ChartMode.Candles then
        val count = Layout.regions(cols + Layout.outlookWidth + 1, rows).priceRows
        val main = frame.slice(1, count + 1).map(Chart.plain).map(_.drop(38))
        assert(!main.exists(_.exists(c => c >= 0x2800 && c <= 0x28ff)))
  }

  test("candle frames draw the last two candles in the main plot, volume and ticks") {
    val at = 119_000L
    val candles = Vector(Candle(0L, 2, 6, 0, 4, 7), Candle(60_000L, 4, 6, 0, 2, 14))
    val s = Snapshot
      .initial("XMR", at)
      .copy(
        chartMode = ChartMode.Candles,
        zoom = State.zooms.indexOf(60),
        candles = Map("1m" -> candles)
      )
    // cols is the chart area. The main plot starts at column 38.
    for cols <- Vector(100, 101, 120) do
      val cells = cols - 38
      val count = Layout.columnsIn(cells, ChartMode.Candles)
      val start = (count - 2) * Layout.candleW
      val end = start + 8
      val frame = market(s, cols, 30, at, ZoneOffset.UTC, Style.colour).map(Chart.plain)
      val main = frame.slice(1, 14).map(_.drop(38))
      assert(main.exists(_.slice(start, end) == "┌─┐ ███ "))
      assert(main.exists(_.slice(start, end) == "│ │ ███ "))
      assert(main.exists(_.slice(start, end) == "└─┘ ███ "))
      assert(main.forall(row => row.take(start).trim.isEmpty && row.drop(end).trim.isEmpty))
      val volume = frame(14).drop(38)
      assertEquals(
        volume.zipWithIndex.collect { case (ch, i) if ch != ' ' => i },
        Vector(start + 1, start + 5)
      )
      // Five minute ticks at fifteen seconds a cell, the last labelled one on the candle at 00:00.
      val clock = frame(17).drop(38)
      assertEquals(clock.length, cells)
      assertEquals(clock.slice(start, start + 9), "|00:00:00")
      assertEquals(
        clock.indices.filter(clock(_) == '|').toVector,
        (start to 0 by -20).reverse.toVector
      )
  }
