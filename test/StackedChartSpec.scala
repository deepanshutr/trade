package desk

import java.time.ZoneOffset

class StackedChartSpec extends munit.FunSuite:
  private def market(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): Vector[String] =
    // cols is the chart area here, so the terminal is that plus the outlook and its bar.
    Screen
      .renderMarket(
        s,
        now,
        zone,
        style,
        Layout
          .regions(cols + Layout.outlookWidth + 1, rows)
          .copy(priceRows = rows - 13 - 3 * State.refCoins.distinct.count(_ != s.coin)),
        AxisDecor.plain
      )
      .lines
  private val now = Golden.now
  private def draw(s: Snapshot, cols: Int = 100, rows: Int = 30): Vector[String] =
    market(s, cols, rows, now, ZoneOffset.UTC, Style.mono).map(Chart.plain)
  private def references(s: Snapshot): Vector[String] =
    State.refCoins.distinct.filterNot(_ == s.coin)
  private def mainEnd(s: Snapshot, rows: Int = 30): Int =
    1 + (rows - 13 - 3 * references(s).size)

  test("reference data changes the lower plots without changing the main plot") {
    val full = Golden.snapshot
    val empty = full.copy(refs = State.refCoins.map(_ -> Ref.empty).toMap)
    val withRefs = draw(full)
    val withoutRefs = draw(empty)
    val end = mainEnd(full)
    assertEquals(withRefs.take(end + 1), withoutRefs.take(end + 1))
    val panels = withRefs.slice(end + 1, end + 7)
    assert(panels.take(3).exists(_.drop(38).trim.nonEmpty))
    assert(panels.drop(3).exists(_.drop(38).trim.nonEmpty))
    assert(withoutRefs.slice(end + 1, end + 7).forall(_.drop(38).trim.isEmpty))
    assert(panels.head.startsWith("BTC"))
    assert(panels(3).startsWith("PAXG"))
  }

  test("each reference uses its own axis and the shared window") {
    val s = Golden.snapshot
    // The chart area of 100 columns has a plot of 62 cells, 124 dots.
    val count =
      Layout.columnsIn(Layout.regions(100 + Layout.outlookWidth + 1, 30).plot, ChartMode.Line)
    val (first, z) = Chart.window(s, count, now)
    for coin <- references(s) do
      val ref = s.refs(coin)
      val expected = Chart.columnsOf(ref.samples, Vector.empty, ref.candles, first, z, count)
      val actual = Chart.referenceColumns(ref, count, s, now)
      assertEquals(actual, expected)
      val axis = Chart.axis(actual, None, 3).get
      assert(actual.flatten.forall(c => c.low >= axis.lo && c.high <= axis.hi))
    val changed = s.copy(refs = s.refs.updated("BTC", Ref.empty))
    assertEquals(draw(s).slice(16, 19), draw(changed).slice(16, 19))
  }

  test("reference scales show their prices and ignore main price movement") {
    val sec = now / 1000
    val s = Snapshot
      .initial("XMR", now)
      .copy(
        samples = Vector(sec -> 100.0),
        refs = Map(
          "BTC" -> Ref(Vector((sec - 1) -> 1000.0, sec -> 2000.0), Map.empty),
          "PAXG" -> Ref.empty
        )
      )
    val frame = draw(s)
    assertEquals(
      frame.slice(13, 16).map(_.slice(30, 38)),
      Vector(" 3050.0 ", "   2000>", "  950.0 ")
    )
    val changedMain = draw(s.copy(samples = Vector(sec -> 9000.0)))
    assertEquals(frame.slice(13, 19), changedMain.slice(13, 19))
  }

  test("a main reference has no duplicate plot and keeps history after a switch") {
    val cs = Vector(Candle(now - 60_000L, 1, 2, 0.5, 1.5, 3))
    val s = Snapshot
      .initial("BTC", now)
      .copy(candles = Map("1m" -> cs), prompt = Some("XMR"))
    assertEquals(references(s), Vector("PAXG"))
    assert(!draw(s).drop(1).exists(_.startsWith("BTC")))
    val switched = State.reduce(s, Event.Key(Command.PromptSubmit), now)
    assertEquals(switched.refs("BTC").candles("1m"), cs)
    assert(draw(switched).drop(1).exists(_.startsWith("BTC")))
  }

  test("reference plots and main volume stay plain in colour mode") {
    val frame = market(Golden.snapshot, 100, 30, now, ZoneOffset.UTC, Style.colour)
    val plainPart = frame.slice(12, 19)
    assert(!plainPart.exists(_.contains(Screen.esc + "[3")))
    assert(!frame.exists(_.contains(Screen.esc + "[35m")))
  }

  test("the lower charts retain dimensions at different sizes and selected reference markets") {
    for coin <- Vector("XMR", "BTC", "PAXG") do
      val s = Golden.snapshot.copy(coin = coin)
      for (cols, rows) <- Vector((100, 30), (101, 31), (120, 40), (166, 50)) do
        val frame = draw(s, cols, rows)
        assertEquals(frame.size, rows - 10)
        assert(frame.forall(_.length == cols), (cols, rows, frame.map(_.length)))
  }
