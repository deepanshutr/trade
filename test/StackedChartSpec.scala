package desk

import java.time.ZoneOffset

class StackedChartSpec extends munit.FunSuite:
  private val now = Golden.now
  private def draw(s: Snapshot, cols: Int = 100, rows: Int = 30): Vector[String] =
    Screen.render(s, cols, rows, now, ZoneOffset.UTC, Style.mono).map(Chart.plain)
  private def references(s: Snapshot): Vector[String] =
    s.referenceCoins.distinct.filterNot(_ == s.coin)
  private def mainEnd(s: Snapshot, rows: Int = 30): Int =
    1 + Layout.priceRows(rows, references(s).size)

  test("the minimum frame gives eleven main rows and reserves four controls") {
    assertEquals(Layout.priceRows(30), 11)
    assertEquals(Layout.priceRows(30, 3), 8)
    assertEquals(Layout.priceRows(40), 21)
    val frame = draw(Golden.snapshot)
    assertEquals(frame.size, 30)
    assert(frame.forall(_.length == 100))
    assertEquals(frame.takeRight(4), Vector.fill(4)(" " * 100))
    assertEquals(frame.slice(20, 26).map(_.take(4)), Vector.fill(6)("BOOK"))
    assertEquals(frame.slice(20, 26).map(_.slice(50, 54)), Vector.fill(6)("TAPE"))
  }

  test("reference data changes the lower plots without changing the main plot") {
    val full = Golden.snapshot
    val empty = full.copy(refs = full.referenceCoins.map(_ -> Ref.empty).toMap)
    val withRefs = draw(full)
    val withoutRefs = draw(empty)
    val end = mainEnd(full)
    assertEquals(withRefs.take(end + 1), withoutRefs.take(end + 1))
    val panels = withRefs.slice(end + 1, end + 7)
    assert(panels.take(3).exists(_.slice(30, 91).trim.nonEmpty))
    assert(panels.drop(3).exists(_.slice(30, 91).trim.nonEmpty))
    assert(withoutRefs.slice(end + 1, end + 7).forall(_.slice(30, 91).trim.isEmpty))
    assert(panels.head.startsWith("bitcoin"))
    assert(panels(3).startsWith("gold"))
  }

  test("each reference uses its own axis and the shared window") {
    val s = Golden.snapshot
    val count = Layout.dotCols(100)
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

  test("a configured third reference receives events and keeps its plot") {
    val s = Snapshot.initial("XMR", now, Vector("BTC", "PAXG", "ETH", "ETH"))
    assertEquals(s.referenceCoins, Vector("BTC", "PAXG", "ETH"))
    val event = Event.BboUpdate("ETH", Level("2000", "1", 1), Level("2002", "1", 1), now)
    val filled = State.reduce(s, event, now)
    assertEquals(filled.refs("ETH").samples, Vector(now / 1000 -> 2001.0))
    val frame = draw(filled)
    assertEquals(frame.size, 30)
    assert(frame.forall(_.length == 100))
    assert(frame.exists(_.startsWith("ETH")))
    intercept[IllegalArgumentException] {
      Snapshot.initial("XMR", now, Vector("BTC", "PAXG", "ETH", "SOL"))
    }
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
    assertEquals(frame.slice(13, 16).map(_.drop(91)), Vector("   2050.0", "   1500.0", "    950.0"))
    val changedMain = draw(s.copy(samples = Vector(sec -> 9000.0)))
    assertEquals(frame.slice(13, 19), changedMain.slice(13, 19))
  }

  test("a main reference has no duplicate plot and keeps history after a switch") {
    val cs = Vector(Candle(now - 60_000L, 1, 2, 0.5, 1.5, 3))
    val s = Snapshot
      .initial("ETH", now, Vector("BTC", "PAXG", "ETH"))
      .copy(candles = Map("1m" -> cs), prompt = Some("XMR"))
    assertEquals(references(s), Vector("BTC", "PAXG"))
    assert(!draw(s).drop(1).exists(_.startsWith("ETH")))
    val switched = State.reduce(s, Event.Key(Command.PromptSubmit), now)
    assertEquals(switched.referenceCoins, s.referenceCoins)
    assertEquals(switched.refs("ETH").candles("1m"), cs)
    assert(draw(switched).drop(1).exists(_.startsWith("ETH")))
  }

  test("footer rows are fitted without broken escapes or wrapping") {
    val footer =
      Vector(Screen.bold + "a" * 130 + Screen.reset, "second", "third", "fourth", "fifth")
    val frame = Screen.render(Golden.snapshot, 100, 30, now, ZoneOffset.UTC, Style.colour, footer)
    assertEquals(frame.size, 30)
    assert(frame.map(Chart.plain).forall(_.length == 100))
    assertEquals(Chart.plain(frame(26)), "a" * 100)
    assert(frame(26).endsWith(Screen.reset))
    assertEquals(Chart.plain(frame.last).trim, "fourth")
  }

  test("reference plots and main volume stay plain in colour mode") {
    val frame = Screen.render(Golden.snapshot, 100, 30, now, ZoneOffset.UTC, Style.colour)
    val plainPart = frame.slice(12, 19)
    assert(!plainPart.exists(_.contains(Screen.esc + "[3")))
    assert(!frame.exists(_.contains(Screen.esc + "[35m")))
  }

  test("the lower charts retain dimensions at different sizes and without references") {
    for refs <- Vector(Vector.empty[String], Vector("BTC"), Vector("BTC", "PAXG", "ETH")) do
      val s = Golden.snapshot.copy(referenceCoins = refs)
      for (cols, rows) <- Vector((100, 30), (101, 31), (120, 40), (166, 50)) do
        val frame = draw(s, cols, rows)
        assertEquals(frame.size, rows)
        assert(frame.forall(_.length == cols), (cols, rows, frame.map(_.length)))
  }
