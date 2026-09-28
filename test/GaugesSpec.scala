package desk

class GaugesSpec extends munit.FunSuite:
  private def lvl(px: String, sz: String): Level = Level(px, sz, 1)
  private def book(bidSz: String, askSz: String, t: Long = 1L): Book =
    Book(Vector(lvl("100", bidSz)), Vector(lvl("101", askSz)), t)
  private val now = 1_700_000_000_000L
  private def trade(agoMs: Long, side: Side, px: String, sz: String): Trade =
    Trade(now - agoMs, side, px, sz)
  private def s0: Snapshot = Snapshot.initial("XMR", now)
  private val FULL = (0x28ff).toChar.toString
  private def tr(v: Option[Double]): String = Gauges.track(v, 13, "")
  test("imbalance keeps the part one formulas") {
    val (q, d) = Gauges.imbalance(book("1", "3"))
    assertEqualsDouble(q, -0.5, 1e-9)
    assertEqualsDouble(d, -0.5, 1e-9)
    assertEquals(Gauges.weights.size, 10)
    assertEqualsDouble(Gauges.weights(1), 0.7, 1e-9)
  }

  test("flow over a window of trades and undefined when the window is empty") {
    val lens = Vector(10L)
    assertEquals(Gauges.tapeStats(Vector.empty, now, lens).head, (Option.empty[Double], 0))
    val ts = Vector(trade(1L, Side.Buy, "1", "3"), trade(2L, Side.Sell, "1", "1"))
    assertEquals(Gauges.tapeStats(ts, now, lens).head, (Some(0.5), 2))
    val zero = Vector(trade(1L, Side.Buy, "1", "0"))
    assertEquals(Gauges.tapeStats(zero, now, lens).head, (Option.empty[Double], 1))
  }

  test("pearson of returns and its edge cases") {
    assertEquals(Gauges.pearson(Vector(1, 2, 3), Vector(2, 4, 6)).map(_.round), Some(1L))
    assertEqualsDouble(Gauges.pearson(Vector(1, 2, 3), Vector(3, 2, 1)).get, -1.0, 1e-9)
    assertEquals(Gauges.pearson(Vector(1, 1, 1), Vector(1, 2, 3)), None)
    assertEquals(Gauges.pearson(Vector(1, 2), Vector(1, 2)), None)
  }

  test("a track marks with a braille cell in the given style") {
    assertEquals(Gauges.track(Some(0.5), 12, ""), "............|....." + FULL + "......")
    assertEquals(
      Gauges.track(Some(0.5), 12, "S"),
      "............|....." + "S" + FULL + Screen.reset + "......"
    )
    assertEquals(Gauges.track(None, 12, "S"), "............|............")
    assert(!Gauges.track(Some(1.0), 4, "").contains("#"))
  }

  test("book rows carry the depth and quote readings") {
    val sec = now / 1000
    val s = s0.copy(
      book = Some(book("1", "1")),
      books = Vector((sec - 20, book("1", "3")), (sec - 5, book("1", "1"))),
      since = now - 15_000
    )
    val row = Gauges.bookRow(s, 0, now, 13, Style.mono)
    assertEquals(row, s"BOOK  now ${tr(Some(0.0))} D+0.00 Q+0.00 ")
    assertEquals(
      Gauges.bookRow(s, 2, now, 13, Style.mono),
      s"BOOK ${Screen.dim} 30s${Screen.reset} ${tr(Some(-0.25))} D-0.25 Q-0.25 "
    )
    val coloured = Gauges.bookRow(s, 0, now, 13, Style.colour)
    assert(
      coloured.contains(Style.colour.bid + FULL + Screen.reset) || coloured.contains(
        Style.colour.ask + FULL + Screen.reset
      ),
      coloured
    )
  }

  test("a lens adds the current book once when the newest kept book is an older one") {
    val sec = now / 1000
    val s = s0.copy(
      book = Some(book("1", "1")),
      books = Vector((sec - 5, book("3", "1"))),
      since = now - 100_000
    )
    assertEquals(
      Gauges.bookRow(s, 1, now, 13, Style.mono),
      s"BOOK  10s ${tr(Some(0.25))} D+0.25 Q+0.25 "
    )
  }

  test("a lens label is bright, not dim, exactly when its window equals the time since since") {
    val s = s0.copy(book = Some(book("1", "1")), since = now - 10_000)
    val line = Gauges.bookRow(s, 1, now, 13, Style.mono)
    assert(!line.contains(Screen.dim), line)
  }

  test("tape rows carry the flow and trade count") {
    val s = s0.copy(
      tape = Vector(trade(5_000L, Side.Buy, "100", "1"), trade(30_000L, Side.Sell, "100", "3")),
      since = now - 1_000_000L
    )
    assertEquals(
      Gauges.compactTapeRow(s, 0, now, 13, Style.mono),
      s"TAPE 10s ${tr(Some(1.0))} F+1.00 n1"
    )
    assertEquals(
      Gauges.compactTapeRow(s, 1, now, 13, Style.mono),
      s"TAPE  1m ${tr(Some(-0.5))} F-0.50 n2"
    )
    val coloured = Gauges.compactTapeRow(s, 1, now, 13, Style.colour)
    assert(coloured.contains(Style.colour.ask + FULL + Screen.reset), coloured)
  }

  test("the tape window edge is exclusive: a trade exactly secs old is left out") {
    val s = s0.copy(tape = Vector(trade(10_000L, Side.Buy, "100", "1")), since = now - 1_000_000L)
    assertEquals(
      Gauges.compactTapeRow(s, 0, now, 13, Style.mono),
      s"TAPE 10s ${tr(None)} F   -- n0"
    )
  }
