package desk

class GaugesSpec extends munit.FunSuite:
  private def lvl(px: String, sz: String): Level = Level(px, sz, 1)
  private def book(bidSz: String, askSz: String, t: Long = 1L): Book =
    Book(Vector(lvl("100", bidSz)), Vector(lvl("101", askSz)), t)
  private val now = 1_700_000_000_000L
  private def trade(agoMs: Long, side: Side, px: String, sz: String): Trade =
    Trade(now - agoMs, side, px, sz)
  private def s0: Snapshot = Snapshot.initial("XMR", now)

  test("the threshold is the 90th percentile of dollar size over fifteen minutes") {
    val ts = (1 to 10).toVector.map(i => trade(i * 1000L, Side.Buy, "1", i.toString))
    assertEquals(Gauges.threshold(ts, now), Some(9.0))
    val old = trade(900_001L, Side.Buy, "1", "1000")
    assertEquals(Gauges.threshold(ts :+ old, now), Some(9.0))
    assertEquals(Gauges.threshold(Vector(old), now), None)
    assertEquals(Gauges.threshold(Vector(trade(1L, Side.Sell, "2", "3")), now), Some(6.0))
  }

  test("the big imbalance uses only levels at or above the threshold and counts them") {
    val b =
      Book(Vector(lvl("100", "1"), lvl("99", "10")), Vector(lvl("101", "1"), lvl("102", "5")), 1L)
    val d = Gauges.imbalanceBig(b, 500.0).get
    assertEqualsDouble(d, (7.0 - 3.5) / 10.5, 1e-9)
    assertEquals(Gauges.bigCounts(b, 500.0), (1, 1))
    assertEquals(Gauges.imbalanceBig(b, 5000.0), None)
    assertEquals(Gauges.bigCounts(b, 5000.0), (0, 0))
    assertEquals(Gauges.imbalanceBig(b, 990.0), Some(1.0))
  }

  test("imbalance keeps the part one formulas") {
    val (q, d) = Gauges.imbalance(book("1", "3"))
    assertEqualsDouble(q, -0.5, 1e-9)
    assertEqualsDouble(d, -0.5, 1e-9)
    assertEquals(Gauges.weights.size, 10)
    assertEqualsDouble(Gauges.weights(1), 0.7, 1e-9)
  }

  test("flow over a window of trades and undefined when the window is empty") {
    val lens = Vector(10L)
    assertEquals(Gauges.tapeStats(Vector.empty, now, None, lens).head._1, (Option.empty[Double], 0))
    val ts = Vector(trade(1L, Side.Buy, "1", "3"), trade(2L, Side.Sell, "1", "1"))
    assertEquals(Gauges.tapeStats(ts, now, None, lens).head._1, (Some(0.5), 2))
    assertEquals(Gauges.tapeStats(ts, now, None, lens).head._2, (Option.empty[Double], 0))
    val zero = Vector(trade(1L, Side.Buy, "1", "0"))
    assertEquals(Gauges.tapeStats(zero, now, None, lens).head._1, (Option.empty[Double], 1))
  }

  test("one pass over the tape reads every window and its big money side") {
    val ts = Vector(
      trade(5_000L, Side.Buy, "100", "1"),
      trade(30_000L, Side.Sell, "100", "3"),
      trade(200_000L, Side.Buy, "100", "10")
    )
    val stats = Gauges.tapeStats(ts, now, Some(300.0), Vector(10L, 60L, 300L))
    assertEquals(stats.map(_._1._2), Vector(1, 2, 3))
    assertEquals(stats.map(_._2._2), Vector(0, 1, 2))
    assertEquals(stats(0)._1._1, Some(1.0))
    assertEquals(stats(1)._1._1, Some(-0.5))
    assertEqualsDouble(stats(2)._1._1.get, 8.0 / 14, 1e-9)
    assertEqualsDouble(stats(2)._2._1.get, 7.0 / 13, 1e-9)
  }

  test("a track puts the marker by the half width and none when undefined") {
    assertEquals(Gauges.track(Some(0.5), 12), "............|.....#......")
    assertEquals(Gauges.track(None, 12), "............|............")
    assertEquals(Gauges.track(Some(-1.0), 4), "#...|....")
    assertEquals(Gauges.track(Some(3.0), 4), "....|...#")
    assertEquals(Gauges.track(Some(0.0), 4), "....#....")
    assertEquals(Gauges.half(100), 12)
    assertEquals(Gauges.half(110), 15)
    assertEquals(Gauges.half(120), 17)
    assertEquals(Gauges.half(80), 7)
    assertEquals(Gauges.half(50), 4)
    assertEquals(Gauges.half(300), 30)
  }

  test("book lens rows average the books in the window and the now row carries the counts") {
    val sec = now / 1000
    val older = book("1", "3")
    val current = book("1", "1")
    val s = s0.copy(
      book = Some(current),
      books = Vector((sec - 20, older), (sec - 5, current)),
      since = now - 15_000
    )
    val t = Some(50.0)
    val tr = Gauges.track(_: Option[Double], 13)
    assertEquals(
      Gauges.bookRow(s, "now", 0L, now, t, 13),
      s"BOOK  now ${tr(Some(0.0))} D+0.00 Q+0.00  big ${tr(Some(0.0))} D+0.00 b1 a1"
    )
    assertEquals(
      Gauges.bookRow(s, "10s", 10L, now, t, 13),
      s"BOOK  10s ${tr(Some(0.0))} D+0.00 Q+0.00  big ${tr(Some(0.0))} D+0.00"
    )
    assertEquals(
      Gauges.bookRow(s, "30s", 30L, now, t, 13),
      s"BOOK ${Screen.dim} 30s${Screen.reset} ${tr(Some(-0.25))} D-0.25 Q-0.25  big ${tr(Some(-0.25))} D-0.25"
    )
    assertEquals(
      Gauges.bookRow(s, "now", 0L, now, None, 13),
      s"BOOK  now ${tr(Some(0.0))} D+0.00 Q+0.00  big ${tr(None)} D --"
    )
    assertEquals(
      Gauges.bookRow(s.copy(book = None, books = Vector.empty), "now", 0L, now, t, 13),
      s"BOOK  now ${tr(None)} D -- Q --  big ${tr(None)} D --"
    )
  }

  test("a lens adds the current book once when the newest kept book is an older one") {
    val sec = now / 1000
    val s = s0.copy(
      book = Some(book("1", "1")),
      books = Vector((sec - 5, book("3", "1"))),
      since = now - 100_000
    )
    val tr = Gauges.track(_: Option[Double], 13)
    assertEquals(
      Gauges.bookRow(s, "10s", 10L, now, None, 13),
      s"BOOK  10s ${tr(Some(0.25))} D+0.25 Q+0.25  big ${tr(None)} D --"
    )
  }

  test("a lens label is bright, not dim, exactly when its window equals the time since since") {
    val s = s0.copy(book = Some(book("1", "1")), since = now - 10_000)
    val line = Gauges.bookRow(s, "10s", 10L, now, None, 13)
    assert(!line.contains(Screen.dim), line)
  }

  test("tape lens rows count the trades in the window and the big ones") {
    val s = s0.copy(
      tape = Vector(
        trade(5_000L, Side.Buy, "100", "1"),
        trade(30_000L, Side.Sell, "100", "3"),
        trade(200_000L, Side.Buy, "100", "10")
      ),
      since = now - 1_000_000L
    )
    val t = Gauges.threshold(s.tape, now)
    assertEquals(t, Some(300.0))
    val tr = Gauges.track(_: Option[Double], 13)
    assertEquals(
      Gauges.tapeRow(s, "10s", 10L, now, t, 13),
      s"TAPE  10s ${tr(Some(1.0))} F+1.00 n=1     big ${tr(None)} F -- n=0"
    )
    assertEquals(
      Gauges.tapeRow(s, "1m", 60L, now, t, 13),
      s"TAPE   1m ${tr(Some(-0.5))} F-0.50 n=2     big ${tr(Some(-1.0))} F-1.00 n=1"
    )
    assertEquals(
      Gauges.tapeRow(s, "5m", 300L, now, t, 13),
      s"TAPE   5m ${tr(Some(8.0 / 14))} F+0.57 n=3     big ${tr(Some(7.0 / 13))} F+0.54 n=2"
    )
    assertEquals(
      Gauges.tapeRow(s.copy(since = now - 100_000), "5m", 300L, now, t, 13),
      s"TAPE ${Screen.dim}  5m${Screen.reset} ${tr(Some(8.0 / 14))} F+0.57 n=3     big ${tr(Some(7.0 / 13))} F+0.54 n=2"
    )
  }

  test("the tape window edge is exclusive: a trade exactly secs old is left out") {
    val s = s0.copy(tape = Vector(trade(10_000L, Side.Buy, "100", "1")), since = now - 1_000_000L)
    val tr = Gauges.track(_: Option[Double], 13)
    assertEquals(
      Gauges.tapeRow(s, "10s", 10L, now, None, 13),
      s"TAPE  10s ${tr(None)} F -- n=0     big ${tr(None)} F -- n=0"
    )
  }

  test("rows gives twelve lines of the width in the fixed order") {
    val rows = Gauges.rows(s0, now, 100, None)
    assertEquals(rows.size, 12)
    assert(rows.map(Chart.plain).forall(_.length == 100))
    assertEquals(
      rows.map(r => Chart.plain(r).take(9)),
      Vector(
        "BOOK  now",
        "BOOK  10s",
        "BOOK  30s",
        "BOOK   1m",
        "BOOK   5m",
        "BOOK  15m",
        "TAPE  10s",
        "TAPE   1m",
        "TAPE   5m",
        "TAPE  15m",
        "TAPE   1h",
        "TAPE   4h"
      )
    )
    assert(Gauges.rows(s0, now, 120, None).map(Chart.plain).forall(_.length == 120))
  }

  test("every gauge row is exactly the width, from a hundred columns to a hundred and seventy") {
    val deep = Book(
      (1 to 20).toVector.map(i => lvl((100 - i).toString, "10")),
      (1 to 20).toVector.map(i => lvl((101 + i).toString, "10")),
      1L
    )
    val sec = now / 1000
    val tape =
      Vector.tabulate(100_000)(i => Trade(now - i.toLong * 9_999L / 100_000L, Side.Buy, "100", "1"))
    val s = s0.copy(
      book = Some(deep),
      books = Vector((sec, deep)),
      tape = tape,
      since = now - 100_000_000L
    )
    val t = Some(50.0)
    for cols <- 100 to 170 do
      val rows = Gauges.rows(s, now, cols, t)
      assertEquals(rows.size, 12)
      assertEquals(rows.map(r => Chart.plain(r).length).distinct, Vector(cols), s"at $cols columns")
    val wide = Gauges.rows(s, now, 100, t)
    assert(Chart.plain(wide.head).contains(" b20 a20"), Chart.plain(wide.head))
    assert(Chart.plain(wide(6)).contains("n=100000"), Chart.plain(wide(6)))
  }

  test("a row too long for the width is cut and keeps its escapes whole") {
    val s = s0.copy(book = Some(book("1", "1")), since = now)
    val rows = Gauges.rows(s, now, 50, Some(50.0))
    assertEquals(rows.map(r => Chart.plain(r).length).distinct, Vector(50))
    assert(rows(1).contains(Screen.dim) && rows(1).contains(Screen.reset), rows(1))
  }

  test("twelve gauge rows over a full tape render in under two seconds") {
    val tape = Vector.tabulate(200_000)(i =>
      Trade(now - i.toLong * 70L, if i % 2 == 0 then Side.Buy else Side.Sell, "100", "1")
    )
    val s = s0.copy(book = Some(book("1", "1")), tape = tape, since = now - 100_000_000L)
    val t = Gauges.threshold(tape, now)
    val started = System.nanoTime()
    val rows = Gauges.rows(s, now, 100, t)
    val elapsed = System.nanoTime() - started
    assertEquals(rows.size, 12)
    assert(elapsed < 2e9, s"took ${elapsed / 1e6} ms")
  }
