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

  test("flow over a set of trades and undefined when empty") {
    assertEquals(Gauges.flow(Vector.empty), (None, 0))
    val (f, n) = Gauges.flow(Vector(trade(1L, Side.Buy, "1", "3"), trade(2L, Side.Sell, "1", "1")))
    assertEquals((f, n), (Some(0.5), 2))
  }

  test("a track puts the marker by the half width and none when undefined") {
    assertEquals(Gauges.track(Some(0.5), 12), "............|.....#......")
    assertEquals(Gauges.track(None, 12), "............|............")
    assertEquals(Gauges.track(Some(-1.0), 4), "#...|....")
    assertEquals(Gauges.track(Some(3.0), 4), "....|...#")
    assertEquals(Gauges.track(Some(0.0), 4), "....#....")
    assertEquals(Gauges.half(100), 13)
    assertEquals(Gauges.half(120), 18)
    assertEquals(Gauges.half(80), 8)
    assertEquals(Gauges.half(50), 4)
    assertEquals(Gauges.half(300), 30)
  }

  test("book lens rows average the books in the window and the now row carries the counts") {
    val sec = now / 1000
    val s = s0.copy(
      book = Some(book("1", "1")),
      books = Vector((sec - 20, book("1", "3")), (sec - 5, book("3", "1"))),
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
      s"BOOK  10s ${tr(Some(0.25))} D+0.25 Q+0.25  big ${tr(Some(0.25))} D+0.25"
    )
    assertEquals(
      Gauges.bookRow(s, "30s", 30L, now, t, 13),
      s"BOOK ${Screen.dim} 30s${Screen.reset} ${tr(Some(0.0))} D+0.00 Q+0.00  big ${tr(Some(0.0))} D+0.00"
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
