package desk

import Polymarket.Window.*

class BandSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val N = now / 1000
  private val base = Snapshot.initial("XMR", now) // zoom 0 (one second), line, no offset
  private def at(x: Int): Long = (N - 19 + x) * 1000L // the time of dot column x at 10 cells
  private def p(x: Int, up: Double) = Polymarket.Point(at(x), up)
  private def odds(
      lines: Map[(String, Polymarket.Window), Vector[Polymarket.Point]],
      stale: Set[(String, Polymarket.Window)] = Set.empty,
      ended: Set[(String, Polymarket.Window)] = Set.empty,
      unreachable: Boolean = false
  ): Odds = Odds(
    (c, w) => lines.getOrElse((c, w), Vector.empty),
    (c, w) =>
      lines.get((c, w)).filter(_.nonEmpty && !ended((c, w))) match
        case Some(points) => Polymarket.State.Ready(points, points.last.at)
        case None         => Polymarket.State.NoMarket
    ,
    (c, w) => stale((c, w)),
    unreachable
  )
  private val btc = Screen.dim + " " * 14 + "Polymarket BTC up%  100" + Screen.reset + " "
  private def draw(o: Odds, rows: Int = 2, s: Snapshot = base) =
    Band.rows(s, o, now, 38, 10, rows, Style.colour)
  private val c = Style.colour

  test("each window draws its line in its colour, extended to now") {
    val two =
      Map(("BTC", M5) -> Vector(p(0, 100.0), p(8, 100.0)), ("BTC", D1) -> Vector(p(0, 0.0)))
    assertEquals(
      draw(odds(two)),
      Vector(
        btc + c.w5 + "⠉" * 10 + Screen.reset,
        " " * 36 + "0 " + Screen.dim + "⣀" * 10 + Screen.reset
      )
    )
  }

  test("four rows carry 100, 50 and 0") {
    assertEquals(
      draw(odds(Map.empty), 4),
      Vector(btc + " " * 10, " " * 48, " " * 35 + "50 " + " " * 10, " " * 36 + "0 " + " " * 10)
    )
  }

  test("two lines on one cell are bold text colour") {
    val both = Map(("BTC", M5) -> Vector(p(0, 100.0)), ("BTC", M15) -> Vector(p(0, 100.0)))
    assertEquals(draw(odds(both))(0), btc + Screen.bold + "⠉" * 10 + Screen.reset)
  }

  test("points are joined by segments") {
    val slope = Map(("BTC", M5) -> Vector(p(0, 100.0), p(7, 12.5)))
    assertEquals(
      draw(odds(slope)),
      Vector(
        btc + c.w5 + "⠑⢄" + Screen.reset + " " * 8,
        " " * 36 + "0 " + "  " + c.w5 + "⠑⢄⣀⣀⣀⣀⣀⣀" + Screen.reset
      )
    )
  }

  test("unreachable draws no line under its label") {
    val two =
      Map(("BTC", M5) -> Vector(p(0, 100.0), p(8, 100.0)), ("BTC", D1) -> Vector(p(0, 0.0)))
    assertEquals(
      draw(odds(two, unreachable = true)),
      Vector(
        Screen.dim + " " * 10 + "Polymarket unreachable  100" + Screen.reset + " " + " " * 10,
        " " * 36 + "0 " + " " * 10
      )
    )
  }

  test("a stale line is faint, and an ended market keeps its points without the extension") {
    val flat = Map(("BTC", M5) -> Vector(p(0, 100.0)))
    assertEquals(
      draw(odds(flat, stale = Set(("BTC", M5))))(0),
      btc + Screen.dim + "⠉" * 10 + Screen.reset
    )
    val closed = Map(("BTC", M5) -> Vector(p(0, 100.0), p(5, 100.0)))
    assertEquals(
      draw(odds(closed, ended = Set(("BTC", M5))))(0),
      btc + c.w5 + "⠉⠉⠉" + Screen.reset + " " * 7
    )
  }

  test("the band follows BTC or PAXG, else BTC") {
    assertEquals(
      Vector("XMR", "BTC", "PAXG", "ETH").map(Band.coin),
      Vector("BTC", "BTC", "PAXG", "BTC")
    )
    val gold = Map(("PAXG", D1) -> Vector(p(0, 0.0)), ("BTC", M5) -> Vector(p(0, 100.0)))
    assertEquals(
      draw(odds(gold), s = base.copy(coin = "PAXG")),
      Vector(
        Screen.dim + " " * 13 + "Polymarket PAXG up%  100" + Screen.reset + " " + " " * 10,
        " " * 36 + "0 " + Screen.dim + "⣀" * 10 + Screen.reset
      )
    )
  }

  test("the band shares the chart's time axis, scrolled and in candles") {
    val back = Map(("BTC", M5) -> Vector(Polymarket.Point((N - 15) * 1000L, 100.0)))
    assertEquals(
      draw(odds(back), s = base.copy(offsetSec = 10))(0),
      btc + " " * 7 + c.w5 + "⠉⠉⠉" + Screen.reset
    )
    // Zoom 5 is 60 seconds a candle.
    val minute = Map(("BTC", M5) -> Vector(Polymarket.Point(1_799_999_940_000L, 100.0)))
    assertEquals(
      draw(odds(minute), s = base.copy(chartMode = ChartMode.Candles, zoom = 5))(0),
      btc + c.w5 + "⠉⠉⠉⠉⠁" + Screen.reset + " " * 5
    )
  }

  test("points far to the left are skipped, and a segment that enters the plot is drawn") {
    val far = Map(
      ("BTC", M5) -> Vector(p(-100000, 100.0), p(-50000, 100.0), p(-2, 100.0), p(2, 100.0))
    )
    assertEquals(
      draw(odds(far, ended = Set(("BTC", M5))))(0),
      btc + c.w5 + "⠉⠁" + Screen.reset + " " * 8
    )
  }

  test("a point on its own is a dot, and a point after now runs no line back to now") {
    val lone = Map(("BTC", M5) -> Vector(p(3, 100.0)))
    assertEquals(
      draw(odds(lone, ended = Set(("BTC", M5))))(0),
      btc + " " + c.w5 + "⠈" + Screen.reset + " " * 8
    )
    val ahead = Map(("BTC", M5) -> Vector(p(21, 0.0)))
    assertEquals(draw(odds(ahead)), Vector(btc + " " * 10, " " * 36 + "0 " + " " * 10))
  }

  test("odds read the reader") {
    assertEquals(Odds.none.chance("BTC", M5), None)
    assert(Odds.none.unreachable)
    val ready = Polymarket.State.Ready(Vector(Polymarket.Point(1L, 54.4)), 1L)
    val one = Odds(
      (_, _) => Vector.empty,
      (cn, w) => if (cn, w) == ("BTC", M5) then ready else Polymarket.State.NoMarket,
      (_, _) => false,
      false
    )
    assertEquals(one.chance("BTC", M5), Some((54.4, false)))
    assertEquals(one.chance("BTC", M15), None)
    assertEquals(one.copy(unreachable = true).chance("BTC", M5), None)
    assertEquals(one.copy(stale = (cn, _) => cn == "BTC").chance("BTC", M5), Some((54.4, true)))
    assert(one.live("BTC", M5))
    assert(!one.live("BTC", M15))
    val idle = Odds.of(
      new PolymarketReader(_ => Left(0), (_, _) => Left(0), () => Set("BTC"), () => 0L, _ => ())
    )
    assert(idle.unreachable)
    assertEquals(idle.state("BTC", M5), Polymarket.State.Unreachable)
    assertEquals(idle.points("BTC", M5), Vector.empty)
  }

  test("K1 chance reads the newest point and the stale flag of its own coin and window") {
    val ring = Vector(Polymarket.Point(1L, 54.4), Polymarket.Point(2L, 61.0))
    val keyed = Odds(
      (_, _) => Vector.empty,
      (_, _) => Polymarket.State.Ready(ring, 2L),
      (cn, w) => (cn, w) == ("PAXG", H1),
      false
    )
    assertEquals(keyed.chance("PAXG", H1), Some((61.0, true)))
    assertEquals(keyed.chance("BTC", H1), Some((61.0, false)))
    assertEquals(keyed.chance("PAXG", M5), Some((61.0, false)))
  }

  test("K2 no odds has every state Unreachable, no points and nothing stale") {
    for cn <- Vector("BTC", "PAXG"); w <- Polymarket.Window.values do
      assertEquals(Odds.none.state(cn, w), Polymarket.State.Unreachable)
      assertEquals(Odds.none.points(cn, w), Vector.empty)
      assert(!Odds.none.stale(cn, w))
  }

  test("K3 a line is faint by the stale flag of its own coin and window") {
    val lines = Map(("PAXG", H1) -> Vector(p(0, 100.0)), ("PAXG", M5) -> Vector(p(0, 0.0)))
    val paxg = Screen.dim + " " * 13 + "Polymarket PAXG up%  100" + Screen.reset + " "
    assertEquals(
      draw(odds(lines, stale = Set(("PAXG", H1), ("BTC", M5))), s = base.copy(coin = "PAXG")),
      Vector(
        paxg + Screen.dim + "⠉" * 10 + Screen.reset,
        " " * 36 + "0 " + c.w5 + "⣀" * 10 + Screen.reset
      )
    )
  }

  test("K4 each coloured window draws in its own colour") {
    for (w, ink) <- Vector(M5 -> c.w5, M15 -> c.w15, H1 -> c.w1h, H4 -> c.w4h) do
      assertEquals(
        draw(odds(Map(("BTC", w) -> Vector(p(0, 100.0)))))(0),
        btc + ink + "⠉" * 10 + Screen.reset
      )
  }

  test("K5 a segment ending on the first column and a point on the last column are drawn") {
    val edge = Map(("BTC", M5) -> Vector(p(-3, 100.0), p(0, 100.0)))
    assertEquals(
      draw(odds(edge, ended = Set(("BTC", M5))))(0),
      btc + c.w5 + "⠁" + Screen.reset + " " * 9
    )
    val last = Map(("BTC", M5) -> Vector(p(19, 100.0)))
    assertEquals(draw(odds(last))(0), btc + " " * 9 + c.w5 + "⠈" + Screen.reset)
  }

  test("K6 a time half a column before the plot is left of it") {
    val early = Map(("BTC", M5) -> Vector(Polymarket.Point(at(0) - 500, 100.0)))
    assertEquals(draw(odds(early, ended = Set(("BTC", M5))))(0), btc + " " * 10)
    val onTime = Map(("BTC", M5) -> Vector(Polymarket.Point(at(0), 100.0)))
    assertEquals(
      draw(odds(onTime, ended = Set(("BTC", M5))))(0),
      btc + c.w5 + "⠁" + Screen.reset + " " * 9
    )
  }

  test("K7 a chance takes the dot row its share falls in, held inside the band") {
    val mid = Map(("BTC", M5) -> Vector(p(0, 55.0)))
    assertEquals(
      draw(odds(mid, ended = Set(("BTC", M5)))),
      Vector(btc + c.w5 + "⡀" + Screen.reset + " " * 9, " " * 36 + "0 " + " " * 10)
    )
    val over = Map(("BTC", M5) -> Vector(p(0, 120.0)))
    assertEquals(
      draw(odds(over, ended = Set(("BTC", M5))))(0),
      btc + c.w5 + "⠁" + Screen.reset + " " * 9
    )
  }

  test("K8 two lines in one cell show the dots of both") {
    val pair = Map(("BTC", M5) -> Vector(p(0, 100.0)), ("BTC", M15) -> Vector(p(1, 100.0)))
    assertEquals(
      draw(odds(pair, ended = pair.keySet))(0),
      btc + Screen.bold + "⠉" + Screen.reset + " " * 9
    )
  }

  test("the band keeps the chart's time per cell when the line chart zooms out") {
    val zoomed = Map(("BTC", M5) -> Vector(Polymarket.Point((N - 18) * 1000L, 100.0)))
    assertEquals(
      draw(odds(zoomed, ended = Set(("BTC", M5))), s = base.copy(zoom = 1))(0),
      btc + " " * 5 + c.w5 + "⠁" + Screen.reset + " " * 4
    )
  }
