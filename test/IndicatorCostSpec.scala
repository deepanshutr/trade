package desk

import Polymarket.Window.*

// A full history: the market and both references hold six hours of one second samples and 5200
// bars of 1m, 5m, 15m and 1h. A day of book and tape minutes, 20,000 trades, and a full ring of
// points for every BTC window and the gold daily window.
object FullHistory:
  val now: Long = Golden.now
  @volatile private var sink: Any = null

  private def walk(seed: Long, start: Double, n: Int): Vector[Double] =
    val r = new scala.util.Random(seed)
    Vector.fill(n)(math.exp(r.nextGaussian() * 0.0005)).scanLeft(start)(_ * _).tail
  private def samples(seed: Long, start: Double): Vector[(Long, Double)] =
    walk(seed, start, 21_600).zipWithIndex.map((p, i) => (now / 1000 - 21_599 + i, p))
  private def candles(seed: Long, start: Double): Map[String, Vector[Candle]] =
    Vector("1m", "5m", "15m", "1h").zipWithIndex.map { (interval, k) =>
      val step = State.intervalMs(interval)
      val prices = walk(seed * 10 + k, start, State.seriesCap + 1)
      val first = now / step - State.seriesCap + 1
      interval -> prices.zip(prices.tail).zipWithIndex.map { case ((o, c), i) =>
        Candle((first + i) * step, o, o.max(c) * 1.001, o.min(c) * 0.999, c, 1.0)
      }
    }.toMap

  lazy val snapshot: Snapshot =
    val minute = now / 60_000L
    val first = minute - Shares.minutes + 1
    Golden.snapshot.copy(
      feed = Health.Live(now - 300),
      samples = samples(1, 300.0),
      candles = candles(2, 300.0),
      refs = Map(
        "BTC" -> Ref(samples(3, 60_000.0), candles(4, 60_000.0)),
        "PAXG" -> Ref(samples(5, 2_600.0), candles(6, 2_600.0))
      ),
      bookMinutes = Vector.tabulate(Shares.minutes)(i => MinuteBook(first + i, 0.1, 4)),
      tapeMinutes = Vector.tabulate(Shares.minutes)(i => MinuteTape(first + i, 3.0, 2.0)),
      tape = DisplayCost.tape(6, 20_000)
    )

  val odds: Odds =
    val ring = Vector.tabulate(Polymarket.ringSize)(i =>
      Polymarket.Point(
        now - Polymarket.midpointEveryMs * (Polymarket.ringSize - 1 - i),
        40.0 + i % 20
      )
    )
    def has(coin: String, w: Polymarket.Window) = coin == "BTC" || (coin == "PAXG" && w == D1)
    Odds(
      (c, w) => if has(c, w) then ring else Vector.empty,
      (c, w) =>
        if has(c, w) then Polymarket.State.Ready(ring, ring.last.at) else Polymarket.State.NoMarket,
      (_, _) => false,
      unreachable = false
    )

  // The median and the 90th percentile in milliseconds of each body, over 200 timed rounds after
  // 40 warm ones. Every round runs every body, so a load on the machine falls on all of them.
  def time(bodies: (() => Any)*): Vector[(Double, Double)] =
    for _ <- 0 until 40; body <- bodies do sink = body()
    val rounds = Vector.fill(200)(bodies.toVector.map { body =>
      val start = System.nanoTime()
      sink = body()
      (System.nanoTime() - start) / 1e6
    })
    bodies.indices.toVector.map { k =>
      val ms = rounds.map(_(k)).sorted
      ((ms(99) + ms(100)) / 2, ms(179))
    }

class IndicatorCostSpec extends munit.FunSuite:
  private val slow = new munit.Tag("slow")

  test("the groups cost less than their long measures with closes per call".tag(slow)) {
    val s = FullHistory.snapshot
    val now = FullHistory.now
    def groups = Indicators.groups(s, FullHistory.odds.chance, now, Style.colour)
    // The hour and longer changes of all three coins have a value, so the timing covers them.
    for g <- groups.drop(1); row <- g.rows.drop(2) do assert(row.marks.exists(_.glyph == '▲'), row)
    // The twelve hour and longer measures of one frame, each building its own closes.
    val (btc, gold) = (s.refs("BTC"), s.refs("PAXG"))
    def perCall = for p <- Indicators.periods.filter(_.ms >= 3_600_000L)
    yield (
      Quant.change(s.samples, s.candles("1m"), p.ms, now),
      Quant.distance(s.samples, s.candles("1m"), p.ms, now),
      Quant.change(btc.samples, btc.candles("1m"), p.ms, now),
      Quant.change(gold.samples, gold.candles("1m"), p.ms, now)
    )
    val times = FullHistory.time(() => groups, () => perCall)
    val ((median, slowest), (bound, _)) = (times(0), times(1))
    println(f"groups median $median%.2f ms, 90th percentile $slowest%.2f ms, bound $bound%.2f ms")
    assert(median < bound, f"groups median $median%.2f ms, bound $bound%.2f ms")
  }
