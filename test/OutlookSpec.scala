package desk

class OutlookSpec extends munit.FunSuite:
  private val short = OutlookHorizon("SHORT", "5m", 12)
  private val shortStep = 300000L

  private def candles(
      horizon: OutlookHorizon = short,
      periods: Int = 100,
      price: Int => Double = i => 1000.0 + i,
      offset: Long = 0L
  ): Vector[Candle] =
    val warmup = ((50 + horizon.bars - 1) / horizon.bars) * horizon.bars
    Vector.tabulate(warmup + periods * horizon.bars) { i =>
      val close = price(i)
      Candle(
        offset + i * State.intervalMs(horizon.interval),
        close,
        close * 1.01,
        close * 0.99,
        close,
        1
      )
    }

  private def state(cs: Vector[Candle], interval: String = "5m"): Snapshot =
    Snapshot
      .initial("BTC", 0L)
      .copy(candles = Map(interval -> cs))

  private def measured(
      cs: Vector[Candle],
      horizon: OutlookHorizon = short,
      at: Option[Long] = None
  ): HorizonOutlook =
    val now = at.getOrElse(cs.map(_.t).max + State.intervalMs(horizon.interval))
    Outlook
      .compute(state(cs, horizon.interval), now)
      .columns
      .find(_.horizon == horizon)
      .get

  private def book(snapshot: Snapshot, price: Double, at: Long): Snapshot =
    snapshot.copy(
      book = Some(
        Book(
          Vector(Level((price - 0.01).toString, "1", 1)),
          Vector(Level((price + 0.01).toString, "1", 1)),
          at
        )
      ),
      feed = Health.Live(at)
    )

  private def shown(
      view: OutlookView,
      snapshot: Snapshot,
      now: Long
  ): HorizonOutlook =
    Outlook.visible(view, snapshot, now).columns.find(_.horizon == short).get

  private def countedHits(): Vector[Candle] =
    val base = candles(price = i => 1000.0 + i * 0.01)
      .map(c => c.copy(h = c.c + 0.001, l = c.c - 0.001))
    val early = (0 until 24).foldLeft(base) { (cs, period) =>
      val anchor = 60 + period * 12
      val index = anchor + 3
      cs.updated(index, cs(index).copy(h = cs(anchor - 1).c * 1.06))
    }
    early.updated(early.size - 1, early.last.copy(h = early.last.c * 1.04))

  test("periods are explicit and ordered long, mid, short") {
    assertEquals(
      Outlook.horizons.map(h => (h.label, h.interval, h.bars, h.periodMs)),
      Vector(
        ("LONG", "1h", 24, 86400000L),
        ("MID", "15m", 16, 14400000L),
        ("SHORT", "5m", 12, 3600000L)
      )
    )
    Outlook.horizons.foreach { horizon =>
      val cs = candles(horizon)
      val result = measured(cs, horizon)
      assertEquals(result.base.samples, 100)
      assertEquals(
        result.at,
        Some(cs.last.t + State.intervalMs(horizon.interval))
      )
    }
  }

  test(
    "a rising series gives a measured upper target and 100 separate outcome periods"
  ) {
    val cs = candles()
    val result = measured(cs)
    assertEquals(result.direction, OutlookDirection.Up)
    assertEquals(result.status, "ready")
    assertEquals(result.reference, Some(cs.last.c))
    assertEquals(result.support, Some(cs(cs.size - 50).l))
    assertEquals(result.resistance, Some(cs.last.h))
    assertEquals(result.target, result.resistance)
    assertEquals(result.base, HistoricalRate(100, 100, 100))
    assertEquals(result.matched, HistoricalRate(100, 100, 50))
  }

  test(
    "the event is a target touch, including wicks, rather than an upward close"
  ) {
    val cs = countedHits()
    val result = measured(cs)
    // All 100 periods close higher. Only 24 early wicks and the final target wick touch 4%.
    assertEquals(result.base.samples, 100)
    assertEquals(result.base.hits, 25)
    assertEquals(result.base.percent, Some(25.0))
    assertEquals(result.matched.samples, 100)
    assertEquals(result.matched.hits, 25)
  }

  test("general and matching samples have distinct hand-counted denominators") {
    val cs = candles(price = i => 1000.0 + math.abs(i - 600))
    val result = measured(cs)
    // The V turns at index 600. At the 12-bar anchors, the first up alignment is index 623.
    // Outcomes beginning at 600 through 1248 rise far enough, giving 55 hits.
    // Matched anchors begin at 624, giving 53 separate cases, all hits.
    assertEquals(result.direction, OutlookDirection.Up)
    assertEquals(result.base, HistoricalRate(55, 100, 100))
    assertEquals(result.matched, HistoricalRate(53, 53, 50))
  }

  test(
    "downward target touches use lows and the corresponding past down alignment"
  ) {
    val cs = candles(price = i => 2000.0 - math.abs(i - 600))
    val result = measured(cs)
    assertEquals(result.direction, OutlookDirection.Down)
    assertEquals(result.target, result.support)
    assertEquals(result.base, HistoricalRate(55, 100, 100))
    assertEquals(result.matched, HistoricalRate(53, 53, 50))
  }

  test(
    "anchors stay on calendar boundaries instead of restarting from the first observation"
  ) {
    val result = measured(candles(offset = 11 * shortStep))
    assertEquals(result.base.samples, 99)
  }

  test("an unfinished last outcome is not counted") {
    val cs = candles()
    val result = measured(cs, at = Some(1260 * shortStep - 1L))
    assertEquals(result.base.samples, 99)
  }

  test(
    "open and future candles cannot change the current view or any past outcome"
  ) {
    val cs = candles()
    val now = 1260 * shortStep
    val extra = Vector(
      Candle(now, 1, 1e50, 0.5, 1, 1),
      Candle(now + shortStep, 1, Double.NaN, 0.5, 1, 1)
    )
    assertEquals(
      measured(cs ++ extra, at = Some(now)),
      measured(cs, at = Some(now))
    )
  }

  test("100 general and 50 matching samples are independent display minimums") {
    assertEquals(HistoricalRate(49, 99, 100).percent, None)
    assertEquals(HistoricalRate(50, 100, 100).percent, Some(50.0))
    assertEquals(HistoricalRate(24, 49, 50).percent, None)
    assertEquals(HistoricalRate(25, 50, 50).percent, Some(50.0))
    assertEquals(HistoricalRate(25, 50, 50, available = false).percent, None)
  }

  test("historical counts reject invalid samples and minimums") {
    intercept[IllegalArgumentException](HistoricalRate(2, 1, 1))
    intercept[IllegalArgumentException](HistoricalRate(-1, 1, 1))
    intercept[IllegalArgumentException](HistoricalRate(0, 0, 0))
  }

  test(
    "exactly ninety percent usable periods is accepted and one fewer is suppressed"
  ) {
    val cs = candles(periods = 200)
    // Missing opens 10..239 spoil precisely the first 20 scheduled feature/outcome windows.
    val boundary = measured(
      cs.filterNot(c => c.t >= 10 * shortStep && c.t <= 239 * shortStep)
    )
    assertEquals(boundary.base.samples, 180)
    assertEquals(boundary.base.percent, Some(100.0))
    assertEquals(boundary.matched.percent, Some(100.0))
    // Extending the hole to open 250 also spoils the window anchored at 300.
    val below = measured(
      cs.filterNot(c => c.t >= 10 * shortStep && c.t <= 250 * shortStep)
    )
    assertEquals(below.base.samples, 179)
    assertEquals(below.base.hits, 179)
    assertEquals(below.status, "gapped history")
    assertEquals(below.base.percent, None)
    assertEquals(below.matched.percent, None)
  }

  test(
    "a bad older bar is a missing window, never silently joined to its neighbor"
  ) {
    val cs = candles()
    val broken = cs.updated(10, cs(10).copy(h = cs(10).l - 1))
    val result = measured(broken)
    assertEquals(result.base.samples, 99)
    assertEquals(result.matched.samples, 99)
  }

  test(
    "inconsistent duplicated timestamps are rejected and identical copies do not add samples"
  ) {
    val cs = candles()
    assertEquals(measured(cs :+ cs(100)), measured(cs))
    val result = measured(cs :+ cs.last.copy(h = cs.last.h + 1))
    assertEquals(result.status, "invalid data")
    assertEquals(result.direction, OutlookDirection.Mixed)
    assertEquals(result.base.percent, None)
    assertEquals(result.target, None)
    val older = measured(cs :+ cs(10).copy(h = cs(10).h + 1))
    assertEquals(older.base.samples, 99)
  }

  test(
    "invalid closed prices, volumes and candle bounds make the current view unavailable"
  ) {
    val cs = candles()
    val c = cs.last
    val invalid = Vector(
      c.copy(h = c.l - 1),
      c.copy(l = c.c + 1),
      c.copy(o = c.h + 1),
      c.copy(c = 0),
      c.copy(h = Double.PositiveInfinity),
      c.copy(l = Double.NaN),
      c.copy(v = -1),
      c.copy(v = Double.PositiveInfinity)
    )
    invalid.foreach { broken =>
      val result = measured(cs.updated(cs.size - 1, broken))
      assertEquals(result.status, "invalid data")
      assertEquals(result.direction, OutlookDirection.Mixed)
      assertEquals(result.base.percent, None)
      assertEquals(result.matched.percent, None)
    }
  }

  test(
    "a missing current feature bar suppresses levels and a missing latest close is stale"
  ) {
    val cs = candles()
    val partial = measured(cs.filterNot(_.t == cs(cs.size - 25).t))
    assertEquals(partial.status, "partial data")
    assertEquals(partial.support, None)
    assertEquals(partial.target, None)
    val stale = measured(cs, at = Some(1261 * shortStep))
    assertEquals(stale.status, "stale")
    assertEquals(stale.direction, OutlookDirection.Mixed)
    assertEquals(stale.matched.percent, None)
  }

  test(
    "mixed averages and a target already at the reference do not invent odds"
  ) {
    val flat = measured(candles(price = _ => 1000.0))
    assertEquals(flat.status, "mixed")
    assertEquals(flat.direction, OutlookDirection.Mixed)
    assertEquals(flat.support, Some(990.0))
    assertEquals(flat.resistance, Some(1010.0))
    assertEquals(flat.target, None)
    assertEquals(flat.base.percent, None)
    val cs = candles().map(c => c.copy(o = c.c, h = c.c, l = c.c))
    val atLevel = measured(cs)
    assertEquals(atLevel.status, "at level")
    assertEquals(atLevel.base.percent, None)
    assertEquals(atLevel.matched.percent, None)
  }

  test(
    "older repairs affect outcomes even when candle count and last close do not change"
  ) {
    val cs = countedHits()
    val anchor = 60 + 50 * 12
    val index = anchor + 2
    val changed = cs.updated(index, cs(index).copy(h = cs(anchor - 1).c * 1.06))
    assertEquals(changed.size, cs.size)
    assertEquals(changed.last, cs.last)
    assertEquals(measured(cs).base.hits, 25)
    assertEquals(measured(changed).base.hits, 26)
  }

  test(
    "no data, over-cap input and a long sparse gap return a bounded honest result"
  ) {
    val empty = Outlook.compute(Snapshot.initial("BTC", 0L), 100000000L)
    assert(
      empty.columns.forall(c => c.base.percent.isEmpty && c.target.isEmpty)
    )
    val cs = candles(periods = 430)
    assert(cs.size > State.seriesCap)
    assertEquals(measured(cs).status, "invalid data")
    val recent = candles(periods = 0, offset = 20L * 365 * 86400000L)
    val sparse = measured(Vector(Candle(0, 100, 101, 99, 100, 1)) ++ recent)
    assertEquals(sparse.status, "gapped history")
    assertEquals(sparse.base.percent, None)
  }

  test(
    "a fresh calculated view and fresh main book retain the measured direction"
  ) {
    val cs = candles()
    val now = 1260 * shortStep
    val snapshot = book(state(cs), cs.last.c, now)
    val result = shown(Outlook.compute(snapshot, now), snapshot, now)
    assertEquals(result.direction, OutlookDirection.Up)
    assertEquals(result.base.percent, Some(100.0))
  }

  test(
    "target equality or a recorded current wick disables already reached targets"
  ) {
    val cs = candles()
    val now = 1260 * shortStep
    val snapshot = book(state(cs), cs.last.c, now)
    val view = Outlook.compute(snapshot, now)
    val hit = shown(view, book(snapshot, cs.last.h, now), now)
    assertEquals(hit.status, "reached")
    assertEquals(hit.direction, OutlookDirection.Mixed)
    assertEquals(hit.base.percent, None)
    assertEquals(hit.base.samples, 100)
    val wick = Candle(now, cs.last.c, cs.last.h + 1, cs.last.l, cs.last.c, 1)
    val retreated =
      shown(view, snapshot.copy(candles = Map("5m" -> (cs :+ wick))), now)
    assertEquals(retreated.status, "reached")
    assertEquals(retreated.matched.percent, None)
    val down = candles(price = i => 10000.0 - i)
    val downState = book(state(down), down.last.l, now)
    val downView = Outlook.compute(downState, now)
    assertEquals(shown(downView, downState, now).status, "reached")
  }

  test(
    "calculation expiry, backward time and market changes hide arrows and rates"
  ) {
    val cs = candles()
    val now = 1260 * shortStep
    val snapshot = book(state(cs), cs.last.c, now)
    val view = Outlook.compute(snapshot, now)
    val expired =
      shown(view, book(snapshot, cs.last.c, now + 10001), now + 10001)
    assertEquals(expired.status, "stale")
    assertEquals(expired.direction, OutlookDirection.Mixed)
    assertEquals(expired.base.percent, None)
    assertEquals(shown(view, snapshot, now - 1).status, "stale")
    val other = Outlook.visible(view, snapshot.copy(coin = "ETH"), now)
    assertEquals(other.coin, "ETH")
    assert(
      other.columns.forall(c => c.direction == OutlookDirection.Mixed && c.target.isEmpty)
    )
    assert(other.columns.forall(_.base.percent.isEmpty))
  }

  test(
    "reference feed activity cannot make an old or future main book current"
  ) {
    val cs = candles()
    val now = 1260 * shortStep
    val snapshot = book(state(cs), cs.last.c, now)
    val view = Outlook.compute(snapshot, now)
    assertEquals(
      shown(view, book(snapshot, cs.last.c, now - 5000), now).base.percent,
      Some(100.0)
    )
    Vector(now - 5001, now + 1).foreach { time =>
      val old = book(snapshot, cs.last.c, time).copy(feed = Health.Live(now))
      val result = shown(view, old, now)
      assertEquals(result.status, "stale price")
      assertEquals(result.direction, OutlookDirection.Mixed)
      assertEquals(result.base.percent, None)
    }
  }

  test(
    "missing, nonfinite, negative and crossed book sides cannot support a visible signal"
  ) {
    val cs = candles()
    val now = 1260 * shortStep
    val snapshot = book(state(cs), cs.last.c, now)
    val view = Outlook.compute(snapshot, now)
    val invalid = Vector(
      Book(Vector.empty, Vector(Level("100", "1", 1)), now),
      Book(Vector(Level("NaN", "1", 1)), Vector(Level("100", "1", 1)), now),
      Book(Vector(Level("-1", "1", 1)), Vector(Level("100", "1", 1)), now),
      Book(Vector(Level("101", "1", 1)), Vector(Level("100", "1", 1)), now)
    )
    invalid.foreach { value =>
      val result = shown(view, snapshot.copy(book = Some(value)), now)
      assertEquals(result.direction, OutlookDirection.Mixed)
      assertEquals(result.base.percent, None)
    }
    assertEquals(
      shown(view, snapshot.copy(book = None), now).base.percent,
      None
    )
  }

  test(
    "crossing a closed-candle boundary invalidates the old anchor before refresh"
  ) {
    val cs = candles()
    val before = 1261 * shortStep - 1L
    val snapshot = book(state(cs), cs.last.c, before)
    val view = Outlook.compute(snapshot, before)
    val after = before + 1L
    val result = shown(view, book(snapshot, cs.last.c, after), after)
    assertEquals(result.status, "stale")
    assertEquals(result.direction, OutlookDirection.Mixed)
    assertEquals(result.base.percent, None)
  }
