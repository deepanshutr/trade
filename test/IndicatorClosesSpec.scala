package desk

class IndicatorClosesSpec extends munit.FunSuite:
  private val slow = new munit.Tag("slow")
  private val now = 1_800_000_000_000L
  private val coins = Vector("XMR", "BTC", "PAXG")
  private val long = Indicators.periods.filter(_.ms >= 3_600_000L)
  private def bar(t: Long, c: Double) = Candle(t, c, c, c, c, 1.0)
  // n one minute bars ending at now, a straight line with its own slope, the last close moved.
  private def minutes(n: Int, start: Double, slope: Double, last: Double): Vector[Candle] =
    Vector.tabulate(n)(i =>
      val c = start * (1 + slope * i / n) * (if i == n - 1 then last else 1.0)
      bar(now - 60_000L * (n - 1 - i), c)
    )
  // Each coin has its own one minute line, also the selected market's, which differs from its
  // reference. `last` moves every coin's latest close.
  private def snapshot(coin: String, n: Int = 1500, last: Double = 1.0): Snapshot =
    def series(k: Int) = Map("1m" -> minutes(n, 100.0 * (k + 1), 0.05 * (k + 1), last))
    Snapshot
      .initial(coin, now)
      .copy(
        candles = series(coins.indexOf(coin) + 3),
        refs = Map("BTC" -> Ref(Vector.empty, series(1)), "PAXG" -> Ref(Vector.empty, series(2)))
      )
  private val none: (String, Polymarket.Window) => Option[(Double, Boolean)] = (_, _) => None
  private def pct(m: Measure) = m.value.fold("--")(v => f"$v%+.2f%%")
  // Each group's own series, as the code before the prebuilt closes read it.
  private def own(s: Snapshot, coin: String) =
    if coin == s.coin then (s.samples, s.candles("1m"))
    else (s.refs(coin).samples, s.refs(coin).candles("1m"))
  // The hour and longer values of the market, BTC and gold groups, and the same measures through
  // the old signatures, each from its group's own bars.
  private def shown(s: Snapshot) =
    val g = Indicators.groups(s, none, now, Style.mono)
    Vector(1, 2, 3).map(k =>
      g(k).rows.slice(2, 5).map(_.values.take(if k == 1 then 2 else 1).map(_._1))
    )
  private def expected(s: Snapshot) =
    Vector(s.coin, "BTC", "PAXG").zipWithIndex.map { (coin, k) =>
      val (samples, bars) = own(s, coin)
      long.map(p =>
        ("▲" + pct(Quant.change(samples, bars, p.ms, now))) +:
          Vector("◇" + pct(Quant.distance(samples, bars, p.ms, now))).take(if k == 0 then 1 else 0)
      )
    }

  test("each group's hour and longer measures read its own coin's closes, whatever is selected") {
    for coin <- coins do
      val s = snapshot(coin)
      val want = expected(s)
      // Every value is there and the groups' changes differ, so a group reading another coin's
      // closes shows.
      assert(want.flatten.flatten.forall(!_.endsWith("--")), want)
      val changes = want.map(_.map(_.head))
      assertEquals(changes.distinct.size, if coin == "XMR" then 3 else 2, changes)
      assertEquals(shown(s), want, coin)
  }

  test("a frame keeps nothing from the frame before") {
    for coin <- coins do
      val (first, moved) = (snapshot(coin), snapshot(coin, last = 1.02))
      assertNotEquals(expected(moved), expected(first), coin)
      for s <- Vector(first, moved, first, moved) do assertEquals(shown(s), expected(s), coin)
  }

  // Slow like IndicatorCostSpec, as a count from allocated memory could flake on another JVM.
  test("the groups build each coin's one minute closes once a frame".tag(slow)) {
    // Only the closes builds and the long measures over them read every one minute bar, so the
    // growth from 2600 bars to 5200 shows the builds a frame makes. The ideal frame builds each
    // coin's closes once and reads every measure from them.
    def ideal(s: Snapshot) =
      val built = Vector(s.coin, "BTC", "PAXG").distinct.map(c => c -> Quant.closes(own(s, c)._2))
      val bars = built.toMap
      for p <- Indicators.periods
      yield (
        Quant.change(s.samples, bars(s.coin), p.ms, now),
        Quant.distance(s.samples, bars(s.coin), p.ms, now),
        Quant.change(own(s, "BTC")._1, bars("BTC"), p.ms, now),
        Quant.change(own(s, "PAXG")._1, bars("PAXG"), p.ms, now)
      )
    def growth(body: Snapshot => Any, coin: String) =
      val (small, big) = (snapshot(coin, 2600), snapshot(coin, 5200))
      DisplayCost.warmedBytes(body(big)) - DisplayCost.warmedBytes(body(small))
    for coin <- coins do
      val build = growth(s => Quant.closes(s.candles("1m")), coin)
      val (frame, best) =
        (growth(s => Indicators.groups(s, none, now, Style.mono), coin), growth(ideal, coin))
      val extra = (frame - best).toDouble / build
      assert(build > 100_000L, s"$coin one build grows by $build bytes")
      assert(
        math.abs(extra) < 0.5,
        f"$coin: the frame makes $extra%+.2f builds more than once a coin"
      )
  }
