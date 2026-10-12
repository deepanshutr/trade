package desk

class MeasuresSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val N = now / 1000
  private def bar(t: Long, c: Double) = Candle(t, c, c, c, c, 1.0)
  private def g(k: Int): Long = N - 300 + 10L * k
  private val noBars = Vector.empty[Candle]
  private val unavailable = Measure(None, "unavailable")
  private val rising = Vector.tabulate(31)(k => (g(k), 100.0 + k))
  private val interleaved =
    Vector.tabulate(31)(k => Vector((g(k) - 7, 500.0), (g(k) - 3, 200.0 + 2 * k))).flatten
  private val late = Vector.tabulate(31)(k => (g(k) - 6, 1.0 + k))

  private def complete(m: Measure, v: Double): Unit =
    assertEquals(m.status, "complete", m)
    assertEqualsDouble(m.value.getOrElse(Double.NaN), v, 1e-9)

  test("change over 5m reads the sample at or before now minus the window") {
    val samples = Vector((N - 400, 50.0), (N - 302, 100.0), (N - 299, 101.0), (N - 1, 102.0))
    complete(Quant.change(samples, noBars, 300_000L, now), 2.0)
  }

  test("the base sits within five seconds of now minus the window") {
    complete(Quant.change(Vector((N - 305, 100.0), (N - 1, 102.0)), noBars, 300_000L, now), 2.0)
    assertEquals(
      Quant.change(Vector((N - 306, 100.0), (N - 1, 102.0)), noBars, 300_000L, now),
      unavailable
    )
  }

  test("a latest sample older than five seconds is stale") {
    complete(Quant.change(Vector((N - 302, 100.0), (N - 5, 102.0)), noBars, 300_000L, now), 2.0)
    assertEquals(
      Quant.change(Vector((N - 302, 100.0), (N - 6, 102.0)), noBars, 300_000L, now),
      Measure(None, "stale")
    )
    assertEquals(Quant.change(Vector.empty, noBars, 300_000L, now), unavailable)
  }

  test("15m reads the samples") {
    complete(Quant.change(Vector((N - 902, 200.0), (N, 201.0)), noBars, 900_000L, now), 0.5)
  }

  test("1h, 4h and 24h read the bar that ends at or before now minus the window") {
    val samples = Vector((N - 3602, 100.0), (N, 102.0))
    val hour = Vector(bar(now - 3_660_000L, 200.0), bar(now - 3_600_000L, 999.0), bar(now, 210.0))
    complete(Quant.change(samples, hour, 3_600_000L, now), 5.0)
    assertEquals(Quant.change(samples, hour.drop(1), 3_600_000L, now), unavailable)
    val four = Vector(bar(now - 14_460_000L, 100.0), bar(now, 103.0))
    complete(Quant.change(samples, four, 14_400_000L, now), 3.0)
    val day = Vector(bar(now - 86_460_000L, 100.0), bar(now - 60_000L, 94.0))
    complete(Quant.change(samples, day, 86_400_000L, now), -6.0)
  }

  test("a latest bar older than two minutes is stale") {
    val samples = Vector((N - 3602, 100.0), (N, 102.0))
    val fresh = Vector(bar(now - 3_660_000L, 200.0), bar(now - 120_000L, 210.0))
    complete(Quant.change(samples, fresh, 3_600_000L, now), 5.0)
    val old = Vector(bar(now - 3_660_000L, 200.0), bar(now - 120_001L, 210.0))
    assertEquals(Quant.change(samples, old, 3_600_000L, now), Measure(None, "stale"))
  }

  test("distance over 5m from the mean of the samples, with half coverage") {
    val samples = Vector.tabulate(149)(i => (N - 149 + i, 99.0)) :+ ((N, 249.0))
    complete(Quant.distance(samples, noBars, 300_000L, now), 149.0)
    assertEquals(Quant.distance(samples.drop(1), noBars, 300_000L, now), unavailable)
  }

  test("distance over 4h from the one minute closes") {
    val bars = Vector.tabulate(119)(k => bar(now - 60_000L * (k + 1), 99.0)) :+ bar(now, 219.0)
    complete(Quant.distance(Vector.empty, bars, 14_400_000L, now), 119.0)
    assertEquals(Quant.distance(Vector.empty, bars.drop(1), 14_400_000L, now), unavailable)
  }

  test("compareSamples pairs both series on a ten second grid") {
    val paired = Quant.compareSamples(rising, interleaved, now)
    assertEquals((paired.count, paired.expected, paired.status), (30, 30, "complete"))
    assertEqualsDouble(paired.value.getOrElse(Double.NaN), 1.0, 1e-9)
    val main = Vector.tabulate(31)(k => (g(k), 100.0 + k + k % 3))
    val ref = Vector.tabulate(31)(k => (g(k), 100.0 + 2 * k))
    def returns(prices: Vector[Double]) = prices.zip(prices.drop(1)).map((a, b) => math.log(b / a))
    val expected = Gauges.pearson(returns(main.map(_._2)), returns(ref.map(_._2)))
    val other = Quant.compareSamples(main, ref, now)
    assertEquals((other.count, other.expected, other.status), (30, 30, "complete"))
    assertEqualsDouble(
      other.value.getOrElse(Double.NaN),
      expected.getOrElse(Double.NaN),
      1e-12
    )
  }

  test("partial, stale, unavailable and flat") {
    val gappy = Vector.tabulate(26)(i => (g(i + 5), 105.0 + i))
    assertEquals(Quant.compareSamples(gappy, interleaved, now), Comparison(None, 25, 30, "partial"))
    assertEquals(Quant.compareSamples(rising, late, now), Comparison(None, 0, 30, "stale"))
    assertEquals(
      Quant.compareSamples(rising, Vector.empty, now),
      Comparison(None, 0, 30, "unavailable")
    )
    val flat = Vector.tabulate(31)(k => (g(k), 100.0))
    assertEquals(Quant.compareSamples(flat, interleaved, now), Comparison(None, 30, 30, "flat"))
  }

  test("compareSamples checks the main series like the reference") {
    assertEquals(Quant.compareSamples(late, interleaved, now), Comparison(None, 0, 30, "stale"))
    assertEquals(
      Quant.compareSamples(Vector.empty, late, now),
      Comparison(None, 0, 30, "unavailable")
    )
  }

  test("only valid bars count, the last given per minute, the latest by time") {
    val samples = Vector((N - 3602, 100.0), (N, 102.0))
    val broken = Candle(now, 999.0, 1.0, 2.0, 999.0, 1.0)
    val withBroken = Vector(bar(now - 3_660_000L, 200.0), bar(now - 60_000L, 210.0), broken)
    complete(Quant.change(samples, withBroken, 3_600_000L, now), 5.0)
    val twice = Vector(bar(now - 3_660_000L, 150.0), bar(now - 3_660_000L, 200.0), bar(now, 210.0))
    complete(Quant.change(samples, twice, 3_600_000L, now), 5.0)
    val reversed = Vector(bar(now, 210.0), bar(now - 3_660_000L, 200.0))
    complete(Quant.change(samples, reversed, 3_600_000L, now), 5.0)
    assertEquals(Quant.change(samples, noBars, 3_600_000L, now), unavailable)
    assertEquals(Quant.change(samples, Vector(broken), 3_600_000L, now), unavailable)
  }

  test("the edges of each window") {
    val onEdge = Vector((N - 302, 50.0), (N - 300, 100.0), (N - 1, 102.0))
    complete(Quant.change(onEdge, noBars, 300_000L, now), 2.0)
    val samples =
      ((N - 300, 1.0) +: Vector.tabulate(149)(i => (N - 149 + i, 99.0))) :+ ((N, 249.0))
    complete(Quant.distance(samples, noBars, 300_000L, now), 149.0)
    val inside = Vector.tabulate(118)(k => bar(now - 60_000L * (k + 1), 99.0))
    val bars = Vector(bar(now - 14_460_000L, 1.0), bar(now - 14_400_000L, 99.0)) ++ inside :+
      bar(now, 219.0)
    complete(Quant.distance(Vector.empty, bars, 14_400_000L, now), 119.0)
    val hour = Vector(bar(now - 3_660_000L, 200.0), bar(now, 210.0))
    complete(Quant.change(Vector.empty, hour, 3_600_000L, now + 30_000L), 5.0)
  }

  test("a 230 second gap in a series is partial, not carried flat") {
    val gap = rising.filter((t, _) => t <= g(5) || t >= g(28))
    assertEquals(Quant.compareSamples(gap, interleaved, now), Comparison(None, 10, 30, "partial"))
  }

  test("a grid value may be thirty seconds older than its point, not thirty one") {
    def hole(age: Int) =
      (rising.filter((t, _) => t < g(15) - age || t > g(15)) :+ ((g(15) - age, 115.0)))
        .sortBy(_._1)
    for age <- Vector(25, 30) do
      val c = Quant.compareSamples(hole(age), interleaved, now)
      assertEquals((c.count, c.status, c.value.isDefined), (30, "complete", true), age)
    assertEquals(
      Quant.compareSamples(hole(31), interleaved, now),
      Comparison(None, 28, 30, "partial")
    )
  }

  test("a ratio that is not finite is unavailable") {
    assertEquals(Quant.distance(Vector((N - 1, 100.0)), noBars, 999L, now), unavailable)
  }

  test("distance is stale by the same rule as the change") {
    val samples = Vector.tabulate(149)(i => (N - 149 + i, 99.0)) :+ ((N, 249.0))
    complete(Quant.distance(samples, noBars, 300_000L, now + 5000L), 149.0)
    assertEquals(Quant.distance(samples, noBars, 300_000L, now + 5001L), Measure(None, "stale"))
    val bars = Vector.tabulate(119)(k => bar(now - 60_000L * (k + 1), 99.0)) :+ bar(now, 219.0)
    complete(Quant.distance(Vector.empty, bars, 14_400_000L, now + 120_000L), 119.0)
    assertEquals(
      Quant.distance(Vector.empty, bars, 14_400_000L, now + 120_001L),
      Measure(None, "stale")
    )
  }

  test("1h distance reads the one minute closes") {
    val samples = Vector((N - 3602, 100.0), (N, 102.0))
    val bars = Vector.tabulate(29)(k => bar(now - 60_000L * (k + 1), 99.0)) :+ bar(now, 129.0)
    complete(Quant.distance(samples, bars, 3_600_000L, now), 29.0)
  }

  test("stale is checked before the base and the coverage") {
    val stale = Measure(None, "stale")
    assertEquals(Quant.change(Vector((N - 6, 102.0)), noBars, 300_000L, now), stale)
    assertEquals(Quant.distance(Vector((N - 6, 102.0)), noBars, 300_000L, now), stale)
    val old = Vector(bar(now - 180_000L, 210.0))
    assertEquals(Quant.change(Vector.empty, old, 3_600_000L, now), stale)
    assertEquals(Quant.distance(Vector.empty, old, 14_400_000L, now), stale)
  }

  test("the grid ends on the ten second floor of now") {
    val ref = Vector.tabulate(31)(k => Vector((g(k), 200.0 + 2 * k), (g(k) + 3, 500.0))).flatten
    val c = Quant.compareSamples(rising, ref, now + 4000L)
    assertEquals((c.count, c.expected, c.status), (30, 30, "complete"))
    assertEqualsDouble(c.value.getOrElse(Double.NaN), 1.0, 1e-9)
  }

  test("a series exactly five seconds old is fresh for the comparison") {
    val c = Quant.compareSamples(rising, interleaved, now + 2000L)
    assertEquals((c.count, c.status), (30, "complete"))
    assertEquals(
      Quant.compareSamples(rising, interleaved, now + 2001L),
      Comparison(None, 0, 30, "stale")
    )
  }

  test("twenty nine returns are partial") {
    assertEquals(
      Quant.compareSamples(rising.drop(1), interleaved, now),
      Comparison(None, 29, 30, "partial")
    )
  }

  test("samples before the 31 point grid are not paired") {
    val main = (g(0) - 10, 50.0) +: rising
    val ref = (g(0) - 10, 400.0) +: Vector.tabulate(31)(k => (g(k), 200.0 + 2 * k))
    val c = Quant.compareSamples(main, ref, now)
    assertEquals((c.count, c.status), (30, "complete"))
    assertEqualsDouble(c.value.getOrElse(Double.NaN), 1.0, 1e-9)
  }

  test("a stale series with gaps is stale, not partial") {
    assertEquals(
      Quant.compareSamples(late.drop(5), interleaved, now),
      Comparison(None, 0, 30, "stale")
    )
  }

  test("distance finds the latest bar by time in any set of minutes") {
    for j <- 0 until 8 do
      val at = now + 60_000L * j
      val bars = Vector.tabulate(29)(k => bar(at - 60_000L * (k + 1), 99.0)) :+ bar(at, 129.0)
      complete(Quant.distance(Vector.empty, bars.reverse, 3_600_000L, at), 29.0)
  }

  test("a gap in the reference is partial too") {
    val ref = interleaved.filter((t, _) => t <= g(14) - 3 || t >= g(18) - 7)
    assertEquals(Quant.compareSamples(rising, ref, now), Comparison(None, 28, 30, "partial"))
  }

  test("an infinite ratio is unavailable") {
    assertEquals(
      Quant.change(Vector((N - 300, 1e-300), (N, 1e300)), noBars, 300_000L, now),
      Measure(None, "unavailable")
    )
  }

  // The change and distance as they were before the prebuilt closes, kept as the reference.
  private object Prior:
    private def measure(latest: Option[(Long, Double)], limit: Long, now: Long)(
        ratio: Double => Option[Double]
    ): Measure =
      latest match
        case None                            => Measure(None, "unavailable")
        case Some((t, _)) if now - t > limit => Measure(None, "stale")
        case Some((_, price))                =>
          ratio(price).map(r => (r - 1) * 100).filter(_.isFinite) match
            case Some(v) => Measure(Some(v), "complete")
            case None    => Measure(None, "unavailable")

    private def lastSample(samples: Vector[(Long, Double)]): Option[(Long, Double)] =
      samples.lastOption.map((t, price) => (t * 1000, price))

    private def closes(minute: Vector[Candle]): Map[Long, Double] =
      minute.filter(_.valid).map(c => c.t -> c.c).toMap

    def change(
        samples: Vector[(Long, Double)],
        minute: Vector[Candle],
        window: Long,
        now: Long
    ): Measure =
      val from = now - window
      if window < 3_600_000L then
        measure(lastSample(samples), 5000, now) { latest =>
          samples.findLast(_._1 * 1000 <= from).collect {
            case (t, base) if from - t * 1000 <= 5000 => latest / base
          }
        }
      else
        val bars = closes(minute)
        measure(bars.maxByOption(_._1), 120_000L, now) { latest =>
          bars.get(Math.floorDiv(from, 60_000L) * 60_000L - 60_000L).map(latest / _)
        }

    def distance(
        samples: Vector[(Long, Double)],
        minute: Vector[Candle],
        window: Long,
        now: Long
    ): Measure =
      val from = now - window
      def mean(prices: Iterable[Double], expected: Long): Option[Double] =
        Option.unless(prices.size * 2L < expected)(prices.sum / prices.size)
      if window < 3_600_000L then
        measure(lastSample(samples), 5000, now) { latest =>
          mean(samples.collect { case (t, p) if t * 1000 > from => p }, window / 1000)
            .map(latest / _)
        }
      else
        val bars = closes(minute)
        measure(bars.maxByOption(_._1), 120_000L, now) { latest =>
          mean(bars.collect { case (t, c) if t >= from => c }, window / 60_000).map(latest / _)
        }

  private def broken(t: Long, p: Double, kind: Int): Candle = kind match
    case 0 => Candle(t, p, p * 0.99, p * 1.01, p, 1.0)
    case 1 => Candle(t, p, p, p, Double.NaN, 1.0)
    case 2 => Candle(t, p, Double.PositiveInfinity, p, p, 1.0)
    case 3 => Candle(t, 0.0, p, 0.0, p, 1.0)
    case _ => Candle(t, p, p, p, p, -1.0)

  // Up to 1600 minutes ending near now: gaps, invalid bars, a repeated start with another close,
  // a start off the minute, extreme prices, a latest bar stale or ahead of now, in any order.
  private def minuteSeries(r: scala.util.Random): Vector[Candle] =
    val n = r.nextInt(4) match
      case 0 => if r.nextBoolean() then 0 else r.nextInt(30)
      case 1 => 1400 + r.nextInt(200)
      case _ => r.nextInt(1600)
    val end = now / 60_000L * 60_000L - 60_000L * (r.nextInt(6) - 1)
    val (gap, bad, twice) = (r.nextDouble() * 0.3, r.nextDouble() * 0.2, r.nextDouble() * 0.2)
    val scale = if r.nextInt(10) == 0 then 1e300 else 1.0
    val prices = Vector.fill(n)(math.exp(r.nextGaussian() * 0.01)).scanLeft(100.0)(_ * _).tail
    val bars = prices.zipWithIndex.flatMap { (price, k) =>
      val p = if r.nextInt(100) == 0 then price * scale else price / scale
      val t = end - 60_000L * (n - 1 - k) + (if r.nextInt(50) == 0 then r.nextInt(60_000) else 0)
      val one = if r.nextDouble() < bad then broken(t, p, r.nextInt(5)) else bar(t, p)
      if r.nextDouble() < gap then Vector.empty
      else if r.nextDouble() < twice then
        if r.nextBoolean() then Vector(one, bar(t, p * 1.01)) else Vector(bar(t, p * 0.99), one)
      else Vector(one)
    }
    if r.nextInt(4) == 0 then r.shuffle(bars) else bars

  // Up to 3800 one second samples ending near now, with gaps.
  private def sampleSeries(r: scala.util.Random): Vector[(Long, Double)] =
    val n = r.nextInt(4) match
      case 0 => if r.nextBoolean() then 0 else r.nextInt(30)
      case 1 => 3600 + r.nextInt(200)
      case _ => r.nextInt(1300)
    val (lag, keep) = (r.nextInt(12), 1 - r.nextDouble() * 0.6)
    Vector
      .tabulate(n)(i => (N - lag - (n - 1 - i), 100.0 + r.nextGaussian()))
      .filter(_ => r.nextDouble() < keep)

  test("the measures from prebuilt closes equal the measures from the bars, bit for bit") {
    val r = new scala.util.Random(5L)
    val windows = Vector(300_000L, 900_000L, 3_599_999L, 3_600_000L, 14_400_000L, 86_400_000L)
    def key(m: Measure) = (m.status, m.value.map(java.lang.Double.doubleToRawLongBits))
    val results = for
      i <- 0 until 250
      minute = minuteSeries(r)
      samples = sampleSeries(r)
      bars = Quant.closes(minute)
      at <- Vector(now, now + 120_000L, now + r.between(-60_000L, 300_000L))
      w <- windows
      (name, prior, old, prebuilt) <- Vector(
        (
          "change",
          Prior.change(samples, minute, w, at),
          Quant.change(samples, minute, w, at),
          Quant.change(samples, bars, w, at)
        ),
        (
          "distance",
          Prior.distance(samples, minute, w, at),
          Quant.distance(samples, minute, w, at),
          Quant.distance(samples, bars, w, at)
        )
      )
    yield ((name, w), s"series $i at now ${at - now}", prior, old, prebuilt)
    val wrong = results.filter((_, _, a, b, c) => key(a) != key(b) || key(a) != key(c))
    assert(wrong.isEmpty, s"${wrong.size} of ${results.size} differ, first ${wrong.headOption}")
    // Each measure over each window meets every status, so the series reach every branch.
    val statuses = results.groupMapReduce(_._1)(x => Set(x._3.status))(_ ++ _)
    for name <- Vector("change", "distance"); w <- windows do
      assertEquals(statuses((name, w)), Set("complete", "stale", "unavailable"), (name, w))
  }
