package desk

final case class Comparison(value: Option[Double], count: Int, expected: Int, status: String)
final case class Measure(value: Option[Double], status: String)

object Quant:
  // The latest point is (epoch millisecond, price). The ratio is the latest price over the base.
  // A value that is not finite is unavailable.
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

  // The valid one minute closes by bar start, the last given for each start.
  private[desk] def closes(minute: Vector[Candle]): Map[Long, Double] =
    minute.filter(_.valid).map(c => c.t -> c.c).toMap

  def change(
      samples: Vector[(Long, Double)],
      minute: Vector[Candle],
      window: Long,
      now: Long
  ): Measure = change(samples, closes(minute), window, now)

  // The same measure from closes built once, for a caller that reads several windows.
  def change(
      samples: Vector[(Long, Double)],
      bars: Map[Long, Double],
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
      measure(bars.maxByOption(_._1), 120_000L, now) { latest =>
        bars.get(Math.floorDiv(from, 60_000L) * 60_000L - 60_000L).map(latest / _)
      }

  def distance(
      samples: Vector[(Long, Double)],
      minute: Vector[Candle],
      window: Long,
      now: Long
  ): Measure = distance(samples, closes(minute), window, now)

  // The same measure from closes built once, for a caller that reads several windows.
  def distance(
      samples: Vector[(Long, Double)],
      bars: Map[Long, Double],
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
      measure(bars.maxByOption(_._1), 120_000L, now) { latest =>
        mean(bars.collect { case (t, c) if t >= from => c }, window / 60_000).map(latest / _)
      }

  def compareSamples(
      main: Vector[(Long, Double)],
      ref: Vector[(Long, Double)],
      now: Long
  ): Comparison =
    if main.isEmpty || ref.isEmpty then Comparison(None, 0, 30, "unavailable")
    else if Vector(main, ref).exists(s => now - s.last._1 * 1000 > 5000) then
      Comparison(None, 0, 30, "stale")
    else
      val end = Math.floorDiv(now / 1000, 10L) * 10L
      // A value more than thirty seconds older than its point is missing, not carried flat.
      def at(s: Vector[(Long, Double)], point: Long) =
        s.findLast(_._1 <= point).collect { case (t, p) if point - t <= 30 => p }
      val points =
        Vector.tabulate(31)(k => end - 300 + 10L * k).map(p => at(main, p).zip(at(ref, p)))
      val returns = points.zip(points.drop(1)).collect { case (Some((a0, b0)), Some((a1, b1))) =>
        (math.log(a1 / a0), math.log(b1 / b0))
      }
      if returns.size < 30 then Comparison(None, returns.size, 30, "partial")
      else
        val value = Gauges.pearson(returns.map(_._1), returns.map(_._2))
        Comparison(value, 30, 30, if value.isEmpty then "flat" else "complete")

  private def closed(cs: Vector[Candle], step: Long, now: Long): Vector[Candle] =
    cs.groupBy(_.t)
      .values
      .filter(copies => copies.distinct.size == 1)
      .map(_.head)
      .filter(c =>
        c.t >= 0 && c.t <= now - step && c.t % step == 0 &&
          c.valid
      )
      .toVector
      .sortBy(_.t)

  private def window(cs: Vector[Candle], step: Long, n: Int, now: Long): (Vector[Candle], String) =
    val end = now / step * step - step
    val start = end - (n - 1L) * step
    val selected = closed(cs.filter(c => c.t >= start && c.t <= end), step, now)
    val status =
      if selected.isEmpty then if closed(cs, step, now).isEmpty then "unavailable" else "stale"
      else if selected.last.t < end then "stale"
      else if selected.size != n then "partial"
      else "complete"
    (selected, status)

  def compare(
      main: Vector[Candle],
      ref: Vector[Candle],
      interval: String,
      n: Int,
      now: Long
  ): Comparison =
    require(State.intervals.contains(interval) && n >= 3 && n <= State.seriesCap - 1)
    val step = State.intervalMs(interval)
    val (a, sa) = window(main, step, n + 1, now)
    val (b, sb) = window(ref, step, n + 1, now)
    val lookup = b.map(c => c.t -> c.c).toMap
    val paired = a.filter(c => lookup.contains(c.t))
    val returns = paired.zip(paired.drop(1)).filter((a, b) => b.t - a.t == step)
    val status =
      if sa == "unavailable" || sb == "unavailable" then "unavailable"
      else if sa == "stale" || sb == "stale" then "stale"
      else if returns.size != n then "partial"
      else "complete"
    val value = Option
      .when(status == "complete") {
        Gauges.pearson(
          returns.map((a, b) => math.log(b.c / a.c)),
          returns.map((a, b) => math.log(lookup(b.t) / lookup(a.t)))
        )
      }
      .flatten
    Comparison(
      value,
      returns.size,
      n,
      if status == "complete" && value.isEmpty then "flat" else status
    )
