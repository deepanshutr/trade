package desk

final case class Comparison(value: Option[Double], count: Int, expected: Int, status: String)

object Quant:
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
