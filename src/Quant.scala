package desk

final case class MarketAnalysis(
    interval: String,
    count: Int,
    expected: Int,
    status: String,
    fast: Option[Double],
    slow: Option[Double],
    support: Option[Double],
    resistance: Option[Double],
    at: Option[Long]
)

final case class Comparison(value: Option[Double], count: Int, expected: Int, status: String)
final case class CrossingStudy(samples: Int, mean: Option[Double], positive: Int)

object Quant:
  private def closed(cs: Vector[Candle], step: Long, now: Long): Vector[Candle] =
    cs.groupBy(_.t)
      .values
      .filter(copies => copies.distinct.size == 1)
      .map(_.head)
      .filter(c =>
        c.t >= 0 && c.t <= now - step && c.t % step == 0 &&
          Vector(c.o, c.h, c.l, c.c).forall(v => v > 0 && v.isFinite) &&
          c.v >= 0 && c.v.isFinite && c.l <= c.o && c.o <= c.h &&
          c.l <= c.c && c.c <= c.h
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

  def measure(cs: Vector[Candle], interval: String, now: Long): MarketAnalysis =
    require(State.intervals.contains(interval))
    val step = State.intervalMs(interval)
    val (slow, status) = window(cs, step, 50, now)
    val (fast, fastStatus) = window(cs, step, 20, now)
    MarketAnalysis(
      interval,
      slow.size,
      50,
      status,
      Option.when(fastStatus == "complete")(fast.map(_.c).sum / 20),
      Option.when(status == "complete")(slow.map(_.c).sum / 50),
      Option.when(status == "complete")(slow.map(_.l).min),
      Option.when(status == "complete")(slow.map(_.h).max),
      slow.lastOption.map(_.t)
    )

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

  def crossings(
      candles: Vector[Candle],
      interval: String,
      fast: Int,
      slow: Int,
      ahead: Int,
      now: Long,
      fee: Double
  ): CrossingStudy =
    require(
      State.intervals.contains(interval) && fast > 0 && slow > fast &&
        slow <= 1000 && ahead > 0 && ahead <= 1000 && fee >= 0 && fee <= 0.1 && fee.isFinite
    )
    val step = State.intervalMs(interval)
    val cs = closed(candles, step, now)
    def difference(at: Int): Double =
      cs.slice(at - fast + 1, at + 1).map(_.c).sum / fast -
        cs.slice(at - slow + 1, at + 1).map(_.c).sum / slow
    val outcomes = Vector.newBuilder[Double]
    var nextEntry = 0
    for i <- slow until (cs.size - ahead - 1) do
      val span = cs.slice(i - slow, i + ahead + 2)
      val continuous = span.zip(span.drop(1)).forall((a, b) => b.t - a.t == step)
      if continuous && i >= nextEntry then
        val previous = difference(i - 1)
        val current = difference(i)
        val direction =
          if previous <= 0 && current > 0 then 1
          else if previous >= 0 && current < 0 then -1
          else 0
        if direction != 0 then
          val entry = cs(i + 1).o
          val exit = cs(i + 1 + ahead).c
          outcomes += direction * (exit - entry) / entry - fee * (1 + exit / entry)
          nextEntry = i + ahead + 1
    val results = outcomes.result()
    CrossingStudy(results.size, Gauges.mean(results), results.count(_ > 0))
