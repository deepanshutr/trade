package desk

enum OutlookDirection:
  case Up, Down, Mixed

final case class OutlookHorizon(label: String, interval: String, bars: Int):
  require(State.intervals.contains(interval) && bars > 0 && bars <= 1000)
  val periodMs: Long = State.intervalMs(interval) * bars

final case class HistoricalRate(
    hits: Int,
    samples: Int,
    required: Int,
    available: Boolean = true
):
  require(hits >= 0 && samples >= hits && required > 0)

  def percent: Option[Double] =
    Option.when(available && samples >= required)(100.0 * hits / samples)

final case class HorizonOutlook(
    horizon: OutlookHorizon,
    direction: OutlookDirection,
    status: String,
    support: Option[Double],
    resistance: Option[Double],
    target: Option[Double],
    reference: Option[Double],
    at: Option[Long],
    base: HistoricalRate,
    matched: HistoricalRate
)

final case class OutlookView(
    coin: String,
    calculatedAt: Long,
    columns: Vector[HorizonOutlook]
)

object Outlook:
  private val baseMinimum = 100
  private val matchedMinimum = 50
  val horizons: Vector[OutlookHorizon] = Vector(
    OutlookHorizon("LONG", "1h", 24),
    OutlookHorizon("MID", "15m", 16),
    OutlookHorizon("SHORT", "5m", 12)
  )

  private def emptyColumn(
      horizon: OutlookHorizon,
      status: String
  ): HorizonOutlook =
    HorizonOutlook(
      horizon,
      OutlookDirection.Mixed,
      status,
      None,
      None,
      None,
      None,
      None,
      HistoricalRate(0, 0, baseMinimum, available = false),
      HistoricalRate(0, 0, matchedMinimum, available = false)
    )

  def empty(coin: String): OutlookView =
    OutlookView(coin, 0L, horizons.map(emptyColumn(_, "waiting")))

  private def average(cs: Vector[Candle]): Double =
    cs.zipWithIndex.foldLeft(0.0) { case (mean, (c, index)) =>
      mean + (c.c - mean) / (index + 1)
    }

  private def direction(cs: Vector[Candle]): OutlookDirection =
    val fast = average(cs.takeRight(20))
    val slow = average(cs)
    val price = cs.last.c
    if price > fast && fast > slow then OutlookDirection.Up
    else if price < fast && fast < slow then OutlookDirection.Down
    else OutlookDirection.Mixed

  private def column(
      raw: Vector[Candle],
      horizon: OutlookHorizon,
      now: Long
  ): HorizonOutlook =
    val step = State.intervalMs(horizon.interval)
    if raw.size > State.seriesCap then return emptyColumn(horizon, "invalid data")
    if now < step then return emptyColumn(horizon, "unavailable")
    val end = now / step * step
    val closed = raw.filter(c => c.t >= 0 && c.t <= now - step)
    if closed.isEmpty then return emptyColumn(horizon, "unavailable")
    val grouped = closed.groupBy(_.t)
    val points = grouped.collect {
      case (time, copies)
          if time % step == 0 && copies.forall(
            _.valid
          ) && copies.distinct.size == 1 =>
        time -> copies.head
    }
    val invalid = grouped.keySet -- points.keySet
    val start = end - 50L * step
    if invalid.exists(time => time >= start && time < end) then
      return emptyColumn(horizon, "invalid data")
    if !points.contains(end - step) then return emptyColumn(horizon, "stale")

    def window(at: Long, size: Int): Option[Vector[Candle]] =
      val selected =
        Vector.tabulate(size)(i => points.get(at + i.toLong * step))
      Option.when(selected.forall(_.nonEmpty))(selected.map(_.get))

    val current = window(start, 50)
    if current.isEmpty then return emptyColumn(horizon, "partial data")
    val cs = current.get
    val side = direction(cs)
    val support = cs.map(_.l).min
    val resistance = cs.map(_.h).max
    val reference = cs.last.c
    val basic = emptyColumn(horizon, "mixed").copy(
      direction = side,
      support = Some(support),
      resistance = Some(resistance),
      reference = Some(reference),
      at = Some(end)
    )
    if side == OutlookDirection.Mixed then return basic
    val target = if side == OutlookDirection.Up then resistance else support
    val ratio = target / reference
    val directed = basic.copy(target = Some(target))
    if !ratio.isFinite || ratio <= 0 then
      return directed.copy(
        direction = OutlookDirection.Mixed,
        status = "invalid target"
      )
    if ratio == 1.0 then
      return directed.copy(
        direction = OutlookDirection.Mixed,
        status = "at level"
      )

    val period = horizon.periodMs
    val aligned = closed.filter(_.t % step == 0)
    val oldest = aligned.map(_.t).min
    val lastAnchor = (end / period - 1L) * period
    if lastAnchor < 50L * step || oldest > lastAnchor - 50L * step then
      return directed.copy(status = "limited history")
    val firstPossible = oldest + 50L * step
    val firstAnchor = firstPossible + (period - firstPossible % period) % period
    if firstAnchor > lastAnchor then return directed.copy(status = "limited history")
    val expected = (lastAnchor - firstAnchor) / period + 1L
    if expected > Int.MaxValue then
      return directed.copy(
        direction = OutlookDirection.Mixed,
        status = "invalid data"
      )

    var samples = 0
    var hits = 0
    var matches = 0
    var matchedHits = 0
    // Iterating observed anchors keeps sparse histories bounded by the input size.
    points.keysIterator.foreach { time =>
      val anchor = time + step
      if anchor >= firstAnchor && anchor <= lastAnchor && anchor % period == 0
      then
        for
          past <- window(anchor - 50L * step, 50)
          future <- window(anchor, horizon.bars)
        do
          val level = past.last.c * ratio
          if level > 0 && level.isFinite then
            val touched =
              if side == OutlookDirection.Up then future.exists(_.h >= level)
              else future.exists(_.l <= level)
            samples += 1
            if touched then hits += 1
            if direction(past) == side then
              matches += 1
              if touched then matchedHits += 1
    }
    val covered = samples > 0 && samples.toLong * 10 >= expected * 9
    val base = HistoricalRate(hits, samples, baseMinimum, covered)
    val matched = HistoricalRate(matchedHits, matches, matchedMinimum, covered)
    val status =
      if !covered then "gapped history"
      else if base.percent.isEmpty || matched.percent.isEmpty then "limited history"
      else "ready"
    directed.copy(
      status = status,
      base = base,
      matched = matched
    )

  def compute(snapshot: Snapshot, now: Long): OutlookView =
    OutlookView(
      snapshot.coin,
      now,
      horizons.map { horizon =>
        column(
          snapshot.candles.getOrElse(horizon.interval, Vector.empty),
          horizon,
          now
        )
      }
    )

  private def disabled(column: HorizonOutlook, status: String): HorizonOutlook =
    column.copy(
      direction = OutlookDirection.Mixed,
      status = status,
      base = column.base.copy(available = false),
      matched = column.matched.copy(available = false)
    )

  private def freshPrice(
      snapshot: Snapshot,
      now: Long
  ): Either[String, Double] =
    snapshot.book.toRight("waiting for price").flatMap { book =>
      if book.time < 0 || book.time > now || now - book.time > 5000L then Left("stale price")
      else
        val mid = for
          bid <- book.bids.headOption.flatMap(_.px.toDoubleOption)
          ask <- book.asks.headOption.flatMap(_.px.toDoubleOption)
          if bid > 0 && ask > 0 && bid.isFinite && ask.isFinite && bid <= ask
        yield bid / 2.0 + ask / 2.0
        mid.toRight("invalid price")
    }

  private[desk] def targetTouched(
      value: HorizonOutlook,
      snapshot: Snapshot,
      now: Long
  ): Boolean =
    val step = State.intervalMs(value.horizon.interval)
    val current = snapshot.candles
      .get(value.horizon.interval)
      .flatMap(_.lastOption)
      .filter(c => value.at.contains(c.t))
    !value.at.exists(_ != now / step * step) && !current.exists(c => !c.valid) &&
    freshPrice(snapshot, now).exists(targetTouched(value, _, current))

  private def targetTouched(
      value: HorizonOutlook,
      price: Double,
      current: Option[Candle]
  ): Boolean = value.target.exists { target =>
    value.direction match
      case OutlookDirection.Up    => price >= target || current.exists(_.h >= target)
      case OutlookDirection.Down  => price <= target || current.exists(_.l <= target)
      case OutlookDirection.Mixed => false
  }

  // This is safe to call while drawing. It reads at most one live candle per column.
  def visible(view: OutlookView, snapshot: Snapshot, now: Long): OutlookView =
    if view.coin != snapshot.coin then
      return empty(snapshot.coin).copy(
        calculatedAt = view.calculatedAt,
        columns = horizons.map(emptyColumn(_, "market changed"))
      )
    if view.calculatedAt < 0 || now < view.calculatedAt || now - view.calculatedAt > 10000L
    then return view.copy(columns = view.columns.map(disabled(_, "stale")))
    freshPrice(snapshot, now) match
      case Left(status) =>
        view.copy(columns = view.columns.map(disabled(_, status)))
      case Right(price) =>
        view.copy(columns = view.columns.map { value =>
          val step = State.intervalMs(value.horizon.interval)
          if value.at.exists(_ != now / step * step) then disabled(value, "stale")
          else
            val current = snapshot.candles
              .get(value.horizon.interval)
              .flatMap(_.lastOption)
              .filter(c => value.at.contains(c.t))
            if current.exists(c => !c.valid) then disabled(value, "invalid data")
            else if targetTouched(value, price, current) then disabled(value, "reached")
            else value
        })
