package desk

// One minute of the selected market's book: the sum and count of depth weighted imbalances.
final case class MinuteBook(minute: Long, sum: Double, count: Int)
// One minute of the selected market's tape: bought and sold size.
final case class MinuteTape(minute: Long, buy: Double, sell: Double)

object Shares:
  val minutes: Int = 1440

  private def minuteOf(time: Long): Long = Math.floorDiv(time, 60_000L)

  def addBook(ring: Vector[MinuteBook], now: Long, imbalance: Double): Vector[MinuteBook] =
    val minute = minuteOf(now)
    ring.lastOption match
      case _ if !imbalance.isFinite            => ring
      case Some(last) if last.minute > minute  => ring
      case Some(last) if last.minute == minute =>
        ring.updated(ring.size - 1, last.copy(sum = last.sum + imbalance, count = last.count + 1))
      case _ =>
        (ring :+ MinuteBook(minute, imbalance, 1)).dropWhile(_.minute <= minute - minutes)

  def addTrades(ring: Vector[MinuteTape], trades: Vector[Trade]): Vector[MinuteTape] =
    val added = trades.foldLeft(ring) { (r, t) =>
      val size = t.displaySize
      if !size.isFinite || size < 0 then r
      else
        val minute = minuteOf(t.time)
        val (buy, sell) = if t.side == Side.Buy then (size, 0.0) else (0.0, size)
        val at = r.lastIndexWhere(_.minute <= minute)
        if at >= 0 && r(at).minute == minute then
          r.updated(at, r(at).copy(buy = r(at).buy + buy, sell = r(at).sell + sell))
        else r.patch(at + 1, Vector(MinuteTape(minute, buy, sell)), 0)
    }
    added.lastOption.fold(added)(last => added.dropWhile(_.minute <= last.minute - minutes))

  // The book ring's minutes inside the window, when they cover at least half of it.
  private def observed(s: Snapshot, window: Long, now: Long): Option[(Long, Long)] =
    val (from, to) = (minuteOf(now - window) + 1, minuteOf(now))
    val n = s.bookMinutes.count(b => b.count > 0 && b.minute >= from && b.minute <= to)
    Option.when(2L * n >= window / 60_000L)((from, to))

  private def percent(d: Double): Double = (d + 1) / 2 * 100

  def book(s: Snapshot, window: Long, now: Long): Option[Double] =
    observed(s, window, now)
      .map { (from, to) =>
        val in = s.bookMinutes.filter(b => b.minute >= from && b.minute <= to)
        percent(in.map(_.sum).sum / in.map(_.count.toDouble).sum)
      }
      .filter(_.isFinite)

  def tape(s: Snapshot, window: Long, now: Long): Option[Double] =
    observed(s, window, now).flatMap { (from, to) =>
      val in = s.tapeMinutes.filter(m => m.minute >= from && m.minute <= to)
      val (buy, sell) = (in.map(_.buy).sum, in.map(_.sell).sum)
      val total = buy + sell
      Option.when(total > 0 && total.isFinite)(percent((buy - sell) / total))
    }
