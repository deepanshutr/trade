package desk

import scala.math.pow

object Gauges:
  val weights: Vector[Double] = (0 until 10).toVector.map(k => pow(0.7, k.toDouble))
  val bookLenses: Vector[(String, Long)] =
    Vector(("now", 0L), ("10s", 10L), ("30s", 30L), ("1m", 60L), ("5m", 300L), ("15m", 900L))
  val tapeLenses: Vector[(String, Long)] =
    Vector(("10s", 10L), ("1m", 60L), ("5m", 300L), ("15m", 900L), ("1h", 3600L), ("4h", 14400L))
  val horizons: Vector[(String, String, Int)] =
    Vector(
      ("15m", "1m", 15),
      ("1h", "1m", 60),
      ("4h", "5m", 48),
      ("1d", "15m", 96),
      ("1w", "1h", 168),
      ("1M", "4h", 180)
    )

  private def weighted(levels: Vector[Level]): Double =
    levels.take(10).zip(weights).map((l, w) => l.sz.toDouble * w).sum

  def imbalance(b: Book): (Double, Double) =
    val q = (b.bids.headOption, b.asks.headOption) match
      case (Some(bb), Some(aa)) =>
        val (x, y) = (bb.sz.toDouble, aa.sz.toDouble)
        if x + y == 0 then 0.0 else (x - y) / (x + y)
      case (Some(_), None) => 1.0
      case (None, Some(_)) => -1.0
      case _               => 0.0
    val (wb, wa) = (weighted(b.bids), weighted(b.asks))
    (q, if wb + wa == 0 then 0.0 else (wb - wa) / (wb + wa))

  private def ratio(buy: Double, sell: Double, n: Int): (Option[Double], Int) =
    val total = buy + sell
    (if n == 0 || total == 0 then None else Some((buy - sell) / total), n)

  def pearson(xs: Vector[Double], ys: Vector[Double]): Option[Double] =
    val n = xs.size min ys.size
    if n < 3 then None
    else
      val (mx, my) = (xs.take(n).sum / n, ys.take(n).sum / n)
      val sxy = xs.zip(ys).map((x, y) => (x - mx) * (y - my)).sum
      val sxx = xs.take(n).map(x => (x - mx) * (x - mx)).sum
      val syy = ys.take(n).map(y => (y - my) * (y - my)).sum
      if sxx <= 0 || syy <= 0 then None else Some(sxy / math.sqrt(sxx * syy))

  def track(v: Option[Double], h: Int, style: String): String =
    val base = "." * h + "|" + "." * h
    v.fold(base) { x =>
      val at = h + Screen.round(x * h).toInt.max(-h).min(h)
      val mark = if style.isEmpty then Chart.FULL else style + Chart.FULL + Screen.reset
      base.take(at) + mark + base.drop(at + 1)
    }

  def mean(xs: Vector[Double]): Option[Double] = if xs.isEmpty then None else Some(xs.sum / xs.size)

  def fmt(v: Option[Double], letter: Char): String = v.fold(s"$letter   --")(x => f"$letter$x%+.2f")

  private def label(s: Snapshot, name: String, secs: Long, now: Long, width: Int = 4): String =
    val text = Chart.rjust(name, width)
    if secs * 1000 > now - s.since then Screen.dim + text + Screen.reset else text

  def tapeStats(
      tape: Vector[Trade],
      now: Long,
      lenses: Vector[Long]
  ): Vector[(Option[Double], Int)] =
    val n = lenses.size
    val cuts = lenses.map(secs => now - secs * 1000).toArray
    val buy = Array.fill(n)(0.0)
    val sell = Array.fill(n)(0.0)
    val count = Array.fill(n)(0)
    tape.foreach { x =>
      val sz = x.displaySize
      val isBuy = x.side == Side.Buy
      var k = 0
      while k < n do
        if x.time > cuts(k) then
          if isBuy then buy(k) += sz else sell(k) += sz
          count(k) += 1
        k += 1
    }
    (0 until n).toVector.map(k => ratio(buy(k), sell(k), count(k)))

  private def sideStyle(v: Option[Double], st: Style): String =
    v.fold("")(x => if x >= 0 then st.bid else st.ask)

  def bookRow(s: Snapshot, i: Int, now: Long, h: Int, st: Style, labelWidth: Int = 4): String =
    val (name, secs) = bookLenses(i)
    val set =
      if secs == 0 then s.book.toVector
      else
        val inWindow = s.books.filter(_._1 >= now / 1000 - secs).map(_._2)
        s.book.fold(inWindow)(b => if inWindow.contains(b) then inWindow else inWindow :+ b)
    val readings = set.map(imbalance)
    val d = mean(readings.map(_._2))
    val q = mean(readings.map(_._1))
    s"BOOK ${label(s, name, secs, now, labelWidth)} ${track(d, h, sideStyle(d, st))} ${fmt(d, 'D')} ${fmt(q, 'Q')} "

  def compactTapeRow(s: Snapshot, i: Int, now: Long, h: Int, st: Style): String =
    val (name, secs) = tapeLenses(i)
    val (value, count) = tapeStats(s.tape, now, Vector(secs)).head
    s"TAPE ${label(s, name, secs, now, 3)} ${track(value, h, sideStyle(value, st))} ${fmt(value, 'F')} n$count"
