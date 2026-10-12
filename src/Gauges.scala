package desk

import scala.math.pow

object Gauges:
  val weights: Vector[Double] = (0 until 10).toVector.map(k => pow(0.7, k.toDouble))

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

  def pearson(xs: Vector[Double], ys: Vector[Double]): Option[Double] =
    val n = xs.size min ys.size
    if n < 3 then None
    else
      val (mx, my) = (xs.take(n).sum / n, ys.take(n).sum / n)
      val sxy = xs.zip(ys).map((x, y) => (x - mx) * (y - my)).sum
      val sxx = xs.take(n).map(x => (x - mx) * (x - mx)).sum
      val syy = ys.take(n).map(y => (y - my) * (y - my)).sum
      if sxx <= 0 || syy <= 0 then None else Some(sxy / math.sqrt(sxx * syy))
