package desk

import scala.math.pow

object Gauges:
  val weights: Vector[Double] = (0 until 10).toVector.map(k => pow(0.7, k.toDouble))
  val bookLenses: Vector[(String, Long)] =
    Vector(("now", 0L), ("10s", 10L), ("30s", 30L), ("1m", 60L), ("5m", 300L), ("15m", 900L))
  val tapeLenses: Vector[(String, Long)] =
    Vector(("10s", 10L), ("1m", 60L), ("5m", 300L), ("15m", 900L), ("1h", 3600L), ("4h", 14400L))

  private def dollars(l: Level): Double = l.px.toDouble * l.sz.toDouble

  private def weighted(levels: Vector[Level], keep: Level => Boolean): Double =
    levels.take(10).zip(weights).map((l, w) => if keep(l) then l.sz.toDouble * w else 0.0).sum

  def imbalance(b: Book): (Double, Double) =
    val q = (b.bids.headOption, b.asks.headOption) match
      case (Some(bb), Some(aa)) =>
        val (x, y) = (bb.sz.toDouble, aa.sz.toDouble)
        if x + y == 0 then 0.0 else (x - y) / (x + y)
      case (Some(_), None) => 1.0
      case (None, Some(_)) => -1.0
      case _               => 0.0
    val (wb, wa) = (weighted(b.bids, _ => true), weighted(b.asks, _ => true))
    (q, if wb + wa == 0 then 0.0 else (wb - wa) / (wb + wa))

  def imbalanceBig(b: Book, t: Double): Option[Double] =
    val (wb, wa) = (weighted(b.bids, dollars(_) >= t), weighted(b.asks, dollars(_) >= t))
    if wb + wa == 0 then None else Some((wb - wa) / (wb + wa))

  def bigCounts(b: Book, t: Double): (Int, Int) =
    (b.bids.count(dollars(_) >= t), b.asks.count(dollars(_) >= t))

  def flow(trades: Vector[Trade]): (Option[Double], Int) =
    val buy = trades.filter(_.side == Side.Buy).map(_.sz.toDouble).sum
    val sell = trades.filter(_.side == Side.Sell).map(_.sz.toDouble).sum
    val total = buy + sell
    (if trades.isEmpty || total == 0 then None else Some((buy - sell) / total), trades.size)

  def threshold(trades: Vector[Trade], now: Long): Option[Double] =
    val xs = trades.filter(_.time > now - 900_000L).map(t => t.px.toDouble * t.sz.toDouble).sorted
    if xs.isEmpty then None else Some(xs((xs.size - 1) * 9 / 10))

  def half(cols: Int): Int = ((cols - 46) / 4).max(4).min(30)

  def track(v: Option[Double], h: Int): String =
    val cells = ("." * h + "|" + "." * h).toCharArray
    v.foreach(x => cells(h + Screen.round(x * h).toInt.max(-h).min(h)) = '#')
    new String(cells)

  def mean(xs: Vector[Double]): Option[Double] = if xs.isEmpty then None else Some(xs.sum / xs.size)

  def fmt(v: Option[Double], letter: Char): String = v.fold(s"$letter --")(x => f"$letter$x%+.2f")

  private def label(s: Snapshot, name: String, secs: Long, now: Long): String =
    val text = Chart.rjust(name, 4)
    if secs * 1000 > now - s.since then Screen.dim + text + Screen.reset else text

  def bookRow(s: Snapshot, name: String, secs: Long, now: Long, t: Option[Double], h: Int): String =
    val set =
      if secs == 0 then s.book.toVector
      else s.books.filter(_._1 >= now / 1000 - secs).map(_._2) ++ s.book.toVector
    val readings = set.map(imbalance)
    val d = mean(readings.map(_._2))
    val q = mean(readings.map(_._1))
    val dBig = mean(set.flatMap(b => t.flatMap(imbalanceBig(b, _))))
    val counts =
      if secs == 0 then
        (for b <- s.book; tt <- t yield bigCounts(b, tt)).fold("")((nb, na) => s" b$nb a$na")
      else ""
    s"BOOK ${label(s, name, secs, now)} ${track(d, h)} ${fmt(d, 'D')} ${fmt(q, 'Q')}  big ${track(dBig, h)} ${fmt(dBig, 'D')}$counts"

  def tapeRow(s: Snapshot, name: String, secs: Long, now: Long, t: Option[Double], h: Int): String =
    val ts = s.tape.filter(_.time > now - secs * 1000)
    val (f, n) = flow(ts)
    val big = t.fold(Vector.empty[Trade])(tt => ts.filter(x => x.px.toDouble * x.sz.toDouble >= tt))
    val (fb, nb) = flow(big)
    f"TAPE ${label(s, name, secs, now)} ${track(f, h)} ${fmt(f, 'F')} n=$n%-5d big ${track(fb, h)} ${fmt(fb, 'F')} n=$nb"

  def rows(s: Snapshot, now: Long, cols: Int, t: Option[Double]): Vector[String] =
    val h = half(cols)
    val lines = bookLenses.map((name, secs) => bookRow(s, name, secs, now, t, h)) ++
      tapeLenses.map((name, secs) => tapeRow(s, name, secs, now, t, h))
    lines.map { l =>
      val visible = Chart.plain(l).length
      (l + " " * (cols - visible).max(0))
    }
