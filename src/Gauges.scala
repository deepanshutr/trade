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

  private def ratio(buy: Double, sell: Double, n: Int): (Option[Double], Int) =
    val total = buy + sell
    (if n == 0 || total == 0 then None else Some((buy - sell) / total), n)

  def tapeStats(
      tape: Vector[Trade],
      now: Long,
      t: Option[Double],
      lenses: Vector[Long]
  ): Vector[((Option[Double], Int), (Option[Double], Int))] =
    val n = lenses.size
    val cuts = lenses.map(secs => now - secs * 1000).toArray
    val oldest = cuts.minOption.getOrElse(now)
    val buy = Array.fill(n)(0.0)
    val sell = Array.fill(n)(0.0)
    val count = Array.fill(n)(0)
    val bigBuy = Array.fill(n)(0.0)
    val bigSell = Array.fill(n)(0.0)
    val bigCount = Array.fill(n)(0)
    tape.view.takeWhile(_.time > oldest).foreach { x =>
      val sz = x.sz.toDouble
      val isBuy = x.side == Side.Buy
      val isBig = t.exists(x.px.toDouble * sz >= _)
      var k = 0
      while k < n do
        if x.time > cuts(k) then
          if isBuy then buy(k) += sz else sell(k) += sz
          count(k) += 1
          if isBig then
            if isBuy then bigBuy(k) += sz else bigSell(k) += sz
            bigCount(k) += 1
        k += 1
    }
    (0 until n).toVector.map(k =>
      (ratio(buy(k), sell(k), count(k)), ratio(bigBuy(k), bigSell(k), bigCount(k)))
    )

  def threshold(trades: Vector[Trade], now: Long): Option[Double] =
    val xs = trades.filter(_.time > now - 900_000L).map(t => t.px.toDouble * t.sz.toDouble)
    if xs.isEmpty then None else Some(Chart.percentile(xs, 0.9))

  def half(cols: Int): Int = ((cols - 50) / 4).max(4).min(30)

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
      else
        val inWindow = s.books.filter(_._1 >= now / 1000 - secs).map(_._2)
        s.book.fold(inWindow)(b => if inWindow.contains(b) then inWindow else inWindow :+ b)
    val readings = set.map(imbalance)
    val d = mean(readings.map(_._2))
    val q = mean(readings.map(_._1))
    val dBig = mean(set.flatMap(b => t.flatMap(imbalanceBig(b, _))))
    val counts =
      if secs == 0 then
        (for b <- s.book; tt <- t yield bigCounts(b, tt)).fold("")((nb, na) => s" b$nb a$na")
      else ""
    s"BOOK ${label(s, name, secs, now)} ${track(d, h)} ${fmt(d, 'D')} ${fmt(q, 'Q')}  big ${track(dBig, h)} ${fmt(dBig, 'D')}$counts"

  private def tapeText(
      s: Snapshot,
      name: String,
      secs: Long,
      now: Long,
      all: (Option[Double], Int),
      big: (Option[Double], Int),
      h: Int
  ): String =
    val (f, n) = all
    val (fb, nb) = big
    f"TAPE ${label(s, name, secs, now)} ${track(f, h)} ${fmt(f, 'F')} n=$n%-5d big ${track(fb, h)} ${fmt(fb, 'F')} n=$nb"

  def tapeRow(s: Snapshot, name: String, secs: Long, now: Long, t: Option[Double], h: Int): String =
    val (all, big) = tapeStats(s.tape, now, t, Vector(secs)).head
    tapeText(s, name, secs, now, all, big, h)

  private def fit(line: String, cols: Int): String =
    val visible = Chart.plain(line).length
    if visible <= cols then line + " " * (cols - visible)
    else
      val (out, _, _) = line.foldLeft((StringBuilder(), 0, false)) { case ((sb, seen, inEsc), c) =>
        if inEsc then (sb.append(c), seen, c != 'm')
        else if c == Screen.esc.head then (sb.append(c), seen, true)
        else if seen < cols then (sb.append(c), seen + 1, false)
        else (sb, seen + 1, false)
      }
      out.toString

  def rows(s: Snapshot, now: Long, cols: Int, t: Option[Double]): Vector[String] =
    val h = half(cols)
    val stats = tapeStats(s.tape, now, t, tapeLenses.map(_._2))
    val lines = bookLenses.map((name, secs) => bookRow(s, name, secs, now, t, h)) ++
      tapeLenses.zip(stats).map { case ((name, secs), (all, big)) =>
        tapeText(s, name, secs, now, all, big, h)
      }
    lines.map(l => fit(l, cols))
