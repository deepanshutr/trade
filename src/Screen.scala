package desk

import scala.math.{floor, log1p, pow}

object Screen:
  val minCols = 100
  val minRows = 30
  val ladderW = 46
  val barW = 20
  val chartRows = 17
  val scaleW = 8
  val esc: String = 27.toChar.toString
  val bold: String = esc + "[1m"
  val dim: String = esc + "[2m"
  val reset: String = esc + "[0m"
  private val emptyTrack = "." * 10 + "|" + "." * 10

  def round(x: Double): Long = floor(x + 0.5).toLong

  def barLen(s: Double, u: Double, r: Double): Long =
    if s <= 0 || u <= 0 || r <= 0 then 0L
    else round(barW * log1p(s / u) / log1p(r / u))

  def lowerMedian(xs: Vector[Double]): Double =
    if xs.isEmpty then 0.0 else xs.sorted.apply((xs.size - 1) / 2)

  def reference(window: Vector[(Long, Vector[Double])], onScreen: Vector[Double]): Double =
    val all = window.flatMap(_._2)
    if all.size >= 40 then all.sorted.apply(floor(0.95 * (all.size - 1)).toInt)
    else
      val desc = onScreen.sorted.reverse
      desc.lift(1).orElse(desc.headOption).getOrElse(0.0)

  def cumulative(ls: Vector[Level]): Vector[Double] =
    ls.scanLeft(0.0)((acc, l) => acc + l.sz.toDouble).tail

  def track(v: Double): String =
    val off = round(v * 10).toInt.max(-10).min(10)
    val cells = emptyTrack.toCharArray
    cells(10 + off) = '#'
    new String(cells)

  def imbalance(b: Book): (Double, Double) =
    val q = (b.bids.headOption, b.asks.headOption) match
      case (Some(bb), Some(aa)) =>
        val (x, y) = (bb.sz.toDouble, aa.sz.toDouble)
        if x + y == 0 then 0.0 else (x - y) / (x + y)
      case (Some(_), None) => 1.0
      case (None, Some(_)) => -1.0
      case _               => 0.0
    val w = (0 until 10).map(k => pow(0.7, k.toDouble))
    val wb = b.bids.take(10).zip(w).map((l, wk) => l.sz.toDouble * wk).sum
    val wa = b.asks.take(10).zip(w).map((l, wk) => l.sz.toDouble * wk).sum
    val d = if wb + wa == 0 then 0.0 else (wb - wa) / (wb + wa)
    (q, d)

  def flow(tape: Vector[Trade], now: Long): (Double, Int) =
    val recent = tape.filter(_.time > now - 60_000L)
    val buy = recent.filter(_.side == Side.Buy).map(_.sz.toDouble).sum
    val sell = recent.filter(_.side == Side.Sell).map(_.sz.toDouble).sum
    val total = buy + sell
    (if total == 0 then 0.0 else (buy - sell) / total, recent.size)

  private def decimals(px: String): Int =
    val i = px.indexOf('.')
    if i < 0 then 0 else px.length - i - 1

  def rule(b: Book, now: Long): String =
    val age = (now - b.time).max(0L) / 1000.0
    val text = (b.asks.headOption, b.bids.headOption) match
      case (Some(a), Some(d)) =>
        val decs = decimals(a.px).max(decimals(d.px))
        val spread = BigDecimal(a.px) - BigDecimal(d.px)
        val mid = (BigDecimal(a.px) + BigDecimal(d.px)) / 2
        val bp = if mid == 0 then 0.0 else (spread / mid * 10000).toDouble
        val sp = spread.setScale(decs, BigDecimal.RoundingMode.HALF_UP)
        f"--------  spread $sp  $bp%.1fbp  book $age%.1fs  "
      case _ => f"--------  spread n/a  book $age%.1fs  "
    text.padTo(ladderW, '-').take(ladderW)

  private def style(text: String, s: String): String =
    if text.trim.isEmpty then text else s + text + reset

  def ladder(s: Snapshot, ladderRows: Int, now: Long): Vector[String] =
    val side = (ladderRows - 1) / 2
    val blank = " " * ladderW
    val extra = if (ladderRows - 1) % 2 == 1 then Vector(blank) else Vector.empty
    s.book match
      case None    => Vector.fill(ladderRows)(blank)
      case Some(b) =>
        val asks = b.asks.take(side)
        val bids = b.bids.take(side)
        val onScreen = (asks ++ bids).map(_.sz.toDouble)
        val u = lowerMedian(onScreen)
        val r = reference(s.sizes, onScreen)
        def row(l: Level, cum: Double): (String, Boolean) =
          val len = barLen(l.sz.toDouble, u, r)
          val clamped = len > barW
          val bar = "#" * len.min(barW).toInt
          val marker = if clamped then ">" else " "
          val px = l.px.take(8)
          val sz = l.sz.take(7)
          val cumText = f"$cum%7.3f".take(7)
          (f"$px%8s $sz%7s $cumText $bar%-20s" + marker, clamped)
        val askRows = asks.zip(cumulative(asks)).map(row).reverse
        val bidRows = bids.zip(cumulative(bids)).map(row)
        val askLines = Vector.fill(side - askRows.size)((blank, false)) ++ askRows
        val bidLines = bidRows ++ Vector.fill(side - bidRows.size)((blank, false))
        val outer = side / 3
        val styledAsks = askLines.zipWithIndex.map { case ((t, clamped), i) =>
          val best = i == side - 1 && asks.nonEmpty
          if best || clamped then style(t, bold) else if i < outer then style(t, dim) else t
        }
        val styledBids = bidLines.zipWithIndex.map { case ((t, clamped), i) =>
          val best = i == 0 && bids.nonEmpty
          if best || clamped then style(t, bold)
          else if i >= side - outer then style(t, dim)
          else t
        }
        (styledAsks :+ rule(b, now)) ++ styledBids ++ extra

  def gauges(s: Snapshot, now: Long): Vector[String] =
    val bookLine = s.book match
      case Some(b) =>
        val (q, d) = imbalance(b)
        f"BOOK ${track(d)} D$d%+.2f Q$q%+.2f"
      case None => s"BOOK $emptyTrack"
    val (f, n) = flow(s.tape, now)
    val tapeLine =
      if n == 0 then s"TAPE $emptyTrack n=0" else f"TAPE ${track(f)} F$f%+.2f 60s n=$n"
    Vector(bookLine.padTo(ladderW, ' ').take(ladderW), tapeLine.padTo(ladderW, ' ').take(ladderW))

  private def digitsBeforePoint(x: Double): Int =
    x.abs.toLong.toString.length

  private def scaleLabel(x: Double, decimals: Int): String =
    val formatted = s"%.${decimals}f".format(x)
    formatted.reverse.padTo(scaleW, ' ').reverse.take(scaleW)

  def chart(s: Snapshot, width: Int): Vector[String] =
    val plotW = width - scaleW
    val cs = s.candles.takeRight(plotW)
    if cs.isEmpty then "no candles".padTo(width, ' ') +: Vector.fill(chartRows - 1)(" " * width)
    else
      val hi = cs.map(_.h).max
      val lo = cs.map(_.l).min
      def rowOf(p: Double): Int =
        if hi == lo then chartRows / 2
        else round((hi - p) / (hi - lo) * (chartRows - 1)).toInt.max(0).min(chartRows - 1)
      val grid = Array.fill(chartRows, plotW)(' ')
      val start = plotW - cs.size
      cs.zipWithIndex.foreach { (c, i) =>
        val col = start + i
        (rowOf(c.h) to rowOf(c.l)).foreach(r => grid(r)(col) = '|')
        val (top, bottom) = (rowOf(c.o).min(rowOf(c.c)), rowOf(c.o).max(rowOf(c.c)))
        (top to bottom).foreach(r => grid(r)(col) = '#')
      }
      val chartDecimals = (5 - digitsBeforePoint(hi)).max(0).min(6)
      val scale = Map(
        0 -> scaleLabel(hi, chartDecimals),
        chartRows / 2 -> scaleLabel((hi + lo) / 2, chartDecimals),
        chartRows - 1 -> scaleLabel(lo, chartDecimals)
      )
      (0 until chartRows).toVector.map { r =>
        val line = new String(grid(r))
        val newest = line.charAt(plotW - 1)
        val styledNewest = if newest == ' ' then " " else bold + newest + reset
        line.substring(0, plotW - 1) + styledNewest + scale.getOrElse(r, " " * scaleW)
      }

  private def hms(ms: Long, zone: java.time.ZoneId): String =
    java.time.Instant
      .ofEpochMilli(ms)
      .atZone(zone)
      .toLocalTime
      .format(java.time.format.DateTimeFormatter.ofPattern("HH:mm:ss"))

  def tape(s: Snapshot, rows: Int, width: Int, zone: java.time.ZoneId): Vector[String] =
    val sizes = s.tape.map(_.sz.toDouble).sorted
    val big =
      if sizes.size < 2 then Double.MaxValue else sizes(floor(0.95 * (sizes.size - 1)).toInt)
    val lines = s.tape.take(rows).map { t =>
      val side = if t.side == Side.Buy then "buy" else "sell"
      val text = f" ${hms(t.time, zone)}  $side%-4s  ${t.px}%7s  ${t.sz}%6s".padTo(width, ' ')
      if t.sz.toDouble > big then bold + text.take(width) + reset else text.take(width)
    }
    lines ++ Vector.fill((rows - lines.size).max(0))(" " * width)

  def short(x: Double): String =
    if x >= 1e9 then f"${x / 1e9}%.1fB"
    else if x >= 1e6 then f"${x / 1e6}%.1fM"
    else if x >= 1e3 then f"${x / 1e3}%.1fk"
    else f"$x%.1f"

  private def midOf(b: Book): Option[String] =
    for
      a <- b.asks.headOption
      d <- b.bids.headOption
    yield ((BigDecimal(a.px) + BigDecimal(d.px)) / 2).bigDecimal
      .stripTrailingZeros()
      .toPlainString()

  def stats(s: Snapshot, now: Long, cols: Int): String =
    val text = s.prompt match
      case Some(p) => s"market: ${p}_"
      case None    =>
        val mid = s.ctx.flatMap(_.mid).orElse(s.book.flatMap(midOf))
        val age = (now - State.lastMsgAt(s.feed)).max(0L)
        val fresh = if age > 5000 then s"STALE ${age / 1000}s" else f"live ${age / 1000.0}%.1fs"
        val badPart = if s.bad > 0 then s"  bad ${s.bad}" else ""
        s.ctx match
          case Some(c) =>
            val pct = mid.map(m => (m.toDouble / c.prevDayPx.toDouble - 1) * 100).getOrElse(0.0)
            f"${s.coin}  ${mid.getOrElse("-")}  mark ${c.mark}  orc ${c.oracle}  24h $pct%+.1f%%  " +
              f"fund ${c.funding.toDouble * 100}%.4f%%  OI ${short(c.openInterest.toDouble)}  " +
              f"vol ${short(c.dayVolume.toDouble)}  ${s.interval}  $fresh$badPart"
          case None => s"${s.coin}  ${mid.getOrElse("-")}  ${s.interval}  $fresh$badPart"
    val fitted = if text.length > cols then text.replace("  ", " ") else text
    fitted.padTo(cols, ' ').take(cols)

  def render(s: Snapshot, cols: Int, rows: Int, now: Long, zone: java.time.ZoneId): Vector[String] =
    if cols < minCols || rows < minRows then Vector(s"need ${minCols}x$minRows, have ${cols}x$rows")
    else
      val right = cols - ladderW
      val left = ladder(s, rows - 3, now) ++ gauges(s, now)
      val rightLines = chart(s, right) ++ tape(s, rows - 1 - chartRows, right, zone)
      stats(s, now, cols) +: left.zip(rightLines).map((l, r) => l + r)
