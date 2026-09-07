package desk

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.math.log

final class Canvas(val cells: Int, val rows: Int):
  val w: Int = cells * 2
  val h: Int = rows * 4
  private val bits = Array.fill(rows, cells)(0)
  private val styles = Array.fill(rows, cells)("")
  private val bit =
    Array(Array(0x01, 0x08), Array(0x02, 0x10), Array(0x04, 0x20), Array(0x40, 0x80))
  def dot(x: Int, y: Int, style: String = ""): Unit =
    if x >= 0 && x < w && y >= 0 && y < h then
      bits(y / 4)(x / 2) |= bit(y % 4)(x % 2)
      if style.nonEmpty then styles(y / 4)(x / 2) = style
  def run(x: Int, y0: Int, y1: Int, style: String = ""): Unit =
    for y <- (y0 min y1) to (y0 max y1) do dot(x, y, style)
  def segment(x0: Int, y0: Int, x1: Int, y1: Int): Unit =
    var x = x0
    var y = y0
    val dx = math.abs(x1 - x0)
    val dy = -math.abs(y1 - y0)
    val sx = if x0 < x1 then 1 else -1
    val sy = if y0 < y1 then 1 else -1
    var error = dx + dy
    var finished = false
    while !finished do
      dot(x, y)
      if x == x1 && y == y1 then finished = true
      else
        val twice = 2 * error
        if twice >= dy then
          error += dy
          x += sx
        if twice <= dx then
          error += dx
          y += sy

  def restyle(cell: Int, row: Int, style: String): Unit =
    if cell >= 0 && cell < cells && row >= 0 && row < rows then styles(row)(cell) = style
  def lines: Vector[String] =
    bits.toVector.zipWithIndex.map { (row, r) =>
      Chart.styled(
        row.toVector.zipWithIndex
          .map((b, c) => (if b == 0 then ' ' else (0x2800 + b).toChar, styles(r)(c)))
      )
    }

final case class Column(first: Double, low: Double, high: Double, last: Double, volume: Double)

final case class Span(lo: Double, hi: Double, size: Double, bid: Boolean)

final case class Axis(lo: Double, hi: Double, rows: Int):
  private def place(p: Double, n: Int): Int =
    Screen.round((hi - p) / (hi - lo) * (n - 1)).toInt.max(0).min(n - 1)
  def row(p: Double): Int = place(p, rows)
  def dot(p: Double): Int = place(p, rows * 4)

object Chart:
  private val esc = Screen.esc

  val FULL: String = (0x28ff).toChar.toString
  val LHALF: String = (0x2847).toChar.toString
  val RHALF: String = (0x28b8).toChar.toString

  def rjust(text: String, w: Int): String = (" " * (w - text.length).max(0) + text).take(w)

  def plain(line: String): String = line.replaceAll(esc + "\\[[0-9;]*m", "")

  def fit(line: String, cols: Int): String =
    val visible = plain(line).length
    if visible <= cols then line + " " * (cols - visible)
    else
      val (out, _, _) = line.foldLeft((StringBuilder(), 0, false)) { case ((sb, seen, inEsc), c) =>
        if inEsc then (sb.append(c), seen, c != 'm')
        else if c == Screen.esc.head then (sb.append(c), seen, true)
        else if seen < cols then (sb.append(c), seen + 1, false)
        else (sb, seen + 1, false)
      }
      out.toString

  def referenceColumns(
      r: Ref,
      count: Int,
      s: Snapshot,
      now: Long
  ): Vector[Option[Column]] =
    val (first, z) = window(s, count, now)
    columnsOf(r.samples, Vector.empty, r.candles, first, z, count)

  def styled(cells: Vector[(Char, String)]): String =
    val sb = new StringBuilder
    var current = ""
    cells.foreach { (ch, st) =>
      if st != current then
        if current.nonEmpty then sb.append(Screen.reset)
        if st.nonEmpty then sb.append(st)
        current = st
      sb.append(ch)
    }
    if current.nonEmpty then sb.append(Screen.reset)
    sb.toString

  def window(s: Snapshot, dotCols: Int, now: Long): (Long, Int) =
    val z = State.zooms(s.zoom)
    val lastCol = Math.floorDiv(now / 1000 - s.offsetSec, z.toLong)
    (lastCol - dotCols + 1, z)

  def columnsOf(
      samples: Vector[(Long, Double)],
      tape: Vector[Trade],
      candles: Map[String, Vector[Candle]],
      first: Long,
      z: Int,
      n: Int
  ): Vector[Option[Column]] =
    State.seriesFor(z) match
      case None    => fromSamples(samples, tape, first, z, n)
      case Some(i) => fromCandles(candles.getOrElse(i, Vector.empty), first, z, n)

  def columns(s: Snapshot, dotCols: Int, now: Long): Vector[Option[Column]] =
    val (first, z) = window(s, dotCols, now)
    columnsOf(s.samples, s.tape, s.candles, first, z, dotCols)

  private def bucket(sec: Long, first: Long, z: Int): Int =
    (Math.floorDiv(sec, z.toLong) - first).toInt

  def fromSamples(
      samples: Vector[(Long, Double)],
      tape: Vector[Trade],
      first: Long,
      z: Int,
      n: Int
  ): Vector[Option[Column]] =
    val lo = first * z
    val hi = (first + n) * z
    val vol = Array.fill(n)(0.0)
    tape.foreach { t =>
      val k = bucket(t.time / 1000, first, z)
      if k >= 0 && k < n then vol(k) += t.displaySize
    }
    val inside =
      samples.filter((sec, _) => sec >= lo && sec < hi).groupBy((sec, _) => bucket(sec, first, z))
    var prev = samples.takeWhile(_._1 < lo).lastOption.map(_._2)
    (0 until n).toVector.map { k =>
      inside.get(k) match
        case Some(ss) =>
          val ms = ss.map(_._2)
          prev = Some(ms.last)
          Some(Column(ms.head, ms.min, ms.max, ms.last, vol(k)))
        case None => prev.map(p => Column(p, p, p, p, vol(k)))
    }

  def fromCandles(candles: Vector[Candle], first: Long, z: Int, n: Int): Vector[Option[Column]] =
    val lo = first * z
    val hi = (first + n) * z
    val inside = candles
      .filter(c => c.t / 1000 >= lo && c.t / 1000 < hi)
      .groupBy(c => bucket(c.t / 1000, first, z))
    (0 until n).toVector.map { k =>
      inside.get(k).map { cs =>
        val sorted = cs.sortBy(_.t)
        Column(
          sorted.head.o,
          sorted.map(_.l).min,
          sorted.map(_.h).max,
          sorted.last.c,
          sorted.map(_.v).sum
        )
      }
    }

  def axis(columns: Vector[Option[Column]], book: Option[Book], rows: Int): Option[Axis] =
    val prices = columns.flatten.flatMap(c => Vector(c.low, c.high)) ++
      book.toVector.flatMap(b => (b.bids ++ b.asks).map(_.px.toDouble))
    if prices.isEmpty then None
    else
      val (lo, hi) = (prices.min, prices.max)
      val pad = if hi > lo then (hi - lo) * 0.05 else hi * 0.001
      Some(Axis(lo - pad, hi + pad, rows))

  def decimals(hi: Double): Int =
    val digits = if hi < 1 then 0 else hi.toLong.toString.length
    (5 - digits).max(0)

  def line(columns: Vector[Option[Column]], a: Axis): Vector[String] =
    val cv = Canvas(columns.size / 2, a.rows)
    var previous: Option[(Int, Int)] = None
    var newest: Option[(Int, Int)] = None
    columns.zipWithIndex.foreach { (column, x) =>
      column match
        case None    => previous = None
        case Some(c) =>
          val y = a.dot(c.last)
          previous match
            case Some((px, py)) => cv.segment(px, py, x, y)
            case None           => cv.dot(x, y)
          previous = Some((x, y))
          newest = Some((x / 2, y / 4))
    }
    newest.foreach((cell, row) => cv.restyle(cell, row, Screen.bold))
    cv.lines

  def candleRows(
      columns: Vector[Option[Column]],
      a: Axis,
      cells: Int
  ): Vector[String] =
    val out = Array.fill(a.rows, cells)(' ')
    def put(x: Int, y: Int, text: String): Unit =
      if y >= 0 && y < a.rows then
        text.zipWithIndex.foreach { (ch, i) =>
          if x + i >= 0 && x + i < cells then out(y)(x + i) = ch
        }
    columns.zipWithIndex.foreach { (column, i) =>
      column.foreach { c =>
        val x = i * Layout.candleW
        val high = a.row(c.high)
        val low = a.row(c.low)
        val open = a.row(c.first)
        val close = a.row(c.last)
        val top = open min close
        val bottom = open max close
        for row <- high to low do put(x + 1, row, "│")
        if c.first == c.last then put(x, open, "───")
        else if c.last < c.first then for row <- top to bottom do put(x, row, "███")
        else if top == bottom then put(x + 1, top, "□")
        else
          put(x, top, "┌─┐")
          for row <- top + 1 until bottom do put(x, row, "│ │")
          put(x, bottom, "└─┘")
      }
    }
    out.toVector.map(row => new String(row))

  def plot(
      columns: Vector[Option[Column]],
      a: Axis,
      cells: Int,
      mode: ChartMode
  ): Vector[String] = mode match
    case ChartMode.Line    => line(columns, a)
    case ChartMode.Candles => candleRows(columns, a, cells)

  def width(p: Double, k: Int): Double = math.pow(10, math.floor(math.log10(p)) + 1 - k)

  def spans(full: Book, coarse: Map[Int, Book]): Vector[Span] =
    val fine = full.bids.map(l => Span(l.px.toDouble, l.px.toDouble, l.sz.toDouble, true)) ++
      full.asks.map(l => Span(l.px.toDouble, l.px.toDouble, l.sz.toDouble, false))
    var lowestBid = full.bids.lastOption.map(_.px.toDouble)
    var highestAsk = full.asks.lastOption.map(_.px.toDouble)
    val out = Vector.newBuilder[Span]
    out ++= fine
    for k <- Vector(4, 3, 2); b <- coarse.get(k) do
      val bids = b.bids
        .map(l => (l.px.toDouble, l.sz.toDouble))
        .filter((p, _) => lowestBid.forall(p + width(p, k) <= _))
      val asks = b.asks
        .map(l => (l.px.toDouble, l.sz.toDouble))
        .filter((p, _) => highestAsk.forall(p - width(p, k) >= _))
      out ++= bids.map((p, s) => Span(p, p + width(p, k), s, true))
      out ++= asks.map((p, s) => Span(p - width(p, k), p, s, false))
      b.bids.lastOption.foreach(l => lowestBid = Some(l.px.toDouble))
      b.asks.lastOption.foreach(l => highestAsk = Some(l.px.toDouble))
    out.result()

  def depthRows(spans: Vector[Span], a: Axis): (Array[Double], Array[Boolean]) =
    val depth = Array.fill(a.rows)(0.0)
    val isBid = Array.fill(a.rows)(false)
    spans.foreach { sp =>
      val vLo = sp.lo.max(a.lo)
      val vHi = sp.hi.min(a.hi)
      if vHi >= vLo then
        val visible = if sp.hi > sp.lo then sp.size * (vHi - vLo) / (sp.hi - sp.lo) else sp.size
        val r1 = a.row(vHi)
        val r2 = a.row(vLo)
        val share = visible / (r2 - r1 + 1)
        for r <- r1 to r2 do
          depth(r) += share
          if sp.bid then isBid(r) = true
    }
    (depth, isBid)

  def bar(n: Int, bid: Boolean, clamp: Boolean): String =
    val full = FULL * (n / 2)
    val ink =
      if bid then full + (if n % 2 == 1 then LHALF else "")
      else (if n % 2 == 1 then RHALF else "") + full
    val marked =
      if clamp then (if bid then ink.dropRight(1) + ">" else ">" + ink.drop(1)) else ink
    if bid then marked.padTo(Layout.barW, ' ') else rjust(marked, Layout.barW)

  private def lowerMedian(xs: Vector[Double]): Double = xs.sorted.apply((xs.size - 1) / 2)

  def percentile(xs: Vector[Double], q: Double): Double =
    val sorted = xs.sorted
    sorted(((sorted.size - 1) * q).toInt.min(sorted.size - 1))

  def ladderRows(
      book: Option[Book],
      coarse: Map[Int, Book],
      ax: Option[Axis],
      rows: Int,
      midRow: Option[Int],
      style: Style
  ): Vector[String] =
    (book, ax) match
      case (Some(b), Some(a)) =>
        val (depth, isBid) = depthRows(spans(b, coarse), a)
        val nz = depth.toVector.filter(_ > 0)
        val u = if nz.isEmpty then 1.0 else lowerMedian(nz)
        val p95 = if nz.isEmpty then 1.0 else percentile(nz, 0.95)
        val r = if p95 > u then p95 else nz.maxOption.getOrElse(1.0)
        val dec = decimals(a.hi)
        val bestRows = Set(
          b.bids.headOption.map(l => a.row(l.px.toDouble)),
          b.asks.headOption.map(l => a.row(l.px.toDouble))
        ).flatten
        val steps = Layout.barW * 2
        (0 until rows).toVector.map { row =>
          val price = a.hi - (a.hi - a.lo) * row / (rows - 1).max(1)
          val s = depth(row)
          val raw = if s <= 0 then 0L else Screen.round(steps * log(1 + s / u) / log(1 + r / u))
          val text =
            if s <= 0 then " " * Layout.barW
            else bar(raw.max(1).min(steps).toInt, isBid(row), raw > steps)
          val coloured = style.wrap(if isBid(row) then style.bid else style.ask, text)
          val label =
            rjust(s"%.${dec}f".format(price), Layout.labelW) + (if midRow.contains(row) then ">"
                                                                else " ")
          if bestRows.contains(row) || midRow.contains(row) then
            Screen.bold + label + Screen.reset + coloured
          else label + coloured
        }
      case _ => Vector.fill(rows)(" " * Layout.leftW)

  def scaleRows(
      ax: Option[Axis],
      rows: Int,
      midRow: Option[Int],
      mid: Option[String]
  ): Vector[String] =
    ax.fold(Vector.fill(rows)(" " * Layout.scaleW)) { a =>
      val dec = decimals(a.hi)
      val middle = a.hi - (a.hi - a.lo) * (rows / 2) / (rows - 1).max(1)
      val labels = Map(0 -> a.hi, rows / 2 -> middle, rows - 1 -> a.lo)
      (0 until rows).toVector.map { row =>
        (midRow, mid) match
          case (Some(m), Some(text)) if m == row =>
            Screen.bold + rjust("<" + text, Layout.scaleW) + Screen.reset
          case _ =>
            labels
              .get(row)
              .fold(" " * Layout.scaleW)(v => rjust(s"%.${dec}f".format(v), Layout.scaleW))
      }
    }

  def volumeRows(
      columns: Vector[Option[Column]],
      style: Style,
      mode: ChartMode = ChartMode.Line,
      cells: Int = 0
  ): (Vector[String], Double) =
    val maximum = columns.map(_.fold(0.0)(_.volume)).maxOption.getOrElse(0.0)
    val width = if cells > 0 then cells else columns.size / 2
    val cv = Canvas(width, Layout.volRows)
    columns.zipWithIndex.foreach { (column, i) =>
      column.foreach { c =>
        if c.volume > 0 then
          val depth = Screen.round(c.volume / maximum * (cv.h - 1)).toInt.max(1)
          val colour = if c.last >= c.first then style.bid else style.ask
          if mode == ChartMode.Candles then
            val x = (i * Layout.candleW + 1) * 2
            cv.run(x, cv.h - depth, cv.h - 1, colour)
            cv.run(x + 1, cv.h - depth, cv.h - 1, colour)
          else cv.run(i, cv.h - depth, cv.h - 1, colour)
      }
    }
    (cv.lines, maximum)

  def axisRow(
      first: Long,
      z: Int,
      count: Int,
      zone: ZoneId,
      width: Int = 0
  ): String =
    val cells = if width > 0 then width else count / 2
    val (pattern, clockWidth) =
      if z < 60 then ("HH:mm:ss", 8)
      else if z < 3600 then ("HH:mm", 5)
      else if z < 86400 then ("MM-dd HH:mm", 11)
      else ("yyyy-MM-dd", 10)
    val fmt = DateTimeFormatter.ofPattern(pattern).withZone(zone)
    def clock(column: Long): String =
      fmt.format(Instant.ofEpochSecond((first + column) * z))
    val out = Array.fill(cells)(' ')
    val spots = Vector(
      (0, clock(0)),
      (cells / 2 - clockWidth / 2, clock(count / 2)),
      (cells - clockWidth, clock(count - 1))
    )
    spots.foreach { (at, text) =>
      text.zipWithIndex.foreach { (ch, i) =>
        if at + i >= 0 && at + i < cells then out(at + i) = ch
      }
    }
    new String(out)
