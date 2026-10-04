package desk

import java.time.{Instant, LocalDate, LocalTime, ZoneId}
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
  def grid: Vector[Vector[(Char, String)]] =
    bits.toVector.zipWithIndex.map { (row, r) =>
      row.toVector.zipWithIndex
        .map((b, c) => (if b == 0 then ' ' else (0x2800 + b).toChar, styles(r)(c)))
    }
  def lines: Vector[String] = grid.map(Chart.styled)

final case class Column(first: Double, low: Double, high: Double, last: Double, volume: Double)

final case class Span(lo: Double, hi: Double, size: Double, bid: Boolean)

final case class Axis(lo: Double, hi: Double, rows: Int):
  private def place(p: Double, n: Int): Int =
    Screen.round((hi - p) / (hi - lo) * (n - 1)).toInt.max(0).min(n - 1)
  def row(p: Double): Int = place(p, rows)
  // The row whose price band holds p, each row an equal share of the range from the top.
  def trueRow(p: Double): Int = math.floor((hi - p) / (hi - lo) * rows).toInt.max(0).min(rows - 1)
  def dot(p: Double): Int = place(p, rows * 4)

// A glyph drawn over one plot cell, such as a fill.
final case class PlotMark(col: Int, row: Int, glyph: Char, style: String)

// What the chart draws beside and over the price rows: the prices the range must hold, the
// label of 17 columns for each price row, the marks over the plot and a ticket row with its style.
final case class AxisDecor(
    held: Vector[Double],
    labels: Axis => Vector[String],
    marks: Axis => Vector[PlotMark],
    ticketRow: Axis => Option[(Int, String)]
)

object AxisDecor:
  // No held prices, the plain numbers, no marks and no ticket row.
  val plain: AxisDecor = AxisDecor(Vector.empty, Chart.plainLabels, _ => Vector.empty, _ => None)

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

  // The held prices join the plotted lows and highs and the book before the pad is taken.
  def axis(
      columns: Vector[Option[Column]],
      book: Option[Book],
      rows: Int,
      held: Vector[Double] = Vector.empty
  ): Option[Axis] =
    val prices = columns.flatten.flatMap(c => Vector(c.low, c.high)) ++
      book.toVector.flatMap(b => (b.bids ++ b.asks).map(_.px.toDouble)) ++
      held.filter(_.isFinite)
    if prices.isEmpty then None
    else
      val (lo, hi) = (prices.min, prices.max)
      val pad = if hi > lo then (hi - lo) * 0.05 else hi * 0.001
      Some(Axis(lo - pad, hi + pad, rows))

  def decimals(hi: Double): Int =
    val digits = if hi < 1 then 0 else hi.toLong.toString.length
    (5 - digits).max(0)

  def line(columns: Vector[Option[Column]], a: Axis): Vector[String] = lineCanvas(columns, a).lines

  private def lineCanvas(columns: Vector[Option[Column]], a: Axis): Canvas =
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
    cv

  def candleRows(
      columns: Vector[Option[Column]],
      a: Axis,
      cells: Int
  ): Vector[String] = candleGrid(columns, a, cells).toVector.map(row => new String(row))

  private def candleGrid(
      columns: Vector[Option[Column]],
      a: Axis,
      cells: Int
  ): Array[Array[Char]] =
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
    out

  def plot(
      columns: Vector[Option[Column]],
      a: Axis,
      cells: Int,
      mode: ChartMode
  ): Vector[String] = mode match
    case ChartMode.Line    => line(columns, a)
    case ChartMode.Candles => candleRows(columns, a, cells)

  // The plot with a ticket row of dots on its empty cells, then the marks over their cells.
  def decorated(
      columns: Vector[Option[Column]],
      a: Axis,
      cells: Int,
      mode: ChartMode,
      ticket: Option[(Int, String)],
      marks: Vector[PlotMark]
  ): Vector[String] =
    val grid = mode match
      case ChartMode.Line    => lineCanvas(columns, a).grid.map(_.toArray).toArray
      case ChartMode.Candles => candleGrid(columns, a, cells).map(_.map(ch => (ch, "")))
    ticket.foreach { (row, style) =>
      if row >= 0 && row < grid.length then
        grid(row).indices.foreach { c =>
          if grid(row)(c)._1 == ' ' then grid(row)(c) = ('⠒', style)
        }
    }
    marks.foreach { m =>
      if m.row >= 0 && m.row < grid.length && m.col >= 0 && m.col < grid(m.row).length then
        grid(m.row)(m.col) = (m.glyph, m.style)
    }
    grid.toVector.map(row => styled(row.toVector))

  // A fill at its time column, counted back from the right edge at now, and its true row. A
  // time past now is held to the last column and one before the first column is left off. Of two
  // fills in one cell the larger in dollars shows, the later on a tie.
  def fillMarks(
      fills: Vector[Fill],
      axis: Axis,
      now: Long,
      msPerCell: Long,
      cells: Int,
      style: Style
  ): Vector[PlotMark] =
    val placed = fills.zipWithIndex.flatMap { (fill, i) =>
      val back = Math.floorDiv(now - fill.time, msPerCell.max(1L)).max(0L)
      val col = cells - 1L - back
      Option.when(col >= 0)((col.toInt, axis.trueRow(fill.price.toDouble)) -> (fill, i))
    }
    placed.groupMap(_._1)(_._2).toVector.sortBy(_._1).map { case ((col, row), here) =>
      val (fill, _) = here.maxBy((f, i) => (f.quantity * f.price, f.time, i))
      if fill.side == Side.Buy then PlotMark(col, row, '▲', style.bid + Screen.bold)
      else PlotMark(col, row, '▼', style.ask + Screen.bold)
    }

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
        val r1 = a.trueRow(vHi)
        val r2 = a.trueRow(vLo)
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

  def percentile(xs: Vector[Double], q: Double): Double =
    val sorted = xs.sorted
    sorted(((sorted.size - 1) * q).toInt.min(sorted.size - 1))

  // The depth bar of each price row, bids from the label side and asks from the plot side.
  def depthBars(
      book: Option[Book],
      coarse: Map[Int, Book],
      ax: Option[Axis],
      rows: Int,
      style: Style
  ): Vector[String] =
    (book, ax) match
      case (Some(b), Some(a)) =>
        val (depth, isBid) = depthRows(spans(b, coarse), a)
        val nz = depth.toVector.filter(_ > 0)
        val u = if nz.isEmpty then 1.0 else percentile(nz, 0.5)
        val p95 = if nz.isEmpty then 1.0 else percentile(nz, 0.95)
        val r = if p95 > u then p95 else nz.maxOption.getOrElse(1.0)
        val steps = Layout.barW * 2
        (0 until rows).toVector.map { row =>
          val s = depth(row)
          val raw = if s <= 0 then 0L else Screen.round(steps * log(1 + s / u) / log(1 + r / u))
          val text =
            if s <= 0 then " " * Layout.barW
            else bar(raw.max(1).min(steps).toInt, isBid(row), raw > steps)
          style.wrap(if isBid(row) then style.bid else style.ask, text)
        }
      case _ => Vector.fill(rows)(" " * Layout.barW)

  // A faint price at the row's middle on every third row from the second and on the last row,
  // in 17 columns: 8 spaces, the price right aligned in 8 and a space.
  def plainLabels(axis: Axis): Vector[String] =
    val dec = decimals(axis.hi)
    (0 until axis.rows).toVector.map { i =>
      if i % 3 == 1 || i == axis.rows - 1 then
        val middle = axis.hi - (i + 0.5) * (axis.hi - axis.lo) / axis.rows
        val fixed = s"%.${dec}f".format(middle)
        val text =
          if fixed.length <= 8 || !middle.isFinite then fixed
          else Trading.display(BigDecimal(middle), 8)
        Screen.dim + " " * 8 + rjust(text, 8) + " " + Screen.reset
      else " " * 17
    }

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

  private val tickSteps = Vector(5L, 10L, 15L, 30L, 60L, 300L, 900L, 3600L, 14400L, 86400L)
  private val clockFormat = DateTimeFormatter.ofPattern("HH:mm:ss")
  private val dayFormat = DateTimeFormatter.ofPattern("MM-dd")

  // A tick at a round local time at the smallest step that keeps ticks fourteen cells apart,
  // never within fourteen cells of the last tick, and its label after it when nine cells remain.
  // Day ticks carry the date, since their clock always reads midnight.
  def timeRow(start: Long, secondsPerCell: Long, cells: Int, zone: ZoneId): String =
    val width = secondsPerCell.max(1L)
    val step = tickSteps.find(_ >= 14 * width).getOrElse(tickSteps.last)
    val out = Array.fill(cells.max(0))(' ')
    var free = 0
    for c <- 0 until cells do
      val from = start + c * width
      val local = from + zone.getRules.getOffset(Instant.ofEpochSecond(from)).getTotalSeconds
      val round = Math.floorDiv(local + step - 1, step) * step
      if c >= free && round < local + width then
        out(c) = '|'
        free = c + 14
        if c + 9 <= cells then
          val label =
            if step == 86400L then
              LocalDate.ofEpochDay(Math.floorDiv(round, 86400L)).format(dayFormat)
            else LocalTime.ofSecondOfDay(Math.floorMod(round, 86400L)).format(clockFormat)
          label.zipWithIndex.foreach((ch, i) => out(c + 1 + i) = ch)
    new String(out)
