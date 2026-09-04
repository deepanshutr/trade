package desk

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.math.log

final class Canvas(val cells: Int, val rows: Int):
  val w: Int = cells * 2
  val h: Int = rows * 4
  private val bits = Array.fill(rows, cells)(0)
  private val bit =
    Array(Array(0x01, 0x08), Array(0x02, 0x10), Array(0x04, 0x20), Array(0x40, 0x80))
  def dot(x: Int, y: Int): Unit =
    if x >= 0 && x < w && y >= 0 && y < h then bits(y / 4)(x / 2) |= bit(y % 4)(x % 2)
  def run(x: Int, y0: Int, y1: Int): Unit =
    for y <- (y0 min y1) to (y0 max y1) do dot(x, y)
  def lines: Vector[String] =
    bits.toVector.map(row => row.map(b => if b == 0 then ' ' else (0x2800 + b).toChar).mkString)

final case class Column(first: Double, low: Double, high: Double, last: Double, volume: Double)

final case class Axis(lo: Double, hi: Double, rows: Int):
  private def place(p: Double, n: Int): Int =
    Screen.round((hi - p) / (hi - lo) * (n - 1)).toInt.max(0).min(n - 1)
  def row(p: Double): Int = place(p, rows)
  def dot(p: Double): Int = place(p, rows * 4)

object Chart:
  private val esc = Screen.esc

  def rjust(text: String, w: Int): String = (" " * (w - text.length).max(0) + text).take(w)

  def plain(line: String): String = line.replaceAll(esc + "\\[[0-9;]*m", "")

  def window(s: Snapshot, dotCols: Int, now: Long): (Long, Int) =
    val z = State.zooms(s.zoom)
    val lastCol = Math.floorDiv(now / 1000 - s.offsetSec, z.toLong)
    (lastCol - dotCols + 1, z)

  def columns(s: Snapshot, dotCols: Int, now: Long): Vector[Option[Column]] =
    val (first, z) = window(s, dotCols, now)
    if z < 60 then fromSamples(s.samples, s.tape, first, z, dotCols)
    else fromCandles(s.candles, first, z, dotCols)

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
      if k >= 0 && k < n then vol(k) += t.sz.toDouble
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
    (5 - digits).max(0).min(6)

  def line(columns: Vector[Option[Column]], a: Axis): Vector[String] =
    val cv = Canvas(columns.size / 2, a.rows)
    var prev: Option[Double] = None
    var newest: Option[(Int, Int)] = None
    columns.zipWithIndex.foreach { (col, x) =>
      col.foreach { c =>
        cv.run(x, a.dot(prev.getOrElse(c.first)), a.dot(c.first))
        cv.run(x, a.dot(c.low), a.dot(c.high))
        prev = Some(c.last)
        newest = Some((x / 2, a.dot(c.last) / 4))
      }
    }
    val ls = cv.lines
    newest.fold(ls) { (cell, row) =>
      ls.updated(row, ls(row).patch(cell, Screen.bold + ls(row)(cell) + Screen.reset, 1))
    }

  private def lowerMedian(xs: Vector[Double]): Double = xs.sorted.apply((xs.size - 1) / 2)

  private def percentile(xs: Vector[Double], q: Double): Double =
    val sorted = xs.sorted
    sorted(((sorted.size - 1) * q).toInt.min(sorted.size - 1))

  def ladderRows(
      book: Option[Book],
      ax: Option[Axis],
      rows: Int,
      midRow: Option[Int]
  ): Vector[String] =
    (book, ax) match
      case (Some(b), Some(a)) =>
        val depth = Array.fill(rows)(0.0)
        val isBid = Array.fill(rows)(false)
        b.asks.foreach(l => depth(a.row(l.px.toDouble)) += l.sz.toDouble)
        b.bids.foreach { l =>
          val r = a.row(l.px.toDouble)
          depth(r) += l.sz.toDouble
          isBid(r) = true
        }
        val nz = depth.toVector.filter(_ > 0)
        val u = if nz.isEmpty then 1.0 else lowerMedian(nz)
        val p95 = if nz.isEmpty then 1.0 else percentile(nz, 0.95)
        val r = if p95 > u then p95 else nz.maxOption.getOrElse(1.0)
        val dec = decimals(a.hi)
        val bestRows = Set(
          b.bids.headOption.map(l => a.row(l.px.toDouble)),
          b.asks.headOption.map(l => a.row(l.px.toDouble))
        ).flatten
        (0 until rows).toVector.map { row =>
          val price = a.hi - (a.hi - a.lo) * row / (rows - 1).max(1)
          val s = depth(row)
          val raw =
            if s <= 0 then 0L else Screen.round(Layout.barW * log(1 + s / u) / log(1 + r / u))
          val n = if s <= 0 then 0 else raw.max(1).min(Layout.barW).toInt
          val ink = "#" * n
          val marked = if raw > Layout.barW then
            (if isBid(row) then ink.dropRight(1) + ">" else ">" + ink.drop(1)
          )
          else ink
          val bar =
            if isBid(row) then marked.padTo(Layout.barW, ' ') else rjust(marked, Layout.barW)
          val label =
            rjust(s"%.${dec}f".format(price), Layout.labelW) + (if midRow.contains(row) then ">"
                                                                else " ")
          if bestRows.contains(row) then Screen.bold + label + bar + Screen.reset
          else if midRow.contains(row) then Screen.bold + label + Screen.reset + bar
          else label + bar
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
      val labels = Map(0 -> a.hi, rows / 2 -> (a.hi + a.lo) / 2, rows - 1 -> a.lo)
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

  def volumeRows(columns: Vector[Option[Column]]): (Vector[String], Double) =
    val vols = columns.map(_.fold(0.0)(_.volume))
    val vmax = vols.maxOption.getOrElse(0.0)
    val cv = Canvas(columns.size / 2, Layout.volRows)
    vols.zipWithIndex.foreach { (v, x) =>
      if v > 0 then
        val d = Screen.round(v / vmax * (cv.h - 1)).toInt.max(1)
        cv.run(x, cv.h - d, cv.h - 1)
    }
    (cv.lines, vmax)

  def axisRow(first: Long, z: Int, dotCols: Int, zone: ZoneId): String =
    val cells = dotCols / 2
    val fmt = DateTimeFormatter.ofPattern(if z < 60 then "HH:mm:ss" else "HH:mm").withZone(zone)
    val width = if z < 60 then 8 else 5
    def clock(col: Long): String = fmt.format(Instant.ofEpochSecond((first + col) * z))
    val out = Array.fill(cells)(' ')
    val spots = Vector(
      (0, clock(0)),
      (cells / 2 - width / 2, clock(dotCols / 2)),
      (cells - width, clock(dotCols - 1))
    )
    spots.foreach((at, text) =>
      text.zipWithIndex.foreach((ch, i) => if at + i < cells then out(at + i) = ch)
    )
    new String(out)
