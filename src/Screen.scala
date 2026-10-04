package desk

import scala.math.floor

object Screen:
  final case class MarketFrame(lines: Vector[String], axis: Option[Axis], priceRows: Int)
  val esc: String = 27.toChar.toString
  val bold: String = esc + "[1m"
  val dim: String = esc + "[2m"
  val reset: String = esc + "[0m"
  val inverse: String = esc + "[7m"

  def round(x: Double): Long = floor(x + 0.5).toLong

  def short(x: Double): String =
    if x >= 1e9 then f"${x / 1e9}%.1fB"
    else if x >= 1e6 then f"${x / 1e6}%.1fM"
    else if x >= 1e3 then f"${x / 1e3}%.1fk"
    else f"$x%.1f"

  // A length of time in its largest whole unit, such as 2s, 4m, 8h, 6d or 2w.
  def span(seconds: Long): String =
    if seconds < 60 then s"${seconds}s"
    else if seconds < 3600 then s"${seconds / 60}m"
    else if seconds < 86400 then s"${seconds / 3600}h"
    else if seconds < 604800 then s"${seconds / 86400}d"
    else s"${seconds / 604800}w"

  private def midOf(b: Book): Option[String] =
    b.mid.map(_.bigDecimal.stripTrailingZeros().toPlainString)

  // The market in bold first, then whole fields while they fit, then the ping in faint text
  // with one space after it.
  def stats(s: Snapshot, now: Long, cols: Int): String =
    val selection = s.marketPending
      .map(r => s"Checking ${r._2}. Esc cancels")
      .orElse(s.marketNotice)
    if s.prompt.isEmpty && selection.nonEmpty then
      return Chart.fit(bold + s.coin + reset + "  " + selection.get, cols)
    val age = (now - State.lastMsgAt(s.feed)).max(0L) / 1000.0
    val ping = s.feed match
      case Health.Down(_, _) => "ping down"
      case _ if age > 5      => "ping stale"
      case _                 => f"ping $age%.1fs"
    val bad = if s.bad > 0 then s" bad ${s.bad}" else ""
    val promptRoom = if s.prompt.nonEmpty then 24 else 0
    val required = 3 + 2 + (if s.prompt.nonEmpty then 11 else 0)
    val status = Vector(ping + bad, ping).find(_.length + required <= cols).getOrElse {
      ping.take((cols - required).max(0))
    }
    // The fields end at least one space before the ping, which has one space after it.
    val room = cols - status.length - 2
    val marketWidth = (room - promptRoom).min(Layout.marketLabelWidth).max(3)
    val coin = if s.coin.length <= marketWidth then s.coin else s.coin.take(marketWidth - 3) + "..."
    val mid = s.book.flatMap(midOf).orElse(s.ctx.flatMap(_.mid))
    val fields = s.prompt match
      case Some(p) =>
        val width = (room - coin.length - 11).max(0)
        val shown =
          if p.length <= width then p
          else if width >= 3 then "..." + p.takeRight(width - 3)
          else p.takeRight(width)
        Vector(s"market: ${shown}_")
      case None =>
        Vector(
          "mid " + mid.map(v => Trading.display(BigDecimal(v), 12)).getOrElse("unavailable")
        ) ++
          s.ctx.toVector.flatMap { c =>
            val pct = mid.map(m => (m.toDouble / c.prevDayPx.toDouble - 1) * 100).getOrElse(0.0)
            Vector(
              s"mark ${c.mark}",
              s"oracle ${c.oracle}",
              f"24h $pct%+.1f%%",
              f"fund ${c.funding.toDouble * 100}%.4f%%",
              "open interest " + short(c.openInterest.toDouble),
              "volume " + short(c.dayVolume.toDouble)
            )
          }
    val line = fields
      .foldLeft((coin, true)) { case ((line, open), field) =>
        if open && line.length + field.length + 2 <= room then (line + "  " + field, true)
        else (line, false)
      }
      ._1
    val gap = " " * (cols - line.length - status.length - 1).max(1)
    Chart.fit(bold + coin + reset + line.drop(coin.length) + gap + dim + status + reset + " ", cols)

  // The chart block: the stats row, the price rows, the volume row, the reference rows and the
  // time axis, in the left pane and the plot of the regions. Each price row is the decor's label
  // of 17 columns, with the marker on the current price row in its last column, then the depth
  // bar of 21 and the plot. Each reference row is its label of 30, its prices in the rest of the
  // left pane and its plot on the main plot's cells and time scale.
  def renderMarket(
      s: Snapshot,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      regions: Regions,
      decor: AxisDecor,
      headerWidth: Option[Int] = None
  ): MarketFrame =
    val references = State.refCoins.distinct.filterNot(_ == s.coin)
    val priceRows = regions.priceRows
    require(priceRows >= 3)
    val width = regions.left + regions.plot
    val cells = regions.plot
    val label = regions.left - Layout.barW
    val count = Layout.columnsIn(cells, s.chartMode)
    val columns = Chart.columns(s, count, now)
    val axis = Chart.axis(columns, s.book, priceRows, decor.held)
    val currentRow = for a <- axis; m <- s.book.flatMap(_.mid) yield a.trueRow(m.toDouble)
    val labels = axis.fold(Vector.fill(priceRows)(""))(decor.labels)
    val bars = Chart.depthBars(s.book, s.coarse, axis, priceRows, style)
    val plotted = axis.fold(Vector.fill(priceRows)(" " * cells))(a =>
      Chart.decorated(columns, a, cells, s.chartMode, decor.ticketRow(a), decor.marks(a))
    )
    val priceLines = (0 until priceRows).toVector.map { r =>
      val text = labels.lift(r).getOrElse("")
      val marked =
        if currentRow.contains(r) then Chart.fit(text, label - 1) + bold + ">" + reset
        else Chart.fit(text, label)
      marked + bars(r) + plotted(r)
    }
    val (volume, _) = Chart.volumeRows(columns, Style.mono, s.chartMode, cells)
    val volumeLine = Chart.rjust("volume", regions.left - 1) + " " + volume.head
    val priceWidth = regions.left - Layout.leftW
    val referenceLines = references.flatMap { coin =>
      val data = s.refs.getOrElse(coin, Ref.empty)
      val values = Chart.referenceColumns(data, count, s, now)
      val current = values.flatten.lastOption.map(_.last)
      val ax = Chart.axis(values, None, Layout.referenceRows).map { a =>
        current.fold(a) { price =>
          val radius = (price - a.lo).max(a.hi - price)
          Axis(price - radius, price + radius, a.rows)
        }
      }
      val ink = ax.fold(Vector.fill(Layout.referenceRows)(" " * cells))(a =>
        Chart.plot(values, a, cells, s.chartMode)
      )
      val label = if coin == "PAXG" then "PAXG (token tied to gold)" else coin
      val labels =
        Vector(Chart.fit(label, Layout.leftW)).padTo(Layout.referenceRows, " " * Layout.leftW)
      val currentText = current.map(p => Trading.display(BigDecimal(p), priceWidth - 1))
      val prices =
        Chart.scaleRows(ax, Layout.referenceRows, current.map(_ => 1), currentText, priceWidth)
      (0 until Layout.referenceRows).map(i => labels(i) + prices(i) + ink(i))
    }
    val (first, z) = Chart.window(s, count, now)
    val candles = s.chartMode == ChartMode.Candles
    val perCell = if candles then z.toLong / Layout.candleW else 2L * z
    val caption =
      (if candles then "c candles " + span(z) + "/candle" else "c line " + span(perCell) + "/col") +
        (if s.offsetSec > 0 then " history" else "")
    val axisLine = Chart.rjust(caption, regions.left - 1) + " " +
      Chart.timeRow(first * z, perCell, cells, zone)
    val market =
      (stats(s, now, headerWidth.getOrElse(width)) +: priceLines) ++ Vector(
        volumeLine
      ) ++ referenceLines ++ Vector(
        axisLine
      )
    MarketFrame(market, axis, priceRows)
