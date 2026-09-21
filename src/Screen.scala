package desk

import scala.math.floor

object Screen:
  final case class MarketFrame(lines: Vector[String], axis: Option[Axis], priceRows: Int)
  val esc: String = 27.toChar.toString
  val bold: String = esc + "[1m"
  val dim: String = esc + "[2m"
  val reset: String = esc + "[0m"

  def round(x: Double): Long = floor(x + 0.5).toLong

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
    val age = (now - State.lastMsgAt(s.feed)).max(0L) / 1000.0
    val health = s.feed match
      case Health.Down(_, _) => "disconnected"
      case _ if age > 5      => "stale"
      case _                 => "active"
    val bad = if s.bad > 0 then s" bad ${s.bad}" else ""
    val status = f"connection $health $age%.1fs$bad"
    val promptRoom = if s.prompt.nonEmpty then 24 else 0
    val marketWidth = (cols - status.length - 2 - promptRoom).min(Layout.marketLabelWidth).max(3)
    val coin = if s.coin.length <= marketWidth then s.coin else s.coin.take(marketWidth - 3) + "..."
    val prefix = status + "  " + coin
    val mid = s.book.flatMap(midOf).orElse(s.ctx.flatMap(_.mid))
    val fields = s.prompt match
      case Some(p) =>
        val width = (cols - prefix.length - 11).max(0)
        val shown = if p.length <= width then p else "..." + p.takeRight((width - 3).max(0))
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
    fields
      .foldLeft(prefix) { (line, field) =>
        if line.length + field.length + 2 <= cols then line + "  " + field else line
      }
      .padTo(cols, ' ')

  def render(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      footer: Vector[String] = Vector.empty,
      marketOnly: Boolean = false,
      reservedRows: Int = Layout.gaugeRows + Layout.footerRows
  ): Vector[String] =
    if cols < (if marketOnly then Layout.leftW + Layout.scaleW + 4
               else Layout.minCols) || rows < Layout.minRows
    then Vector(s"need ${Layout.minCols}x${Layout.minRows}, have ${cols}x$rows")
    else
      val market = renderMarket(s, cols, rows, now, zone, style, marketOnly, reservedRows)
      val controls = footer
        .take(Layout.footerRows)
        .padTo(Layout.footerRows, "")
        .map(Chart.fit(_, cols))
      if marketOnly then market.lines
      else market.lines ++ Gauges.rows(s, now, cols, style) ++ controls

  def renderMarket(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      marketOnly: Boolean,
      reservedRows: Int,
      maxPriceRows: Int = Int.MaxValue
  ): MarketFrame =
    val references = s.referenceCoins.distinct.filterNot(_ == s.coin)
    val priceRows = (Layout.priceRows(
      rows,
      references.size
    ) + Layout.gaugeRows + Layout.footerRows - reservedRows).min(maxPriceRows)
    require(priceRows >= 3)
    val cells = Layout.cells(cols)
    val count = Layout.plotColumns(cols, s.chartMode)
    val columns = Chart.columns(s, count, now)
    val axis = Chart.axis(columns, s.book, priceRows)
    val mid = s.book.flatMap(midOf)
    val midRow = for a <- axis; m <- mid yield a.row(m.toDouble)
    val left = Chart.ladderRows(s.book, s.coarse, axis, priceRows, midRow, style)
    val chart = axis.fold(Vector.fill(priceRows)(" " * cells))(a =>
      Chart.plot(columns, a, cells, s.chartMode)
    )
    val scale = Chart.scaleRows(axis, priceRows, midRow, mid)
    val priceLines = (0 until priceRows).toVector.map(r => left(r) + chart(r) + scale(r))
    val (volume, maximum) = Chart.volumeRows(columns, Style.mono, s.chartMode, cells)
    val volumeLine = Chart.rjust("volume", Layout.leftW - 1) + " " + volume.head +
      Chart.rjust("v " + short(maximum), Layout.scaleW)
    val referenceLines = references.flatMap { coin =>
      val data = s.refs.getOrElse(coin, Ref.empty)
      val values = Chart.referenceColumns(data, count, s, now)
      val ax = Chart.axis(values, None, Layout.referenceRows)
      val ink = ax.fold(Vector.fill(Layout.referenceRows)(" " * cells))(a =>
        Chart.plot(values, a, cells, s.chartMode)
      )
      val labels = if marketOnly then
        val label = if coin == "PAXG" then "PAXG (token tied to gold)" else coin
        Vector(Chart.fit(label, Layout.leftW)).padTo(Layout.referenceRows, " " * Layout.leftW)
      else Gauges.correlationRows(s, coin)
      val prices = Chart.scaleRows(ax, Layout.referenceRows, None, None)
      (0 until Layout.referenceRows).map(i => labels(i) + ink(i) + prices(i))
    }
    val (first, z) = Chart.window(s, count, now)
    val unit = if s.chartMode == ChartMode.Candles then "/candle" else "/col"
    val mode = if s.chartMode == ChartMode.Candles then "candles" else "line"
    val caption = "c " + mode + " " + State.zoomLabel(z) + unit +
      (if s.offsetSec > 0 then " history" else "")
    val axisLine = Chart.rjust(caption, Layout.leftW - 1) + " " +
      Chart.axisRow(first, z, count, zone, cells) + " " * Layout.scaleW
    val market =
      (stats(s, now, cols) +: priceLines) ++ Vector(volumeLine) ++ referenceLines ++ Vector(
        axisLine
      )
    MarketFrame(market, axis, priceRows)
