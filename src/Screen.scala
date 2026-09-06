package desk

import scala.math.floor

object Screen:
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
    val text = s.prompt match
      case Some(p) => s"market: ${p}_"
      case None    =>
        val mid = s.book.flatMap(midOf).orElse(s.ctx.flatMap(_.mid))
        val age = (now - State.lastMsgAt(s.feed)).max(0L)
        val fresh = if age > 5000 then s"STALE ${age / 1000}s" else f"live ${age / 1000.0}%.1fs"
        val badPart = if s.bad > 0 then s"  bad ${s.bad}" else ""
        s.ctx match
          case Some(c) =>
            val pct = mid.map(m => (m.toDouble / c.prevDayPx.toDouble - 1) * 100).getOrElse(0.0)
            f"${s.coin}  ${mid.getOrElse("-")}  mark ${c.mark}  orc ${c.oracle}  24h $pct%+.1f%%  " +
              f"fund ${c.funding.toDouble * 100}%.4f%%  OI ${short(c.openInterest.toDouble)}  " +
              f"vol ${short(c.dayVolume.toDouble)}  $fresh$badPart"
          case None => s"${s.coin}  ${mid.getOrElse("-")}  $fresh$badPart"
    val fitted = if text.length > cols then text.replace("  ", " ") else text
    fitted.padTo(cols, ' ').take(cols)

  def render(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      footer: Vector[String] = Vector.empty
  ): Vector[String] =
    if cols < Layout.minCols || rows < Layout.minRows then
      Vector(s"need ${Layout.minCols}x${Layout.minRows}, have ${cols}x$rows")
    else
      val references = s.referenceCoins.distinct.filterNot(_ == s.coin)
      val priceRows = Layout.priceRows(rows, references.size)
      val cells = Layout.cells(cols)
      val count = Layout.dotCols(cols)
      val columns = Chart.columns(s, count, now)
      val axis = Chart.axis(columns, s.book, priceRows)
      val mid = s.book.flatMap(midOf)
      val midRow = for a <- axis; m <- mid yield a.row(m.toDouble)
      val left = Chart.ladderRows(s.book, s.coarse, axis, priceRows, midRow, style)
      val chart =
        axis.fold(Vector.fill(priceRows)(" " * cells))(a => Chart.line(columns, a, Vector.empty))
      val scale = Chart.scaleRows(axis, priceRows, midRow, mid)
      val priceLines = (0 until priceRows).toVector.map(r => left(r) + chart(r) + scale(r))
      val (volume, maximum) = Chart.volumeRows(columns, Style.mono)
      val volumeLine = Chart.rjust("volume", Layout.leftW - 1) + " " + volume.head +
        Chart.rjust("v " + short(maximum), Layout.scaleW)
      val referenceLines = references.flatMap { coin =>
        val data = s.refs.getOrElse(coin, Ref.empty)
        val values = Chart.referenceColumns(data, count, s, now)
        val ax = Chart.axis(values, None, Layout.referenceRows)
        val ink = ax.fold(Vector.fill(Layout.referenceRows)(" " * cells))(a =>
          Chart.line(values, a, Vector.empty)
        )
        val labels = Gauges.correlationRows(s, coin)
        val prices = Chart.scaleRows(ax, Layout.referenceRows, None, None)
        (0 until Layout.referenceRows).map(i => labels(i) + ink(i) + prices(i))
      }
      val (first, z) = Chart.window(s, count, now)
      val axisLine = Chart.rjust(State.zoomLabel(z) + "/col", Layout.leftW - 1) + " " +
        Chart.axisRow(first, z, count, zone) + " " * Layout.scaleW
      val controls = footer
        .take(Layout.footerRows)
        .padTo(Layout.footerRows, "")
        .map(Chart.fit(_, cols))
      (stats(s, now, cols) +: priceLines) ++ Vector(volumeLine) ++ referenceLines ++
        Vector(axisLine) ++ Gauges.rows(s, now, cols, style) ++ controls
