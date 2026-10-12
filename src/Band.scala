package desk

import Polymarket.Window.*

// What the band and the rails read from the Polymarket reader for one frame.
final case class Odds(
    points: (String, Polymarket.Window) => Vector[Polymarket.Point],
    state: (String, Polymarket.Window) => Polymarket.State,
    stale: (String, Polymarket.Window) => Boolean,
    unreachable: Boolean
):
  // The newest chance of up in percent and whether it is stale, while the window has a market.
  def chance(coin: String, window: Polymarket.Window): Option[(Double, Boolean)] =
    if unreachable then None
    else
      state(coin, window) match
        case Polymarket.State.Ready(ring, _) =>
          ring.lastOption.map(p => (p.up, stale(coin, window)))
        case _ => None
  def live(coin: String, window: Polymarket.Window): Boolean = chance(coin, window).nonEmpty

object Odds:
  // No points, every state Unreachable and nothing stale.
  val none: Odds =
    Odds((_, _) => Vector.empty, (_, _) => Polymarket.State.Unreachable, (_, _) => false, true)
  def of(reader: PolymarketReader): Odds =
    Odds(reader.points, reader.state, reader.isStale, reader.unreachable)

// The Polymarket band under the chart. Each window draws its chance of up on the chart's time
// axis, 100 at the top and 0 at the bottom, and two lines on one cell draw in bold.
object Band:
  def coin(selected: String): String = if selected == "PAXG" then selected else "BTC"

  def rows(
      s: Snapshot,
      odds: Odds,
      now: Long,
      left: Int,
      cells: Int,
      rows: Int,
      style: Style
  ): Vector[String] =
    val band = coin(s.coin)
    val name = if odds.unreachable then "Polymarket unreachable" else s"Polymarket $band up%"
    val labels = Vector.tabulate(rows) { r =>
      if r == 0 then Screen.dim + Chart.rjust(name + "  100", left - 1) + Screen.reset + " "
      else if r == rows - 1 then Chart.rjust("0", left - 1) + " "
      else if rows == 4 && r == 2 then Chart.rjust("50", left - 1) + " "
      else " " * left
    }
    // A dot column takes the chart's time per cell over two, a dot row a share of 0 to 100.
    val (first, z) = Chart.window(s, Layout.columnsIn(cells, s.chartMode), now)
    val perCell = if s.chartMode == ChartMode.Candles then z.toLong / Layout.candleW else 2L * z
    val startMs = first * z * 1000L
    val msPerDot = perCell * 500L
    def x(t: Long): Long = Math.floorDiv(t - startMs, msPerDot)
    def y(up: Double): Int = math.floor((100 - up) / 100 * rows * 4).toInt.max(0).min(rows * 4 - 1)
    def line(window: Polymarket.Window, ink: String): Option[(Canvas, String)] =
      val points = odds.points(band, window)
      Option.when(points.nonEmpty) {
        val canvas = Canvas(cells, rows)
        val dots = points.map(p => (x(p.at), y(p.up)))
        val (lastX, lastY) = dots.last
        val ready = odds.state(band, window).isInstanceOf[Polymarket.State.Ready]
        val path = if ready && x(now) > lastX then dots :+ (x(now), lastY) else dots
        val pairs = if path.size == 1 then Vector((path.head, path.head)) else path.zip(path.tail)
        // A segment wholly left or wholly right of the plot is skipped unseen.
        pairs.foreach { case ((x0, y0), (x1, y1)) =>
          if !(x0 < 0 && x1 < 0) && !(x0 >= canvas.w && x1 >= canvas.w) then
            canvas.segment(x0.toInt, y0, x1.toInt, y1)
        }
        (canvas, if odds.stale(band, window) then Screen.dim else ink)
      }
    val inks =
      Vector(M5 -> style.w5, M15 -> style.w15, H1 -> style.w1h, H4 -> style.w4h, D1 -> style.w1d)
    val grids =
      if odds.unreachable then Vector.empty
      else inks.flatMap((window, ink) => line(window, ink)).map((cv, ink) => (cv.grid, ink))
    val plots = Vector.tabulate(rows) { r =>
      Chart.styled(Vector.tabulate(cells) { c =>
        val owners = grids.filter((grid, _) => grid(r)(c)._1 != ' ')
        val bits = owners.map((grid, _) => grid(r)(c)._1 - 0x2800).foldLeft(0)(_ | _)
        owners.map(_._2) match
          case Vector()    => (' ', "")
          case Vector(ink) => ((0x2800 + bits).toChar, ink)
          case _           => ((0x2800 + bits).toChar, style.overlap)
      })
    }
    labels.zip(plots).map(_ + _)
