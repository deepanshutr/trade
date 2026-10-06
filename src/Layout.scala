package desk

// The screen's regions. The left pane holds the axis labels and the depth bars above the band
// and the panel inside it, the plot is the chart's cells, and the band's right region holds the
// gauges. Live, prop and view share this one shape.
final case class Regions(
    left: Int,
    plot: Int,
    outlook: Int,
    priceRows: Int,
    bandRight: Int,
    panel: Int
)

object Layout:
  val marketLabelWidth = 40
  val minCols = 100
  val minRows = 30
  // The label of a reference row. Its prices take the rest of the left pane.
  val leftW = 30
  val barW = 21
  val volRows = 1
  val referenceRows = 3
  val candleW = 4
  val outlookWidth = 38
  def workspaceChartWidth(cols: Int): Int = cols - outlookWidth - 1
  // A line draws two dot columns in a cell and a candle takes four cells.
  def columnsIn(cells: Int, mode: ChartMode): Int = mode match
    case ChartMode.Line    => cells * 2
    case ChartMode.Candles => cells / candleW
  def regions(cols: Int, rows: Int): Regions =
    Regions(38, cols - 77, outlookWidth, rows - 21, cols - 39, 38)
