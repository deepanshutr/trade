package desk

object Layout:
  val marketLabelWidth = 40
  val minCols = 100
  val minRows = 30
  val leftW = 30
  val labelW = 8
  val barW = 21
  val scaleW = 9
  val volRows = 1
  val referenceRows = 3
  val gaugeRows = 6
  val footerRows = 4
  val candleW = 4
  def workspaceMarketWidth(cols: Int): Int = (cols * 3 / 5).min(cols - 47)
  val outlookWidth = 38
  def workspaceChartWidth(cols: Int): Int = cols - outlookWidth - 1
  def cells(cols: Int): Int = cols - leftW - scaleW
  def dotCols(cols: Int): Int = cells(cols) * 2
  def plotColumns(cols: Int, mode: ChartMode): Int = mode match
    case ChartMode.Line    => dotCols(cols)
    case ChartMode.Candles => cells(cols) / candleW
  def priceRows(rows: Int, referenceCount: Int = 2): Int =
    rows - 1 - volRows - referenceCount * referenceRows - 1 - gaugeRows - footerRows
