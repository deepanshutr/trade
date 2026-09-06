package desk

object Layout:
  val minCols = 100
  val minRows = 30
  val leftW = 30
  val labelW = 8
  val barW = 21
  val scaleW = 9
  val volRows = 2
  val gaugeRows = 12
  def cells(cols: Int): Int = cols - leftW - scaleW
  def dotCols(cols: Int): Int = cells(cols) * 2
  def priceRows(rows: Int): Int = rows - 1 - volRows - 1 - gaugeRows - 4
