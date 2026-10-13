package desk

class IndependentCompactDisplaySpec extends munit.FunSuite:
  import CompactDisplayFixtures.*

  test("the prop frame keeps its rows, its width and the panel's first row, live and in history") {
    for
      (cols, rows) <- sizes
      mode <- ChartMode.values
      selected <- Vector("XMR", "BTC", "PAXG")
      history <- Vector(false, true)
    do
      val offset = if history then (if mode == ChartMode.Line then 596L else 600L) else 0L
      val frame = draw(snapshot(selected, mode, cols, rows, offset), panel(selected))
      val plain = frame.lines.map(Chart.plain)
      val at = s"$mode $cols x $rows $selected history $history"
      assertEquals(plain.size, rows, at)
      assert(plain.forall(_.length == cols), at)
      assert(plain(rows - 12).startsWith("PROP " + selected), at)
      // The time axis closes the chart block on the row above the panel, so a block that
      // overflows loses it.
      val caption = plain(rows - 13).take(38).trim
      assert(caption.startsWith(if mode == ChartMode.Line then "c line " else "c candles "), at)
      assertEquals(caption.endsWith(" history"), history, at)
  }
