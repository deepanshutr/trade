package desk

class WorkspaceHeightSpec extends munit.FunSuite:
  test("market rendering fits above trading and gauges fill the remaining workspace rows") {
    // For every allowed height, priceRows <= tradeTop - 3 - 3 * referenceCount.
    // renderMarket emits priceRows + 3 + 3 * referenceCount lines, independent of width.
    for
      cols <- Vector(Layout.minCols, Layout.minCols + 1, 160, 360)
      rows <- (Layout.minRows to Layout.minRows + 10).toVector ++ Vector(60, 99)
      coin <- Vector("XMR", "BTC", "PAXG")
      mode <- ChartMode.values.toVector
      populated <- Vector(false, true)
    do
      val snapshot =
        if populated then CompactDisplayFixtures.snapshot(coin, mode, cols, rows)
        else Snapshot.initial(coin, CompactDisplayFixtures.now).copy(chartMode = mode)
      val tradeTop = rows - 15
      val references = State.refCoins.distinct.count(_ != coin)
      val priceRows = (tradeTop - 8).min(tradeTop - 3 - Layout.referenceRows * references)
      assert(priceRows >= 3, s"$cols x $rows $coin")
      val market = Screen.renderMarket(
        snapshot,
        Layout.workspaceChartWidth(cols),
        priceRows,
        CompactDisplayFixtures.now,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      assertEquals(market.lines.size, priceRows + 3 + Layout.referenceRows * references)
      assert(market.lines.size <= tradeTop, s"$cols x $rows $coin $mode")
      val gauges = Workspace.indicatorRows(
        snapshot,
        CompactDisplayFixtures.now,
        (Layout.workspaceMarketWidth(cols) - 1) / 2,
        Style.mono
      )
      assertEquals(gauges.size, 12)
      assertEquals((market.lines.padTo(tradeTop, "") ++ Vector.fill(3)("") ++ gauges).size, rows)
  }

  test("every workspace frame and button fits the requested dimensions") {
    for
      cols <- Vector(100, 101, 160, 360)
      rows <- Vector(30, 31, 60, 99)
      coin <- Vector("XMR", "BTC", "PAXG")
      style <- Vector(Style.mono, Style.colour)
      mode <- ChartMode.values.toVector
    do
      val snapshot = CompactDisplayFixtures.snapshot(coin, mode, cols, rows)
      val frame = CompactDisplayFixtures.draw(snapshot, CompactDisplayFixtures.panel(coin), style)
      assertEquals(frame.lines.size, rows)
      assert(frame.lines.forall(line => Chart.plain(line).length == cols))
      assert(frame.controls.forall(button => button.y >= 1 && button.y <= rows))
  }
