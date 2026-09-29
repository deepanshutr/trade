package desk

class WorkspaceHeightSpec extends munit.FunSuite:
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
