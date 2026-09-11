package desk

class WorkspaceViewportSpec extends munit.FunSuite:
  test("history moves by a quarter of the visible market plot after resize") {
    for (width, seconds) <- Vector(100 -> 7L, 120 -> 16L, 160 -> 28L) do
      val initial = Snapshot.initial("XMR", 0L).copy(workspace = true)
      val sized = State.reduce(initial, Event.Resize(width, 40), 0L)
      val back = State.reduce(sized, Event.Key(Command.ScrubBack), 0L)
      assertEquals(back.offsetSec, seconds)
      assertEquals(State.reduce(back, Event.Key(Command.ScrubForward), 0L).offsetSec, 0L)
    assertEquals(Layout.workspaceMarketWidth(100), 53)
    assertEquals(Layout.workspaceMarketWidth(120), 72)
  }

  test("even a three-candle plot moves by at least one candle") {
    val line = Snapshot.initial("XMR", 0L).copy(workspace = true)
    val candles = State.reduce(line, Event.Key(Command.ToggleCandles), 0L)
    assertEquals(candles.chartMode, ChartMode.Candles)
    assertEquals(State.reduce(candles, Event.Key(Command.ScrubBack), 0L).offsetSec, 60L)
  }
