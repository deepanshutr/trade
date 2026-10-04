package desk

class DisplayCleanupSpec extends munit.FunSuite:
  private def market(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): Vector[String] =
    // cols is the chart area here, so the terminal is that plus the outlook and its bar.
    Screen
      .renderMarket(
        s,
        now,
        zone,
        style,
        Layout
          .regions(cols + Layout.outlookWidth + 1, rows)
          .copy(priceRows = rows - 13 - 3 * State.refCoins.distinct.count(_ != s.coin)),
        AxisDecor.plain
      )
      .lines
  private val now = Golden.now
  private val zone = java.time.ZoneId.of("UTC")
  test("header keeps the market first and the ping last beside the market prompt") {
    for cols <- Vector(100, 120) do
      for (health, label) <- Vector(
          Health.Live(now - 300) -> "ping 0.3s",
          Health.Live(now - 5500) -> "ping stale",
          Health.Down(now - 100, now - 300) -> "ping down"
        )
      do
        for prompt <- Vector(None, Some("BTC")) do
          val s = Golden.snapshot.copy(coin = "X" * 20, feed = health, bad = 3, prompt = prompt)
          val header = Chart.plain(Screen.stats(s, now, cols))
          assertEquals(header.length, cols)
          assert(header.startsWith(s.coin + "  "), header)
          assert(header.endsWith(label + " bad 3 "), header)
          assert(header.indexOf(label) > header.indexOf("mark"), header)
          prompt.foreach(p => assert(header.contains(s"market: ${p}_"), header))
  }

  test("the actual market prompt keeps full identity while the header reserves health and input") {
    val name = "X" * 37 + "END"
    val typed =
      name.foldLeft(State.reduce(Golden.snapshot, Event.Key(Command.StartMarketPrompt), now)) {
        (s, c) => State.reduce(s, Event.Key(Command.PromptChar(c)), now)
      }
    val submitted = State.reduce(typed, Event.Key(Command.PromptSubmit), now)
    assertEquals(submitted.coin, name)
    assertEquals(submitted.prompt, None)
    for cols <- Vector(100, 120) do
      for (health, label) <- Vector(
          Health.Live(now - 300) -> "ping 0.3s",
          Health.Live(now - 5500) -> "ping stale",
          Health.Down(now - 100, now - 300) -> "ping down"
        )
      do
        val current = submitted.copy(feed = health, bad = 2147483647)
        val header = Chart.plain(Screen.stats(current, now, cols))
        assertEquals(header.length, cols)
        assert(header.endsWith(label + " bad 2147483647 "), header)
        assert(header.startsWith(name + "  "), header)
        val input = ("Y" * 200 + "TAIL").foldLeft(
          State.reduce(current, Event.Key(Command.StartMarketPrompt), now)
        )((s, c) => State.reduce(s, Event.Key(Command.PromptChar(c)), now))
        val editing = Chart.plain(Screen.stats(input, now, cols))
        assertEquals(editing.length, cols)
        assert(editing.endsWith(label + " bad 2147483647 "), editing)
        assert(editing.startsWith(name + "  "), editing)
        assert(editing.contains("market: ...") && editing.contains("TAIL_"), editing)
        assertEquals(input.coin, name)
  }

  test("scrubbing labels chart history without changing the displayed book or midpoint") {
    for mode <- Vector(ChartMode.Line, ChartMode.Candles) do
      val s = Golden.snapshot.copy(
        chartMode = mode,
        zoom = if mode == ChartMode.Line then 0 else State.candleMinZoom
      )
      for (cols, height) <- Vector((100, 30), (120, 40)) do
        val live = market(s, cols, height, now, zone, Style.mono).map(Chart.plain)
        val past = market(s.copy(offsetSec = 60), cols, height, now, zone, Style.mono)
          .map(Chart.plain)
        val caption = live.size - 1
        assert(!live(caption).contains("history"))
        assert(past(caption).contains("history"), past(caption))
        assertEquals(past.head, live.head)
        assert(
          past(caption)
            .take(Layout.leftW)
            .contains(if mode == ChartMode.Line then "line" else "candles")
        )
  }
