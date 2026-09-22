package desk

class DisplayCleanupSpec extends munit.FunSuite:
  private val now = Golden.now
  private val zone = java.time.ZoneId.of("UTC")
  private val rules = MarketRules(0, 3, 5)
  private val view = TradingView(
    "XMR",
    Some(BigDecimal(1)),
    "Ready",
    ready = true,
    rules = Some(rules),
    bid = Some(BigDecimal("99.99")),
    ask = Some(BigDecimal(100))
  )
  private val plan = OrderPlan(
    "XMR",
    0,
    Side.Buy,
    OrderKind.Target,
    BigDecimal(2),
    BigDecimal("100.1"),
    BigDecimal(100),
    BigDecimal("1.5"),
    BigDecimal(150),
    Some(BigDecimal(100)),
    BigDecimal("2.5")
  )

  test("header keeps connection health before statistics and beside the market prompt") {
    for cols <- Vector(100, 120) do
      for (health, label) <- Vector(
          Health.Live(now - 300) -> "connection active 0.3s",
          Health.Live(now - 5500) -> "connection stale 5.5s",
          Health.Down(now - 100, now - 300) -> "connection disconnected 0.3s"
        )
      do
        for prompt <- Vector(None, Some("BTC")) do
          val s = Golden.snapshot.copy(coin = "X" * 20, feed = health, bad = 3, prompt = prompt)
          val header = Screen.stats(s, now, cols)
          assertEquals(header.length, cols)
          assert(header.startsWith(label), header)
          assert(header.contains(s.coin), header)
          assert(header.contains(label), header)
          assert(header.contains("bad 3"), header)
          assert(header.indexOf(label) < header.indexOf("mark") || !header.contains("mark"), header)
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
          Health.Live(now - 300) -> "connection active 0.3s",
          Health.Live(now - 5500) -> "connection stale 5.5s",
          Health.Down(now - 100, now - 300) -> "connection disconnected 0.3s"
        )
      do
        val current = submitted.copy(feed = health, bad = 2147483647)
        val header = Screen.stats(current, now, cols)
        assertEquals(header.length, cols)
        assert(header.startsWith(label + " bad 2147483647"), header)
        assert(header.contains(name), header)
        val input = ("Y" * 200 + "TAIL").foldLeft(
          State.reduce(current, Event.Key(Command.StartMarketPrompt), now)
        )((s, c) => State.reduce(s, Event.Key(Command.PromptChar(c)), now))
        val editing = Screen.stats(input, now, cols)
        assertEquals(editing.length, cols)
        assert(editing.startsWith(label + " bad 2147483647"), editing)
        assert(editing.contains("market: ...") && editing.contains("TAIL_"), editing)
        assertEquals(input.coin, name)
  }

  test("idle action keys keep their columns when estimates and configured amounts change") {
    val cases = Vector(
      (TradeSizes(), Left("waiting")),
      (TradeSizes(BigDecimal("501.01"), BigDecimal(999), BigDecimal(1000)), Right(plan)),
      (
        TradeSizes(BigDecimal("1000000000"), BigDecimal("1000000000"), BigDecimal("1000000000")),
        Right(plan.copy(estimatedValue = BigDecimal("999999999.99")))
      )
    )
    val footers = cases.map((sizes, preview) => TradingScreen.rows(view, None, _ => preview, sizes))
    for (row, keys) <- Vector(
        0 -> Vector("1 buy", "2 buy", "3 sell", "4 sell"),
        1 -> Vector("5 buy", "6 buy", "7 sell", "8 sell")
      )
    do for key <- keys do assertEquals(footers.map(_(row).indexOf(key)).distinct.size, 1, key)
    assert(footers.forall(_(2).contains("9 buy 0 sell")))
    assert(footers.forall(_(2).contains("up/down zoom")))
    assert(!footers.exists(_(2).contains("enter sends")))
  }

  test("selected Target keeps its exact limit details and applicable disabled keys") {
    val rows = TradingScreen.rows(view, Some(Side.Buy -> BigDecimal("100.1")), _ => Right(plan))
    assert(rows(0).startsWith("Target XMR buy limit $100.1"), rows.mkString("\n"))
    assert(rows(0).contains("Enter sends") && rows(0).contains("Escape cancels"))
    assert(rows(1).contains("request 2 estimate 1.5 value $150 average $100"))
    assert(rows(2).contains("position long 1") && rows(2).contains("target estimate long 2.5"))
    assert(rows(2).contains("1-8 and left/right inactive"))
  }

  test("Target preview failures keep the actual reason without borrowing the last press") {
    val reason = "Target extends beyond the known exact book"
    val rows = TradingScreen.rows(
      view.copy(ready = false, lastPlan = Some(plan)),
      Some(Side.Sell -> BigDecimal("0.012345")),
      _ => Left(reason)
    )
    assert(rows(0).contains("sell limit $0.012345"), rows.mkString("\n"))
    assert(rows(0).contains("Enter disabled") && rows(0).contains("Escape cancels"))
    assert(rows(1).contains(reason))
    assert(!rows.exists(_.contains("target estimate")))
  }

  test("pending rows use the captured order kind and separate confirmed position") {
    for kind <- Vector(OrderKind.Hard, OrderKind.Best, OrderKind.Target) do
      val captured = plan.copy(kind = kind, side = Side.Sell, positionAfter = BigDecimal("-0.5"))
      val rows = TradingScreen.rows(
        view.copy(
          pending = true,
          ready = false,
          message = "XMR waiting for order result",
          lastPlan = Some(captured)
        ),
        None,
        _ => Left("waiting")
      )
      assert(rows(0).startsWith(s"$kind XMR sell limit $$100.1"), rows.mkString("\n"))
      assert(!rows(0).contains("Target 9"))
      assert(rows(1).contains("request 2 estimate 1.5 value $150 average $100"))
      assert(rows(2).contains("position long 1") && rows(2).contains("press estimate short 0.5"))
  }

  test("scrubbing labels chart history without changing the displayed book or midpoint") {
    for mode <- Vector(ChartMode.Line, ChartMode.Candles) do
      val s = Golden.snapshot.copy(
        chartMode = mode,
        zoom = if mode == ChartMode.Line then 0 else State.candleMinZoom
      )
      for (cols, height) <- Vector((100, 30), (120, 40)) do
        val live = Screen.render(s, cols, height, now, zone, Style.mono).map(Chart.plain)
        val past = Screen
          .render(s.copy(offsetSec = 60), cols, height, now, zone, Style.mono)
          .map(Chart.plain)
        val caption = 1 + Layout.priceRows(height) + Layout.volRows + 2 * Layout.referenceRows
        assert(!live(caption).contains("history"))
        assert(past(caption).contains("history"), past(caption))
        assertEquals(past.head, live.head)
        assert(
          past(caption)
            .take(Layout.leftW)
            .contains(if mode == ChartMode.Line then "line" else "candles")
        )
  }
