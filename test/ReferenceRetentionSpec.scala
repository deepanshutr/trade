package desk

class ReferenceRetentionSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val sec = now / 1000

  private def candle(at: Long, close: Double): Candle =
    Candle(at, close - 1, close + 2, close - 2, close, close + 3)

  private def quote(s: Snapshot, coin: String, at: Long, mid: Int): Snapshot =
    State.reduce(
      s,
      Event.BboUpdate(
        coin,
        Level((mid - 1).toString, "1", 1),
        Level((mid + 1).toString, "1", 1),
        at
      ),
      at
    )

  private def select(s: Snapshot, coin: String, at: Long = now): Snapshot =
    val opened = State.reduce(s, Event.Key(Command.StartMarketPrompt), at)
    val typed =
      coin.foldLeft(opened)((state, c) => State.reduce(state, Event.Key(Command.PromptChar(c)), at))
    State.reduce(typed, Event.Key(Command.PromptSubmit), at)

  test("a reference keeps earlier samples after serving as the main market") {
    val first = now - 30000
    val second = now - 20000
    val third = now - 9000
    val original = quote(
      quote(Snapshot.initial("XMR", first), "BTC", first, 100),
      "BTC",
      second,
      102
    )
    val control = quote(original, "BTC", third, 104)
    assertEquals(
      control.refs("BTC").samples,
      Vector(
        first / 1000 -> 100.0,
        second / 1000 -> 102.0,
        third / 1000 -> 104.0
      )
    )
    val selected = select(original, "BTC", now - 10000)
    val restored = select(quote(selected, "BTC", third, 104), "XMR")
    assertEquals(restored.refs("BTC").samples, control.refs("BTC").samples)
  }

  test(
    "a reference keeps older candles beside updates received as the main market"
  ) {
    val older = Vector(candle(now - 180000, 102), candle(now - 120000, 103))
    val latest = candle(now - 60000, 104)
    val original = State.reduce(
      Snapshot.initial("XMR", now - 180000),
      Event.CandleHistory("BTC", "1m", older),
      now - 10000
    )
    val control = State.reduce(
      original,
      Event.CandleUpdate("BTC", "1m", latest),
      now - 5000
    )
    assertEquals(control.refs("BTC").candles("1m"), older :+ latest)
    val selected = select(original, "BTC", now - 10000)
    val live = State.reduce(
      selected,
      Event.CandleUpdate("BTC", "1m", latest),
      now - 5000
    )
    val restored = select(live, "XMR")
    assertEquals(
      restored.refs("BTC").candles("1m"),
      control.refs("BTC").candles("1m")
    )
  }

  test(
    "overlapping histories are ordered and outgoing main values win equal timestamps"
  ) {
    val first = candle(now - 180000, 100)
    val middle = candle(now - 120000, 101)
    val previous = candle(now - 60000, 102)
    val replaced = candle(now - 60000, 110)
    val hour = candle(now - 3600000, 95)
    val old = Ref(
      Vector(sec - 30 -> 100.0, sec - 10 -> 102.0),
      Map("1m" -> Vector(first, previous), "1h" -> Vector(hour))
    )
    val gold = Ref(
      Vector(sec - 40 -> 200.0),
      Map("5m" -> Vector(candle(now - 300000, 200)))
    )
    val busy = Snapshot
      .initial("BTC", now)
      .copy(
        refs = Map("BTC" -> old, "PAXG" -> gold),
        samples = Vector(sec - 20 -> 101.0, sec - 10 -> 110.0),
        candles = Map("1m" -> Vector(middle, replaced))
      )
    val restored = select(busy, "XMR")
    assertEquals(
      restored.refs,
      Map(
        "BTC" -> Ref(
          Vector(sec - 30 -> 100.0, sec - 20 -> 101.0, sec - 10 -> 110.0),
          Map("1m" -> Vector(first, middle, replaced), "1h" -> Vector(hour))
        ),
        "PAXG" -> gold
      )
    )
    assertEquals(restored.samples, Vector.empty[(Long, Double)])
    assertEquals(restored.candles, Map.empty[String, Vector[Candle]])
  }

  test(
    "switching retains the exact six hour sample boundary and removes older samples"
  ) {
    val cutoff = sec - 21_600L
    val old = Ref(Vector(cutoff - 1 -> 90.0, cutoff -> 100.0), Map.empty)
    val start = Snapshot.initial("BTC", now).copy(refs = Map("BTC" -> old))
    assertEquals(
      select(start, "XMR").refs("BTC").samples,
      Vector(cutoff -> 100.0)
    )
    val busy = start.copy(samples = Vector(cutoff - 2 -> 80.0, sec -> 110.0))
    assertEquals(
      select(busy, "XMR").refs("BTC").samples,
      Vector(cutoff -> 100.0, sec -> 110.0)
    )
  }

  test(
    "merged candle history keeps the newest 5200 entries after duplicate replacement"
  ) {
    val older = Vector.tabulate(5200)(i => candle(i * 60000L, i.toDouble))
    val replaced = candle(5199L * 60000L, 9000)
    val latest = candle(5200L * 60000L, 9001)
    val busy = Snapshot
      .initial("BTC", now)
      .copy(
        refs = Map("BTC" -> Ref(Vector.empty, Map("1m" -> older))),
        candles = Map("1m" -> Vector(replaced, latest))
      )
    val merged = select(busy, "XMR").refs("BTC").candles("1m")
    val expected =
      (1 until 5199).map(i => candle(i * 60000L, i.toDouble)).toVector ++
        Vector(replaced, latest)
    assertEquals(merged, expected)
    assertEquals(merged.size, 5200)
    assertEquals(merged.head.t, 60000L)
    assertEquals(merged.last, latest)
  }

  test("empty outgoing vectors keep recent history and untouched intervals") {
    val old = Ref(
      Vector(sec - 1 -> 100.0),
      Map(
        "1m" -> Vector(candle(now - 60000, 100)),
        "1h" -> Vector(candle(now - 3600000, 95))
      )
    )
    val gold = Ref(
      Vector(sec - 2 -> 200.0),
      Map("5m" -> Vector(candle(now - 300000, 200)))
    )
    val start = Snapshot
      .initial("BTC", now)
      .copy(
        refs = Map("BTC" -> old, "PAXG" -> gold),
        candles = Map("1m" -> Vector.empty, "1d" -> Vector.empty)
      )
    assertEquals(select(start, "XMR").refs, start.refs)
    assertEquals(
      select(Snapshot.initial("BTC", now), "XMR").refs("BTC"),
      Ref.empty
    )
  }

  test(
    "an empty retained reference accepts outgoing samples and a new candle interval"
  ) {
    val sample = sec -> 110.0
    val latest = candle(now - 60000, 110)
    val start = Snapshot
      .initial("BTC", now)
      .copy(
        samples = Vector(sample),
        candles = Map("1m" -> Vector(latest))
      )
    val restored = select(start, "XMR")
    assertEquals(
      restored.refs("BTC"),
      Ref(Vector(sample), Map("1m" -> Vector(latest)))
    )
    assertEquals(restored.refs("PAXG"), Ref.empty)
  }
