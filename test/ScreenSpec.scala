package desk

class ScreenSpec extends munit.FunSuite:
  private def market(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): Vector[String] =
    Screen
      .renderMarket(
        s,
        cols,
        rows - 13 - 3 * State.refCoins.distinct.count(_ != s.coin),
        now,
        zone,
        style
      )
      .lines
  val t0 = 1_700_000_000_000L
  val zone = java.time.ZoneId.of("UTC")
  def lv(px: String, sz: String) = Level(px, sz, 1)
  private def plain(l: String): String = Chart.plain(l)

  test("the escape sequences are built from the escape character") {
    assertEquals(Screen.esc, 27.toChar.toString)
    assertEquals(Screen.bold, Screen.esc + "[1m")
    assertEquals(Screen.dim, Screen.esc + "[2m")
    assertEquals(Screen.reset, Screen.esc + "[0m")
  }
  test("rounding is half up") {
    assertEquals(Screen.round(0.5), 1L)
    assertEquals(Screen.round(-0.5), 0L)
    assertEquals(Screen.round(2.49), 2L)
  }
  test("the stats line shows the figures, the age, stale, bad and the prompt") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot.initial("XMR", t0).copy(ctx = Some(ctx), feed = Health.Live(t0 - 300))
    val line = Screen.stats(s, t0, 110)
    assertEquals(line.length, 110)
    assert(line.startsWith("connection active 0.3s  XMR  mid 522.26"), line)
    assert(line.contains("mark 522.28") && line.contains("oracle 521.515"), line)
    val down = s.copy(feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val staleLine = Screen.stats(down, t0, 110)
    assertEquals(staleLine.length, 110)
    assert(staleLine.contains("connection disconnected 12.3s"), staleLine)
    assert(staleLine.contains("bad 3"), staleLine)
    assert(Screen.stats(s.copy(prompt = Some("BT")), t0, 110).contains("market: BT_"))
    val bare = Snapshot.initial("XMR", t0).copy(feed = Health.Live(t0))
    assert(Screen.stats(bare, t0, 110).startsWith("connection active 0.0s  XMR  mid unavailable"))
  }
  test("the stats line prints the book mid, falling back to the context mid without a book") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val book = Book(Vector(lv("500.10", "1")), Vector(lv("500.20", "1")), t0)
    val s = Snapshot.initial("XMR", t0).copy(ctx = Some(ctx), feed = Health.Live(t0))
    assert(Screen.stats(s, t0, 110).contains("mid 522.26"), "no book")
    val withBook = Screen.stats(s.copy(book = Some(book)), t0, 110)
    assert(withBook.contains("mid 500.15"), withBook)
  }
  test("the stats mid uses the trading width format") {
    for value <- Vector("0.000012345678", "123456789.123") do
      val s = Snapshot
        .initial("XMR", t0)
        .copy(ctx =
          Some(
            Ctx("400", "401", "0.0001", "1", "1", "300", Some(value))
          )
        )
      val line = Screen.stats(s, t0, 100)
      assert(line.contains("mid " + Trading.display(BigDecimal(value), 12)), line)
  }
  test("the feed is stale past five seconds, not six") {
    val s = Snapshot.initial("XMR", t0).copy(feed = Health.Live(t0 - 5500))
    val line = Screen.stats(s, t0, 100)
    assert(line.contains("connection stale"), line)
  }
  test("the socket age never goes negative, even if the last message is ahead of now") {
    val s = Snapshot.initial("X", t0).copy(feed = Health.Live(t0 + 400))
    val line = Screen.stats(s, t0, 100)
    assert(line.contains("connection active 0.0s"), line)
  }
  test("a stats line never exceeds the terminal width") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot
      .initial("flx:GOLD", t0)
      .copy(ctx = Some(ctx), feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val line = Screen.stats(s, t0, 100)
    assertEquals(line.length, 100)
    assert(line.contains("connection disconnected 12.3s"), line)
  }
  test("short numbers use k, M and B with one decimal") {
    assertEquals(Screen.short(31_022_816.0), "31.0M")
    assertEquals(Screen.short(208_410.0), "208.4k")
    assertEquals(Screen.short(12.34), "12.3")
    assertEquals(Screen.short(3_314_300_000.0), "3.3B")
  }

  test("render yields rows lines of the width at both golden sizes") {
    for (cols, rows) <- Vector((100, 30), (110, 30), (120, 40), (166, 50)) do
      val frame = market(Golden.snapshot, cols, rows, Golden.now, zone, Style.colour)
      assertEquals(frame.size, rows - 10)
      assert(frame.map(plain).forall(_.length == cols), frame.map(plain).map(_.length))
  }

  test("the price rows carry the mid marker on one row with the bold exact mid in the scale") {
    val frame = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.colour)
    val priceRows = frame.slice(1, 12)
    val marked = priceRows.filter(l => plain(l).charAt(8) == '>')
    assertEquals(marked.size, 1)
    assert(
      marked.head.contains(Screen.bold + "  <528.045" + Screen.reset) || marked.head.contains(
        Screen.bold + " <528.045" + Screen.reset
      ),
      plain(marked.head)
    )
    assert(priceRows.count(_.contains("<528.045")) == 1)
  }

  test("the colour frame keeps side colours and reference plots read without them") {
    val colour = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.colour)
    val mono = market(Golden.snapshot, 120, 40, Golden.now, zone, Style.mono)
    assert(colour.exists(_.contains(Style.colour.bid)))
    assert(colour.exists(_.contains(Style.colour.ask)))
    assert(!colour.slice(12, 19).exists(_.contains(Screen.esc + "[3")))
    assert(!mono.exists(_.contains(Screen.esc + "[3")))
  }

  test("the price rows carry braille bars on every visible row and no hash marks") {
    val frame = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.mono).map(plain)
    val bars = frame.slice(1, 12).map(_.slice(9, 30))
    assert(bars.count(_.trim.nonEmpty) >= 10, bars)
    assert(!frame.exists(_.contains("#")))
    assert(bars.exists(_.contains(Chart.FULL)))
  }

  test("when the main market is itself a reference market, that reference draws no line") {
    val asBtc = Golden.snapshot.copy(coin = "BTC")
    val withBtcRefData = market(asBtc, 100, 30, Golden.now, zone, Style.mono).map(plain)
    val withoutBtcRefData = market(
      asBtc.copy(refs = asBtc.refs.updated("BTC", Ref.empty)),
      100,
      30,
      Golden.now,
      zone,
      Style.mono
    )
      .map(plain)
    assertEquals(withBtcRefData, withoutBtcRefData)
  }

  test("volume and shared clocks occupy their own rows") {
    val frame = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.colour).map(plain)
    assert(frame(12).startsWith(" " * 23 + "volume "))
    assert(frame(12).drop(91).trim.startsWith("v "))
    assert(frame(13).startsWith("BTC"))
    assert(frame(16).startsWith("PAXG"))
    assert(frame(19).startsWith(" " * 16 + "c line 1s/col "))
    assert(frame(19).drop(30).take(8).matches("\\d\\d:\\d\\d:\\d\\d"))
    assert(frame(19).drop(83).take(8).matches("\\d\\d:\\d\\d:\\d\\d"))
  }

  test("a coarse zoom draws the candles, the minute clocks and the candle volume") {
    val minute = Golden.now / 60_000L * 60_000L
    val candles = Vector.tabulate(130) { i =>
      val o = 503.0 + (i % 7) * 0.1
      Candle(minute - (129 - i) * 60_000L, o, o + 0.2, o - 0.2, o + 0.05, 1.0 + i % 5)
    }
    val s = Golden.snapshot.copy(candles = Map("1m" -> candles), zoom = 5)
    val frame = market(s, 100, 30, Golden.now, zone, Style.mono).map(plain)
    assertEquals(frame.size, 20)
    assert(frame.forall(_.length == 100), frame.map(_.length))
    assert(frame(19).contains("1m/col"), frame(19))
    assertEquals("""\d\d:\d\d""".r.findAllIn(frame(19)).size, 3, frame(19))
    val braille = frame.slice(1, 12).filter(_.exists(c => c >= 0x2800 && c <= 0x28ff))
    assert(braille.nonEmpty, frame.slice(1, 12).mkString("\n"))
    assert(frame(12).endsWith("v 5.0"), frame(12))
  }

  test("the volume label uses short form, not a raw decimal, for large volumes") {
    val minute = Golden.now / 60_000L * 60_000L
    val candles = Vector.tabulate(130) { i =>
      val o = 503.0 + (i % 7) * 0.1
      val v = if i == 129 then 12345.0 else 1.0 + i % 5
      Candle(minute - (129 - i) * 60_000L, o, o + 0.2, o - 0.2, o + 0.05, v)
    }
    val s = Golden.snapshot.copy(candles = Map("1m" -> candles), zoom = 5)
    val frame = market(s, 100, 30, Golden.now, zone, Style.mono).map(plain)
    assert(frame(12).endsWith("v 12.3k"), frame(12))
  }

  test("a coarse zoom draws candles with dates on the axis and history from the series") {
    val s = Golden.snapshot.copy(zoom = State.zooms.indexOf(86400))
    val frame = market(s, 100, 30, Golden.now, zone, Style.mono).map(plain)
    assert(frame(19).startsWith(" " * 16 + "c line 1d/col "), frame(19))
    assert(frame(19).drop(30).take(10).matches("\\d{4}-\\d\\d-\\d\\d"), frame(19))
    assert(frame.slice(1, 12).exists(_.slice(30, 91).exists(c => c >= 0x2800 && c <= 0x28ff)))
  }
