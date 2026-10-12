package desk

class IndicatorsSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val N = now / 1000
  private val M = now / 60_000L
  // A sample every ten seconds for five minutes, from 100 to 100.5 in a zigzag. BTC moves from
  // 50000 to 49750 as a fixed negative power of it, so their ten second returns correlate at -1.
  private val power = math.log(50000.0 / 49750.0) / math.log(100.5 / 100.0)
  private val path = Vector.tabulate(31)(k =>
    (N - 300 + 10L * k, 100.0 + k / 60.0 + (if k % 2 == 1 then 0.1 else 0.0))
  )
  private val btc = path.map((t, p) => (t, 50000.0 * math.pow(p / 100.0, -power)))
  private val s = Snapshot
    .initial("XMR", now)
    .copy(
      samples = path,
      refs = Map(
        "BTC" -> Ref(btc, Map.empty),
        "PAXG" -> Ref.empty
      ),
      bookMinutes =
        Vector(MinuteBook(M - 2, 0.24, 1), MinuteBook(M - 1, 0.24, 1), MinuteBook(M, 0.24, 1)),
      tapeMinutes = Vector(MinuteTape(M - 2, 11.0, 9.0))
    )
  private val chance: (String, Polymarket.Window) => Option[(Double, Boolean)] = (c, w) =>
    (c, w) match
      case ("BTC", Polymarket.Window.M5)  => Some((54.0, false))
      case ("PAXG", Polymarket.Window.D1) => Some((61.0, true))
      case _                              => None
  private def texts(g: RailGroup) = g.rows.map(_.values.map(_._1))
  private val c = Style.colour
  private def marks(r: RailRow) = r.marks.map(m => (m.glyph, m.ink))
  private lazy val g = Indicators.groups(s, chance, now, c)

  test("the four groups and their shapes") {
    assertEquals(
      g.map(_.title),
      Vector(
        Vector(("◆ book", c.book), ("● tape", c.tape), ("share of buying", Screen.dim)),
        Vector(
          ("XMR", Screen.bold),
          ("▲ change", ""),
          ("◇ vs average", ""),
          ("● Polymarket up", "")
        ),
        Vector(
          ("BTC", c.bitcoin + Screen.bold),
          ("▲ change", c.bitcoin),
          ("◆ correlation", c.bitcoin),
          ("● Polymarket up", c.bitcoin)
        ),
        Vector(
          ("Gold", c.gold + Screen.bold),
          ("▲ change", c.gold),
          ("◆ correlation", c.gold),
          ("● Polymarket up", c.gold)
        )
      )
    )
    assertEquals(
      g.map(x => (x.labelWidth, x.lo, x.hi, x.scale, x.valuesWidth)),
      Vector((4, 0.0, 100.0, Vector("0%", "50%", "100%"), 13)) ++
        Vector.fill(3)((10, -1.0, 1.0, Vector("down", "neutral", "up"), 21))
    )
    assertEquals(g(0).rows.map(_.label), Vector("5m", "15m", "1h", "4h", "24h"))
    for k <- 1 to 3 do
      assertEquals(
        g(k).rows.map(_.label),
        Vector("5m  ±0.5%", "15m ±1%", "1h  ±2%", "4h  ±3%", "24h ±6%")
      )
  }

  test("the values") {
    assertEquals(
      texts(g(0)),
      Vector(Vector("B 62%", "T 55%")) ++ Vector.fill(4)(Vector("B --", "T --"))
    )
    assertEquals(
      texts(g(1)),
      Vector(Vector("▲+0.50%", "◇--", "●--")) ++ Vector.fill(4)(Vector("▲--", "◇--", "●--"))
    )
    assertEquals(
      texts(g(2)),
      Vector(Vector("▲-0.50%", "◆-1.00", "●54%")) ++ Vector.fill(4)(Vector("▲--", "◆--", "●--"))
    )
    assertEquals(
      texts(g(3)),
      Vector.fill(4)(Vector("▲--", "◆--", "●--")) :+ Vector("▲--", "◆--", "●61%")
    )
  }

  test("the marks") {
    assertEquals(marks(g(0).rows(0)), Vector(('◆', c.book), ('●', c.tape)))
    assertEqualsDouble(g(0).rows(0).marks(0).value, 62.0, 1e-9)
    assertEqualsDouble(g(0).rows(0).marks(1).value, 55.0, 1e-9)
    assertEquals(marks(g(1).rows(0)), Vector(('▲', Screen.bold)))
    assertEqualsDouble(g(1).rows(0).marks(0).value, 1.0, 1e-9)
    assert(g(1).rows.drop(1).forall(_.marks.isEmpty))
    assertEquals(marks(g(2).rows(0)), Vector(('▲', c.bitcoin), ('◆', c.bitcoin), ('●', c.bitcoin)))
    assertEquals(g(2).rows(0).marks.map(m => math.round(m.value * 100)), Vector(-100L, -100L, 8L))
    assertEquals(marks(g(3).rows(4)), Vector(('●', Screen.dim)))
    assertEqualsDouble(g(3).rows(4).marks(0).value, 0.22, 1e-9)
    assertEquals(g(3).rows(4).values.last, ("●61%", Screen.dim))
  }

  test("the value inks") {
    assertEquals(g(0).rows(0).values.map(_._2), Vector(c.book, c.tape))
    assertEquals(g(1).rows(0).values.map(_._2), Vector("", "", ""))
    assertEquals(g(2).rows(0).values.map(_._2), Vector.fill(3)(c.bitcoin))
  }

  test("a selected BTC or PAXG reads its own series and shows no own correlation") {
    val onBtc = Indicators.groups(s.copy(coin = "BTC"), chance, now, c)
    assertEquals(onBtc(1).title.head, ("BTC", Screen.bold))
    assertEquals(texts(onBtc(1))(0), Vector("▲+0.50%", "◇--", "●54%"))
    assertEquals(texts(onBtc(2))(0), Vector("▲+0.50%", "◆--", "●54%"))
    assertEquals(onBtc(2).rows(0).marks.map(_.glyph), Vector('▲', '●'))
    val onGold = Indicators.groups(s.copy(coin = "PAXG"), chance, now, c)
    assertEquals(texts(onGold(3))(0), Vector("▲+0.50%", "◆--", "●--"))
    assertEquals(texts(onGold(3))(4), Vector("▲--", "◆--", "●61%"))
    assertEquals(texts(onGold(2))(0), Vector("▲-0.50%", "◆-1.00", "●54%"))
    // The selected market's chance is plain when fresh and faint when stale.
    assertEquals(marks(onBtc(1).rows(0)), Vector(('▲', Screen.bold), ('●', "")))
    assertEquals(onBtc(1).rows(0).values.last, ("●54%", ""))
    assertEquals(marks(onGold(1).rows(4)), Vector(('●', Screen.dim)))
    assertEquals(onGold(1).rows(4).values.last, ("●61%", Screen.dim))
  }

  test("the key") {
    val k = Indicators.key("XMR", c, _ == Polymarket.Window.M5)
    assertEquals(
      Chart.plain(k),
      "■ XMR  ■ buy  ■ sell  ▲ buy fill  ▼ sell fill  ■ 5m  ■ 15m  ■ 1h  ■ 4h  ■ 1d  ◆ book  ● tape  ■ BTC  ■ gold  ▲ change  ◇ vs average  ◆ correlation  ● Polymarket  ◉ overlap"
    )
    assert(
      k.startsWith(
        Screen.bold + "■" + Screen.reset + " XMR  " + c.bid + Screen.bold + "■" + Screen.reset + " buy  "
      )
    )
    assert(k.contains(c.w5 + Screen.bold + "■" + Screen.reset + " 5m"))
    assert(k.contains(Screen.dim + "■" + Screen.reset + " 15m"))
    assert(k.contains(Screen.dim + "■" + Screen.reset + " 1d"))
    assert(k.contains(Screen.bold + "◉" + Screen.reset + " overlap"))
    assertEquals(Chart.plain(Indicators.key("ETH", Style.mono, _ => true)).take(5), "■ ETH")
  }

  test("a chance that is not finite gives no mark and no value") {
    val odd: (String, Polymarket.Window) => Option[(Double, Boolean)] = (coin, w) =>
      (coin, w) match
        case ("BTC", Polymarket.Window.M5)  => Some((Double.NaN, false))
        case ("XMR", Polymarket.Window.M5)  => Some((Double.PositiveInfinity, false))
        case ("PAXG", Polymarket.Window.D1) => Some((61.0, true))
        case _                              => None
    val h = Indicators.groups(s, odd, now, c)
    assertEquals(texts(h(1))(0), Vector("▲+0.50%", "◇--", "●--"))
    assertEquals(h(1).rows(0).marks.map(_.glyph), Vector('▲'))
    assertEquals(texts(h(2))(0), Vector("▲-0.50%", "◆-1.00", "●--"))
    assertEquals(h(2).rows(0).marks.map(_.glyph), Vector('▲', '◆'))
    assertEquals(texts(h(3))(4), Vector("▲--", "◆--", "●61%"))
  }

  test("the hour measures read the one minute bars and the longer correlations read candles") {
    // The last 61 closed minutes, from 100 to 102 in a zigzag. BTC closes at 5,000,000 over it, so
    // their one minute returns correlate at -1.
    def bar(k: Int, close: Double) = Candle((M - k) * 60_000L, close, close, close, close, 1.0)
    val closes =
      (61 to 1 by -1).toVector.map(k =>
        (k, 102.0 - (k - 1) / 30.0 + (if k % 2 == 0 then 0.1 else 0.0))
      )
    val xmr = closes.map(bar)
    val versus = closes.map((k, x) => bar(k, 5_000_000.0 / x))
    val h = Indicators.groups(
      s.copy(
        candles = Map("1m" -> xmr),
        refs = s.refs.updated("BTC", Ref(btc, Map("1m" -> versus)))
      ),
      chance,
      now,
      c
    )
    assertEquals(texts(h(1))(2), Vector("▲+2.00%", "◇+0.92%", "●--"))
    assertEquals(marks(h(1).rows(2)), Vector(('▲', Screen.bold), ('◇', "")))
    assertEqualsDouble(h(1).rows(2).marks(0).value, 1.0, 1e-9)
    // The distance over the scale: the latest close over the mean of the last 60, minus one.
    val mean = closes.filter(_._1 <= 60).map(_._2).sum / 60
    assertEqualsDouble(h(1).rows(2).marks(1).value, (102.0 / mean - 1) * 100 / 2, 1e-9)
    // The BTC change over the 1h scale.
    assertEqualsDouble(h(2).rows(2).marks(0).value, (100.0 / 102.0 - 1) * 100 / 2, 1e-9)
    assertEquals(
      texts(h(2)).slice(1, 4),
      Vector(
        Vector("▲--", "◆-1.00", "●--"),
        Vector("▲-1.96%", "◆-1.00", "●--"),
        Vector("▲--", "◆--", "●--")
      )
    )
  }

  test("the period table") {
    import Polymarket.Window.*
    assertEquals(
      Indicators.periods,
      Vector(
        Indicators.Period("5m", 300_000L, M5, 0.5, "±0.5%", None),
        Indicators.Period("15m", 900_000L, M15, 1.0, "±1%", Some(("1m", 15))),
        Indicators.Period("1h", 3_600_000L, H1, 2.0, "±2%", Some(("1m", 60))),
        Indicators.Period("4h", 14_400_000L, H4, 3.0, "±3%", Some(("5m", 48))),
        Indicators.Period("24h", 86_400_000L, D1, 6.0, "±6%", Some(("15m", 96)))
      )
    )
  }

  test("each correlation reads its own interval's candles and needs n returns") {
    // Fifteen closed one minute bars, one short of the 16 a 15m correlation needs, and 49 closed
    // five minute bars, the 4h correlation's 48 returns. BTC is their inverse each time.
    def bar(t: Long, close: Double) = Candle(t, close, close, close, close, 1.0)
    def zigzag(k: Int) = 100.0 + k / 30.0 + (if k % 2 == 0 then 0.1 else 0.0)
    val minute = (15 to 1 by -1).toVector.map(k => bar((M - k) * 60_000L, zigzag(k)))
    val five = (49 to 1 by -1).toVector.map(j => bar(now - j * 300_000L, zigzag(j)))
    def inverse(cs: Vector[Candle]) = cs.map(x => bar(x.t, 5_000_000.0 / x.c))
    val h = Indicators.groups(
      s.copy(
        candles = Map("1m" -> minute, "5m" -> five),
        refs = s.refs.updated("BTC", Ref(btc, Map("1m" -> inverse(minute), "5m" -> inverse(five))))
      ),
      chance,
      now,
      c
    )
    assertEquals(texts(h(2)).map(_(1)), Vector("◆-1.00", "◆--", "◆--", "◆-1.00", "◆--"))
  }

  test("a zero correlation shows +0.00 and its mark, not --") {
    // 49 closed five minute bars, the 4h correlation's 48 returns. XMR doubles and halves in turn.
    // BTC doubles twice, then halves twice. Every return is the log of 2 or of 0.5. In this order
    // each running sum is exact, so the deviations from each mean are equal and opposite and the
    // products cancel in fours. The zero holds whatever the last bit of each log, so it does not
    // need the log of 0.5 to be exactly minus the log of 2.
    def bars(closes: Vector[Double]) =
      closes.zipWithIndex.map((close, i) =>
        Candle(now - (49 - i) * 300_000L, close, close, close, close, 1.0)
      )
    val xmr = bars(Vector.tabulate(49)(i => if i % 2 == 0 then 100.0 else 200.0))
    val versus =
      bars(Vector.tabulate(49)(i => Vector(50_000.0, 100_000.0, 200_000.0, 100_000.0)(i % 4)))
    val h = Indicators.groups(
      s.copy(
        candles = Map("5m" -> xmr),
        refs = s.refs.updated("BTC", Ref(btc, Map("5m" -> versus)))
      ),
      chance,
      now,
      c
    )
    assertEquals(texts(h(2))(3)(1), "◆+0.00")
    assertEquals(h(2).rows(3).marks.filter(_.glyph == '◆').map(_.value), Vector(0.0))
  }

  test("a stale chance is faint at any window and its text rounds to the nearest percent") {
    val odd: (String, Polymarket.Window) => Option[(Double, Boolean)] = (coin, w) =>
      (coin, w) match
        case ("BTC", Polymarket.Window.M5)  => Some((54.6, true))
        case ("PAXG", Polymarket.Window.D1) => Some((61.4, false))
        case _                              => None
    val h = Indicators.groups(s, odd, now, c)
    assertEquals(h(2).rows(0).values.last, ("●55%", Screen.dim))
    assertEquals(marks(h(2).rows(0)).last, ('●', Screen.dim))
    assertEqualsDouble(h(2).rows(0).marks.last.value, 0.092, 1e-9)
    assertEquals(h(3).rows(4).values.last, ("●61%", c.gold))
    assertEquals(marks(h(3).rows(4)), Vector(('●', c.gold)))
  }

  test("gold's correlation shows against another market and never against PAXG itself") {
    val withGold = s.copy(refs = s.refs.updated("PAXG", Ref(path, Map.empty)))
    val onXmr = Indicators.groups(withGold, chance, now, c)
    assertEquals(texts(onXmr(3))(0), Vector("▲+0.50%", "◆+1.00", "●--"))
    assertEquals(marks(onXmr(3).rows(0)), Vector(('▲', c.gold), ('◆', c.gold)))
    val onGold = Indicators.groups(withGold.copy(coin = "PAXG"), chance, now, c)
    assertEquals(texts(onGold(3))(0), Vector("▲+0.50%", "◆--", "●--"))
    assertEquals(marks(onGold(3).rows(0)), Vector(('▲', c.gold)))
  }

  test("every key entry has its ink, and only the four windows follow their markets") {
    import Polymarket.Window.*
    val b = Screen.bold
    val dim = Screen.dim
    def entry(ink: String, glyph: String, word: String) = ink + glyph + Screen.reset + " " + word
    def key(live: Set[Polymarket.Window]) = Indicators.key("XMR", c, live).split("  ").toVector
    val head = Vector(
      entry(b, "■", "XMR"),
      entry(c.bid + b, "■", "buy"),
      entry(c.ask + b, "■", "sell"),
      entry(c.bid + b, "▲", "buy fill"),
      entry(c.ask + b, "▼", "sell fill")
    )
    val tail = Vector(
      entry(c.w1d, "■", "1d"),
      entry(c.book + b, "◆", "book"),
      entry(c.tape + b, "●", "tape"),
      entry(c.bitcoin + b, "■", "BTC"),
      entry(c.gold + b, "■", "gold"),
      entry(b, "▲", "change"),
      entry(b, "◇", "vs average"),
      entry(b, "◆", "correlation"),
      entry(b, "●", "Polymarket"),
      entry(c.overlap, "◉", "overlap")
    )
    assertEquals(
      key(Set(M5, H1, D1)),
      head ++ Vector(
        entry(c.w5 + b, "■", "5m"),
        entry(dim, "■", "15m"),
        entry(c.w1h + b, "■", "1h"),
        entry(dim, "■", "4h")
      ) ++ tail
    )
    assertEquals(
      key(Set(M15, H4)),
      head ++ Vector(
        entry(dim, "■", "5m"),
        entry(c.w15 + b, "■", "15m"),
        entry(dim, "■", "1h"),
        entry(c.w4h + b, "■", "4h")
      ) ++ tail
    )
  }
