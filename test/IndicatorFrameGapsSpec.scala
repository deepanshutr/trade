package desk

import Polymarket.Window.*

class IndicatorFrameGapsSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val M = now / 60_000L
  private val zone = java.time.ZoneOffset.UTC
  private def market(coin: String) = Snapshot
    .initial(coin, now)
    .copy(
      feed = Health.Live(now - 300),
      bookMinutes =
        Vector(MinuteBook(M - 2, 0.24, 1), MinuteBook(M - 1, 0.24, 1), MinuteBook(M, 0.24, 1)),
      tapeMinutes = Vector(MinuteTape(M - 2, 11.0, 9.0))
    )
  private val btc5m = Vector(Polymarket.Point(now - 40_000L, 54.0))
  private def odds(
      ready: Set[(String, Polymarket.Window)],
      stale: Set[(String, Polymarket.Window)] = Set.empty,
      count: () => Unit = () => ()
  ) = Odds(
    (c, w) => if ready((c, w)) then btc5m else Vector.empty,
    (c, w) => {
      count()
      if ready((c, w)) then Polymarket.State.Ready(btc5m, now - 40_000L)
      else Polymarket.State.NoMarket
    },
    (c, w) => stale((c, w)),
    unreachable = false
  )
  private def frame(coin: String, cols: Int, rows: Int, style: Style, o: Odds) =
    Workspace.draw(
      market(coin),
      TradingView(coin, message = "Ready"),
      PanelView(mode = "LIVE"),
      cols,
      rows,
      now,
      zone,
      style,
      odds = o
    )
  private def bandOf(f: WorkspaceFrame): Vector[String] =
    val r = Layout.regions(f.cols, f.rows)
    f.lines.slice(r.priceRows + 2, r.priceRows + 2 + r.band)
  private def rightOf(f: WorkspaceFrame): Vector[String] =
    f.lines.drop(f.rows - 12).map(l => l.substring(l.indexOf('|') + 1))

  test("the band and the rails carry their inks in colour") {
    val c = Style.colour
    val f = frame("XMR", 269, 73, c, odds(Set(("BTC", M5))))
    // The 5m line in its colour on the band's second row.
    assert(bandOf(f)(1).contains(c.w5 + "⢀" + "⣀" * 20 + Screen.reset), bandOf(f)(1))
    // The 5m row: the book and tape marks and values in their inks, BTC's chance in its own.
    val row = rightOf(f)(2)
    for shown <- Vector(
        c.book + "◆" + Screen.reset,
        c.tape + "●" + Screen.reset,
        c.book + "B 62%" + Screen.reset,
        c.bitcoin + "●" + Screen.reset,
        c.bitcoin + "●54%" + Screen.reset
      )
    do assert(row.contains(shown), shown)
  }

  test("a stale window is faint on the band and on its rail") {
    val c = Style.colour
    val f = frame("XMR", 269, 73, c, odds(Set(("BTC", M5)), stale = Set(("BTC", M5))))
    assert(bandOf(f)(1).contains(Screen.dim + "⢀" + "⣀" * 20 + Screen.reset), bandOf(f)(1))
    val row = rightOf(f)(2)
    assert(row.contains(Screen.dim + "●54%" + Screen.reset), row)
    assert(!row.contains(c.bitcoin + "●54%"), row)
  }

  test("with gold selected the key lights gold's windows, not BTC's") {
    // BTC has a 5m market and gold its daily only, so gold's band leaves the 5m square faint.
    val f = frame("PAXG", 269, 73, Style.colour, odds(Set(("BTC", M5), ("PAXG", D1))))
    val key = rightOf(f).last
    assert(key.contains(Screen.dim + "■" + Screen.reset + " 5m"), key)
    assert(!key.contains(Style.colour.w5 + Screen.bold + "■"), key)
  }

  test("two marks on one cell of a drawn rail are ◉ in bold") {
    // Tape 12.4 against 7.6 is 62% buying, the book's share, so both land on cell 15 of 25.
    val s = market("XMR").copy(tapeMinutes = Vector(MinuteTape(M - 2, 12.4, 7.6)))
    val f = Workspace.draw(
      s,
      TradingView("XMR", message = "Ready"),
      PanelView(mode = "LIVE"),
      269,
      73,
      now,
      zone,
      Style.colour,
      odds = odds(Set(("BTC", M5)))
    )
    val row = rightOf(f)(2)
    assert(row.contains(Screen.bold + "◉" + Screen.reset), row)
    assert(Chart.plain(row).contains("B 62% T 62%"), row)
  }

  test("a frame builds the rails' groups once") {
    // The groups read every window's state once, so a frame that builds them once reads fewer
    // than twice the states one build reads.
    var reads = 0
    val o = odds(Set(("BTC", M5)), count = () => reads += 1)
    val _ = Indicators.groups(market("XMR"), o.chance, now, Style.colour)
    val once = reads
    reads = 0
    val _ = frame("XMR", 269, 73, Style.colour, o)
    assert(once > 0)
    assert(reads < 2 * once, s"$reads reads in a frame, $once in one build of the groups")
  }
