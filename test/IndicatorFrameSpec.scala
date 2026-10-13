package desk

import Polymarket.Window.*

class IndicatorFrameSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val N = now / 1000
  private val M = now / 60_000L
  private val zone = java.time.ZoneOffset.UTC
  private val market = Snapshot
    .initial("XMR", now)
    .copy(
      feed = Health.Live(now - 300),
      bookMinutes =
        Vector(MinuteBook(M - 2, 0.24, 1), MinuteBook(M - 1, 0.24, 1), MinuteBook(M, 0.24, 1)),
      tapeMinutes = Vector(MinuteTape(M - 2, 11.0, 9.0))
    )
  private val btc5m = Vector(Polymarket.Point(now - 40_000L, 54.0))
  private val odds = Odds(
    (c, w) => if (c, w) == ("BTC", M5) then btc5m else Vector.empty,
    (c, w) =>
      if (c, w) == ("BTC", M5) then Polymarket.State.Ready(btc5m, now - 40_000L)
      else Polymarket.State.NoMarket,
    (_, _) => false,
    unreachable = false
  )
  private def frame(mode: String, cols: Int, rows: Int, style: Style, s: Snapshot = market) =
    Workspace.draw(
      s,
      TradingView("XMR", message = "Ready"),
      PanelView(mode = mode),
      cols,
      rows,
      now,
      zone,
      style,
      odds = odds
    )
  private def sp(n: Int): String = " " * n
  private def dots(cells: Int, at: Map[Int, Char] = Map.empty): String =
    (0 until cells).map(i => at.getOrElse(i, if i == (cells - 1) / 2 then '|' else '·')).mkString
  private val key =
    "■ XMR  ■ buy  ■ sell  ▲ buy fill  ▼ sell fill  ■ 5m  ■ 15m  ■ 1h  ■ 4h  ■ 1d  ◆ book  ● tape  ■ BTC  ■ gold  ▲ change  ◇ vs average  ◆ correlation  ● Polymarket  ◉ overlap"
  private val book = "◆ book  ● tape  share of buying"
  private val xmr = "XMR  ▲ change  ◇ vs average  ● Polymarket up"
  private val btc = "BTC  ▲ change  ◆ correlation  ● Polymarket up"
  private val gold = "Gold  ▲ change  ◆ correlation  ● Polymarket up"
  private val names = Vector("5m", "15m", "1h", "4h", "24h")
  private val scales = Vector("±0.5%", "±1%", "±2%", "±3%", "±6%")
  private def label(i: Int) = (names(i).padTo(4, ' ') + scales(i)).padTo(10, ' ')
  private val bandLabel = sp(14) + "Polymarket BTC up%  100 "

  private val right100 =
    Vector(book.padTo(61, ' '), (sp(4) + "0%" + sp(17) + "50%" + sp(15) + "100%").padTo(61, ' ')) ++
      (0 until 5).map(i =>
        (names(i)
          .padTo(4, ' ') + dots(41, if i == 0 then Map(22 -> '●', 25 -> '◆') else Map.empty) + " " +
          (if i == 0 then "B 62% T 55%" else "B -- T --")).padTo(61, ' ')
      ) ++ Vector.fill(4)(sp(61)) :+ key.take(61)

  private def wide(
      cells: Int,
      widths: Vector[Int],
      bookMarks: Map[Int, Char],
      btcMarks: Map[Int, Char],
      scale: (String, String)
  ): Vector[String] =
    val total = widths.sum
    Vector(
      book.padTo(widths(0), ' ') + xmr.padTo(widths(1), ' ') + btc.padTo(widths(2), ' ') + gold
        .padTo(widths(3), ' '),
      scale._1.padTo(widths(0), ' ') + scale._2.padTo(widths(1), ' ') + scale._2
        .padTo(widths(2), ' ') + scale._2.padTo(widths(3), ' ')
    ) ++ (0 until 5).map { i =>
      val first = i == 0
      (names(i).padTo(4, ' ') + dots(
        cells,
        if first then bookMarks else Map.empty
      ) + " " + (if first then "B 62% T 55%" else "B -- T --")).padTo(widths(0), ' ') +
        (label(i) + dots(cells) + " ▲-- ◇-- ●--").padTo(widths(1), ' ') +
        (label(i) + dots(
          cells,
          if first then btcMarks else Map.empty
        ) + " " + (if first then "▲-- ◆-- ●54%" else "▲-- ◆-- ●--")).padTo(widths(2), ' ') +
        (label(i) + dots(cells) + " ▲-- ◆-- ●--").padTo(widths(3), ' ')
    } ++ Vector.fill(4)(sp(total)) :+ key.padTo(total, ' ')

  private val right237 = wide(
    17,
    Vector(38, 52, 52, 56),
    Map(9 -> '●', 10 -> '◆'),
    Map(9 -> '●'),
    (
      sp(4) + "0%" + sp(5) + "50%" + sp(3) + "100%",
      sp(10) + "down" + sp(1) + "neutral" + sp(3) + "up"
    )
  )
  private val right269 = wide(
    25,
    Vector(46, 60, 60, 64),
    Map(13 -> '●', 15 -> '◆'),
    Map(13 -> '●'),
    (
      sp(4) + "0%" + sp(9) + "50%" + sp(7) + "100%",
      sp(10) + "down" + sp(5) + "neutral" + sp(7) + "up"
    )
  )

  private val band100 = Vector(bandLabel + sp(2) + "⢀" + "⣀" * 20, sp(36) + "0 " + sp(23))
  private val band237 = Vector(
    bandLabel + sp(160),
    sp(38) + sp(139) + "⢀" + "⣀" * 20,
    sp(35) + "50 " + sp(160),
    sp(36) + "0 " + sp(160)
  )
  private val band269 = Vector(
    bandLabel + sp(192),
    sp(38) + sp(171) + "⢀" + "⣀" * 20,
    sp(35) + "50 " + sp(192),
    sp(36) + "0 " + sp(192)
  )

  private val sizes = Vector((100, 30), (237, 71), (269, 73))
  private def bandOf(f: WorkspaceFrame): Vector[String] =
    val r = Layout.regions(f.cols, f.rows)
    f.lines.slice(r.priceRows + 2, r.priceRows + 2 + r.band)
  // The band's right region of the last twelve lines, after the panel's separator.
  private def rightOf(f: WorkspaceFrame): Vector[String] =
    f.lines.drop(f.rows - 12).map(l => l.substring(l.indexOf('|') + 1))

  test("the band and the rails match the hand written rows") {
    for (cols, rows, band, right) <- Vector(
        (100, 30, band100, right100),
        (237, 71, band237, right237),
        (269, 73, band269, right269)
      )
    do
      val f = frame("LIVE", cols, rows, Style.mono)
      val r = Layout.regions(cols, rows)
      val plain = f.lines.map(Chart.plain)
      assertEquals(
        plain.slice(r.priceRows + 2, r.priceRows + 2 + r.band).map(_.take(r.left + r.plot)),
        band,
        s"$cols x $rows"
      )
      assertEquals(plain.drop(rows - 12).map(_.drop(39)), right, s"$cols x $rows")
  }

  test("live, prop and view draw the band and the rails byte for byte the same") {
    for (cols, rows) <- sizes do
      val shown = Vector("LIVE", "PROP", "VIEW").map(mode => frame(mode, cols, rows, Style.colour))
      val at = s"$cols x $rows"
      for f <- shown.tail do
        assertEquals(bandOf(f), bandOf(shown.head), s"${f.mode} $at")
        assertEquals(rightOf(f), rightOf(shown.head), s"${f.mode} $at")
      val first = bandOf(shown.head).head
      assert(first.contains(Screen.dim + sp(14) + "Polymarket BTC up%  100" + Screen.reset), first)
  }

  test("a drawn rail keeps its glyphs") {
    val joined = rightOf(frame("LIVE", 269, 73, Style.colour)).mkString
    for glyph <- "·◆●◇▲◉■±" do assert(joined.contains(glyph), s"$glyph in $joined")
  }

  test("a clamped marker is inverse, also in mono") {
    val s = market.copy(samples = Vector((N - 300, 100.0), (N, 101.0)))
    val line = frame("LIVE", 269, 73, Style.mono, s).lines(73 - 10)
    assert(line.contains(Screen.inverse + "▲" + Screen.reset), line)
    val right = Chart.plain(line).drop(39)
    assertEquals(right(80), '▲', right)
    assert(right.contains("▲+1.00%"), right)
  }

  test("the gauges are gone") {
    for (cols, rows) <- sizes; style <- Vector(Style.mono, Style.colour) do
      val plain = frame("LIVE", cols, rows, style).lines.map(Chart.plain)
      for word <- Vector("BOOK ", "TAPE ", "GOLD ") do
        assert(!plain.exists(_.contains(word)), s"$word at $cols x $rows")
  }

  // Not in the brief's list: these two kill mutants the brief's list names.
  test("the key lights the windows the band coin has a market for") {
    // XMR is selected, so the band and the key read BTC, whose 5m window alone has a market.
    for (cols, rows) <- sizes do
      val last = rightOf(frame("LIVE", cols, rows, Style.colour)).last
      assert(last.contains(Style.colour.w5 + Screen.bold + "■" + Screen.reset + " 5m"), last)
      assert(last.contains(Screen.dim + "■" + Screen.reset + " 15m"), last)
  }

  test("a frame drawn for another market's view keeps the odds") {
    // The view still names BTC while the snapshot has moved to XMR, so the frame draws again for
    // XMR, with the same odds.
    val f = Workspace.draw(
      market,
      TradingView("BTC", message = "Ready"),
      PanelView(mode = "LIVE"),
      100,
      30,
      now,
      zone,
      Style.mono,
      odds = odds
    )
    assertEquals(bandOf(f).map(l => Chart.plain(l).take(61)), band100)
    assertEquals(f.lines.drop(30 - 12).map(_.drop(39)), right100)
  }
