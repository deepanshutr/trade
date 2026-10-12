package desk

import java.nio.file.{Files, Path}
import java.time.ZoneOffset

import Polymarket.Window.*

class IndicatorWiringSpec extends munit.FunSuite:
  test("drawFrame hands the odds to the frame and draws none by default") {
    val now = 1_800_000_000_000L
    val s = Snapshot.initial("XMR", now)
    val point = Polymarket.Point(now - 40_000L, 54.0)
    val odds = Odds(
      (c, w) => if (c, w) == ("BTC", M5) then Vector(point) else Vector.empty,
      (c, w) =>
        if (c, w) == ("BTC", M5) then Polymarket.State.Ready(Vector(point), point.at)
        else Polymarket.State.NoMarket,
      (_, _) => false,
      unreachable = false
    )
    val outlook = new OutlookWorker(() => s, () => now, _ => ())
    val view = TradingView("XMR", message = "Ready")
    def band(frame: WorkspaceFrame): String = Chart.plain(frame.lines(15)).take(38)
    val withOdds = WorkspaceApp.drawFrame(
      s,
      view,
      PanelView(),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono,
      outlook,
      odds = odds
    )
    val default =
      WorkspaceApp.drawFrame(
        s,
        view,
        PanelView(),
        100,
        30,
        now,
        ZoneOffset.UTC,
        Style.mono,
        outlook
      )
    assertEquals(band(withOdds), " " * 14 + "Polymarket BTC up%  100 ")
    assertEquals(band(default), " " * 10 + "Polymarket unreachable  100 ")
  }

  // The label reads only `unreachable`. Here every part of the odds shows: a fresh BTC 5m
  // line runs flat to now, a stale 1h line is faint, an ended 15m market keeps its history, and
  // at 230 columns the BTC group shows its chance of up. The band rows and the bottom twelve rows
  // must match Workspace.draw given the same odds, in each mode and for another market's view.
  test("drawFrame hands every part of the odds to the band and the rails in every mode") {
    val now = 1_800_000_000_000L
    val s = Snapshot.initial("XMR", now)
    val fresh = Vector(Polymarket.Point(now - 40_000L, 54.0))
    val old = Vector(Polymarket.Point(now - 600_000L, 70.0), Polymarket.Point(now - 300_000L, 30.0))
    val odds = Odds(
      (c, w) =>
        if (c, w) == ("BTC", M5) then fresh
        else if c == "BTC" && (w == H1 || w == M15) then old
        else Vector.empty,
      (c, w) =>
        if (c, w) == ("BTC", M5) then Polymarket.State.Ready(fresh, fresh.last.at)
        else if (c, w) == ("BTC", H1) then Polymarket.State.Ready(old, old.last.at)
        else Polymarket.State.NoMarket,
      (c, w) => (c, w) == ("BTC", H1),
      unreachable = false
    )
    val outlook = new OutlookWorker(() => s, () => now, _ => ())
    val (cols, rows) = (230, 40)
    val r = Layout.regions(cols, rows)
    def parts(f: WorkspaceFrame) =
      (f.lines.slice(r.priceRows + 2, r.priceRows + 2 + r.band), f.lines.drop(rows - 12))
    for mode <- Vector("LIVE", "PROP", "VIEW"); coin <- Vector("XMR", "BTC") do
      val view = TradingView(coin, message = "Ready")
      val panel = PanelView(mode = mode)
      val withOdds = WorkspaceApp.drawFrame(
        s,
        view,
        panel,
        cols,
        rows,
        now,
        ZoneOffset.UTC,
        Style.colour,
        outlook,
        odds = odds
      )
      val without =
        WorkspaceApp.drawFrame(
          s,
          view,
          panel,
          cols,
          rows,
          now,
          ZoneOffset.UTC,
          Style.colour,
          outlook
        )
      val expected = Workspace.draw(
        s,
        view,
        panel.copy(outlook = outlook.forDisplay(panel.outlook, s, now)),
        cols,
        rows,
        now,
        ZoneOffset.UTC,
        Style.colour,
        odds = odds
      )
      assertEquals(parts(withOdds), parts(expected), s"$mode $coin")
      assertNotEquals(parts(withOdds), parts(without), s"$mode $coin")
  }

  // Recording reads no Polymarket odds. Both record tests stop at the archive, so this reads
  // the method itself: it names no Polymarket client or reader.
  test("the record path names no Polymarket client or reader") {
    val source = Files.readString(Path.of("src/WorkspaceApp.scala"))
    val start = source.indexOf("  private def record(")
    assert(start >= 0, "record not found")
    // The method ends at the next member: a line of two spaces and neither a space nor `)`.
    val end = "(?m)^  [^\\s)]".r
      .findFirstMatchIn(source.substring(start + 1))
      .fold(source.length)(_.start + start + 1)
    val body = source.slice(start, end)
    assert(body.contains("new PublicRecorder("), body)
    assert(!body.toLowerCase.contains("polymarket"), body)
  }
