package desk

import java.time.ZoneOffset

class CompactFollowupSpec extends munit.FunSuite:
  test("the next drawn frame retains a touched target before the worker refreshes") {
    val step = 300000L
    val now = 1260 * step
    val bars = Vector.tabulate(1260) { i =>
      val price = 1000.0 + i
      Candle(i * step, price, price * 1.01, price * 0.99, price, 1)
    }
    def quote(price: Double): Book =
      Book(Vector(Level(price.toString, "1", 1)), Vector(Level(price.toString, "1", 1)), now)
    val market = Snapshot
      .initial("XMR", now)
      .copy(candles = Map("5m" -> bars), book = Some(quote(bars.last.c)), feed = Health.Live(now))
    val controller = new DeskController(
      PanelView(),
      () => now,
      (_, _) => Left("disabled"),
      _ => fail("unexpected order"),
      _ => Left("disabled")
    )
    val worker = new OutlookWorker(() => market, () => now, controller.outlook)
    val view = TradingView("XMR", message = "View only")
    def draw(snapshot: Snapshot): WorkspaceFrame = WorkspaceApp.drawFrame(
      snapshot,
      view,
      controller.panel,
      120,
      40,
      now,
      ZoneOffset.UTC,
      Style.mono,
      worker
    )
    try
      worker.refresh()
      assert(controller.panel.outlook.columns.last.base.percent.nonEmpty)
      val hit = draw(market.copy(book = Some(quote(bars.last.h))))
      assertEquals(hit.outlook.get.columns.last.status, "reached")
      val retreat = draw(market)
      assertEquals(retreat.outlook.get.columns.last.status, "reached")
      assertEquals(retreat.outlook.get.columns.last.base.percent, None)
      assertEquals(retreat.outlook.get.columns.last.direction, OutlookDirection.Mixed)
    finally worker.close()
  }
