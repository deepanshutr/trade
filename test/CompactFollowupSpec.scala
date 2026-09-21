package desk

import java.time.ZoneOffset

class CompactFollowupSpec extends munit.FunSuite:
  test("a failed target review retains the selected price and a visible reason") {
    val now = 1800000000000L
    val market = Snapshot.initial("XMR", now)
    val view = TradingView(
      "XMR",
      message = "Ready",
      ready = true,
      rules = Some(MarketRules(0, 2, 5)),
      bid = Some(BigDecimal(99)),
      ask = Some(BigDecimal(100))
    )
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      _ => Left("No visible quantity fits this order"),
      _ => fail("unexpected order"),
      _ => false,
      _ => Left("disabled")
    )
    def draw(): WorkspaceFrame =
      val frame =
        Workspace.draw(market, view, controller.panel, 100, 30, now, ZoneOffset.UTC, Style.mono)
      controller.publish(frame)
      frame
    draw()
    controller.handle(Command.SelectTarget(Side.Buy), market, view, (100, 30), false)
    val selected = controller.panel.target
    assert(selected.nonEmpty)
    draw()
    controller.handle(Command.SubmitTarget, market, view, (100, 30), false)
    val failed = draw()
    assertEquals(controller.panel.target, selected)
    assert(failed.lines.mkString.contains("No visible quantity fits this order"))
    assert(failed.buttons.exists(_.label == "Enter Review"))
    assert(failed.buttons.exists(_.action == DeskAction.Cancel))
  }

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
      _ => Left("disabled"),
      _ => fail("unexpected order"),
      _ => false,
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
