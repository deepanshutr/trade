package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

class OutlookWorkerSpec extends munit.FunSuite:
  private val step = 300000L
  private val bars = Vector
    .tabulate(1260) { i =>
      val c = 1000.0 + i * 0.01
      Candle(i * step, c, c + 0.001, c - 0.001, c, 1)
    }
    .updated(1259, Candle(1259 * step, 1012.59, 1012.59 * 1.04, 1012.589, 1012.59, 1))
  private val now = 1260 * step
  private def quote(s: Snapshot, price: Double, at: Long): Snapshot = s.copy(
    book =
      Some(Book(Vector(Level(price.toString, "1", 1)), Vector(Level(price.toString, "1", 1)), at)),
    feed = Health.Live(at)
  )
  private val initial =
    quote(Snapshot.initial("XMR", now).copy(candles = Map("5m" -> bars)), bars.last.c, now)
  private def short(view: OutlookView): HorizonOutlook = view.columns.last

  test("unchanged closed history is cached while an older repair recomputes the rates") {
    var market = initial
    var time = now
    var calls = 0
    var shown = Outlook.empty("XMR")
    val worker = new OutlookWorker(
      () => market,
      () => time,
      value => shown = value,
      (snapshot, at) => { calls += 1; Outlook.compute(snapshot, at) }
    )
    try
      worker.refresh()
      val before = short(shown).base.hits
      time += 1000
      market = quote(market, bars.last.c, time)
      worker.refresh()
      assertEquals(calls, 1)
      assertEquals(shown.calculatedAt, time)
      val repaired = bars.updated(100, bars(100).copy(h = bars(100).c * 1.07))
      market = market.copy(candles = Map("5m" -> repaired))
      worker.refresh()
      assertEquals(calls, 2)
      assertEquals(short(shown).base.hits, before + 1)
      assert(short(shown).base.percent.nonEmpty)
    finally worker.close()
  }

  test("an observed target touch remains reached after the quote retreats") {
    var market = initial
    var shown = Outlook.empty("XMR")
    val worker = new OutlookWorker(() => market, () => now, value => shown = value)
    try
      worker.refresh()
      shown = worker.forDisplay(shown, market, now)
      assert(short(shown).base.percent.nonEmpty)
      market = quote(market, bars.last.h, now)
      worker.refresh()
      shown = worker.forDisplay(shown, market, now)
      assertEquals(short(shown).status, "reached")
      market = quote(market, bars.last.c, now)
      worker.refresh()
      shown = worker.forDisplay(shown, market, now)
      assertEquals(short(shown).status, "reached")
      assertEquals(short(shown).matched.percent, None)
      market = market.copy(coin = "BTC")
      worker.refresh()
      shown = worker.forDisplay(shown, market, now)
      assertEquals(shown.coin, "BTC")
      assertNotEquals(short(shown).status, "reached")
    finally worker.close()
  }

  test("a new closed-bar boundary invalidates the cached outlook") {
    var time = now
    var market = initial
    var shown = Outlook.empty("XMR")
    val worker = new OutlookWorker(() => market, () => time, value => shown = value)
    try
      worker.refresh()
      shown = worker.forDisplay(shown, market, time)
      time += step
      market = quote(market, bars.last.c, time)
      worker.refresh()
      shown = worker.forDisplay(shown, market, time)
      assertEquals(short(shown).status, "stale")
      assertEquals(short(shown).base.percent, None)
    finally worker.close()
  }

  test("a target touch displayed between worker samples stays reached after retreat") {
    var market = initial
    var shown = Outlook.empty("XMR")
    val worker = new OutlookWorker(() => market, () => now, value => shown = value)
    try
      worker.refresh()
      shown = worker.forDisplay(shown, market, now)
      val touched = quote(market, bars.last.h, now)
      val frame = WorkspaceApp.drawFrame(
        touched,
        TradingView("XMR", message = "View only"),
        PanelView(outlook = shown),
        120,
        40,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono,
        worker
      )
      assertEquals(short(frame.outlook.get).status, "reached")
      market = quote(market, bars.last.c, now)
      worker.refresh()
      shown = worker.forDisplay(shown, market, now)
      assertEquals(short(shown).status, "reached")
      assertEquals(short(shown).base.percent, None)
    finally worker.close()
  }

  test("blocked historical calculation cannot block controller mute or details") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val calculationThread = new AtomicReference[Thread]()
    val controller = new DeskController(
      PanelView(),
      () => now,
      _ => Left("disabled"),
      _ => fail("unexpected order"),
      _ => Left("disabled")
    )
    val worker = new OutlookWorker(
      () => initial,
      () => now,
      controller.outlook,
      (snapshot, at) => {
        calculationThread.set(Thread.currentThread())
        entered.countDown()
        release.await()
        Outlook.compute(snapshot, at)
      }
    )
    try
      worker.start()
      assert(entered.await(5, TimeUnit.SECONDS))
      val frame = Workspace.draw(
        initial,
        TradingView("XMR", message = "View only"),
        controller.panel,
        120,
        40,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      controller.publish(frame)
      controller.handle(
        Command.ToggleMute,
        initial,
        TradingView("XMR", message = "View only"),
        (120, 40),
        false
      )
      assert(controller.panel.muted)
      assertEquals(release.getCount, 1L)
      worker.close()
      assert(!calculationThread.get().isAlive)
    finally
      release.countDown()
      worker.close()
  }

  test("a fresh draw recovers rates after refresh sampled a stale quote") {
    val market = quote(initial, bars.last.c, now - 6000)
    var shown = Outlook.empty("XMR")
    val worker = new OutlookWorker(() => market, () => now, value => shown = value)
    try
      worker.refresh()
      val frame = WorkspaceApp.drawFrame(
        initial,
        TradingView("XMR", message = "View only"),
        PanelView(outlook = shown),
        120,
        40,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono,
        worker
      )
      val column = short(frame.outlook.get)
      assertNotEquals(column.status, "stale price")
      assert(column.base.percent.nonEmpty, "fresh quote did not recover computed rates")
      assertEquals(column.direction, short(Outlook.compute(initial, now)).direction)
    finally worker.close()
  }
