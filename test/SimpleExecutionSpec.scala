package desk

import java.time.ZoneOffset
import java.nio.file.{Files, Path}

class SimpleExecutionSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val view = TradingView(
    "XMR",
    Some(BigDecimal(0)),
    "Paper ready",
    true,
    false,
    Some(MarketRules(0, 2, 5)),
    Some(BigDecimal(99)),
    Some(BigDecimal(100))
  )

  test("the initial execution area has only two primary trade actions") {
    val frame = Workspace.draw(
      snapshot,
      view,
      PanelView(mode = "PAPER"),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    val trades = frame.buttons.filter(_.action.isInstanceOf[DeskAction.Prepare])
    assertEquals(trades.size, 2)
    assertEquals(
      trades.map(_.action).toSet,
      Set(DeskAction.Prepare(Trading.action(0)), DeskAction.Prepare(Trading.action(2)))
    )
    assert(!frame.lines.exists(_.contains("Trade buttons open a review")))
    assert(!frame.lines.exists(_.contains("No funding or liquidation")))
    assert(
      frame.lines.exists(_.contains("$1000")),
      "ordinary presets should use ordinary dollar amounts"
    )
  }

  test("primary controls cover a large three-row target at minimum width") {
    val frame = Workspace.draw(
      snapshot,
      view,
      PanelView(mode = "PAPER"),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    val buy = frame.buttons.find(_.action == DeskAction.Prepare(Trading.action(0))).get
    assert(buy.width >= 20)
    for y <- buy.y until buy.y + 3; x <- buy.x until buy.x + buy.width do
      assertEquals(frame.hit(x, y, 100, 30, "XMR"), Some(buy.action))
  }

  private final class Fixture:
    var sent = Vector.empty[OrderReview]
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      intent =>
        Trading.plan(
          "XMR",
          view.rules.get,
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), now),
          view.reference(intent.side).get,
          BigDecimal(0),
          intent,
          TradeSizes()
        ),
      review => { sent :+= review; true },
      _ => false,
      _ => Right("Synthetic account")
    )
    def draw(v: TradingView = view): WorkspaceFrame =
      val f =
        Workspace.draw(snapshot, v, controller.panel, 100, 30, now, ZoneOffset.UTC, Style.mono)
      controller.publish(f)
      f
    def key(c: Command, v: TradingView = view): Unit = {
      val _ = controller.handle(c, snapshot, v, (100, 30), false)
    }
    def click(frame: WorkspaceFrame, action: DeskAction): Unit =
      val b = frame.buttons.find(_.action == action).get
      key(Command.PointerPress(b.x, b.y, 0))
      key(Command.Pointer(b.x + b.width - 1, b.y + b.height - 1, 0))

  test("size and type selectors update the two large actions before reviewing") {
    val f = new Fixture
    f.click(f.draw(), DeskAction.OrderType(OrderKind.Best))
    f.click(f.draw(), DeskAction.OrderSize(true))
    val intent = OrderIntent(Side.Sell, OrderKind.Best, big = true)
    val selected = f.draw()
    assertEquals(selected.buttons.count(_.action.isInstanceOf[DeskAction.Prepare]), 2)
    f.click(selected, DeskAction.Prepare(intent))
    assertEquals(f.controller.panel.review.map(_.intent), Some(intent))
    assertEquals(f.sent.size, 0)
    val reviewed = f.draw()
    assert(reviewed.completeReview)
    assertEquals(reviewed.buttons.find(_.action == DeskAction.Submit).get.height, 3)
    f.click(reviewed, DeskAction.Submit)
    assertEquals(f.sent.map(_.intent), Vector(intent))
  }

  test("all eight existing numbered shortcuts retain their exact intent and require Send") {
    val intents = Vector(
      OrderIntent(Side.Buy, OrderKind.Hard),
      OrderIntent(Side.Buy, OrderKind.Hard, true),
      OrderIntent(Side.Sell, OrderKind.Hard),
      OrderIntent(Side.Sell, OrderKind.Hard, true),
      OrderIntent(Side.Buy, OrderKind.Best),
      OrderIntent(Side.Buy, OrderKind.Best, true),
      OrderIntent(Side.Sell, OrderKind.Best),
      OrderIntent(Side.Sell, OrderKind.Best, true)
    )
    intents.zipWithIndex.foreach { (intent, index) =>
      val f = new Fixture
      f.draw()
      val command = Keys.step(Vector.empty, '1'.toInt + index, false)._2.get
      f.key(command)
      assertEquals(f.controller.panel.review.map(_.intent), Some(intent))
      f.key(Command.SubmitTarget)
      assertEquals(f.sent.size, 0, "an unpublished review cannot send")
      assert(f.draw().completeReview)
      f.key(Command.SubmitTarget)
      assertEquals(f.sent.map(_.intent), Vector(intent))
    }
  }

  test("a size change between press and release cannot prepare the old or new amount") {
    val f = new Fixture
    val frame = f.draw()
    val buy =
      frame.buttons.find(_.action == DeskAction.Prepare(OrderIntent(Side.Buy, OrderKind.Hard))).get
    f.key(Command.PointerPress(buy.x, buy.y, 0))
    f.controller.publish(frame.copy(buttons = frame.buttons.filterNot(_ == buy)))
    f.controller.publish(frame)
    f.key(Command.Pointer(buy.x, buy.y + 2, 0))
    assertEquals(f.controller.panel.review, None)
    assertEquals(f.sent.size, 0)
  }

  test("target selection and adjustment still require a full exact review") {
    val f = new Fixture
    f.click(f.draw(), DeskAction.OrderType(OrderKind.Target))
    f.click(f.draw(), DeskAction.Target(Side.Buy))
    f.key(Command.ZoomIn)
    val target = f.controller.panel.target.get._2
    assert(target > 100)
    f.draw()
    f.key(Command.SubmitTarget)
    assertEquals(f.sent.size, 0)
    assertEquals(f.controller.panel.review.map(_.plan.limit), Some(target))
    assert(f.draw().completeReview)
  }

  test("controls fit without overlap across execution states and monochrome and colour") {
    val f = new Fixture
    f.draw()
    f.key(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
    val panels = Vector(
      "ready" -> PanelView(mode = "PAPER"),
      "best-large" -> PanelView(mode = "PAPER", orderKind = OrderKind.Best, bigOrder = true),
      "target" -> PanelView(
        mode = "PAPER",
        orderKind = OrderKind.Target,
        target = Some(Side.Buy -> BigDecimal("100.1"))
      ),
      "review" -> f.controller.panel,
      "mode" -> PanelView(mode = "PAPER", modeReview = Some("LIVE"))
    )
    for
      (cols, rows) <- Vector((100, 30), (101, 31), (120, 40), (160, 45))
      style <- Vector(Style.mono, Style.colour)
      (name, panel) <- panels
      ready <- Vector(true, false)
    do
      val v = if ready then view
      else
        view.copy(
          ready = false,
          message = "Reconnecting. Waiting for fresh prices",
          block = Some("Reconnecting. Waiting for fresh prices")
        )
      val frame = Workspace.draw(snapshot, v, panel, cols, rows, now, ZoneOffset.UTC, style)
      val plain = frame.lines.map(Chart.plain)
      assertEquals(plain.size, rows)
      assert(plain.forall(_.length == cols))
      val occupied = scala.collection.mutable.Set.empty[(Int, Int)]
      for b <- frame.controls; y <- b.y until b.y + b.height; x <- b.x until b.x + b.width do
        assert(y >= 1 && y <= rows && x >= 1 && x <= cols)
        assert(occupied.add((x, y)), s"overlap $name at $x,$y")
        if frame.buttons.contains(b) then
          assertEquals(frame.hit(x, y, cols, rows, "XMR"), Some(b.action))
        else assertEquals(frame.hit(x, y, cols, rows, "XMR"), None)
      sys.props.get("desk.simple.frames").foreach { path =>
        val root = Path.of(path)
        Files.createDirectories(root)
        val label = s"${cols}x$rows-$name-${if ready then "ready" else "reconnecting"}-${
            if style.colour then "colour" else "mono"
          }"
        Files.writeString(root.resolve(label + ".txt"), plain.mkString("\n") + "\n")
        Files.writeString(root.resolve(label + ".ansi"), frame.lines.mkString("\n") + "\n")
      }
  }
