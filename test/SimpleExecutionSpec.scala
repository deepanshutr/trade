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
    val f = new Fixture
    val frame = f.draw()
    val buy = frame.buttons.find(_.action == DeskAction.Prepare(Trading.action(0))).get
    assert(buy.width >= 20)
    for y <- buy.y until buy.y + buy.height; x <- buy.x until buy.x + buy.width do
      val before = f.sent.size
      f.key(Command.PointerPress(x, y, 0))
      f.key(Command.Pointer(x, y, 0))
      assertEquals(f.sent.size, before + 1)
      assertEquals(f.sent.last.intent, Trading.action(0))
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
      _ => Right(())
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

  test("selected size changes both displayed actions and one click submits exactly that size") {
    val f = new Fixture
    f.click(f.draw(), DeskAction.OrderSize(true))
    val intent = OrderIntent(Side.Sell, OrderKind.Hard, big = true)
    val selected = f.draw()
    assertEquals(selected.buttons.count(_.action.isInstanceOf[DeskAction.Prepare]), 2)
    f.click(selected, DeskAction.Prepare(intent))
    assertEquals(f.sent.map(_.intent), Vector(intent))
  }

  test("only currently displayed numbered shortcuts may submit") {
    for big <- Vector(false, true); index <- 0 until 8 do
      val f = new Fixture
      if big then f.click(f.draw(), DeskAction.OrderSize(true))
      f.draw()
      val intent = Trading.action(index)
      val command = Keys.step(Vector.empty, '1'.toInt + index, false)._2.get
      f.key(command)
      val expected =
        if intent.kind == OrderKind.Hard && intent.big == big then Vector(intent) else Vector.empty
      assertEquals(f.sent.map(_.intent), expected)
      f.key(Command.SubmitTarget)
      assertEquals(f.sent.map(_.intent), expected)
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
    assertEquals(f.sent.size, 0)
  }

  test("controls fit without overlap across execution states and monochrome and colour") {
    val f = new Fixture
    f.draw()
    f.key(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
    val panels = Vector(
      "ready" -> PanelView(mode = "PAPER"),
      "large" -> PanelView(mode = "PAPER", bigOrder = true),
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
          assertEquals(frame.buttons.find(_.contains(x, y)).map(_.action), Some(b.action))
        else assertEquals(frame.buttons.find(_.contains(x, y)).map(_.action), None)
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
