package desk

import java.time.ZoneOffset

class RefineTradingSpec extends munit.FunSuite:
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

  private class Fixture:
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
      _ => fail("Ask is removed"),
      _ => Right("Synthetic account")
    )
    def draw(v: TradingView = view): WorkspaceFrame =
      val frame =
        Workspace.draw(snapshot, v, controller.panel, 100, 30, now, ZoneOffset.UTC, Style.mono)
      controller.publish(frame)
      frame
    def key(c: Command, v: TradingView = view): Unit =
      val _ = controller.handle(c, snapshot, v, (100, 30), false)
    def click(frame: WorkspaceFrame, action: DeskAction): Unit =
      val b = frame.controls.find(_.action == action).get
      key(Command.PointerPress(b.x, b.y, 0))
      key(Command.Pointer(b.x, b.y, 0))

  test("the workspace removes Ask and prose analysis and retains three level columns") {
    val f = new Fixture
    f.controller.addMessage("analysis", "This prose must not appear")
    val frame = f.draw()
    assert(!frame.controls.exists(_.action == DeskAction.Ask))
    assert(!frame.lines.mkString.contains("This prose"))
    assert(
      frame.lines.head.contains("LONG") && frame.lines.head.contains("MID") &&
        frame.lines.head.contains("SHORT")
    )
    f.key(Command.Ask)
    assertEquals(f.controller.panel.input, None)
  }

  test("one complete Buy click submits the displayed amount once without Send") {
    val f = new Fixture
    val frame = f.draw()
    val action = DeskAction.Prepare(OrderIntent(Side.Buy, OrderKind.Hard))
    f.click(frame, action)
    assertEquals(f.sent.map(_.intent), Vector(OrderIntent(Side.Buy, OrderKind.Hard)))
    assertEquals(f.controller.panel.review, None)
    val b = frame.buttons.find(_.action == action).get
    f.key(Command.Pointer(b.x, b.y, 0))
    assertEquals(f.sent.size, 1)
  }

  test("a resize between press and release cannot submit an order") {
    val f = new Fixture
    val frame = f.draw()
    val b = frame.buttons.find(_.action.isInstanceOf[DeskAction.Prepare]).get
    f.key(Command.PointerPress(b.x, b.y, 0))
    val _ = f.controller.handle(Command.Pointer(b.x, b.y, 0), snapshot, view, (120, 40), false)
    assertEquals(f.sent.size, 0)
  }

  test("input records a completed order before another submission can replace its receipt") {
    val f = new Fixture
    val first =
      view.copy(outcome = Some("PAPER XMR buy filled 5"), outcomeId = OrderUpdates.nextId())
    f.draw(first)
    f.key(Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Hard)), first)
    assertEquals(f.sent.size, 1)
    assertEquals(f.controller.panel.messages.map(_.text), Vector(first.outcome.get))
    val second =
      view.copy(outcome = Some("PAPER XMR sell filled 5"), outcomeId = OrderUpdates.nextId())
    f.draw(second)
    f.key(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)), second)
    assertEquals(
      f.controller.panel.messages.map(_.text),
      Vector(first.outcome.get, second.outcome.get)
    )
    assertEquals(f.controller.panel.actionNumber, 2L)
    f.key(Command.ToggleMute, second)
    assertEquals(f.controller.panel.actionNumber, 2L)
  }
