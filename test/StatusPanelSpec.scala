package desk

class StatusPanelSpec extends munit.FunSuite:
  test("full status remains readable through the message pane at minimum width") {
    val snapshot = Snapshot.initial("XMR", 1800000000000L)
    val reason = "Account data unavailable because the latest position response is stale"
    val view = TradingView("XMR", message = reason)
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => 1800000000000L,
      _ => fail("status must not prepare an order"),
      _ => fail("status must not submit an order"),
      _ => fail("status must not ask the assistant"),
      _ => fail("status must not change mode")
    )
    val account = "Cash $10000 realized $123.45 open $678.90"
    val analysis =
      "Analysis updated 2026-09-11T14:16:27Z last request 26092 tokens. News partial 1 unavailable"
    controller.status("Background capture saved 5s ago", analysis, account)
    val frame = Workspace.draw(
      snapshot,
      view,
      controller.panel,
      100,
      30,
      1800000000000L,
      java.time.ZoneOffset.UTC,
      Style.mono
    )
    controller.publish(frame)
    val button = frame.buttons.find(_.label == "Status")
    assert(button.nonEmpty, "minimum-width status details are unreachable")
    val chosen = button.get
    controller.handle(Command.PointerPress(chosen.x, chosen.y, 0), snapshot, view, (100, 30), false)
    controller.handle(Command.Pointer(chosen.x, chosen.y, 0), snapshot, view, (100, 30), false)
    val message = controller.panel.messages.last
    assertEquals(message.kind, "data")
    for expected <- Vector(account, analysis, reason, "Background capture saved 5s ago") do
      assert(message.text.contains(expected))
    assert(controller.panel.review.isEmpty)
  }
