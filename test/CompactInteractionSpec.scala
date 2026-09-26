package desk

import java.time.ZoneOffset

class CompactInteractionSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val view = TradingView("XMR", ready = true, message = "Ready")

  private class Fixture:
    var sent = 0
    var muted = Vector.empty[Boolean]
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => now,
      _ => Left("Order price is unavailable"),
      _ => { sent += 1; true },
      _ => Left("Live account is not configured"),
      mute = value => muted :+= value
    )
    def draw(): WorkspaceFrame =
      val frame =
        Workspace.draw(snapshot, view, controller.panel, 120, 40, now, ZoneOffset.UTC, Style.mono)
      controller.publish(frame)
      frame
    def key(command: Command): Unit =
      controller.handle(command, snapshot, view, (120, 40), false)
    def click(label: String): Unit =
      val button = draw().buttons.find(_.label == label).getOrElse(fail("missing " + label))
      key(Command.PointerPress(button.x, button.y, 0))
      key(Command.Pointer(button.x, button.y, 0))

  test("failed order and mode actions explain themselves in the compact view") {
    val f = new Fixture
    f.draw()
    f.key(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
    assert(f.draw().lines.mkString.contains("Waiting for prices"))
    f.key(Command.Mode("LIVE"))
    f.draw()
    f.key(Command.SubmitTarget)
    assert(f.draw().lines.mkString.contains("Account unavailable"))
    assertEquals(f.controller.panel.mode, "PAPER")
    assertEquals(f.sent, 0)
  }

  test("mute remains immediate and market prompt text cannot toggle it") {
    val f = new Fixture
    f.draw()
    f.key(Command.ToggleMute)
    assertEquals(f.muted, Vector(true))
    assert(f.draw().lines.mkString.contains("Muted"))
    f.key(Command.ToggleMute)
    assertEquals(f.muted, Vector(true, false))
    f.draw()
    var pending = Vector.empty[Int]
    "hv123".foreach { c =>
      val (next, command) = Keys.step(pending, c.toInt, true, mouseEnabled = true, textInput = true)
      pending = next
      command.foreach(f.key)
    }
    assertEquals(f.muted, Vector(true, false))
    assertEquals(f.sent, 0)
  }

  test("historical move is labelled from close rather than remaining live distance") {
    val columns = Outlook
      .empty("XMR")
      .columns
      .map(
        _.copy(
          direction = OutlookDirection.Up,
          status = "ready",
          support = Some(90),
          resistance = Some(105),
          target = Some(105),
          reference = Some(100),
          base = HistoricalRate(65, 100, 100),
          matched = HistoricalRate(40, 50, 50)
        )
      )
    val outlook = OutlookView("XMR", now, columns)
    val rows =
      OutlookScreen.rows(outlook, 46, Some(Axis(89, 111, 9)), 9, ChartMode.Candles).mkString("\n")
    assert(!rows.contains("To hit"), rows)
    assert(rows.contains("Test 5%"), rows)
  }
