package desk

class WorkspaceSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val s = Snapshot.initial("XMR", now)
  private val view = TradingView("XMR", message = "View only")

  test("book is above tape, references beside them, simple trading controls at right") {
    for (cols, rows) <- Vector((100, 30), (120, 40), (160, 45)) do
      val frame = Workspace.draw(
        s,
        view,
        PanelView(),
        cols,
        rows,
        now,
        java.time.ZoneId.of("UTC"),
        Style.mono
      )
      assertEquals(frame.lines.size, rows)
      assert(frame.lines.forall(_.length == cols))
      val book = frame.lines.indexWhere(_.startsWith("BOOK"))
      val tape = frame.lines.indexWhere(_.startsWith("TAPE"))
      assert(book >= 0 && tape == book + 6)
      assert(frame.lines(book).contains("BTC"))
      assert(frame.lines(tape).contains("GOLD"))
      assert(!frame.lines.exists(_.contains("Ask")))
      assert(frame.buttons.nonEmpty)
      assertEquals(frame.hit(1, 1, cols - 1, rows, "XMR"), None)
      assertEquals(frame.hit(1, 1, cols, rows, "BTC"), None)
  }

  test("a hidden or clipped review never exposes a send button") {
    val plan = OrderPlan(
      "XMR",
      0,
      Side.Buy,
      OrderKind.Hard,
      BigDecimal(1),
      BigDecimal("1" + "234567890" * 11),
      BigDecimal(100),
      BigDecimal(1),
      BigDecimal(100),
      Some(BigDecimal(100)),
      BigDecimal(1)
    )
    val panel = PanelView(
      mode = "PAPER",
      review = Some(OrderReview(OrderIntent(Side.Buy, OrderKind.Hard), plan, now, now + 30000))
    )
    val frame = Workspace.draw(
      s,
      view.copy(ready = true),
      panel,
      100,
      30,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    assert(!frame.completeReview)
    assert(!frame.buttons.exists(_.label.contains("Send")))
    val small = Workspace.draw(s, view, panel, 99, 29, now, java.time.ZoneId.of("UTC"), Style.mono)
    assertEquals(small.buttons, Vector.empty[DeskButton])
  }

  test("assistant and news text cannot emit terminal control bytes") {
    val text = "news " + 27.toChar + "[2J" + 7.toChar + " hostile"
    val panel = PanelView(
      messages = Vector(DeskMessage(1, now, "analysis", text)),
      details = true,
      detailPage = DetailPage.Answers
    )
    val frame = Workspace.draw(s, view, panel, 120, 40, now, java.time.ZoneId.of("UTC"), Style.mono)
    assert(!frame.lines.mkString.contains(27.toChar))
    assert(!frame.lines.mkString.contains(7.toChar))
  }

  test("no feedback control is offered when no analysis text is visible") {
    val messages = Vector(DeskMessage(1, now, "analysis", "hidden answer")) ++
      (2L to 30L).map(id => DeskMessage(id, now, "news", "news " + id))
    val frame = Workspace.draw(
      s,
      view,
      PanelView(messages = messages, details = true, detailPage = DetailPage.Answers),
      120,
      40,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    assert(!frame.lines.exists(_.contains("hidden answer")))
    assert(!frame.buttons.exists(_.action.isInstanceOf[DeskAction.Feedback]))
  }

  test("displayed mode shortcut letters decode to the indicated mode") {
    val frame =
      Workspace.draw(s, view, PanelView(), 120, 40, now, java.time.ZoneId.of("UTC"), Style.mono)
    frame.buttons.foreach { button =>
      button.action match
        case DeskAction.Mode(name) =>
          assertEquals(
            Keys.step(Vector.empty, button.label.head.toInt, false)._2,
            Some(Command.Mode(name))
          )
        case _ => ()
    }
  }
