package desk

class WorkspaceSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val s = Snapshot.initial("XMR", now)
  private val view = TradingView("XMR", message = "View only")

  test("pending owner mismatch is visible at the smallest workspace size") {
    val message = TradingWorker.ownerMismatchMessage
    val frame = Workspace.draw(
      s,
      view.copy(pending = true, message = message),
      PanelView(mode = "LIVE"),
      Layout.minCols,
      Layout.minRows,
      now,
      java.time.ZoneOffset.UTC,
      Style.mono
    )
    val text = frame.lines.mkString("\n")
    assert(text.contains(message), text)
    assert(!text.contains("Sending"), text)
  }

  test("pending saved-owner mismatch is visible at the smallest workspace size") {
    val message = TradingWorker.savedOwnerMismatchMessage
    val frame = Workspace.draw(
      s,
      view.copy(pending = true, message = message),
      PanelView(mode = "LIVE"),
      Layout.minCols,
      Layout.minRows,
      now,
      java.time.ZoneOffset.UTC,
      Style.mono
    )
    val text = frame.lines.mkString("\n")
    assert(text.contains(message), text)
    assert(!text.contains("Sending"), text)
  }

  test("paper limit precision errors retain their reason in the trade status") {
    val notice = "Paper limit does not match market precision"
    val frame = Workspace.draw(
      s,
      view.copy(ready = true),
      PanelView(mode = "PAPER", actionNotice = Some(notice), actionNumber = 1),
      160,
      45,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    val status = frame.lines(45 - 15 + 4).drop(Layout.workspaceMarketWidth(160) + 1).trim
    assert(!status.contains("Insufficient funds"), status)
    assertEquals(status, s"$notice #1")
  }

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
  }

  test("one-click controls have no send button and a small screen has no controls") {
    val panel = PanelView(mode = "PAPER")
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
    assert(!frame.buttons.exists(_.label.contains("Send")))
    val small = Workspace.draw(s, view, panel, 99, 29, now, java.time.ZoneId.of("UTC"), Style.mono)
    assertEquals(small.buttons, Vector.empty[DeskButton])
  }

  test("action notices cannot emit terminal control bytes") {
    val text = "news " + 27.toChar + "[2J" + 7.toChar + " hostile"
    val panel = PanelView(
      actionNotice = Some(text)
    )
    val frame = Workspace.draw(s, view, panel, 120, 40, now, java.time.ZoneId.of("UTC"), Style.mono)
    assert(!frame.lines.mkString.contains(27.toChar))
    assert(!frame.lines.mkString.contains(7.toChar))
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
