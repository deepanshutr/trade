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

  test("pending outcomes use the same compact labels as finished outcomes") {
    val submitting = "XMR submitting"
    val messages = Vector(
      "Wallet change paused this account or account state is unavailable" -> "Account unavailable",
      "XMR recovered openInterestCapCanceled" -> "Not sent",
      "XMR recovered marginCanceled" -> "Not sent",
      "XMR canceled before sending" -> "Not sent",
      "XMR filled 0.5 at 300" -> "Filled",
      "Saved order state failed. Restart to recover" -> "Saved order state failed. Restart to recover",
      submitting -> "Sending"
    )
    messages.foreach { (message, expected) =>
      val frame = Workspace.draw(
        s,
        view.copy(pending = true, message = message),
        PanelView(mode = "LIVE"),
        100,
        30,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      val text = frame.lines.mkString("\n")
      assert(text.contains(expected), text)
      if message != submitting && expected != message then assert(!text.contains(message), text)
      assertEquals(frame.lines.size, 30)
      assert(frame.lines.forall(_.length == 100), text)

      if message != submitting && expected != message then
        val finished = Workspace.draw(
          s,
          view.copy(pending = false),
          PanelView(mode = "LIVE", actionNotice = Some(message)),
          100,
          30,
          now,
          java.time.ZoneOffset.UTC,
          Style.mono
        )
        assert(finished.lines.mkString("\n").contains(expected), finished.lines.mkString("\n"))
    }
  }

  test("prop limit precision errors retain their reason in the trade status") {
    val notice = "Prop limit does not match market precision"
    val frame = Workspace.draw(
      s,
      view.copy(ready = true),
      PanelView(mode = "PROP", actionNotice = Some(notice), actionNumber = 1),
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
    val panel = PanelView(mode = "PROP")
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
            Keys
              .step(Vector.empty, button.label.head.toInt, false, bindings = Bindings.defaults)
              ._2,
            Some(Command.Mode(name))
          )
        case _ => ()
    }
  }

  test("CLOSE is coloured like a sell tile when enabled and dimmed when disabled") {
    val live = view.copy(ready = true, leverage = Some(5))
    def sizeRow(position: BigDecimal): String =
      Workspace
        .draw(
          s,
          live.copy(position = Some(position)),
          PanelView(mode = "LIVE"),
          100,
          30,
          now,
          java.time.ZoneOffset.UTC,
          Style.colour
        )
        .lines(30 - 15 + 5)
    val sell = Style.colour.ask + Screen.bold
    assert(sizeRow(BigDecimal("0.2")).contains(sell + "[x Close 0.2 XMR]" + Screen.reset))
    assert(sizeRow(BigDecimal(0)).contains(Screen.dim + "[x Close]" + Screen.reset))
  }

  test("the CLOSE label starts with the key that presses it") {
    val frame = Workspace.draw(
      s,
      view.copy(ready = true, leverage = Some(5), position = Some(BigDecimal("-0.2"))),
      PanelView(mode = "LIVE"),
      120,
      40,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    val close = frame.buttons.find(_.action == DeskAction.Close).get
    assertEquals(
      Keys.step(Vector.empty, close.label.head.toInt, false, bindings = Bindings.defaults)._2,
      Some(Command.Close)
    )
  }

  test("buttons and tiles show the bound keys, and each key presses its control") {
    def key(text: String): KeyGroup = KeyGroup(text.getBytes("UTF-8").toVector.map(_ & 0xff))
    val bindings = Vector(
      Bind.Close -> Binding(Vector(key("∫"))),
      Bind.LongSmall -> Binding(Vector(KeyGroup(Vector(11)))),
      Bind.Prop -> Binding(Vector(key("g"), key("p"))),
      Bind.Market -> Binding(Vector(key("M")))
    ).foldLeft(Bindings.defaults)((all, next) => all.updated(next._1, next._2).toOption.get)
    val frame = Workspace.draw(
      s,
      view.copy(ready = true, leverage = Some(5), position = Some(BigDecimal("-0.2"))),
      PanelView(mode = "LIVE", bindings = bindings),
      120,
      40,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    val labels = frame.buttons.map(_.label)
    assert(labels.exists(_.startsWith("∫ Close 0.2")), labels)
    assert(labels.exists(_.startsWith("^K REDUCE $500")), labels)
    assert(labels.contains("g p Prop"), labels)
    assert(labels.contains("l Live"), labels)
    val text = frame.lines.mkString("\n")
    assert(text.contains("M Market c Chart v Sound"), text)
    assert(text.contains("[g p Prop]  [l Live]"), text)
    def press(bytes: Vector[Int]): Option[Command] =
      bytes
        .foldLeft((Vector.empty[Int], Option.empty[Command])) { case ((pending, _), byte) =>
          Keys.step(pending, byte, false, bindings = bindings)
        }
        ._2
    assertEquals(press(key("∫").bytes), Some(Command.Close))
    assertEquals(press(Vector(11)), Some(Command.PlaceOrder(Trading.action(0))))
    assertEquals(press(Vector('g'.toInt, 'p'.toInt)), Some(Command.Mode("PROP")))
  }

  test("a key name too long for a tile gives way before the tile does") {
    val long = Bindings.defaults
      .updated(Bind.LongSmall, Binding(Vector(KeyGroup("\u001b[15;2~".toVector.map(_.toInt)))))
      .toOption
      .get
    val frame = Workspace.draw(
      s,
      view.copy(ready = true, leverage = Some(5), position = Some(BigDecimal("-0.2"))),
      PanelView(mode = "LIVE", bindings = long),
      100,
      30,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    val tile = frame.buttons.find(_.action == DeskAction.Prepare(Trading.action(0)))
    assertEquals(tile.map(_.label), Some("REDUCE $500 x5"))
  }

  test("the live setup line names the key bound to settings, as the buttons do") {
    def bound(text: String): Bindings =
      Bindings.defaults
        .updated(
          Bind.Settings,
          Binding(Vector(KeyGroup(text.getBytes("UTF-8").toVector.map(_ & 0xff))))
        )
        .toOption
        .get
    def status(view: TradingView, panel: PanelView): String =
      Workspace
        .draw(s, view, panel, 100, 30, now, java.time.ZoneOffset.UTC, Style.mono)
        .lines(30 - 15 + 4)
    val blocked = view.copy(
      message = TradingWorker.setupMessage,
      block = Some(TradingWorker.setupMessage),
      blockCode = BlockCode.Blocked
    )
    val live = PanelView(mode = "LIVE")
    assert(status(blocked, live).contains("Live setup required. Press s for settings"))
    val control = status(blocked, live.copy(bindings = bound("\u000b")))
    assert(control.contains("Live setup required. Press ^K for settings"), control)
    val option = status(blocked, live.copy(bindings = bound("∫")))
    assert(option.contains("Live setup required. Press ∫ for settings"), option)
    // The refusal of l in prop mode is the same sentence, with its action number.
    val refused = PanelView(
      mode = "PROP",
      actionNotice = Some(TradingWorker.setupMessage),
      actionNumber = 1,
      bindings = bound("\u000b")
    )
    val notice = status(view.copy(message = "", block = None), refused)
    assert(notice.contains("Live setup required. Press ^K for settings #1"), notice)
  }
