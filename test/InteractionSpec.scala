package desk

class InteractionSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val intent = OrderIntent(Side.Buy, OrderKind.Hard)
  private val plan = OrderPlan(
    "XMR",
    0,
    Side.Buy,
    OrderKind.Hard,
    BigDecimal(5),
    BigDecimal(100),
    BigDecimal(100),
    BigDecimal(5),
    BigDecimal(500),
    Some(BigDecimal(100)),
    BigDecimal(5)
  )
  private val view = TradingView(
    "XMR",
    message = "Ready",
    ready = true,
    rules = Some(MarketRules(0, 2, 5)),
    bid = Some(BigDecimal(99)),
    ask = Some(BigDecimal(100))
  )

  private class Fixture:
    var time = now
    var sent = Vector.empty[OrderReview]
    var questions = Vector.empty[String]
    var modes = Vector.empty[String]
    var modeResult: Either[String, String] = Left("disabled")
    private var pending = Vector.empty[Int]
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => time,
      _ => Right(plan),
      review => { sent :+= review; true },
      question => { questions :+= question; true },
      name => { modes :+= name; modeResult }
    )
    def draw(cols: Int = 120, rows: Int = 40): WorkspaceFrame =
      val frame = Workspace.draw(
        snapshot,
        view,
        controller.panel,
        cols,
        rows,
        time,
        java.time.ZoneId.of("UTC"),
        Style.mono
      )
      controller.publish(frame)
      frame
    def key(command: Command, size: (Int, Int) = (120, 40)): Unit =
      controller.handle(command, snapshot, view, size, false)
    def feed(text: String): Vector[Command] =
      val commands = Vector.newBuilder[Command]
      text.foreach { c =>
        val input = controller.panel.input.nonEmpty
        val (next, command) =
          Keys.step(pending, c.toInt, input, mouseEnabled = true, textInput = input)
        pending = next
        command.foreach { value => commands += value; key(value) }
      }
      commands.result()
    def prepare(): WorkspaceFrame =
      draw()
      key(Command.PlaceOrder(intent))
      draw()
    def press(button: DeskButton): Unit = key(Command.PointerPress(button.x, button.y, 0))
    def release(button: DeskButton): Unit = key(Command.Pointer(button.x, button.y, 0))

  test("order shortcut only prepares and send requires that exact complete review on screen") {
    val f = new Fixture
    f.draw()
    f.key(Command.PlaceOrder(intent))
    assert(f.controller.panel.review.nonEmpty)
    assertEquals(f.sent, Vector.empty)
    f.key(Command.SubmitTarget)
    assertEquals(f.sent, Vector.empty)
    f.draw()
    f.key(Command.SubmitTarget, (119, 40))
    assertEquals(f.sent, Vector.empty)
    f.draw()
    f.key(Command.SubmitTarget)
    assertEquals(f.sent.map(_.plan), Vector(plan))
    f.key(Command.SubmitTarget)
    assertEquals(f.sent.size, 1)
  }

  test("expired review, market prompt and question submission never trade") {
    val f = new Fixture
    f.draw()
    f.key(Command.PlaceOrder(intent))
    f.draw()
    f.time += 30001
    f.key(Command.SubmitTarget)
    assertEquals(f.sent.size, 0)
    f.key(Command.CancelTarget)
    f.draw()
    f.key(Command.Ask)
    "12390 buy?".foreach(c => f.key(Command.PromptChar(c)))
    f.draw()
    f.key(Command.PromptSubmit)
    assertEquals(f.questions, Vector("12390 buy?"))
    assertEquals(f.sent.size, 0)
    f.draw()
    f.controller.handle(
      Command.PlaceOrder(intent),
      snapshot.copy(prompt = Some("1")),
      view,
      (120, 40),
      true
    )
    assertEquals(f.controller.panel.review, None)
  }

  test("feedback belongs to the displayed message even when a reply arrives before a click") {
    val f = new Fixture
    f.controller.addMessage("analysis", "first")
    val frame = f.draw()
    val accept = frame.buttons.find(_.label.startsWith("y Accept")).get
    val first = f.controller.panel.messages.last.id
    f.key(Command.PointerPress(accept.x, accept.y, 0))
    f.controller.addMessage("analysis", "second")
    f.key(Command.Pointer(accept.x, accept.y, 0))
    assertEquals(f.controller.panel.messages.find(_.id == first).flatMap(_.feedback), Some(true))
    assertEquals(f.controller.panel.messages.last.feedback, None)
    assertEquals(f.sent.size, 0)
  }

  test("mouse release activates once and pasted numeric questions remain text") {
    def decode(text: String, prompt: Boolean, textInput: Boolean): Vector[Command] =
      text.toVector
        .foldLeft((Vector.empty[Int], Vector.empty[Command])) { case ((pending, out), c) =>
          val (next, command) =
            Keys.step(pending, c.toInt, prompt, mouseEnabled = true, textInput = textInput)
          (next, out ++ command)
        }
        ._2
    val esc = 27.toChar.toString
    assertEquals(
      decode(esc + "[<0;88;4M" + esc + "[<0;88;4m", false, false),
      Vector(Command.PointerPress(88, 4, 0), Command.Pointer(88, 4, 0))
    )
    val pasted = decode(esc + "[200~12390 buy\n" + esc + "[201~", true, true)
    assertEquals(pasted.collect { case Command.PromptText(text) => text }.mkString, "12390 buy ")
    assert(!pasted.exists {
      case Command.PlaceOrder(_) | Command.SubmitTarget | Command.PromptSubmit => true;
      case _                                                                   => false
    })
    assertEquals(decode(esc + "[<0;999999999;4m", false, false), Vector.empty)
  }

  test("a left release without a matching press cannot send a reviewed order") {
    val f = new Fixture
    f.draw()
    f.key(Command.PlaceOrder(intent))
    val frame = f.draw()
    val send = frame.buttons.find(_.action == DeskAction.Submit).get
    f.key(Command.Pointer(send.x, send.y, 0))
    assertEquals(f.sent, Vector.empty)
    assert(f.controller.panel.review.nonEmpty)
  }

  test("a real press and release sends once through an ordinary redraw") {
    val f = new Fixture
    val before = f.prepare()
    val send = before.buttons.find(_.action == DeskAction.Submit).get
    val reviewed = f.controller.panel.review.get
    val esc = 27.toChar.toString
    f.feed(esc + s"[<0;${send.x};${send.y}M")
    assertEquals(f.sent, Vector.empty)
    f.controller.invalidate()
    f.controller.status("Public data updated", "Analysis ready", "same paper account")
    val after = f.draw()
    assertNotEquals(after.lines, before.lines)
    f.feed(esc + s"[<0;${send.x};${send.y}m")
    assertEquals(f.sent, Vector(reviewed))
    f.feed(esc + s"[<0;${send.x};${send.y}m")
    assertEquals(f.sent, Vector(reviewed))
  }

  test("a press cannot survive a changed control context even when the old frame returns") {
    val changes: Vector[(String, WorkspaceFrame => WorkspaceFrame)] = Vector(
      "columns" -> (f => f.copy(cols = f.cols + 1)),
      "rows" -> (f => f.copy(rows = f.rows + 1)),
      "market" -> (f => f.copy(coin = "BTC")),
      "mode" -> (f => f.copy(mode = "LIVE")),
      "review" -> (f =>
        f.copy(review = f.review.map(r => r.copy(plan = r.plan.copy(limit = BigDecimal(101)))))
      ),
      "review terms" -> (f =>
        f.copy(review = f.review.map(_.copy(leverage = Some(2), marginMode = Some("isolated"))))
      ),
      "action" -> (f =>
        f.copy(buttons =
          f.buttons.map(b =>
            if b.action == DeskAction.Submit then b.copy(action = DeskAction.Ask) else b
          )
        )
      ),
      "position" -> (f =>
        f.copy(buttons =
          f.buttons.map(b => if b.action == DeskAction.Submit then b.copy(x = b.x + 1) else b)
        )
      ),
      "width" -> (f =>
        f.copy(buttons =
          f.buttons.map(b =>
            if b.action == DeskAction.Submit then b.copy(width = b.width - 1) else b
          )
        )
      )
    )
    changes.foreach { (name, change) =>
      val f = new Fixture
      val frame = f.prepare()
      val send = frame.buttons.find(_.action == DeskAction.Submit).get
      f.press(send)
      f.controller.publish(change(frame))
      f.controller.publish(frame)
      f.release(send)
      assertEquals(f.sent, Vector.empty, name)
      f.press(send)
      f.release(send)
      assertEquals(f.sent.size, 1, name + " permits a fresh click")
    }
  }

  test("press and release must hit the same button within the actual terminal size") {
    val f = new Fixture
    val frame = f.prepare()
    val send = frame.buttons.find(_.action == DeskAction.Submit).get
    f.press(send)
    f.key(Command.Pointer(send.x + send.width, send.y, 0))
    f.release(send)
    assertEquals(f.sent, Vector.empty)
    f.key(Command.PointerPress(send.x - 1, send.y, 0))
    f.release(send)
    assertEquals(f.sent, Vector.empty)
    f.key(Command.PointerPress(send.x, send.y, 0), (119, 40))
    f.release(send)
    assertEquals(f.sent, Vector.empty)
    f.press(send)
    f.key(Command.Pointer(send.x, send.y, 0), (120, 39))
    f.release(send)
    assertEquals(f.sent, Vector.empty)
    f.press(send)
    f.controller.invalidate()
    f.release(send)
    f.controller.publish(frame)
    f.release(send)
    assertEquals(f.sent, Vector.empty)
    f.press(send)
    f.release(send)
    assertEquals(f.sent.size, 1)
  }

  test("keyboard input ends a pending mouse gesture") {
    val f = new Fixture
    val frame = f.prepare()
    val send = frame.buttons.find(_.action == DeskAction.Submit).get
    f.press(send)
    f.key(Command.ScrollMessages(5))
    f.draw()
    f.release(send)
    assertEquals(f.sent, Vector.empty)
  }

  test("submission rejects unavailable, incomplete, mismatched and out of time reviews") {
    val cases = Vector(
      "before capture",
      "at expiry",
      "incomplete",
      "other review",
      "terms changed",
      "pending",
      "not ready",
      "quit",
      "prompt",
      "other view"
    )
    cases.foreach { name =>
      val f = new Fixture
      val frame = f.prepare()
      if name == "before capture" then f.time = now - 1
      if name == "at expiry" then f.time = now + 30000
      if name == "incomplete" then f.controller.publish(frame.copy(completeReview = false))
      if name == "other review" then
        f.controller.publish(frame.copy(review = frame.review.map(_.copy(createdAt = now + 1))))
      if name == "terms changed" then
        f.controller.publish(frame.copy(review = frame.review.map(_.copy(leverage = Some(2)))))
      val actualView = name match
        case "pending"    => view.copy(pending = true)
        case "not ready"  => view.copy(ready = false)
        case "other view" => view.copy(coin = "BTC")
        case _            => view
      val actualSnapshot = name match
        case "quit"   => snapshot.copy(quit = true)
        case "prompt" => snapshot.copy(prompt = Some(""))
        case _        => snapshot
      f.controller.handle(Command.SubmitTarget, actualSnapshot, actualView, (120, 40), false)
      assertEquals(f.sent, Vector.empty, name)
    }
  }

  test("mode selection needs its drawn confirmation and invalidates the old mode frame") {
    val f = new Fixture
    f.modeResult = Right("configured live account")
    val paper = f.draw()
    f.feed("l")
    assertEquals(f.controller.panel.modeReview, Some("LIVE"))
    assertEquals(f.modes, Vector.empty)
    f.feed("\r")
    assertEquals(f.modes, Vector.empty)
    f.draw()
    f.key(Command.SubmitTarget, (119, 40))
    assertEquals(f.modes, Vector.empty)
    f.feed("\r")
    assertEquals(f.modes, Vector("LIVE"))
    assertEquals(f.controller.panel.mode, "LIVE")
    assertEquals(f.controller.panel.account, "configured live account")
    assertEquals(f.sent, Vector.empty)
    f.controller.publish(paper)
    f.key(Command.PlaceOrder(intent))
    assertEquals(f.controller.panel.review, None)
    f.draw()
    f.key(Command.PlaceOrder(intent))
    assert(f.controller.panel.review.nonEmpty)
    assertEquals(f.sent, Vector.empty)
  }

  test("cancelled and failed mode selections preserve the active account") {
    val f = new Fixture
    f.draw()
    f.feed("l")
    f.draw()
    f.key(Command.CancelTarget)
    f.feed("\r")
    assertEquals(f.modes, Vector.empty)
    f.draw()
    f.feed("l")
    f.draw()
    f.feed("\r")
    assertEquals(f.modes, Vector("LIVE"))
    assertEquals(f.controller.panel.mode, "PAPER")
    assertEquals(f.controller.panel.modeReview, None)
    assert(f.controller.panel.messages.exists(_.text == "disabled"))
    assertEquals(f.sent, Vector.empty)
  }

  test("typed and pasted command characters remain one question through the decoder") {
    val f = new Fixture
    val esc = 27.toChar.toString
    f.draw()
    f.feed(esc + "[200~12390\np l q" + esc + "[201~")
    assertEquals(f.controller.panel.review, None)
    assertEquals(f.controller.panel.modeReview, None)
    f.feed("a")
    val typed = "1234567890plqmy[n] + buy? "
    f.feed(typed)
    f.feed(esc + "[200~9090 buy\np l\rq\t" + esc + "[20")
    f.feed("1~")
    val expected = typed + "9090 buy p l q "
    assertEquals(f.controller.panel.input, Some(expected))
    assertEquals(f.questions, Vector.empty)
    assertEquals(f.modes, Vector.empty)
    assertEquals(f.controller.panel.target, None)
    assertEquals(f.controller.panel.review, None)
    f.draw()
    f.feed("\r")
    assertEquals(f.questions, Vector(expected.trim))
    assertEquals(f.sent, Vector.empty)
  }

  test("question length stays bounded and mouse submission uses the drawn Ask button") {
    val f = new Fixture
    f.draw()
    f.feed("a")
    f.key(Command.PromptText("7" * 4100))
    assertEquals(f.controller.panel.input.map(_.length), Some(4000))
    f.key(Command.PromptChar('8'))
    assertEquals(f.controller.panel.input.map(_.length), Some(4000))
    f.key(Command.PromptBackspace)
    val frame = f.draw()
    val ask = frame.buttons.find(_.action == DeskAction.SendQuestion).get
    f.release(ask)
    assertEquals(f.questions, Vector.empty)
    f.press(ask)
    f.release(ask)
    assertEquals(f.questions, Vector("7" * 3999))
    assertEquals(f.sent, Vector.empty)
  }

  test("feedback shortcut retains the displayed id while a new response arrives") {
    val f = new Fixture
    f.controller.addMessage("analysis", "first")
    f.draw()
    val first = f.controller.panel.messages.last.id
    f.controller.addMessage("analysis", "second")
    f.feed("n")
    assertEquals(f.controller.panel.messages.find(_.id == first).flatMap(_.feedback), Some(false))
    assertEquals(f.controller.panel.messages.last.feedback, None)
    f.draw()
    f.feed("y")
    assertEquals(f.controller.panel.messages.last.feedback, Some(true))
    assertEquals(f.sent, Vector.empty)
  }
