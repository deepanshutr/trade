package desk

class KeysSpec extends munit.FunSuite:
  private def feed(bytes: Int*)(promptOpen: Boolean = false): Vector[Command] =
    bytes
      .foldLeft((Vector.empty[Int], Vector.empty[Command])) { case ((pending, out), b) =>
        val (p, c) = Keys.step(pending, b, promptOpen)
        (p, out ++ c.toVector)
      }
      ._2

  test("plain keys map as before") {
    assertEquals(feed(113)(), Vector(Command.Quit))
    assertEquals(feed(3)(), Vector(Command.Quit))
    assertEquals(feed(109)(), Vector(Command.StartMarketPrompt))
    assertEquals(feed(120)(), Vector.empty)
  }

  test("arrows in both sequences map to zoom and scrub") {
    assertEquals(feed(27, 91, 65)(), Vector(Command.ZoomOut))
    assertEquals(feed(27, 91, 66)(), Vector(Command.ZoomIn))
    assertEquals(feed(27, 91, 67)(), Vector(Command.ScrubForward))
    assertEquals(feed(27, 91, 68)(), Vector(Command.ScrubBack))
    assertEquals(feed(27, 79, 65)(), Vector(Command.ZoomOut))
    assertEquals(feed(27, 79, 68)(), Vector(Command.ScrubBack))
    assertEquals(feed(27, 91, 68, 27, 91, 67)(), Vector(Command.ScrubBack, Command.ScrubForward))
  }

  test("an escape followed by a plain key decodes the key on its own") {
    assertEquals(feed(27, 113)(), Vector(Command.Quit))
    assertEquals(feed(27, 27, 91, 65)(), Vector(Command.ZoomOut))
    assertEquals(feed(27, 91, 120, 113)(), Vector(Command.Quit))
  }

  test("inside the prompt an escape cancels at once and other bytes type") {
    assertEquals(feed(27)(promptOpen = true), Vector(Command.PromptCancel))
    assertEquals(feed(13)(promptOpen = true), Vector(Command.PromptSubmit))
    assertEquals(feed(10)(promptOpen = true), Vector(Command.PromptSubmit))
    assertEquals(feed(127)(promptOpen = true), Vector(Command.PromptBackspace))
    assertEquals(feed(8)(promptOpen = true), Vector(Command.PromptBackspace))
    assertEquals(feed(3)(promptOpen = true), Vector(Command.Quit))
    assertEquals(feed(66)(promptOpen = true), Vector(Command.PromptChar('B')))
    assertEquals(feed(1)(promptOpen = true), Vector.empty)
    assertEquals(
      Keys.step(Vector(27), 91, promptOpen = true),
      (Vector.empty, Some(Command.PromptChar('[')))
    )
  }
