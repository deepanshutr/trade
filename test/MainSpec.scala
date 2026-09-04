package desk

class MainSpec extends munit.FunSuite:
  test("parseArgs takes an optional coin and nothing else") {
    assertEquals(Main.parseArgs(Nil), Right(Main.Args("XMR")))
    assertEquals(Main.parseArgs(Seq("BTC")), Right(Main.Args("BTC")))
    assert(Main.parseArgs(Seq("--interval", "5m")).isLeft)
    assert(Main.parseArgs(Seq("BTC", "ETH")).isLeft)
  }
  test("a size event fires only when the size changes") {
    assertEquals(Main.sizeEvent((100, 30), (100, 30)), None)
    assertEquals(Main.sizeEvent((100, 30), (120, 40)), Some(Event.Resize(120, 40)))
  }
  test("the key thread's prompt mode follows the commands it emits") {
    assertEquals(Main.nextPromptOpen(false, Command.StartMarketPrompt), true)
    assertEquals(Main.nextPromptOpen(true, Command.PromptSubmit), false)
    assertEquals(Main.nextPromptOpen(true, Command.PromptCancel), false)
    assertEquals(Main.nextPromptOpen(true, Command.PromptChar('x')), true)
    assertEquals(Main.nextPromptOpen(true, Command.PromptBackspace), true)
    assertEquals(Main.nextPromptOpen(false, Command.Quit), false)
    assertEquals(Main.nextPromptOpen(false, Command.ZoomIn), false)
  }
