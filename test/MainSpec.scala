package desk

class MainSpec extends munit.FunSuite:
  test("arguments: defaults, a coin, an interval, and errors") {
    assertEquals(Main.parseArgs(Nil), Right(Main.Args("XMR", "5m")))
    assertEquals(Main.parseArgs(Seq("BTC")), Right(Main.Args("BTC", "5m")))
    assertEquals(
      Main.parseArgs(Seq("flx:GOLD", "--interval", "1h")),
      Right(Main.Args("flx:GOLD", "1h"))
    )
    assertEquals(Main.parseArgs(Seq("--interval", "15m")), Right(Main.Args("XMR", "15m")))
    assert(Main.parseArgs(Seq("--interval", "7m")).isLeft)
    assert(Main.parseArgs(Seq("--interval")).isLeft)
    assert(Main.parseArgs(Seq("--nope")).isLeft)
    assert(Main.parseArgs(Seq("A", "B")).isLeft)
  }
  test("keys with the prompt closed") {
    assertEquals(Main.keyToCommand(113, false), Some(Command.Quit))
    assertEquals(Main.keyToCommand(3, false), Some(Command.Quit))
    assertEquals(Main.keyToCommand(105, false), Some(Command.NextInterval))
    assertEquals(Main.keyToCommand(109, false), Some(Command.StartMarketPrompt))
    assertEquals(Main.keyToCommand(120, false), None)
    assertEquals(Main.keyToCommand(13, false), None)
  }
  test("keys with the prompt open") {
    assertEquals(Main.keyToCommand(13, true), Some(Command.PromptSubmit))
    assertEquals(Main.keyToCommand(10, true), Some(Command.PromptSubmit))
    assertEquals(Main.keyToCommand(27, true), Some(Command.PromptCancel))
    assertEquals(Main.keyToCommand(127, true), Some(Command.PromptBackspace))
    assertEquals(Main.keyToCommand(8, true), Some(Command.PromptBackspace))
    assertEquals(Main.keyToCommand(3, true), Some(Command.Quit))
    assertEquals(Main.keyToCommand(113, true), Some(Command.PromptChar('q')))
    assertEquals(Main.keyToCommand(58, true), Some(Command.PromptChar(':')))
    assertEquals(Main.keyToCommand(200, true), None)
  }
  test("the key thread's prompt mode follows the commands it emits") {
    assertEquals(Main.nextPromptOpen(false, Command.StartMarketPrompt), true)
    assertEquals(Main.nextPromptOpen(true, Command.PromptSubmit), false)
    assertEquals(Main.nextPromptOpen(true, Command.PromptCancel), false)
    assertEquals(Main.nextPromptOpen(true, Command.PromptChar('x')), true)
    assertEquals(Main.nextPromptOpen(true, Command.PromptBackspace), true)
    assertEquals(Main.nextPromptOpen(false, Command.Quit), false)
    assertEquals(Main.nextPromptOpen(false, Command.NextInterval), false)
  }
