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
    assertEquals(Main.nextPromptOpen(false, Command.NextInterval), false)
  }
