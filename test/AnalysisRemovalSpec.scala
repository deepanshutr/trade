package desk

class AnalysisRemovalSpec extends munit.FunSuite:
  test("legacy no-analysis flag is accepted as a no-op and absent from usage") {
    assertEquals(
      Main.parseArgs(Seq("XMR", "--record", "--no-analysis")),
      Main.parseArgs(Seq("XMR", "--record"))
    )
    assert(!Main.usage.contains("--no-analysis"))
  }
