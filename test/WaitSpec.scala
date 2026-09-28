package desk

class WaitSpec extends munit.FunSuite:
  test("timeout reports the caller label") {
    val error = intercept[AssertionError] { Wait.until("missing update", seconds = 0)(false) }
    assert(error.getMessage.contains("missing update"))
  }
  test("polls until the condition becomes true") {
    var checks = 0
    Wait.until("third check") { checks += 1; checks >= 3 }
    assert(checks >= 3)
  }
