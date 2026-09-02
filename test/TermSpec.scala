package desk

class TermSpec extends munit.FunSuite:
  val esc = 27.toChar.toString
  test(
    "a frame homes the cursor, clears each line to its end, separates lines with CRLF, and clears to the end of the screen"
  ) {
    assertEquals(
      Term.frame(Vector("ab", "cd")),
      esc + "[Hab" + esc + "[K\r\ncd" + esc + "[K" + esc + "[J"
    )
    assertEquals(Term.frame(Vector.empty), esc + "[H" + esc + "[J")
  }
  test("stty size output parses as columns then rows") {
    assertEquals(Term.parseSize("30 100\n"), Some((100, 30)))
    assertEquals(Term.parseSize(""), None)
    assertEquals(Term.parseSize("x y"), None)
  }
