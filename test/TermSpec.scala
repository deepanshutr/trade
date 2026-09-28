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
  test("frames are encoded as UTF-8 bytes") {
    val sink = new java.io.ByteArrayOutputStream()
    new Term.Writer(new java.io.PrintStream(sink))
      .write(Vector((0x2801).toChar.toString), (100, 30))
    val bytes = sink.toByteArray.toVector.map(_ & 0xff)
    assert(bytes.containsSlice(Vector(0xe2, 0xa0, 0x81)), bytes)
  }

  test("the alternate screen enables bracketed paste and restoration disables it") {
    assertEquals(Term.enterCodes, esc + "[?1049h" + esc + "[?25l" + esc + "[?2004h")
    assertEquals(
      Term.leaveCodes,
      esc + "[?2026l" + esc + "[?2004l" + esc + "[?25h" + esc + "[?1049l"
    )
    assertEquals(Main.escapeWaitNanos, 40_000_000L)
  }
