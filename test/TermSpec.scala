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
    val bytes = Term.bytes(Vector((0x2801).toChar.toString)).toVector.map(_ & 0xff)
    assert(bytes.containsSlice(Vector(0xe2, 0xa0, 0x81)), bytes)
  }

  test("the alternate screen enables bracketed paste and restoration disables it") {
    assertEquals(Term.enterCodes, esc + "[?1049h" + esc + "[?25l" + esc + "[?2004h")
    assertEquals(Term.leaveCodes, esc + "[?2004l" + esc + "[?25h" + esc + "[?1049l")
    assertEquals(Main.escapeWaitNanos, 40_000_000L)
  }

  test("a recorded write or flush error cannot publish a completed market frame") {
    for failFlush <- Vector(false, true) do
      val sink = new java.io.OutputStream:
        override def write(byte: Int): Unit =
          if !failFlush then throw new java.io.IOException("held write")
        override def flush(): Unit =
          if failFlush then throw new java.io.IOException("held flush")
      val output = new java.io.PrintStream(sink)
      val shown = new java.util.concurrent.atomic.AtomicReference[Option[String]](Some("XMR"))
      try
        intercept[java.io.IOException] {
          Main.writeMarketFrame(
            "BTC",
            (100, 30),
            shown,
            () => Term.write(Vector("BTC"), output),
            complete = true
          )
        }
        assert(output.checkError())
        assertEquals(shown.get(), None)
      finally output.close()
  }

  test("a successful byte write publishes its market after output finishes") {
    val sink = new java.io.ByteArrayOutputStream()
    val output = new java.io.PrintStream(sink)
    val shown = new java.util.concurrent.atomic.AtomicReference(Option.empty[String])
    val canvas = new Canvas(1, 1)
    canvas.dot(0, 0)
    val lines = Vector("XMR " + canvas.lines.head)
    try
      Main.writeMarketFrame(
        "XMR",
        (100, 30),
        shown,
        () => Term.write(lines, output),
        complete = true
      )
      assertEquals(shown.get(), Some("XMR"))
      assertEquals(sink.toByteArray.toVector, Term.bytes(lines).toVector)
    finally output.close()
  }
