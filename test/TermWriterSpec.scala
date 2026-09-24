package desk

import java.io.{ByteArrayOutputStream, IOException, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8

class TermWriterSpec extends munit.FunSuite:
  private val esc = 27.toChar.toString
  private val begin = esc + "[?2026h"
  private val end = esc + "[?2026l"
  private val reset = esc + "[0m"
  private def row(text: String): String = reset + text + reset + esc + "[K"
  private def full(lines: Vector[String]): String =
    begin + esc + "[H" + lines.map(row).mkString("\r\n") + esc + "[J" + end

  private class Sink extends ByteArrayOutputStream:
    var flushes = 0
    var failWrite = false
    var failFlush = false
    override def write(bytes: Array[Byte], offset: Int, count: Int): Unit =
      if failWrite then
        super.write(bytes, offset, count.min(4))
        throw new IOException("partial write")
      super.write(bytes, offset, count)
    override def flush(): Unit =
      flushes += 1
      if failFlush then throw new IOException("failed flush")
    def text: String = toString(UTF_8)

  private class RecoverableOutput(sink: OutputStream) extends PrintStream(sink):
    def recover(): Unit = clearError()

  test(
    "first output is one synchronized full frame and unchanged output performs no write or flush"
  ) {
    val sink = new Sink
    val output = new PrintStream(sink)
    val writer = new Term.Writer(output)
    val lines = Vector("XMR", "BUY SELL")
    writer.write(lines, (100, 30))
    assertEquals(sink.text, full(lines))
    val flushes = sink.flushes
    for _ <- 1 to 40 do writer.write(lines, (100, 30))
    assertEquals(sink.text, full(lines))
    assertEquals(sink.flushes, flushes)
  }

  test("only changed rows are written at their absolute positions with no intervening full clear") {
    val sink = new Sink
    val writer = new Term.Writer(new PrintStream(sink))
    writer.write(Vector("header", "BUY SELL", "bottom"), (100, 30))
    sink.reset()
    writer.write(Vector("header", "BUY", "bottom"), (100, 30))
    assertEquals(sink.text, begin + esc + "[2;1H" + row("BUY") + end)
  }

  test("empty changed rows clear the old text and styled UTF-8 rows remain independent") {
    val sink = new Sink
    val writer = new Term.Writer(new PrintStream(sink))
    val glyph = (0x28ff).toChar.toString
    writer.write(Vector(esc + "[31mSELL", "old", "footer"), (100, 30))
    sink.reset()
    writer.write(Vector(esc + "[32mBUY " + glyph, "", "footer"), (100, 30))
    assertEquals(
      sink.text,
      begin + esc + "[1;1H" + row(esc + "[32mBUY " + glyph) + esc + "[2;1H" + row("") + end
    )
    assert(sink.toByteArray.toVector.containsSlice(glyph.getBytes(UTF_8).toVector))
  }

  test("growing or shrinking row count and both terminal dimensions force a full repaint") {
    val sink = new Sink
    val writer = new Term.Writer(new PrintStream(sink))
    val cases = Vector(
      (Vector("a", "b", "c"), (100, 30)),
      (Vector("a"), (100, 30)),
      (Vector("a", "b"), (100, 30)),
      (Vector("a", "b"), (120, 30)),
      (Vector("a", "b"), (120, 40)),
      (Vector.empty[String], (120, 40)),
      (Vector("back"), (120, 40))
    )
    for (lines, size) <- cases do
      sink.reset()
      writer.write(lines, size)
      assertEquals(sink.text, full(lines))
  }

  test("separate writer instances do not inherit another terminal's state") {
    val lines = Vector("same")
    for _ <- 1 to 2 do
      val sink = new Sink
      new Term.Writer(new PrintStream(sink)).write(lines, (100, 30))
      assertEquals(sink.text, full(lines))
  }

  test(
    "a partial write or failed flush never becomes the successful screen and recovery repaints"
  ) {
    for onFlush <- Vector(false, true) do
      val sink = new Sink
      val output = new RecoverableOutput(sink)
      val writer = new Term.Writer(output)
      val previous = Vector("XMR", "BUY SELL")
      writer.write(previous, (100, 30))
      sink.failFlush = onFlush
      sink.failWrite = !onFlush
      intercept[IOException] {
        writer.write(Vector("XMR", "pending"), (100, 30))
      }
      sink.failFlush = false
      sink.failWrite = false
      output.recover()
      sink.reset()
      writer.write(previous, (100, 30))
      assertEquals(sink.text, full(previous))
      sink.reset()
      writer.write(previous, (100, 30))
      assertEquals(sink.text, "")
  }

  test("restoration always ends synchronized painting before restoring the normal terminal") {
    assert(Term.leaveCodes.startsWith(end), Term.leaveCodes)
    assert(Term.leaveCodes.endsWith(esc + "[?1049l"))
  }
