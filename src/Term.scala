package desk

import scala.util.Try

object Term:
  private val esc = Screen.esc
  val beginPaint: String = esc + "[?2026h"
  val endPaint: String = esc + "[?2026l"
  val enterCodes: String = esc + "[?1049h" + esc + "[?25l" + esc + "[?2004h"
  val leaveCodes: String = endPaint + esc + "[?2004l" + esc + "[?25h" + esc + "[?1049l"

  private def stty(args: String): String =
    val process = new ProcessBuilder((Vector("/bin/stty") ++ args.split("\\s+"))*)
      .redirectInput(new java.io.File("/dev/tty"))
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    try
      if !process.waitFor(1000L, java.util.concurrent.TimeUnit.MILLISECONDS) then
        throw new java.io.IOException("terminal settings timed out")
      if process.exitValue() != 0 then
        throw new java.io.IOException("terminal settings unavailable")
      val output = process.getInputStream.readNBytes(4097)
      if output.length > 4096 then throw new java.io.IOException("terminal settings too large")
      new String(output, java.nio.charset.StandardCharsets.UTF_8).trim
    finally
      if process.isAlive then
        process.destroyForcibly()
        val _ = process.waitFor(100L, java.util.concurrent.TimeUnit.MILLISECONDS)
      process.getInputStream.close()

  val windows: Boolean = System.getProperty("os.name", "").startsWith("Windows")

  def enterRaw(): String =
    val saved =
      if windows then WindowsConsole.enterRaw()
      else
        val saved = Try(stty("-g")).getOrElse("")
        val _ = stty("raw -echo")
        saved
    print(enterCodes)
    Console.flush()
    saved

  def restore(saved: String): Unit =
    print(leaveCodes)
    Console.flush()
    val _ =
      if windows then Try(WindowsConsole.restore(saved))
      else if saved.nonEmpty then Try(stty(saved))
      else Try(stty("sane"))

  // The key stream. On Windows the JDK counts console bytes as available only up to the last
  // Enter, so a reader thread moves each byte into a queue that answers available instead.
  lazy val input: java.io.InputStream = if windows then new Pumped(System.in) else System.in

  final class Pumped(source: java.io.InputStream) extends java.io.InputStream:
    private val queue = new java.util.concurrent.LinkedBlockingQueue[Integer]
    private val reader = Threads.daemon("console input") {
      var byte = 0
      while byte >= 0 do
        byte = Try(source.read()).getOrElse(-1)
        queue.put(byte)
    }
    reader.start()
    override def available(): Int = queue.size
    def read(): Int =
      val byte: Int = queue.take()
      if byte < 0 then queue.put(byte)
      byte

  def parseSize(text: String): Option[(Int, Int)] =
    text.trim.split("\\s+") match
      case Array(rows, cols) => Try((cols.toInt, rows.toInt)).toOption
      case _                 => None

  def size(): (Int, Int) =
    if windows then Try(WindowsConsole.size()).getOrElse((0, 0))
    else Try(stty("size")).toOption.flatMap(parseSize).getOrElse((0, 0))

  def frame(lines: Vector[String]): String =
    esc + "[H" + lines.map(_ + esc + "[K").mkString("\r\n") + esc + "[J"

  private def checkedWrite(payload: Array[Byte], output: java.io.PrintStream): Unit =
    output.write(payload)
    output.flush()
    if output.checkError() then throw new java.io.IOException("terminal write failed")

  final class Writer(output: java.io.PrintStream = System.out):
    private var previous: Option[((Int, Int), Vector[String])] = None
    // Forgets the last frame, so the next write repaints everything.
    def reset(): Unit = previous = None

    def write(lines: Vector[String], size: (Int, Int)): Unit =
      val full = previous.forall { case (oldSize, oldLines) =>
        oldSize != size || oldLines.size != lines.size
      }
      val changed = if full then Vector.empty[Int]
      else lines.indices.filter(i => previous.exists(_._2(i) != lines(i)))
      if full || changed.nonEmpty then
        def styled(line: String): String = Screen.reset + line + Screen.reset
        val body =
          if full then frame(lines.map(styled))
          else changed.map(i => esc + s"[${i + 1};1H" + styled(lines(i)) + esc + "[K").mkString
        previous = None
        checkedWrite(
          (beginPaint + body + endPaint).getBytes(java.nio.charset.StandardCharsets.UTF_8),
          output
        )
        previous = Some((size, lines))

// The Windows console through kernel32, in place of stty. Raw mode turns off line input, echo and
// Control C handling, turns on escape sequences both ways and sets both code pages to UTF-8.
// The saved text holds the four values it found, so restore can put them back.
object WindowsConsole:
  import java.lang.foreign.{Arena, FunctionDescriptor, Linker, MemorySegment, SymbolLookup}
  import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_INT, JAVA_SHORT}

  private val inputHandle = -10
  private val outputHandle = -11
  private val utf8 = 65001
  private val processedInput = 0x1
  private val lineInput = 0x2
  private val echoInput = 0x4
  private val quickEdit = 0x40
  private val extendedFlags = 0x80
  private val terminalInput = 0x200
  private val processedOutput = 0x1
  private val terminalOutput = 0x4
  private val noAutoReturn = 0x8

  private lazy val kernel = SymbolLookup.libraryLookup("kernel32", Arena.global())
  private def function(
      name: String,
      result: Option[java.lang.foreign.MemoryLayout],
      args: java.lang.foreign.MemoryLayout*
  ) =
    val symbol = kernel.find(name).orElseThrow()
    val descriptor = result.fold(FunctionDescriptor.ofVoid(args*))(FunctionDescriptor.of(_, args*))
    Linker.nativeLinker().downcallHandle(symbol, descriptor)
  private lazy val getStdHandle = function("GetStdHandle", Some(ADDRESS), JAVA_INT)
  private lazy val getMode = function("GetConsoleMode", Some(JAVA_INT), ADDRESS, ADDRESS)
  private lazy val setMode = function("SetConsoleMode", Some(JAVA_INT), ADDRESS, JAVA_INT)
  private lazy val getInputPage = function("GetConsoleCP", Some(JAVA_INT))
  private lazy val getOutputPage = function("GetConsoleOutputCP", Some(JAVA_INT))
  private lazy val setInputPage = function("SetConsoleCP", Some(JAVA_INT), JAVA_INT)
  private lazy val setOutputPage = function("SetConsoleOutputCP", Some(JAVA_INT), JAVA_INT)
  private lazy val bufferInfo =
    function("GetConsoleScreenBufferInfo", Some(JAVA_INT), ADDRESS, ADDRESS)

  private def call(handle: java.lang.invoke.MethodHandle, args: Any*): Int =
    handle.invokeWithArguments(args.map(_.asInstanceOf[AnyRef])*).asInstanceOf[Integer]

  private def checked(result: Int, what: String): Unit =
    if result == 0 then throw new java.io.IOException("console " + what + " failed")

  private def handle(which: Int): MemorySegment =
    getStdHandle.invokeWithArguments(Integer.valueOf(which)).asInstanceOf[MemorySegment]

  private def mode(which: Int): Int =
    val arena = Arena.ofConfined()
    try
      val cell = arena.allocate(JAVA_INT)
      checked(call(getMode, handle(which), cell), "mode")
      cell.get(JAVA_INT, 0)
    finally arena.close()

  private[desk] def rawInput(mode: Int): Int =
    (mode & ~(processedInput | lineInput | echoInput | quickEdit)) | extendedFlags | terminalInput

  private[desk] def rawOutput(mode: Int): Int =
    mode | processedOutput | terminalOutput | noAutoReturn

  // A step that fails puts back what the earlier steps changed.
  def enterRaw(): String =
    val in = mode(inputHandle)
    val out = mode(outputHandle)
    val saved = Seq(in, out, call(getInputPage), call(getOutputPage)).mkString(",")
    try
      checked(call(setMode, handle(inputHandle), rawInput(in)), "input mode")
      checked(call(setMode, handle(outputHandle), rawOutput(out)), "output mode")
      checked(call(setInputPage, utf8), "input code page")
      checked(call(setOutputPage, utf8), "output code page")
    catch
      case error: Throwable =>
        Try(restore(saved))
        throw error
    saved

  def restore(saved: String): Unit =
    saved.split(",").toSeq.map(_.toInt) match
      case Seq(in, out, inPage, outPage) =>
        val _ = call(setMode, handle(inputHandle), in)
        val _ = call(setMode, handle(outputHandle), out)
        val _ = call(setInputPage, inPage)
        val _ = call(setOutputPage, outPage)
      case _ => ()

  // The visible window of the screen buffer, as columns and rows.
  def size(): (Int, Int) =
    val arena = Arena.ofConfined()
    try
      val info = arena.allocate(22L, 2L)
      checked(call(bufferInfo, handle(outputHandle), info), "size")
      def at(offset: Long): Int = info.get(JAVA_SHORT, offset).toInt
      (at(14) - at(10) + 1, at(16) - at(12) + 1)
    finally arena.close()
