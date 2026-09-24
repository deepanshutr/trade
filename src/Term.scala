package desk

import scala.util.Try

object Term:
  private val esc = Screen.esc
  val enterCodes: String = esc + "[?1049h" + esc + "[?25l" + esc + "[?2004h"
  val leaveCodes: String = esc + "[?2004l" + esc + "[?25h" + esc + "[?1049l"

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

  def enterRaw(): String =
    val saved = Try(stty("-g")).getOrElse("")
    val _ = stty("raw -echo")
    print(enterCodes)
    Console.flush()
    saved

  def restore(saved: String): Unit =
    print(leaveCodes)
    Console.flush()
    val _ = if saved.nonEmpty then Try(stty(saved)) else Try(stty("sane"))

  def parseSize(text: String): Option[(Int, Int)] =
    text.trim.split("\\s+") match
      case Array(rows, cols) => Try((cols.toInt, rows.toInt)).toOption
      case _                 => None

  def size(): (Int, Int) =
    Try(stty("size")).toOption.flatMap(parseSize).getOrElse((0, 0))

  def frame(lines: Vector[String]): String =
    esc + "[H" + lines.map(_ + esc + "[K").mkString("\r\n") + esc + "[J"

  def bytes(lines: Vector[String]): Array[Byte] =
    frame(lines).getBytes(java.nio.charset.StandardCharsets.UTF_8)

  def write(lines: Vector[String], output: java.io.PrintStream = System.out): Unit =
    output.write(bytes(lines))
    output.flush()
    if output.checkError() then throw new java.io.IOException("terminal write failed")
