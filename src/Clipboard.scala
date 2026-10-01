package desk

import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.util.Try

final case class ProcessRun(exit: Int, stdout: Array[Byte])

object ProcessRun:
  // The real runner. It writes the standard input, closes it, waits at most 30
  // seconds and keeps at most 4096 bytes of output. Errors are discarded. The
  // callers decide what the output means and never log it.
  def system(args: Seq[String], stdin: Array[Byte]): ProcessRun =
    val process = new ProcessBuilder(args.asJava).redirectError(Redirect.DISCARD).start()
    try
      val input = process.getOutputStream
      try input.write(stdin)
      finally input.close()
      require(process.waitFor(30, TimeUnit.SECONDS), "process did not finish")
      ProcessRun(process.exitValue(), process.getInputStream.readNBytes(4096))
    finally
      if process.isAlive then
        process.destroyForcibly()
        val _ = process.waitFor(1, TimeUnit.SECONDS)
      process.getInputStream.close()

// Writes and clears the general clipboard. The seed text goes in on standard
// input only. The scripts are fixed text, so no argument holds a secret.
object Clipboard:
  private val program = "/usr/bin/osascript"

  private val copyScript = Vector(
    "ObjC.import('AppKit');",
    "function run() {",
    "  var data = $.NSFileHandle.fileHandleWithStandardInput.readDataToEndOfFile;",
    "  var text = $.NSString.alloc.initWithDataEncoding(data, $.NSUTF8StringEncoding);",
    "  var board = $.NSPasteboard.generalPasteboard;",
    "  board.prepareForNewContentsWithOptions(1);",
    "  if (!board.setStringForType(text, 'public.utf8-plain-text')) throw new Error('copy failed');",
    "  board.setDataForType($.NSData.data, 'org.nspasteboard.ConcealedType');",
    "  board.setDataForType($.NSData.data, 'org.nspasteboard.TransientType');",
    "  return '';",
    "}"
  ).mkString("\n")

  private val clearScript = Vector(
    "ObjC.import('AppKit');",
    "function run() { $.NSPasteboard.generalPasteboard.clearContents; return ''; }"
  ).mkString("\n")

  private def command(script: String): Seq[String] =
    Seq(program, "-l", "JavaScript", "-e", script)

  private def succeeded(
      script: String,
      stdin: Array[Byte],
      run: (Seq[String], Array[Byte]) => ProcessRun,
      failure: String
  ): Either[String, Unit] =
    Try(run(command(script), stdin)).toOption.filter(_.exit == 0) match
      case Some(_) => Right(())
      case None    => Left(failure)

  def copySecret(
      text: Array[Byte],
      run: (Seq[String], Array[Byte]) => ProcessRun
  ): Either[String, Unit] =
    succeeded(copyScript, text, run, "Clipboard copy failed")

  def clear(run: (Seq[String], Array[Byte]) => ProcessRun): Either[String, Unit] =
    succeeded(clearScript, Array.emptyByteArray, run, "Clipboard clear failed")
