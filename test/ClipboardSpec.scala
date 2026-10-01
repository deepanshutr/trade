package desk

import java.nio.charset.StandardCharsets.UTF_8

class ClipboardSpec extends munit.FunSuite:
  private val program = "/usr/bin/osascript"
  private val secret = "0ABCD-1EFGH-2JKMN-3PQRS-4TVWX"
  private val secretBytes = secret.getBytes(UTF_8)

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

  private final class Runner(exit: Int = 0, stdout: Array[Byte] = Array.emptyByteArray):
    var calls = Vector.empty[(Seq[String], Array[Byte], Array[Byte])]
    def apply(args: Seq[String], stdin: Array[Byte]): ProcessRun =
      calls = calls :+ ((args, stdin, stdin.clone()))
      ProcessRun(exit, stdout)

  test("copySecret runs the fixed script with the secret on standard input only") {
    val runner = new Runner()
    val text = secretBytes.clone()
    assertEquals(Clipboard.copySecret(text, runner(_, _)), Right(()))
    assertEquals(runner.calls.size, 1)
    val (args, stdin, seen) = runner.calls.head
    assertEquals(args, Seq(program, "-l", "JavaScript", "-e", copyScript))
    assertEquals(seen.toSeq, secretBytes.toSeq)
    assert(stdin eq text, "the secret should be handed over without a second copy")
    args.foreach(arg => assert(!arg.contains(secret), "the secret reached an argument"))
  }

  test("the copy script keeps the secret off the page and marks it concealed and transient") {
    val runner = new Runner()
    val _ = Clipboard.copySecret(secretBytes.clone(), runner(_, _))
    val script = runner.calls.head._1.last
    assert(script.contains("fileHandleWithStandardInput.readDataToEndOfFile"))
    assert(script.contains("prepareForNewContentsWithOptions(1)"))
    assert(script.contains("org.nspasteboard.ConcealedType"))
    assert(script.contains("org.nspasteboard.TransientType"))
  }

  test("copySecret gives a fixed failure for a nonzero exit and never echoes the output") {
    val runner = new Runner(exit = 1, stdout = "leaked-output".getBytes(UTF_8))
    val result = Clipboard.copySecret(secretBytes.clone(), runner(_, _))
    assert(result.isLeft)
    assert(!result.left.getOrElse("").contains("leaked-output"))
    assert(!result.left.getOrElse("").contains(secret))
  }

  test("copySecret gives a failure when the runner throws and never echoes the message") {
    val result = Clipboard.copySecret(
      secretBytes.clone(),
      (_, _) => throw new java.io.IOException("leaked-error " + secret)
    )
    assert(result.isLeft)
    assert(!result.left.getOrElse("").contains("leaked-error"))
    assert(!result.left.getOrElse("").contains(secret))
  }

  test("clear runs the fixed clear script with empty standard input") {
    val runner = new Runner()
    assertEquals(Clipboard.clear(runner(_, _)), Right(()))
    assertEquals(runner.calls.size, 1)
    val (args, _, seen) = runner.calls.head
    assertEquals(args, Seq(program, "-l", "JavaScript", "-e", clearScript))
    assertEquals(seen.length, 0)
  }

  test("clear gives a failure for a nonzero exit or a thrown error") {
    assert(Clipboard.clear(new Runner(exit = 2)(_, _)).isLeft)
    assert(Clipboard.clear((_, _) => throw new java.io.IOException("missing")).isLeft)
  }
