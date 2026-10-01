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
    "  board.setDataForType($.NSData.data, 'org.nspasteboard.ConcealedType');",
    "  board.setDataForType($.NSData.data, 'org.nspasteboard.TransientType');",
    "  if (!board.setStringForType(text, 'public.utf8-plain-text')) throw new Error('copy failed');",
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

  test("the copy script sets both markers before the text and still checks the text write") {
    val runner = new Runner()
    val _ = Clipboard.copySecret(secretBytes.clone(), runner(_, _))
    val lines = runner.calls.head._1.last.split("\n").toVector
    def at(part: String): Int =
      val found = lines.zipWithIndex.collect { case (line, index) if line.contains(part) => index }
      assertEquals(found.size, 1, s"expected one line with $part")
      found.head
    val prepare = at("prepareForNewContentsWithOptions(1)")
    val concealed = at("org.nspasteboard.ConcealedType")
    val transient = at("org.nspasteboard.TransientType")
    val text = at("public.utf8-plain-text")
    assert(prepare < concealed && prepare < transient, "the markers must follow the clear")
    assert(concealed < text && transient < text, "the markers must come before the text")
    assert(lines(text).contains("throw new Error('copy failed')"), "the text write is checked")
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

  // The real runner, run against harmless system tools only.
  test("the real runner hands standard input to the child and returns its output") {
    val input = "0ABCD-1EFGH\n".getBytes(UTF_8)
    val run = ProcessRun.system(Seq("/bin/cat"), input)
    assertEquals(run.exit, 0)
    assertEquals(run.stdout.toSeq, input.toSeq)
  }

  test("the real runner keeps at most 4096 bytes of output") {
    val input = Array.fill[Byte](5000)('x'.toByte)
    val run = ProcessRun.system(Seq("/bin/cat"), input)
    assertEquals(run.exit, 0)
    assertEquals(run.stdout.length, 4096)
  }

  test("the real runner reports a nonzero exit") {
    val run = ProcessRun.system(Seq("/usr/bin/false"), Array.emptyByteArray)
    assertEquals(run.exit, 1)
    assertEquals(run.stdout.length, 0)
  }

  test("the real runner stops a child that outlives the limit and fails") {
    val marker = "29.001"
    def child = ProcessHandle
      .allProcesses()
      .filter(h => h.info.arguments.map(_.toSeq).orElse(Array.empty[String].toSeq).contains(marker))
      .findFirst()
    val outcome = new java.util.concurrent.atomic.AtomicReference[scala.util.Try[ProcessRun]]
    val caller = new Thread(() =>
      outcome.set(
        scala.util.Try(ProcessRun.systemWithin(Seq("/bin/sleep", marker), Array.emptyByteArray, 2L))
      )
    )
    caller.setDaemon(true)
    caller.start()
    try
      Wait.until("the child never started")(child.isPresent)
      val started = child.get
      caller.join(15000L)
      assert(!caller.isAlive, "the runner waited past its limit")
      assert(outcome.get != null && outcome.get.isFailure, "a child past the limit must fail")
      assert(
        outcome.get.failed.get.getMessage.contains("process did not finish"),
        "the failure must come from the limit"
      )
      Wait.until("the child outlived the runner")(!started.isAlive)
    finally child.ifPresent(h => { val _ = h.destroyForcibly() })
  }
