package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class PanInputSpec extends munit.FunSuite:
  override val munitTimeout = 90.seconds
  private val initial = Snapshot.initial("XMR", 0L).copy(workspace = true)
  private def fold(start: Snapshot, queue: LinkedBlockingQueue[Event]): Snapshot =
    queue.iterator().asScala.foldLeft(start)((state, event) => State.reduce(state, event, 0L))

  test("the first pan after resize uses the newly measured plot") {
    val queue = new LinkedBlockingQueue[Event]()
    WorkspaceApp.enqueueInput(Command.ScrubBack, (120, 40), queue, new AtomicInteger(0))
    val after = fold(initial, queue)
    assertEquals(after.cols, 120)
    assertEquals(after.rows, 40)
    assertEquals(after.offsetSec, 16L)
  }

  test("queued resizes retain the size measured for each pan") {
    val queue = new LinkedBlockingQueue[Event]()
    val pending = new AtomicInteger(0)
    for size <- Vector((120, 40), (100, 30)) do
      WorkspaceApp.enqueueInput(Command.ScrubBack, size, queue, pending)
    val after = fold(initial, queue)
    assertEquals(after.cols, 100)
    assertEquals(after.offsetSec, 23L)
  }

  test("the prompt completion guard survives resized input enqueueing") {
    val queue = new LinkedBlockingQueue[Event]()
    val pending = new AtomicInteger(0)
    WorkspaceApp.enqueueInput(Command.PromptSubmit, (120, 40), queue, pending)
    assertEquals(pending.get(), 1)
    val after = fold(initial.copy(prompt = Some("ETH")), queue)
    assertEquals(after.coin, "ETH")
    assertEquals(after.cols, 120)
  }

  test("actual input orders resize before an immediate pan while the fold waits") {
    val directory = Files.createTempDirectory("desk-resize-runtime-")
    val log = directory.resolve("runner.log")
    val process = new ProcessBuilder(
      "python3",
      "test/resize_witness.py",
      "--java",
      Path.of(System.getProperty("java.home"), "bin/java").toString,
      "--classpath",
      System.getProperty("java.class.path"),
      "--output",
      directory.toString
    ).redirectErrorStream(true).redirectOutput(log.toFile).start()
    try
      assert(process.waitFor(75, TimeUnit.SECONDS), s"resize witness watchdog, see $log")
      println(s"resize witness evidence: $directory")
      assertEquals(process.exitValue(), 0, Files.readString(log))
    finally
      if process.isAlive then
        val descendants = process.descendants()
        val owned =
          try descendants.toArray.map(_.asInstanceOf[ProcessHandle]).toVector :+ process.toHandle
          finally descendants.close()
        owned.foreach(_.destroyForcibly())
        val deadline = System.nanoTime() + 5.seconds.toNanos
        while owned.exists(_.isAlive) && System.nanoTime() < deadline do Thread.sleep(10)
        assert(owned.forall(!_.isAlive), s"resize witness cleanup watchdog, see $log")
  }
