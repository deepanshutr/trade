package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

object PublicBudgetProcess:
  def main(args: Array[String]): Unit =
    val root = Path.of(args(0))
    Files.writeString(root.resolve("ready-" + args(1)), "ready")
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60)
    while !Files.exists(root.resolve("go")) && System.nanoTime() < deadline do Thread.sleep(10)
    require(Files.exists(root.resolve("go")))
    val budget = new PublicRequestBudget(root.resolve("budget"), () => 1800000000000L)
    for _ <- 0 until 20 do budget.reserve(1)

class PublicBudgetProcessSpec extends TempDirectorySuite:
  override val munitTimeout = 90.seconds
  test("separate recorder and terminal processes cannot overwrite each other's public allowance") {
    val root = tempDirectory("desk-two-request-processes", keepOnFailure = true)
    val children = Vector("recorder", "terminal").map { name =>
      new ProcessBuilder(
        Path.of(System.getProperty("java.home"), "bin/java").toString,
        "-cp",
        System.getProperty("java.class.path"),
        "desk.PublicBudgetProcess",
        root.toString,
        name
      )
        .redirectErrorStream(true)
        .redirectOutput(root.resolve(name + ".log").toFile)
        .start()
    }
    try
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60)
      def ready: Boolean =
        Files.exists(root.resolve("ready-recorder")) && Files.exists(root.resolve("ready-terminal"))
      while !ready && System.nanoTime() < deadline && children.forall(_.isAlive) do Thread.sleep(10)
      assert(ready, root.toString)
      Files.writeString(root.resolve("go"), "go")
      children.zipWithIndex.foreach { (child, index) =>
        val completed = child.waitFor(60, TimeUnit.SECONDS)
        assert(completed, root.toString)
        assertEquals(child.exitValue(), 0, s"child $index: $root")
      }
      assertEquals(
        Files.readString(root.resolve("budget/schedule.txt")).toLong,
        1800000000000L + 40L * 86L
      )
    finally
      children.foreach { child =>
        if child.isAlive then
          child.destroyForcibly()
          child.waitFor(5, TimeUnit.SECONDS)
      }
  }
