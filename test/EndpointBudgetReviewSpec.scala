package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

object EndpointBudgetProbe:
  def main(args: Array[String]): Unit =
    val root = Path.of(args(0))
    val endpoint = new EndpointRest("]")
    val before = System.currentTimeMillis()
    require(endpoint.post("""{"type":"meta"}""").isEmpty)
    val saved = root.resolve(".local/state/desk/public-request-budget/schedule.txt")
    require(Files.exists(saved), "public request did not reserve its allowance")
    val deadline = Files.readString(saved).toLong
    require(deadline >= before + 1715L, "public request reserved too little allowance")
    require(System.currentTimeMillis() >= deadline, "public request ignored its reserved wait")

class EndpointBudgetReviewSpec extends TempDirectorySuite:
  test("the real public adapter reserves and waits before constructing an HTTP request") {
    val root = tempDirectory("desk-review-endpoint-budget", keepOnFailure = true)
    val child = new ProcessBuilder(
      Path.of(System.getProperty("java.home"), "bin/java").toString,
      "-Duser.home=" + root.toString,
      "-cp",
      System.getProperty("java.class.path"),
      "desk.EndpointBudgetProbe",
      root.toString
    ).redirectErrorStream(true).redirectOutput(root.resolve("probe.log").toFile).start()
    try
      val completed = child.waitFor(20L, TimeUnit.SECONDS)
      assert(completed, root.toString)
      assertEquals(child.exitValue(), 0, Files.readString(root.resolve("probe.log")))
    finally
      if child.isAlive then
        child.destroyForcibly()
        child.waitFor(5L, TimeUnit.SECONDS)
  }
