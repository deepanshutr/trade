package desk

import java.nio.file.Files

class OrderResultLogSpec extends munit.FunSuite:
  private def appendExpired(directory: java.nio.file.Path): Unit =
    val method = OrderResultLog.getClass.getDeclaredMethods.find(_.getName == "appendLocked").get
    method.setAccessible(true)
    try
      method.invoke(
        OrderResultLog,
        directory,
        Long.box(2L),
        "second",
        Long.box(System.nanoTime() - 1000000000L)
      )
    catch case error: java.lang.reflect.InvocationTargetException => throw error.getCause

  test("an expired append budget still attempts an uncontended lock") {
    val directory = Files.createTempDirectory("desk-order-expired")
    appendExpired(directory)
    val lines = Files.readAllLines(directory.resolve("order-results.jsonl"))
    assertEquals(lines.size(), 1)
    assertEquals(ujson.read(lines.get(0))("text").str, "second")
  }

  test("two controllers append all 600 order results without keeping general messages") {
    val directory = Files.createTempDirectory("desk-order-results")
    def controller = new DeskController(
      PanelView(),
      () => 123L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = text => OrderResultLog.append(directory, 123L, text)
    )
    val first = controller
    val second = controller
    try
      val writers = Vector(first, second).map { c =>
        new Thread(() => {
          for i <- 1 to 300 do
            c.addMessage("analysis", "ignored")
            c.addMessage("order", s"Order pending $i")
        })
      }
      writers.foreach(_.start())
      writers.foreach(_.join())
      val lines = Files.readAllLines(directory.resolve("order-results.jsonl"))
      import scala.jdk.CollectionConverters.*
      val records = lines.asScala.map(ujson.read(_)).toVector
      assertEquals(records.size, 600)
      assertEquals(records.map(_("id").str).distinct.size, 600)
      assert(records.forall(_("kind").str == "order"))
      assertEquals(first.panel.actionNumber, 300L)
      assertEquals(second.panel.actionNumber, 300L)
      assert(!Files.exists(directory.resolve("messages")))
    finally
      val paths = Files.walk(directory)
      try paths.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
      finally paths.close()
  }

  test("an append starts a new line after a torn record") {
    val directory = Files.createTempDirectory("desk-order-torn")
    OrderResultLog.append(directory, 1L, "first")
    val path = directory.resolve("order-results.jsonl")
    Files.writeString(path, "{\"id\":\"torn", java.nio.file.StandardOpenOption.APPEND)
    OrderResultLog.append(directory, 2L, "second")
    import scala.jdk.CollectionConverters.*
    val lines = Files.readAllLines(path).asScala.toVector
    assertEquals(lines.size, 3)
    assertEquals(ujson.read(lines.last)("text").str, "second")
    assertEquals(lines(1), "{\"id\":\"torn")
  }

  test("a lock held by another process times out promptly") {
    val directory = Files.createTempDirectory("desk-order-locked")
    OrderResultLog.append(directory, 1L, "first")
    val child = new ProcessBuilder(
      "python3",
      "-c",
      "import fcntl, sys, time; f = open(sys.argv[1], 'r+'); fcntl.lockf(f, fcntl.LOCK_EX); print('locked', flush=True); time.sleep(1)",
      directory.resolve("order-results.lock").toString
    ).start()
    val diagnostics = Diagnostics.open(directory, () => 123L, 2000L)
    diagnostics.start()
    try
      assertEquals(child.inputReader().readLine(), "locked")
      for expired <- Vector(false, true) do
        val began = System.nanoTime()
        val result = scala.util.Try {
          if expired then appendExpired(directory)
          else OrderResultLog.append(directory, 2L, "blocked")
        }
        val elapsed = java.util.concurrent.TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began)
        assert(elapsed < 250, s"lock blocked the caller for $elapsed ms")
        val error = result.failed.get
        assert(error.isInstanceOf[java.io.IOException])
        assertEquals(error.getMessage, "order result lock timed out")
        diagnostics.failure(DiagnosticPart.OrderResults, error)
      diagnostics.close()
      import scala.jdk.CollectionConverters.*
      val failures = Files
        .readAllLines(directory.resolve("diagnostics/events-0.jsonl"))
        .asScala
        .map(ujson.read(_))
        .filter(_("event").str == "exception")
      assertEquals(failures.size, 2)
      assert(failures.forall(_("part").str == "OrderResults"))
      assert(failures.forall(_("class").str == "IOException"))
      assertEquals(Files.readAllLines(directory.resolve("order-results.jsonl")).size(), 1)
    finally
      diagnostics.close()
      if !child.waitFor(3, java.util.concurrent.TimeUnit.SECONDS) then
        child.destroyForcibly()
        child.waitFor()
  }
