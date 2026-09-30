package desk

import java.nio.file.Files

class OrderResultLogSpec extends TempDirectorySuite:
  private def writingLock: java.util.concurrent.locks.ReentrantLock =
    val field = OrderResultLog.getClass.getDeclaredFields.find(_.getName == "writing").get
    field.setAccessible(true)
    field.get(OrderResultLog).asInstanceOf[java.util.concurrent.locks.ReentrantLock]

  private def holdWritingLock(): (Thread, java.util.concurrent.CountDownLatch) =
    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val thread = new Thread(() => {
      writingLock.lock()
      try
        entered.countDown()
        release.await()
      finally writingLock.unlock()
    })
    thread.start()
    assert(entered.await(5, java.util.concurrent.TimeUnit.SECONDS))
    (thread, release)

  private def appendExpired(directory: java.nio.file.Path): Unit =
    val method = OrderResultLog.getClass.getDeclaredMethods.find(_.getName == "appendLocked").get
    method.setAccessible(true)
    try
      method.invoke(
        OrderResultLog,
        directory,
        Long.box(2L),
        "second",
        Long.box(System.nanoTime() - 1000000000L),
        () => ()
      )
    catch case error: java.lang.reflect.InvocationTargetException => throw error.getCause

  test("an expired append budget still attempts an uncontended lock") {
    val directory = tempDirectory("desk-order-expired")
    appendExpired(directory)
    val lines = Files.readAllLines(directory.resolve("order-results.jsonl"))
    assertEquals(lines.size(), 1)
    assertEquals(ujson.read(lines.get(0))("text").str, "second")
  }

  test("an interrupted trim leaves the old log readable and the temporary file private") {
    val directory = tempDirectory("desk-order-interrupted")
    OrderResultLog.append(directory, 1L, "first")
    val path = directory.resolve("order-results.jsonl")
    val line = Files.readString(path)
    Files.writeString(path, line * 1001)
    val original = Files.readString(path)
    val child = new ProcessBuilder(
      java.nio.file.Path.of(sys.props("java.home"), "bin", "java").toString,
      "-cp",
      sys.props("java.class.path"),
      "desk.OrderResultTrimCrash",
      directory.toString
    ).inheritIO().start()
    try
      assert(child.waitFor(10, java.util.concurrent.TimeUnit.SECONDS))
      assertEquals(child.exitValue(), 23)
    finally
      if child.isAlive then
        child.destroyForcibly()
        child.waitFor()
    val paths = Files.list(directory)
    val temporary = try
      import scala.jdk.CollectionConverters.*
      paths.iterator().asScala.find(_.getFileName.toString.startsWith(".desk-")).get
    finally paths.close()
    assertEquals(Files.readAllLines(temporary).size(), 1000)
    assertEquals(
      Files.getPosixFilePermissions(temporary),
      java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    )
    assertEquals(Files.readString(path), original)
    import scala.jdk.CollectionConverters.*
    assert(Files.readAllLines(path).asScala.forall(line => ujson.read(line)("text").str == "first"))
  }

  test("the background writer returns while a write is blocked and drains in order") {
    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val records = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val writer = new OrderResultLog.Writer(
      text => {
        entered.countDown()
        release.await()
        records.add(text)
        ()
      },
      _ => ()
    )
    try
      writer.append("first")
      assert(entered.await(5, java.util.concurrent.TimeUnit.SECONDS))
      writer.append("second")
      assertEquals(records.size(), 0)
    finally
      release.countDown()
      writer.close()
    assertEquals(records.toArray.toVector, Vector("first", "second"))
  }

  test("appending after writer close is a quiet no-op that records a diagnostic") {
    val records = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val failures = new java.util.concurrent.CopyOnWriteArrayList[Throwable]()
    val writer = new OrderResultLog.Writer(
      text => { records.add(text); () },
      error => { failures.add(error); () }
    )
    writer.append("first")
    writer.close()
    writer.append("late")
    assertEquals(records.toArray.toVector, Vector("first"))
    assertEquals(failures.size(), 1)
    assert(failures.get(0).isInstanceOf[java.util.concurrent.RejectedExecutionException])
  }

  test("retention repairs a truncated UTF-8 character before subsequent appends") {
    val directory = tempDirectory("desk-order-torn-utf8")
    OrderResultLog.append(directory, 1L, "first")
    val path = directory.resolve("order-results.jsonl")
    val good = (Files.readString(path) * 1000).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    Files.write(path, good ++ Array(0xe2.toByte, 0x82.toByte))
    OrderResultLog.append(directory, 2L, "second")
    OrderResultLog.append(directory, 3L, "third")
    import scala.jdk.CollectionConverters.*
    val records = Files.readAllLines(path).asScala.map(ujson.read(_)).toVector
    assertEquals(records.size, 1000)
    assertEquals(records.takeRight(2).map(_("text").str), Vector("second", "third"))
  }

  test("retention excludes torn lines and keeps repaired appends readable") {
    val directory = tempDirectory("desk-order-torn-trim")
    OrderResultLog.append(directory, 1L, "first")
    val path = directory.resolve("order-results.jsonl")
    Files.writeString(path, Files.readString(path) * 1000 + "{\"id\":\"torn")
    OrderResultLog.append(directory, 2L, "second")
    import scala.jdk.CollectionConverters.*
    val records = Files.readAllLines(path).asScala.map(ujson.read(_)).toVector
    assertEquals(records.size, 1000)
    assertEquals(records.last("text").str, "second")
  }

  test("appending 1100 results keeps the newest 1000 in order and private") {
    val directory = tempDirectory("desk-order-retention")
    for i <- 1 to 1100 do OrderResultLog.append(directory, i.toLong, s"result $i")
    import scala.jdk.CollectionConverters.*
    val path = directory.resolve("order-results.jsonl")
    val records = Files.readAllLines(path).asScala.map(ujson.read(_)).toVector
    assertEquals(records.map(_("at").str.toInt), (101 to 1100).toVector)
    assertEquals(records.map(_("text").str), (101 to 1100).map(i => s"result $i").toVector)
    assertEquals(
      Files.getPosixFilePermissions(path),
      java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    )
  }

  test("two controllers append all 600 order results without keeping general messages") {
    val directory = tempDirectory("desk-order-results")
    val failures = new java.util.concurrent.CopyOnWriteArrayList[Throwable]()
    val resultWriters = Vector.fill(2)(
      new OrderResultLog.Writer(
        text => OrderResultLog.append(directory, 123L, text),
        error => { failures.add(error); () }
      )
    )
    val first = new DeskController(
      PanelView(),
      () => 123L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = (_, text) => resultWriters.head.append(text)
    )
    val second = new DeskController(
      PanelView(),
      () => 123L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = (_, text) => resultWriters(1).append(text)
    )
    val (holder, release) = holdWritingLock()
    try
      val producers = Vector(
        new Thread(() =>
          for i <- 1 to 300 do
            first.addMessage("analysis", "ignored")
            first.addMessage("order", s"Order pending $i")
        ),
        new Thread(() =>
          for i <- 1 to 300 do
            second.addMessage("analysis", "ignored")
            second.addMessage("order", s"Order pending $i")
        )
      )
      producers.foreach(_.start())
      producers.foreach(_.join())
      Thread.sleep(150L)
    finally
      release.countDown()
      holder.join()
      resultWriters.foreach(_.close())
    val lines = Files.readAllLines(directory.resolve("order-results.jsonl"))
    import scala.jdk.CollectionConverters.*
    val records = lines.asScala.map(ujson.read(_)).toVector
    assertEquals(records.size, 600)
    assertEquals(records.map(_("id").str).distinct.size, 600)
    assert(records.forall(_("kind").str == "order"))
    assertEquals(first.panel.actionNumber, 300L)
    assertEquals(second.panel.actionNumber, 300L)
    assertEquals(failures.size(), 0)
    assert(!Files.exists(directory.resolve("messages")))
  }

  test("a result held past the retry deadline reaches the failure callback once") {
    val directory = tempDirectory("desk-order-timeout")
    val failures = new java.util.concurrent.CopyOnWriteArrayList[Throwable]()
    val writer = new OrderResultLog.Writer(
      text => OrderResultLog.append(directory, 123L, text),
      error => { failures.add(error); () }
    )
    val (holder, release) = holdWritingLock()
    try
      writer.append("not saved")
      Thread.sleep(5200L)
    finally
      release.countDown()
      holder.join()
      writer.close()
    assert(!Files.exists(directory.resolve("order-results.jsonl")))
    assertEquals(failures.size(), 1)
    assert(failures.get(0).isInstanceOf[java.io.IOException])
    assertEquals(failures.get(0).getMessage, "order result lock timed out")
  }

  test("a held lock retries until the configured window") {
    val directory = tempDirectory("desk-order-short-window")
    val failures = new java.util.concurrent.CopyOnWriteArrayList[Throwable]()
    val failed = new java.util.concurrent.CountDownLatch(1)
    val window = java.util.concurrent.TimeUnit.MILLISECONDS.toNanos(250L)
    val writer = new OrderResultLog.Writer(
      text => OrderResultLog.append(directory, 123L, text),
      error => { failures.add(error); failed.countDown(); () },
      retryWindowNanos = window
    )
    val (holder, release) = holdWritingLock()
    val began = System.nanoTime()
    try
      writer.append("not saved")
      assert(failed.await(2, java.util.concurrent.TimeUnit.SECONDS))
    finally
      release.countDown()
      holder.join()
      writer.close()
    val elapsed = System.nanoTime() - began
    assert(elapsed >= window, s"failed after only $elapsed ns")
    assert(elapsed <= window + java.util.concurrent.TimeUnit.MILLISECONDS.toNanos(150L))
    assertEquals(failures.size(), 1)
  }

  test("closing stops retries for queued records") {
    val directory = tempDirectory("desk-order-close-retries")
    val firstAttempt = new java.util.concurrent.CountDownLatch(1)
    val failures = new java.util.concurrent.CopyOnWriteArrayList[Throwable]()
    val writer = new OrderResultLog.Writer(
      text => {
        firstAttempt.countDown()
        OrderResultLog.append(directory, 123L, text)
      },
      error => { failures.add(error); () },
      retryWindowNanos = java.util.concurrent.TimeUnit.MILLISECONDS.toNanos(400L)
    )
    val (holder, release) = holdWritingLock()
    try
      (1 to 4).foreach(i => writer.append(s"result $i"))
      assert(firstAttempt.await(2, java.util.concurrent.TimeUnit.SECONDS))
      val began = System.nanoTime()
      writer.close()
      val elapsed = java.util.concurrent.TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began)
      assert(elapsed < 800L, s"close waited $elapsed ms for four queued records")
      assertEquals(failures.size(), 4)
    finally
      release.countDown()
      holder.join()
  }

  test("an append starts a new line after a torn record") {
    val directory = tempDirectory("desk-order-torn")
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
    val directory = tempDirectory("desk-order-locked")
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
        assertEquals(error.getClass.getSimpleName, "LockTimeout")
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
      assert(failures.forall(_("class").str == "LockTimeout"))
      assertEquals(Files.readAllLines(directory.resolve("order-results.jsonl")).size(), 1)
    finally
      diagnostics.close()
      if !child.waitFor(3, java.util.concurrent.TimeUnit.SECONDS) then
        child.destroyForcibly()
        child.waitFor()
  }

object OrderResultTrimCrash:
  def main(args: Array[String]): Unit =
    OrderResultLog.trim(
      java.nio.file.Path.of(args(0)).resolve("order-results.jsonl"),
      () => Runtime.getRuntime.halt(23)
    )
