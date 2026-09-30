package desk

class WorkspaceOrderResultSpec extends TempDirectorySuite:
  test("order result writer failure reaches diagnostics and the controller notice") {
    val records = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val reported = new java.util.concurrent.CountDownLatch(1)
    val diagnostics = new Diagnostics(
      () =>
        new Diagnostics.Sink:
          def append(line: String): Unit = { records.add(line); () }
          def close(): Unit = ()
      ,
      () => 123L,
      2000L
    )
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => 123L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused")
    )
    val writer = new OrderResultLog.Writer(
      _ => throw new java.io.IOException("private detail"),
      _ => ()
    )
    val handle = WorkspaceApp.orderResultHandler(
      writer,
      diagnostics,
      number => { controller.orderResultNotSaved(number); reported.countDown() }
    )
    diagnostics.start()
    try
      handle.handle(1, "order result")
      assert(reported.await(2, java.util.concurrent.TimeUnit.SECONDS))
    finally
      writer.close()
      diagnostics.close()
    assertEquals(controller.panel.actionNotice, Some("Order result #1 not saved"))
    assert(handle.unavailable())
    val failures = records.toArray.toVector
      .map(_.toString)
      .map(ujson.read(_))
      .filter(
        _("event").str == "exception"
      )
    assertEquals(failures.size, 1)
    assertEquals(failures.head("part").str, "OrderResults")
    assertEquals(failures.head("class").str, "IOException")
  }

  test("a post-force failure is diagnostic only") {
    val directory = tempDirectory("desk-order-post-force")
    val records = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val diagnostics = new Diagnostics(
      () =>
        new Diagnostics.Sink:
          def append(line: String): Unit = { records.add(line); () }
          def close(): Unit = ()
      ,
      () => 123L,
      2000L
    )
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => 123L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused")
    )
    val reported = new java.util.concurrent.CountDownLatch(1)
    val marked = new java.util.concurrent.atomic.AtomicBoolean(false)
    val handler = WorkspaceApp.orderResultFailureHandler(
      diagnostics,
      controller.orderResultNotSaved,
      () => marked.set(true)
    )
    val writer = new OrderResultLog.Writer(
      text =>
        OrderResultLog.appendAfterForce(
          directory,
          123L,
          text,
          afterForce = () => throw new java.io.IOException("trim failed")
        ),
      error => { handler(0, error); reported.countDown(); () }
    )
    diagnostics.start()
    try
      writer.append("saved")
      assert(reported.await(2, java.util.concurrent.TimeUnit.SECONDS))
    finally
      writer.close()
      diagnostics.close()
    assertEquals(controller.panel.actionNotice, None)
    assert(!marked.get())
    assertEquals(
      java.nio.file.Files.readAllLines(directory.resolve("order-results.jsonl")).size(),
      1
    )
    val failures = records.toArray.toVector
      .map(_.toString)
      .map(ujson.read(_))
      .filter(
        _("event").str == "exception"
      )
    assertEquals(failures.size, 1)
    assertEquals(failures.head("part").str, "OrderResults")
  }
