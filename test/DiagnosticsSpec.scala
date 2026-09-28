package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ArrayBuffer

class RefineDiagnosticsSpec extends munit.FunSuite:
  private class Memory extends Diagnostics.Sink:
    private val saved = ArrayBuffer.empty[String]
    def append(line: String): Unit = synchronized { saved += line }
    def lines: Vector[String] = synchronized(saved.toVector)
    def close(): Unit = ()

  test("selected quote recovery excludes reference prices, heartbeat replies and stale quotes") {
    val clock = new AtomicLong(10000L)
    val saved = new Memory
    val log = new Diagnostics(() => saved, clock.get, 2000L, monotonic = clock.get)
    val snapshot = Snapshot.initial("XMR", clock.get())
    def observe(event: Event): Unit =
      log.folded(snapshot, event, State.reduce(snapshot, event, clock.get()))
    log.start()
    try
      observe(Event.BboUpdate("BTC", Level("10", "1", 1), Level("11", "1", 1), clock.get()))
      observe(Event.Pong)
      log.tick()
      Wait.until("DiagnosticsSpec condition")(saved.lines.exists(_.contains("missing")))
      assert(!saved.lines.exists(_.contains("fresh")))
      observe(Event.BboUpdate("XMR", Level("10", "1", 1), Level("11", "1", 1), clock.get()))
      log.tick()
      Wait.until("DiagnosticsSpec condition")(saved.lines.exists(_.contains("fresh")))
      clock.addAndGet(3000L)
      observe(Event.BboUpdate("XMR", Level("10", "1", 1), Level("11", "1", 1), 10000L))
      observe(Event.Pong)
      log.tick()
      Wait.until("DiagnosticsSpec condition")(saved.lines.exists(_.contains("stale")))
      log.presented(snapshot, 100, 30)
      log.tick()
      Wait.until("DiagnosticsSpec condition")(saved.lines.exists(_.contains("recovered")))
    finally log.close()
  }

  test("blocked storage never blocks producers and overflow clears after reporting") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val sink = new Memory:
      override def append(line: String): Unit =
        entered.countDown()
        release.await()
        super.append(line)
    val log = new Diagnostics(() => sink, () => 10000L, 2000L, capacity = 4)
    log.start()
    try
      assert(entered.await(2, TimeUnit.SECONDS))
      val start = System.nanoTime()
      (1 to 5000).foreach(_ => log.slow(DiagnosticPart.Input, 100000000L))
      assert(System.nanoTime() - start < TimeUnit.SECONDS.toNanos(1))
      assert(log.droppedCount > 0)
      assert(log.unavailable)
      release.countDown()
      Wait.until("DiagnosticsSpec condition")(sink.lines.exists(_.contains("dropped")))
      Wait.until("DiagnosticsSpec condition")(log.status.isEmpty)
    finally
      release.countDown()
      log.close()
      log.close()
  }

  test("duplicate timestamps cannot renew freshness and wall clock jumps cannot hide stalls") {
    val wall = new AtomicLong(10000L)
    val elapsed = new AtomicLong(10000L)
    val saved = new Memory
    val log = new Diagnostics(() => saved, wall.get, 2000L, monotonic = elapsed.get)
    val s = Snapshot.initial("XMR", wall.get())
    val quote = Event.BboUpdate("XMR", Level("10", "1", 1), Level("11", "1", 1), 10000L)
    log.start()
    try
      log.folded(s, quote, s)
      log.tick()
      elapsed.set(11900L)
      wall.set(11900L)
      log.folded(s, quote, s)
      elapsed.set(13800L)
      wall.set(1L)
      log.tick()
      Wait.until("DiagnosticsSpec condition")(saved.lines.exists(_.contains("stale")))
      Wait.until("DiagnosticsSpec condition")(saved.lines.exists(_.contains("stalled")))
      val records =
        saved.lines.map(line => ujson.read(line)).filter(_("event").str == "quote_health")
      assertEquals(records.last("age_ms").num.toLong, 3800L)
    finally log.close()
  }

  test(
    "linked files are rejected before opening and before rotation without changing their targets"
  ) {
    val directory = Files.createTempDirectory("desk-diagnostic-links-")
    val target = directory.resolve("target")
    LocalFiles.write(target, "preserved")
    val events = directory.resolve("events-3.jsonl")
    try
      Files.createSymbolicLink(events, target)
      intercept[IllegalArgumentException](new Diagnostics.FilesSink(directory, 256L))
      assertEquals(Files.readString(target), "preserved")
      Files.delete(events)
      val sink = new Diagnostics.FilesSink(directory, 256L)
      try
        sink.append("x" * 200)
        Files.createSymbolicLink(events, target)
        intercept[IllegalArgumentException](sink.append("y" * 100))
        assertEquals(Files.readString(target), "preserved")
      finally sink.close()
    finally
      val files = Files.walk(directory)
      try files.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
      finally files.close()
  }

  test("write exceptions are visible and cannot escape into the application") {
    val log = new Diagnostics(
      () =>
        new Diagnostics.Sink:
          def append(line: String): Unit = throw new java.io.IOException("secret failure body")
          def close(): Unit = ()
      ,
      () => 10000L,
      2000L
    )
    log.start()
    try Wait.until("DiagnosticsSpec condition")(log.unavailable)
    finally log.close()
  }

  test("switch trace excludes raw prompt text, rejected market names and exception messages") {
    val saved = new Memory
    val log = new Diagnostics(() => saved, () => 10000L, 2000L)
    val initial = Snapshot.initial("XMR", 10000L)
    val pending = initial.copy(marketPending = Some(1L -> "PRIVATE_SECRET"))
    log.folded(
      initial.copy(prompt = Some("PRIVATE_SECRET")),
      Event.Key(Command.PromptSubmit),
      pending
    )
    log.folded(pending, Event.MarketChecked((1L, "PRIVATE_SECRET"), Left("SECRET_REASON")), initial)
    log.failure(DiagnosticPart.Feed, new IllegalArgumentException("SECRET_EXCEPTION"))
    log.folded(initial, Event.Bad, initial)
    log.start()
    log.close()
    val text = saved.lines.mkString("\n")
    assert(text.contains("switch_requested"))
    assert(text.contains("rejected"))
    assert(text.contains("IllegalArgumentException"))
    assert(!text.contains("SECRET"))
  }

  test("rotation stays within four private complete files and preserves unrelated files") {
    val directory = Files.createTempDirectory("desk-diagnostics-test-")
    try
      LocalFiles.write(directory.resolve("unrelated"), "preserved")
      val sink = new Diagnostics.FilesSink(directory, 256L)
      try
        (1 to 100).foreach(n =>
          sink.append(ujson.write(ujson.Obj("event" -> "test", "count" -> n)))
        )
      finally sink.close()
      val paths = (0 until 4).map(i => directory.resolve(s"events-$i.jsonl"))
      assert(paths.forall(p => TradingConfig.privateFile(p) && Files.size(p) <= 256L))
      paths.foreach(p =>
        Files.readString(p).linesIterator.foreach(line => assert(ujson.read(line).obj.nonEmpty))
      )
      assertEquals(Files.readString(directory.resolve("unrelated")), "preserved")
    finally
      val files = Files.walk(directory)
      try files.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
      finally files.close()
  }

  test("shutdown is bounded even when a writer ignores interruption") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val exited = new CountDownLatch(1)
    val log = new Diagnostics(
      () =>
        new Diagnostics.Sink:
          def append(line: String): Unit =
            entered.countDown()
            while release.getCount > 0 do
              try release.await()
              catch case _: InterruptedException => ()
          def close(): Unit = exited.countDown()
      ,
      () => 10000L,
      2000L
    )
    log.start()
    try
      assert(entered.await(2, TimeUnit.SECONDS))
      val began = System.nanoTime()
      log.close()
      log.close()
      assert(System.nanoTime() - began < TimeUnit.SECONDS.toNanos(1))
      assert(log.unavailable)
    finally
      release.countDown()
      assert(exited.await(2, TimeUnit.SECONDS))
  }

  test("overflow stays visible until the drop record is written") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val inDrop = new CountDownLatch(1)
    val releaseDrop = new CountDownLatch(1)
    val sink = new Memory:
      override def append(line: String): Unit =
        entered.countDown()
        release.await()
        if line.contains("\"dropped\"") then
          inDrop.countDown()
          releaseDrop.await()
        super.append(line)
    val log = new Diagnostics(() => sink, () => 10000L, 2000L, capacity = 4)
    log.start()
    try
      assert(entered.await(2, TimeUnit.SECONDS))
      (1 to 5000).foreach(_ => log.slow(DiagnosticPart.Input, 100000000L))
      release.countDown()
      assert(inDrop.await(2, TimeUnit.SECONDS))
      assert(log.unavailable)
      releaseDrop.countDown()
      Wait.until("DiagnosticsSpec condition")(log.status.isEmpty)
    finally
      release.countDown()
      releaseDrop.countDown()
      log.close()
  }
