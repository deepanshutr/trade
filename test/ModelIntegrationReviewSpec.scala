package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable
import scala.util.Try

class ModelIntegrationReviewSpec extends munit.FunSuite:
  private val threadId = "public-thread"
  private val turnId = "analysis-turn"
  private def item(answer: String, thread: String = threadId, turn: String = turnId): ujson.Value =
    ujson.Obj(
      "method" -> "item/completed",
      "params" -> ujson.Obj(
        "threadId" -> thread,
        "turnId" -> turn,
        "completedAtMs" -> ujson.Num(1800000000000d),
        "item" -> ujson.Obj(
          "id" -> "answer",
          "type" -> "agentMessage",
          "phase" -> "final_answer",
          "text" -> ujson.write(ujson.Obj("answer" -> answer))
        )
      )
    )
  private def complete(thread: String = threadId, turn: String = turnId): ujson.Value =
    ujson.Obj(
      "method" -> "turn/completed",
      "params" -> ujson.Obj(
        "threadId" -> thread,
        "turn" -> ujson.Obj("id" -> turn, "status" -> "completed", "items" -> ujson.Arr())
      )
    )
  private def tokens(count: Long, thread: String = threadId, turn: String = turnId): ujson.Value =
    ujson.Obj(
      "method" -> "thread/tokenUsage/updated",
      "params" -> ujson.Obj(
        "threadId" -> thread,
        "turnId" -> turn,
        "tokenUsage" -> ujson.Obj("total" -> ujson.Obj("totalTokens" -> ujson.Num(count.toDouble)))
      )
    )

  private class Wire(
      events: Vector[ujson.Value] = Vector(item("current answer"), complete()),
      beforeRead: ujson.Value => Unit = _ => (),
      servers: ujson.Value = ujson.Obj("data" -> ujson.Arr()),
      eventsBeforeAck: Boolean = false
  ) extends ModelTransport:
    val sent = mutable.ArrayBuffer.empty[ujson.Value]
    private val incoming = mutable.Queue.empty[ujson.Value]
    var stopped = false
    def send(value: ujson.Value): Unit =
      sent += value
      val method = value.obj.get("method").map(_.str).getOrElse("")
      val id = value.obj.getOrElse("id", ujson.Null)
      def result(answer: ujson.Value): Unit =
        incoming.enqueue(ujson.Obj("id" -> id, "result" -> answer))
      method match
        case "initialize"           => result(ujson.Obj())
        case "thread/start"         => result(ujson.Obj("thread" -> ujson.Obj("id" -> threadId)))
        case "mcpServerStatus/list" => result(servers)
        case "turn/start"           =>
          if eventsBeforeAck then incoming.enqueueAll(events)
          result(ujson.Obj("turn" -> ujson.Obj("id" -> turnId)))
          if !eventsBeforeAck then incoming.enqueueAll(events)
        case _ => ()
    def receive(timeoutMs: Long): Option[ujson.Value] =
      if incoming.isEmpty then
        Thread.sleep(math.min(5L, timeoutMs))
        None
      else
        val value = incoming.dequeue()
        beforeRead(value)
        Some(value)
    def close(): Unit = stopped = true
    def methods: Vector[String] = sent.toVector.flatMap(_.obj.get("method").map(_.str))

  test("an already canceled analysis does not open a model process") {
    val opens = new AtomicInteger()
    val wire = new Wire
    val connection =
      new CodexConnection("/tmp/public-only", () => { opens.incrementAndGet(); wire })
    assertEquals(connection.reply("public candles", () => true, false), Left("Analysis canceled"))
    assertEquals(opens.get(), 0)
    assertEquals(wire.methods, Vector.empty)
  }

  test("cancellation after connector preflight prevents a new model turn") {
    val canceled = new AtomicBoolean()
    val wire = new Wire(beforeRead = value => {
      if value.obj.get("id").contains(ujson.Num(3)) then canceled.set(true)
    })
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assert(connection.reply("public candles", () => canceled.get(), false).isLeft)
    assert(!wire.methods.contains("turn/start"))
    assert(wire.stopped)
  }

  test("foreign thread notices cannot finish the current analysis or replace its answer") {
    val wire = new Wire(
      Vector(
        item("foreign answer", thread = "other-thread"),
        complete(thread = "other-thread"),
        item("current answer"),
        complete()
      )
    )
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assertEquals(
      connection.reply("public candles", () => false, false).toOption.map(_.answer),
      Some("current answer")
    )
  }

  test("valid current turn notices can arrive before the turn start acknowledgement") {
    val wire = new Wire(eventsBeforeAck = true)
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assertEquals(
      connection.reply("public candles", () => false, false).toOption.map(_.answer),
      Some("current answer")
    )
  }

  test("notifications buffered before the turn acknowledgement have a count limit") {
    val wire = new Wire(Vector.fill(64)(item("answer")) :+ complete(), eventsBeforeAck = true)
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 1000L)
    assert(connection.reply("public candles", () => false, false).isLeft)
    assert(wire.stopped)
  }

  test("foreign turn notices cannot finish the current analysis or replace its answer") {
    val wire = new Wire(
      Vector(
        item("earlier answer", turn = "earlier-turn"),
        complete(turn = "earlier-turn"),
        item("current answer"),
        complete()
      )
    )
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assertEquals(
      connection.reply("public candles", () => false, false).toOption.map(_.answer),
      Some("current answer")
    )
  }

  test("usage belongs to the active thread and turn") {
    val wire = new Wire(
      Vector(
        tokens(1234),
        tokens(9999, thread = "other-thread"),
        tokens(8888, turn = "earlier-turn"),
        item("current answer"),
        complete()
      )
    )
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assert(connection.reply("public candles", () => false, false).isRight)
    assertEquals(connection.totalTokens, Some(1234L))
  }

  test("a new request without a usage event does not report previous request tokens") {
    val wires = mutable.Queue(new Wire(Vector(tokens(1234), item("first"), complete())), new Wire)
    val connection = new CodexConnection("/tmp/public-only", () => wires.dequeue())
    assert(connection.reply("public candles", () => false, false).isRight)
    assertEquals(connection.totalTokens, Some(1234L))
    assert(connection.reply("public candles again", () => false, false).isRight)
    assertEquals(connection.totalTokens, None)
  }

  test("cancellation of an active turn sends interrupt and unsubscribe then closes") {
    val canceled = new AtomicBoolean()
    val wire = new Wire(beforeRead = value => {
      if value.obj.get("id").contains(ujson.Num(4)) then canceled.set(true)
    })
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assertEquals(
      connection.reply("public candles", () => canceled.get(), false),
      Left("Analysis canceled")
    )
    assert(wire.methods.contains("turn/interrupt"))
    assert(wire.methods.contains("thread/unsubscribe"))
    assert(wire.stopped)
    val interrupt = wire.sent.find(_.obj.get("method").contains(ujson.Str("turn/interrupt"))).get
    assertEquals(interrupt("params")("threadId").str, threadId)
    assertEquals(interrupt("params")("turnId").str, turnId)
  }

  test("cancellation observed with turn completion does not return an answer") {
    val canceled = new AtomicBoolean()
    val wire = new Wire(beforeRead = value => {
      if value.obj.get("method").contains(ujson.Str("turn/completed")) then canceled.set(true)
    })
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assertEquals(
      connection.reply("public candles", () => canceled.get(), false),
      Left("Analysis canceled")
    )
    assert(wire.stopped)
  }

  test("unexpected executable model operations are refused before returning any answer") {
    Vector(
      "item/commandExecution/requestApproval",
      "item/permissions/requestApproval",
      "item/tool/call",
      "mcpServer/elicitation/request"
    ).foreach { method =>
      val request = ujson.Obj(
        "id" -> 901,
        "method" -> method,
        "params" -> ujson.Obj("threadId" -> threadId, "turnId" -> turnId)
      )
      val wire = new Wire(Vector(request, item("should not be accepted"), complete()))
      val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
      assert(connection.reply("public candles", () => false, false).isLeft)
      val refusal = wire.sent.find(_.obj.get("id").contains(ujson.Num(901))).get
      assert(refusal.obj.contains("error"))
      assert(!refusal.obj.contains("result"))
      assert(wire.methods.contains("turn/interrupt"))
      assert(wire.stopped)
    }
  }

  test("an empty connector page with a continuation is not proof that connectors are absent") {
    val wire = new Wire(servers = ujson.Obj("data" -> ujson.Arr(), "nextCursor" -> "more"))
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    assert(connection.reply("public candles", () => false, false).isLeft)
    assert(!wire.methods.contains("turn/start"))
    assert(wire.stopped)
  }

  test("questions and intermediate messages do not become final analysis") {
    val base = ujson.Obj(
      "type" -> "agentMessage",
      "phase" -> "final_answer",
      "text" -> "{\"answer\":\"measured answer\"}"
    )
    assertEquals(CodexProtocol.answer(base), Some("measured answer"))
    for change <- Vector(
        "phase" -> ujson.Str("commentary"),
        "delivery" -> ujson.Str("async"),
        "questions" -> ujson.Arr(ujson.Obj("id" -> "preference"))
      )
    do
      val changed = ujson.read(ujson.write(base))
      changed(change._1) = change._2
      assertEquals(CodexProtocol.answer(changed), None)
  }

  private def childTransport(): (Process, ModelTransport) =
    val child = new ProcessBuilder(
      "python3",
      "-c",
      "import signal,time; signal.signal(signal.SIGTERM,signal.SIG_IGN); print('{\"ready\":true}',flush=True); time.sleep(60)"
    ).start()
    val constructor = Class
      .forName("desk.CodexConnection$ProcessTransport")
      .getDeclaredConstructor(classOf[Process])
    constructor.setAccessible(true)
    val transport = constructor.newInstance(child).asInstanceOf[ModelTransport]
    assertEquals(transport.receive(3000L).map(_("ready").bool), Some(true))
    (child, transport)

  test("normal transport closure kills an owned child that ignores stdin closure and terminate") {
    val (child, transport) = childTransport()
    try
      transport.close()
      assert(child.waitFor(2000L, TimeUnit.MILLISECONDS))
    finally
      val _ = child.destroyForcibly()
      val _ = child.waitFor(2000L, TimeUnit.MILLISECONDS)
  }

  test("interrupted transport closure restores the caller interruption after cleanup") {
    val (child, transport) = childTransport()
    try
      Thread.currentThread().interrupt()
      try transport.close()
      catch case _: InterruptedException => ()
      assert(Thread.currentThread().isInterrupted)
      val _ = Thread.interrupted()
      assert(child.waitFor(2000L, TimeUnit.MILLISECONDS))
    finally
      val _ = Thread.interrupted()
      val _ = child.destroyForcibly()
      val _ = child.waitFor(2000L, TimeUnit.MILLISECONDS)
      val _ = Try(transport.close())
  }

  test("analysis shutdown during transport closure still destroys its owned child") {
    val (child, transport) = childTransport()
    val closing = new CountDownLatch(1)
    val answers = new AtomicInteger()
    val wire = new Wire:
      override def close(): Unit =
        closing.countDown()
        transport.close()
        super.close()
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 2500L)
    val worker = new AnalysisWorker(
      () => Snapshot.initial("XMR", 1800000000000L),
      () => "calendar unavailable",
      (context, canceled, study) =>
        connection
          .reply(context, canceled, study)
          .map(reply => desk.ModelReply(reply.answer, reply.study)),
      (kind, _) => { if kind == "analysis" then { val _ = answers.incrementAndGet() } },
      () => 1800000000000L,
      automaticEnabled = false
    )
    try
      assert(worker.ask("measured averages"))
      worker.start()
      assert(closing.await(3000L, TimeUnit.MILLISECONDS))
      worker.close()
      assert(
        child.waitFor(1000L, TimeUnit.MILLISECONDS),
        "owned model child survived analysis shutdown"
      )
      assertEquals(answers.get(), 0)
    finally
      worker.close()
      val _ = child.destroyForcibly()
      val _ = child.waitFor(2000L, TimeUnit.MILLISECONDS)
      val _ = Try(transport.close())
  }

  test("transport closure also stops an owned descendant when its parent exits on stdin closure") {
    val script =
      "import subprocess,sys,json; child=subprocess.Popen([sys.executable,'-c','import time; time.sleep(60)']); print(json.dumps({'child':child.pid}),flush=True); sys.stdin.read()"
    val parent = new ProcessBuilder("python3", "-c", script).start()
    val constructor = Class
      .forName("desk.CodexConnection$ProcessTransport")
      .getDeclaredConstructor(classOf[Process])
    constructor.setAccessible(true)
    val transport = constructor.newInstance(parent).asInstanceOf[ModelTransport]
    val childId = transport.receive(3000L).get("child").num.toLong
    val child = ProcessHandle.of(childId).get()
    try
      transport.close()
      assert(parent.waitFor(2000L, TimeUnit.MILLISECONDS))
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(1)
      while child.isAlive && System.nanoTime() < deadline do Thread.sleep(5L)
      assert(!child.isAlive, "owned descendant survived parent shutdown")
    finally
      val _ = child.destroyForcibly()
      val _ = parent.destroyForcibly()
      val _ = parent.waitFor(2000L, TimeUnit.MILLISECONDS)
      val _ = Try(transport.close())
  }

  test("market context excludes pending UI input and sanitizes public text") {
    val snapshot =
      Snapshot.initial("XMR", 1800000000000L).copy(prompt = Some("PRIVATE_UNSUBMITTED_INPUT"))
    val context = QuantContext.build(
      snapshot,
      1800000000000L,
      "calendar\u001b[31mtext",
      "averages\u0000question"
    )
    assert(!context.contains("PRIVATE_UNSUBMITTED_INPUT"))
    assert(!context.contains('\u001b'))
    assert(!context.contains('\u0000'))
    assert(context.contains("coverage"))
  }
