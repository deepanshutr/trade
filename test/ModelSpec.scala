package desk

import scala.collection.mutable

class ModelSpec extends munit.FunSuite:
  private class Wire(withServer: Boolean = false) extends ModelTransport:
    val sent = mutable.ArrayBuffer.empty[ujson.Value]
    val incoming = mutable.Queue.empty[ujson.Value]
    var stopped = false
    def send(value: ujson.Value): Unit =
      sent += value
      val method = value.obj.get("method").map(_.str).getOrElse("")
      val id = value.obj.getOrElse("id", ujson.Null)
      def result(v: ujson.Value): Unit = incoming.enqueue(ujson.Obj("id" -> id, "result" -> v))
      method match
        case "initialize"   => result(ujson.Obj())
        case "thread/start" => result(ujson.Obj("thread" -> ujson.Obj("id" -> "public-thread")))
        case "mcpServerStatus/list" =>
          result(ujson.Obj("data" -> (if withServer then ujson.Arr(ujson.Obj("name" -> "mail"))
                                      else ujson.Arr())))
        case "turn/start" =>
          result(ujson.Obj("turn" -> ujson.Obj("id" -> "analysis-turn")))
          incoming.enqueue(
            ujson.Obj(
              "method" -> "item/completed",
              "params" -> ujson.Obj(
                "threadId" -> "public-thread",
                "turnId" -> "analysis-turn",
                "item" -> ujson.Obj(
                  "type" -> "agentMessage",
                  "phase" -> "final_answer",
                  "delivery" -> "async",
                  "text" -> "{\"answer\":\"Do you prefer A?\"}"
                )
              )
            )
          )
          incoming.enqueue(
            ujson.Obj(
              "method" -> "item/completed",
              "params" -> ujson.Obj(
                "threadId" -> "public-thread",
                "turnId" -> "analysis-turn",
                "item" -> ujson.Obj(
                  "type" -> "agentMessage",
                  "phase" -> "final_answer",
                  "text" -> "{\"answer\":\"The measured averages disagree.\",\"study\":null}"
                )
              )
            )
          )
          for _ <- 0 until 2 do
            incoming.enqueue(
              ujson.Obj(
                "method" -> "thread/tokenUsage/updated",
                "params" -> ujson.Obj(
                  "threadId" -> "public-thread",
                  "turnId" -> "analysis-turn",
                  "tokenUsage" -> ujson.Obj("total" -> ujson.Obj("totalTokens" -> 1234))
                )
              )
            )
          incoming.enqueue(
            ujson.Obj(
              "method" -> "turn/completed",
              "params" -> ujson.Obj(
                "threadId" -> "public-thread",
                "turn" -> ujson.Obj("id" -> "analysis-turn", "status" -> "completed")
              )
            )
          )
        case "thread/unsubscribe" => result(ujson.Obj())
        case _                    => ()
    def receive(timeoutMs: Long): Option[ujson.Value] = incoming.dequeueFirst(_ => true)
    def close(): Unit = stopped = true

  test(
    "model request checks connector absence, isolates the thread and returns only a completed answer"
  ) {
    val wire = new Wire
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 1000L)
    val answer = connection.reply("Public Monero candles only", () => false, false)
    assertEquals(answer.toOption.map(_.answer), Some("The measured averages disagree."))
    assertEquals(connection.totalTokens, Some(1234L))
    assert(wire.stopped)
    val start = wire.sent.find(_.obj.get("method").contains(ujson.Str("thread/start"))).get
    assertEquals(start("params")("environments"), ujson.Arr())
    val turn = wire.sent.find(_.obj.get("method").contains(ujson.Str("turn/start"))).get
    assertEquals(turn("params")("environments"), ujson.Arr())
    assertEquals(turn("params")("input")(0)("text").str, "Public Monero candles only")
  }

  test("an unexpected connector stops before any market prompt is sent") {
    val wire = new Wire(true)
    val connection = new CodexConnection("/tmp/public-only", () => wire, timeoutMs = 1000L)
    assert(connection.reply("public candles", () => false, false).isLeft)
    assert(!wire.sent.exists(_.obj.get("method").contains(ujson.Str("turn/start"))))
    assert(wire.stopped)
  }

  test(
    "model study parameters must be bounded and results are computed from chosen public candles"
  ) {
    val good =
      ujson.Obj("interval" -> "1m", "fast" -> 20, "slow" -> 50, "ahead" -> 4, "lookback" -> 1000)
    assert(StudyRequest.parse(good).isRight)
    good("fast") = 50
    assert(StudyRequest.parse(good).isLeft)
    good("fast") = 20
    good("lookback") = 10000000
    assert(StudyRequest.parse(good).isLeft)
    val snapshot = Snapshot.initial("XMR", 1800000000000L)
    val context = QuantContext.build(
      snapshot,
      1800000000000L,
      "Official calendar unavailable",
      "moving averages"
    )
    assert(context.contains("coverage"))
    assert(context.contains("unavailable"))
    assert(!context.contains("owner"))
    assert(!context.contains("privateKey"))
  }
