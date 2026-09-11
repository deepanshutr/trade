package desk

import scala.util.Try

object CodexProtocol:
  def request(id: Int, method: String, params: ujson.Value): ujson.Obj =
    ujson.Obj("id" -> id, "method" -> method, "params" -> params)
  def threadStart(workspace: String): ujson.Obj = request(
    2,
    "thread/start",
    ujson.Obj(
      "cwd" -> workspace,
      "ephemeral" -> true,
      "environments" -> ujson.Arr(),
      "runtimeWorkspaceRoots" -> ujson.Arr(),
      "selectedCapabilityRoots" -> ujson.Arr(),
      "sandbox" -> "read-only",
      "approvalPolicy" -> "never"
    )
  )
  def emptyServers(result: ujson.Value): Boolean = Try {
    result("data").arr.isEmpty && result.obj.get("nextCursor").forall(_ == ujson.Null)
  }.getOrElse(false)
  def answer(item: ujson.Value): Option[String] = Try {
    val phase = item.obj.get("phase")
    val acceptable = item("type").str == "agentMessage" && phase.forall(p =>
      p == ujson.Null || p.str == "final_answer"
    ) && !item.obj.get("delivery").contains(ujson.Str("async")) && item.obj
      .get("questions")
      .forall(q => q == ujson.Null || q.arr.isEmpty)
    Option
      .when(acceptable)(ujson.read(item("text").str)("answer").str)
      .filter(s => s.nonEmpty && s.length <= 30000)
  }.toOption.flatten

final class AnalysisSchedule:
  private var attempted = Option.empty[Long]
  private var previous = ""
  def automatic(now: Long, fingerprint: String): Boolean = synchronized {
    if fingerprint == previous || attempted.exists(t => now < t || now - t < 300000L) then false
    else
      attempted = Some(now)
      previous = fingerprint
      true
  }
