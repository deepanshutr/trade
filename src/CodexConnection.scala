package desk

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import scala.util.Try

trait ModelTransport extends AutoCloseable:
  def send(value: ujson.Value): Unit
  def receive(timeoutMs: Long): Option[ujson.Value]

final case class ModelReply(answer: String, study: Option[StudyRequest])

object CodexConnection:
  // This exact installed release and model were independently checked with a
  // loopback provider. A changed binary needs a new access-boundary review.
  private val binary = Path.of(
    "/opt/homebrew/lib/node_modules/@openai/codex/node_modules/@openai/codex-darwin-arm64/vendor/aarch64-apple-darwin/bin/codex"
  )
  private val digest = "b973d440acac501fd2594a43e7ca9ce41e0a65b9dfb28d0d7a7837c99e1261e3"
  private val disabled = Vector(
    "apps",
    "multi_agent",
    "shell_tool",
    "shell_snapshot",
    "view_image",
    "browser_use",
    "browser_use_external",
    "computer_use",
    "image_generation",
    "token_budget",
    "goals",
    "memories",
    "deferred_executor",
    "tool_suggest",
    "skill_mcp_dependency_install"
  )
  private val settings = Vector(
    "model=\"gpt-6-astra\"",
    "model_provider=\"openai\"",
    "sandbox_mode=\"read-only\"",
    "approval_policy=\"never\"",
    "web_search=\"disabled\"",
    "agents.enabled=false",
    "orchestrator.skills.enabled=false",
    "orchestrator.mcp.enabled=false"
  )

  def create(workspace: Path): CodexConnection =
    LocalFiles.directory(workspace)
    new CodexConnection(
      workspace.toAbsolutePath.toString,
      () => {
        require(Files.isRegularFile(binary), "Reviewed Codex installation is unavailable")
        val stream = Files.newInputStream(binary)
        val sha = MessageDigest.getInstance("SHA-256")
        try
          val block = new Array[Byte](65536)
          var count = stream.read(block)
          while count >= 0 do
            if count > 0 then sha.update(block, 0, count)
            count = stream.read(block)
        finally stream.close()
        require(
          java.util.HexFormat.of().formatHex(sha.digest()) == digest,
          "Codex changed. Its analysis access needs review"
        )
        val command =
          Vector(binary.toString, "app-server", "--stdio", "--strict-config") ++ settings.flatMap(
            s => Vector("-c", s)
          ) ++ disabled.flatMap(s => Vector("--disable", s))
        new ProcessTransport(
          new ProcessBuilder(command*)
            .directory(workspace.toFile)
            .redirectError(ProcessBuilder.Redirect.DISCARD)
            .start()
        )
      }
    )

  private class ProcessTransport(process: Process) extends ModelTransport:
    private val writer =
      new PrintWriter(process.getOutputStream, false, java.nio.charset.StandardCharsets.UTF_8)
    private val queue = new ArrayBlockingQueue[Either[Exception, ujson.Value]](512)
    private val reader = new Thread(
      () => {
        try
          val lines = new BufferedReader(
            new InputStreamReader(process.getInputStream, java.nio.charset.StandardCharsets.UTF_8)
          )
          var line = lines.readLine()
          while line != null do
            require(line.length <= 2000000, "Oversized model protocol message")
            queue.put(Right(ujson.read(line)))
            line = lines.readLine()
          queue.put(Left(new java.io.EOFException("Model connection ended")))
        catch
          case _: InterruptedException => ()
          case error: Exception        =>
            val _ = queue.offer(Left(error))
      },
      "analysis-protocol"
    )
    reader.setDaemon(true)
    reader.start()
    def send(value: ujson.Value): Unit = synchronized {
      writer.println(ujson.write(value))
      writer.flush()
      require(!writer.checkError(), "Model input closed")
    }
    def receive(timeoutMs: Long): Option[ujson.Value] =
      Option(queue.poll(timeoutMs, TimeUnit.MILLISECONDS)).map(_.fold(throw _, identity))
    def close(): Unit =
      val children = new java.util.ArrayList[ProcessHandle]()
      var interrupted = false
      try
        val descendants = process.descendants()
        try descendants.forEach(p => { val _ = children.add(p) })
        finally descendants.close()
        writer.close()
        if !process.waitFor(2000L, TimeUnit.MILLISECONDS) then
          process.destroy()
          val _ = process.waitFor(1000L, TimeUnit.MILLISECONDS)
      catch case _: InterruptedException => interrupted = true
      finally
        try children.forEach(p => { if p.isAlive then { val _ = Try(p.destroyForcibly()) } })
        finally
          try if process.isAlive then { val _ = process.destroyForcibly() }
          finally
            reader.interrupt()
            try reader.join(1000L)
            catch case _: InterruptedException => interrupted = true
            if interrupted then Thread.currentThread().interrupt()

final class CodexConnection(
    workspace: String,
    open: () => ModelTransport,
    timeoutMs: Long = 300000L
):
  @volatile private var usage = Option.empty[Long]
  def totalTokens: Option[Long] = usage
  def reply(
      publicContext: String,
      canceled: () => Boolean,
      allowStudy: Boolean
  ): Either[String, ModelReply] =
    var connection = Option.empty[ModelTransport]
    var thread = Option.empty[String]
    var turn = Option.empty[String]
    var result = Option.empty[ModelReply]
    var completed = Option.empty[String]
    var pendingNotices = Vector.empty[ujson.Value]
    val deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMs)
    def active(): Unit =
      require(!canceled(), "Analysis canceled")
      require(System.nanoTime() < deadline, "Analysis timed out")
    try
      active()
      require(publicContext.length <= 100000, "Analysis context exceeds its limit")
      usage = None
      val wire = open()
      connection = Some(wire)
      def read(): ujson.Value =
        var value = Option.empty[ujson.Value]
        while value.isEmpty do
          active()
          value = wire.receive(50L)
        value.get
      def notice(value: ujson.Value): Unit =
        val method = value.obj.get("method").map(_.str).getOrElse("")
        val params = value.obj.getOrElse("params", ujson.Obj())
        def current(id: Option[ujson.Value]): Boolean =
          thread.exists(t => params.obj.get("threadId").contains(ujson.Str(t))) &&
            turn.exists(t => id.contains(ujson.Str(t)))
        method match
          case "thread/tokenUsage/updated" if current(params.obj.get("turnId")) =>
            usage = Try(params("tokenUsage")("total")("totalTokens").num).toOption
              .filter(n => n >= 0 && n.isWhole && n <= 9007199254740991d)
              .map(_.toLong)
          case "item/completed" if current(params.obj.get("turnId")) =>
            val item = params("item")
            CodexProtocol.answer(item).foreach { answer =>
              val json = ujson.read(item("text").str)
              val study = json.obj.get("study").filter(_ != ujson.Null).map { value =>
                require(allowStudy, "Unexpected study request")
                StudyRequest
                  .parse(value)
                  .fold(reason => throw new IllegalArgumentException(reason), identity)
              }
              result = Some(ModelReply(answer, study))
            }
          case "turn/completed" if current(Try(params("turn")("id")).toOption) =>
            completed = Some(params("turn")("status").str)
          case "item/tool/requestUserInput" if value.obj.contains("id") =>
            wire.send(
              ujson.Obj("id" -> value("id"), "result" -> ujson.Obj("answers" -> ujson.Obj()))
            )
          case _ if value.obj.contains("id") && value.obj.contains("method") =>
            wire.send(
              ujson.Obj(
                "id" -> value("id"),
                "error" -> ujson.Obj(
                  "code" -> -32601,
                  "message" -> "Operation unavailable in market analysis"
                )
              )
            )
            throw new IllegalStateException("Unexpected model operation")
          case _ => ()
      def rpc(id: Int, method: String, params: ujson.Value): ujson.Value =
        active()
        wire.send(CodexProtocol.request(id, method, params))
        var answer = Option.empty[ujson.Value]
        while answer.isEmpty do
          val value = read()
          if value.obj.get("id").contains(ujson.Num(id)) && !value.obj.contains("method") then
            require(!value.obj.contains("error"), "Model request unavailable")
            answer = Some(value("result"))
          else if method == "turn/start" && !value.obj.contains("id") then
            require(pendingNotices.size < 64, "Too many notices before the turn acknowledgement")
            pendingNotices :+= value
          else notice(value)
        answer.get
      val _ = rpc(
        1,
        "initialize",
        ujson.Obj(
          "clientInfo" -> ujson.Obj("name" -> "desk", "version" -> "1.0.0"),
          "capabilities" -> ujson.Obj("experimentalApi" -> true)
        )
      )
      wire.send(ujson.Obj("method" -> "initialized", "params" -> ujson.Obj()))
      val created = rpc(2, "thread/start", CodexProtocol.threadStart(workspace)("params"))
      thread = Some(created("thread")("id").str)
      val servers = rpc(
        3,
        "mcpServerStatus/list",
        ujson.Obj("threadId" -> thread.get, "detail" -> "toolsAndAuthOnly", "limit" -> 100)
      )
      require(CodexProtocol.emptyServers(servers), "Unexpected account connector")
      val schema = ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("answer" -> ujson.Obj("type" -> "string")),
        "required" -> ujson.Arr("answer"),
        "additionalProperties" -> false
      )
      if allowStudy then
        schema("properties")("study") =
          ujson.Obj("anyOf" -> ujson.Arr(ujson.Obj("type" -> "null"), StudyRequest.schema))
        schema("required").arr += ujson.Str("study")
      val started = rpc(
        4,
        "turn/start",
        ujson.Obj(
          "threadId" -> thread.get,
          "environments" -> ujson.Arr(),
          "input" -> ujson.Arr(
            ujson.Obj("type" -> "text", "text" -> publicContext, "text_elements" -> ujson.Arr())
          ),
          "outputSchema" -> schema
        )
      )
      turn = Some(started("turn")("id").str)
      pendingNotices.foreach(notice)
      pendingNotices = Vector.empty
      while completed.isEmpty do notice(read())
      active()
      require(
        completed.contains("completed") && result.nonEmpty,
        "Analysis did not return a complete answer"
      )
      Right(result.get)
    catch
      case _: Exception =>
        Left(if canceled() then "Analysis canceled"
        else "Codex analysis unavailable. Local calculations remain available")
    finally
      connection.foreach { wire =>
        if completed.isEmpty then
          for t <- thread; id <- turn do
            Try(
              wire.send(
                CodexProtocol.request(
                  8,
                  "turn/interrupt",
                  ujson.Obj("threadId" -> t, "turnId" -> id)
                )
              )
            )
        thread.foreach(t => {
          val _ = Try(
            wire.send(CodexProtocol.request(9, "thread/unsubscribe", ujson.Obj("threadId" -> t)))
          )
        })
        val _ = Try(wire.close())
      }
