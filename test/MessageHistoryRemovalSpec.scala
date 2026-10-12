package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

object HistorylessWorkspaceProcess:
  def main(args: Array[String]): Unit =
    Main.run(
      Main
        .Args("XMR", true, config = Some(args(1)), stateDirectory = Some(args(0)), audio = "mute"),
      _ =>
        (
          new Connector:
            def connect(receive: String => Unit, close: () => Unit): Socket =
              throw new AssertionError("unexpected connection")
          ,
          new Rest:
            def post(body: String): Option[String] = throw new AssertionError("unexpected request")
        ),
      keys = KeyStartup.Hooks(home = Path.of(args(0))),
      polymarket = OfflinePolymarket.client
    )

class MessageHistoryRemovalSpec extends TempDirectorySuite:
  test("order notices survive ignored prose and remain responsive to keyboard input") {
    val now = 1800000000000L
    val controller = new DeskController(
      PanelView(mode = "PROP"),
      () => now,
      (_, _) => Left("unused"),
      _ => false,
      _ => Left("unused")
    )
    val market = Snapshot.initial("XMR", now)
    val view = TradingView("XMR", Some(BigDecimal(5)), "Prop ready", ready = true)
    for (side, visible, number) <- Vector(("buy", "Bought", 1L), ("sell", "Sold", 2L)) do
      controller.addMessage("order", s"PROP XMR $side filled 5")
      for i <- 1 to 400 do controller.addMessage("analysis", s"Ignored prose $i")
      val frame = Workspace.draw(
        market,
        view,
        controller.panel,
        100,
        30,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      assertEquals(controller.panel.actionNumber, number)
      assert(frame.lines.exists(_.contains(visible)))
      assert(!frame.lines.mkString.contains("!save"))
      controller.publish(frame)
      controller.handle(Command.ToggleMute, market, view, (100, 30), false)
      assertEquals(controller.panel.muted, number == 1L)
  }

  test("a second diagnostic writer cannot modify the first writer's files") {
    val root = tempDirectory("desk-diagnostic-lease")
    val first = new Diagnostics.FilesSink(root)
    try
      first.append("first")
      intercept[Exception](new Diagnostics.FilesSink(root))
      first.append("second")
      assertEquals(Files.readString(root.resolve("events-0.jsonl")), "first\nsecond\n")
    finally first.close()
  }

  test("workspace passes a held legacy history lock and leaves saved messages untouched") {
    val root = tempDirectory("desk-history-removal", keepOnFailure = true)
    val settings = root.resolve("settings.json")
    LocalFiles.write(settings, "{}")
    val saved = root.resolve("messages/messages.json")
    val legacy = """[{"id":1,"at":123,"kind":"analysis","text":"old answer","feedback":false}]"""
    LocalFiles.write(saved, legacy)
    val lease = LocalFiles.lock(root.resolve("messages/messages.lock"))
    val log = root.resolve("startup.log")
    val child = new ProcessBuilder(
      Path.of(System.getProperty("java.home"), "bin/java").toString,
      "-cp",
      System.getProperty("java.class.path"),
      "desk.HistorylessWorkspaceProcess",
      root.toString,
      settings.toString
    ).redirectErrorStream(true).redirectOutput(log.toFile).start()
    child.getOutputStream.close()
    try
      assert(child.waitFor(20, TimeUnit.SECONDS), "workspace startup timed out")
      val output = Files.readString(log)
      assert(output.contains("desk needs a terminal"), output)
      assert(!output.contains("Desk message storage is unavailable"), output)
      assertEquals(Files.readString(saved), legacy)
    finally
      if child.isAlive then
        child.destroyForcibly()
        child.waitFor(5, TimeUnit.SECONDS)
      lease.close()
  }
