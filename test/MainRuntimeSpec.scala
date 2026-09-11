package desk

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions

class MainRuntimeSpec extends munit.FunSuite:
  test("live review captures and displays observed leverage and margin") {
    val now = 1800000000000L
    val snapshot = Snapshot.initial("XMR", now)
    val intent = OrderIntent(Side.Buy, OrderKind.Hard)
    val plan = OrderPlan(
      "XMR",
      0,
      Side.Buy,
      OrderKind.Hard,
      BigDecimal(5),
      BigDecimal(100),
      BigDecimal(100),
      BigDecimal(5),
      BigDecimal(500),
      Some(BigDecimal(100)),
      BigDecimal(5)
    )
    val view = TradingView(
      "XMR",
      message = "Ready",
      ready = true,
      rules = Some(MarketRules(0, 2, 5)),
      leverage = Some(3),
      marginMode = Some("isolated")
    )
    val controller = new DeskController(
      PanelView(mode = "LIVE"),
      () => now,
      _ => Right(plan),
      _ => true,
      _ => true,
      _ => Left("unused")
    )
    def draw(): WorkspaceFrame = Workspace.draw(
      snapshot,
      view,
      controller.panel,
      120,
      40,
      now,
      java.time.ZoneId.of("UTC"),
      Style.mono
    )
    controller.publish(draw())
    controller.handle(Command.PlaceOrder(intent), snapshot, view, (120, 40), false)
    val review = controller.panel.review.get
    assertEquals(review.leverage, Some(3))
    assertEquals(review.marginMode, Some("isolated"))
    val frame = draw()
    assert(frame.completeReview)
    assert(frame.lines.exists(_.contains("3x isolated")))
  }

  test("paper and recorder modes have explicit independent startup choices") {
    assert(
      Main.parseArgs(Seq("XMR", "--paper", "--state", "/tmp/desk-private", "--no-analysis")).isRight
    )
    assert(Main.parseArgs(Seq("--record", "--no-analysis")).isRight)
    assert(Main.parseArgs(Seq("--paper", "--trade")).isLeft)
    assert(Main.parseArgs(Seq("--record", "--trade")).isLeft)
    assert(Main.parseArgs(Seq("--record", "--paper")).isLeft)
    assert(Main.parseArgs(Seq("--state")).isLeft)
  }

  test("paper balance and fee can be configured without live addresses") {
    val directory = Files.createTempDirectory("desk-paper-settings")
    val path = directory.resolve("settings.json")
    Files.writeString(path, """{"paperBalance":"20000","paperFee":"0.001"}""")
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------"))
    assert(TradingConfig.load(Some(path)).isRight)
    Files.writeString(path, """{"paperFee":"-1"}""")
    assert(TradingConfig.load(Some(path)).isLeft)
  }
