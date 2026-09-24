package desk

import java.time.ZoneOffset

class LiveControlsSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private def frame(
      exposure: BigDecimal,
      mode: String,
      cols: Int,
      rows: Int,
      leverage: Option[Int] = Some(5)
  ): WorkspaceFrame =
    Workspace.draw(
      Snapshot.initial("XMR", now),
      TradingView(
        "XMR",
        message = "Ready",
        ready = true,
        leverage = leverage,
        portfolio = PortfolioView(tokenValue = Some(exposure), stables = Some(1000))
      ),
      PanelView(mode = mode),
      cols,
      rows,
      now,
      ZoneOffset.UTC,
      Style.mono
    )

  test("live controls name both directions and the observed five-times leverage") {
    for (cols, rows) <- Vector((100, 30), (120, 40), (360, 99)) do
      val live = frame(500, "LIVE", cols, rows)
      val text = live.lines.mkString("\n")
      assert(text.contains("LIVE XMR 5x"), text)
      assert(text.contains("XMR LONG"), text)
      assert(
        live.buttons.exists(b =>
          b.label.contains("BUY/LONG") &&
            b.action == DeskAction.Prepare(OrderIntent(Side.Buy, OrderKind.Hard))
        )
      )
      assert(
        live.buttons.exists(b =>
          b.label.contains("SELL/SHORT") &&
            b.action == DeskAction.Prepare(OrderIntent(Side.Sell, OrderKind.Hard))
        )
      )
      assert(live.lines.forall(_.length == cols))
  }

  test("short exposure is named without changing the fixed buy and sell targets") {
    val long = frame(500, "LIVE", 100, 30)
    val short = frame(-500, "LIVE", 100, 30)
    assert(short.lines.mkString("\n").contains("XMR SHORT"))
    assertEquals(short.buttons, long.buttons)
  }

  test("the live heading does not invent leverage and flat exposure is explicit") {
    val flat = frame(0, "LIVE", 100, 30, None)
    val text = flat.lines.mkString("\n")
    assert(text.contains("XMR FLAT"))
    assert(!text.contains("5x"))
  }

  test("simulated wallet controls keep buy and sell without suggesting borrowed tokens") {
    val paper = frame(500, "PAPER", 100, 30)
    val execution = paper.lines
      .drop(15)
      .map(_.drop(Layout.workspaceMarketWidth(100) + 1))
      .mkString("\n")
    assert(!execution.contains("5x"))
    assert(!execution.contains("LONG"))
    assert(!execution.contains("SHORT"))
    assert(paper.buttons.exists(_.label == "1 BUY $500"))
    assert(paper.buttons.exists(_.label == "3 SELL $500"))
  }
