package desk

import java.time.ZoneOffset

class RefinePortfolioSpec extends munit.FunSuite:
  private val now = 1800000000000L
  test("simulated holdings and net include realized results, open results and fees exactly") {
    val account = PropAccount(
      BigDecimal("9519.75"),
      BigDecimal(20),
      BigDecimal("0.25"),
      Map("XMR" -> PropPosition(BigDecimal(5), BigDecimal(100))),
      deposit = BigDecimal(10000)
    )
    val p = PortfolioView.prop(account, "XMR", Some(BigDecimal(102)))
    assertEquals(p.tokenValue, Some(BigDecimal(510)))
    assertEquals(p.stables, Some(BigDecimal("9519.75")))
    assertEquals(p.net, Some(BigDecimal("29.75")))
    assertEquals(p.entry, Some(BigDecimal(100)))
    assertEquals(p.liquidation, None)
    assertEquals(p.net, Some(p.stables.get + p.tokenValue.get - BigDecimal(10000)))
    val lower = PortfolioView.prop(account, "XMR", Some(BigDecimal(98)))
    assertEquals(lower.tokenValue, Some(BigDecimal(490)))
    assertEquals(lower.net, Some(BigDecimal("9.75")))
    assertEquals(lower.net, Some(lower.stables.get + lower.tokenValue.get - BigDecimal(10000)))
    assertEquals(PortfolioView.prop(account, "XMR", None).net, None)
    assertEquals(PortfolioView.prop(account, "BTC", Some(BigDecimal(100))).net, None)
    val multiple = account.copy(positions = account.positions.updated("BTC", PropPosition(1, 100)))
    assertEquals(PortfolioView.prop(multiple, "XMR", Some(BigDecimal(102))).net, None)
    assertEquals(
      PortfolioView
        .prop(account.copy(cash = BigDecimal("10019.75"), positions = Map.empty), "XMR", None)
        .net,
      Some(BigDecimal("19.75"))
    )
  }

  test("live values retain direction and nullable fields without inventing lifetime profit") {
    val account = ujson.read(
      """{"marginSummary":{"accountValue":"1005"},"assetPositions":[
      {"position":{"coin":"XMR","szi":"-5","positionValue":"510","entryPx":"100","liquidationPx":"140","unrealizedPnl":"-10"}},
      {"position":{"coin":"BTC","szi":"1","positionValue":"115","entryPx":"100","liquidationPx":null,"unrealizedPnl":"15"}}]}"""
    )
    val p = PortfolioView.live(account, "XMR", PortfolioView.marginCash(account))
    assertEquals(p.tokenValue, Some(BigDecimal(-510)))
    assertEquals(p.stables, Some(BigDecimal(1000)))
    assertEquals(p.net, None)
    assertEquals(p.entry, Some(BigDecimal(100)))
    assertEquals(p.liquidation, Some(BigDecimal(140)))
    assertEquals(PortfolioView.live(account, "BTC", None).liquidation, None)
    account("assetPositions")(0)("position")("liquidationPx") = "NaN"
    assertEquals(PortfolioView.live(account, "XMR", None).liquidation, None)
    account("assetPositions")(1)("position")("unrealizedPnl") = ujson.Null
    assertEquals(PortfolioView.marginCash(account), None)
    val spot = ujson.read(
      """{"balances":[{"coin":"USDC","token":0,"total":"45.12"},{"coin":"OTHER","token":5,"total":"99"}]}"""
    )
    assertEquals(PortfolioView.usdcCash(spot), Some(BigDecimal("45.12")))
    spot("balances").arr += ujson.Obj("coin" -> "USDC", "token" -> 0, "total" -> "1")
    assertEquals(PortfolioView.usdcCash(spot), None)
  }

  test("entry and liquidation labels preserve every plotted cell and scale in all chart modes") {
    for mode <- ChartMode.values; style <- Vector(Style.mono, Style.colour) do
      val s = Snapshot
        .initial("XMR", now)
        .copy(
          chartMode = mode,
          samples = Vector.tabulate(200)(i => (now / 1000 - 199 + i, 99.0 + i % 3)),
          book = Some(Book(Vector(Level("99", "1", 1)), Vector(Level("101", "1", 1)), now))
        )
      val original = Screen.renderMarket(s, 61, 9, now, ZoneOffset.UTC, style)
      val p =
        PortfolioView(entry = Some(BigDecimal("100")), liquidation = Some(BigDecimal("100.01")))
      val marked =
        Screen.renderMarket(s, 61, 9, now, ZoneOffset.UTC, style, portfolio = p)
      assertEquals(marked.axis, original.axis)
      for row <- 1 to original.priceRows do
        assertEquals(Chart.plain(marked.lines(row)).length, 61)
        assertEquals(
          Chart.plain(marked.lines(row)).drop(Layout.leftW),
          Chart.plain(original.lines(row)).drop(Layout.leftW)
        )
      val text = marked.lines.map(Chart.plain).mkString("\n")
      assert(text.contains("Entry 100"))
      assert(text.contains("Liq 100.01"))
      assert(marked.lines.exists(line => line.contains("Entry 100") && line.contains("Liq 100.01")))
      val overflow = p
        .copy(entry = Some(BigDecimal(1000)), liquidation = Some(BigDecimal(1)))
        .markerRows(original.axis, mode, Layout.leftW, style)
      assert(overflow(0).contains("↑Entry"))
      assert(overflow(original.priceRows - 1).contains("↓Liq"))
      assertEquals(p.markerRows(None, mode, Layout.leftW, style), Map.empty)
  }

  test("another market never inherits the previous market holdings or chart markers") {
    val view = TradingView(
      "BTC",
      message = "ready",
      ready = true,
      portfolio = PortfolioView(Some(123), Some(456), Some(789), Some(100), Some(50))
    )
    val frame = Workspace.draw(
      Snapshot.initial("XMR", now),
      view,
      PanelView(mode = "PROP"),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    assert(!frame.lines.mkString.contains("$123"))
    assert(!frame.lines.mkString.contains("$456"))
    assert(!frame.buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
  }
