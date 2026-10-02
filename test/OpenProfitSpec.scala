package desk

import java.time.ZoneOffset

class OpenProfitSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val held = PropAccount(
    cash = BigDecimal("9519.75"),
    realized = BigDecimal(20),
    fees = BigDecimal("0.25"),
    positions = Map("XMR" -> PropPosition(5, 100)),
    deposit = BigDecimal(10000)
  )

  private def row(portfolio: PortfolioView, cols: Int = 100, rows: Int = 30): String =
    val frame = Workspace.draw(
      Snapshot.initial("XMR", now),
      TradingView("XMR", message = "Ready", portfolio = portfolio),
      PanelView(mode = "PROP"),
      cols,
      rows,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    assertEquals(frame.lines.size, rows)
    assert(frame.lines.forall(_.length == cols))
    frame.lines(rows - 12).drop(Layout.workspaceMarketWidth(cols) + 1)

  test("open gains exclude previous sales while total net retains them and fees") {
    for (cols, rows) <- Vector((100, 30), (120, 40), (160, 45)) do
      val text = row(PortfolioView.prop(held, "XMR", Some(102)), cols, rows)
      assert(text.contains("Open P&L $10"), text)
      assert(text.contains("Net P&L $29.75"), text)
  }

  test("an open loss remains negative while earlier realized gains keep total net positive") {
    val text = row(PortfolioView.prop(held, "XMR", Some(98)))
    assert(text.contains("Open P&L $-10"), text)
    assert(text.contains("Net P&L $9.75"), text)
  }

  test("closing the holding resets open profit and keeps the total result") {
    val closed = held.copy(cash = BigDecimal("10019.75"), positions = Map.empty)
    val text = row(PortfolioView.prop(closed, "XMR", None))
    assert(text.contains("Open P&L $0"), text)
    assert(text.contains("Net P&L $19.75"), text)
  }

  test("a missing mark never invents an open or total result for held tokens") {
    val text = row(PortfolioView.prop(held, "XMR", None))
    assert(text.contains("Open P&L --"), text)
    assert(text.contains("Net P&L --"), text)
  }

  test("another market has no selected holding and cannot show the previous market profit") {
    val text = row(PortfolioView.prop(held, "BTC", Some(100)))
    assert(text.contains("Open P&L $0"), text)
    assert(text.contains("Net P&L --"), text)
  }

  private def account(profit: ujson.Value): ujson.Value = ujson.Obj(
    "assetPositions" -> ujson.Arr(
      ujson.Obj(
        "position" -> ujson.Obj(
          "coin" -> "XMR",
          "szi" -> "-5",
          "positionValue" -> "510",
          "entryPx" -> "100",
          "unrealizedPnl" -> profit
        )
      )
    )
  )

  test("real positions use the venue open result without inventing total lifetime profit") {
    val text = row(PortfolioView.live(account(ujson.Str("-10")), "XMR", Some(1000)))
    assert(text.contains("Open P&L $-10"), text)
    assert(text.contains("Net P&L --"), text)
  }

  test("missing and malformed real open results remain unavailable") {
    for value <- Vector(ujson.Null, ujson.Str("NaN"), ujson.Str("1e31")) do
      val text = row(PortfolioView.live(account(value), "XMR", None))
      assert(text.contains("Open P&L --"), text)
  }

  test("a confirmed flat real account shows zero open profit without an entry") {
    val text = row(PortfolioView.live(ujson.Obj("assetPositions" -> ujson.Arr()), "XMR", None))
    assert(text.contains("Open P&L $0"), text)
    assert(text.contains("Net P&L --"), text)
  }

  test("an unidentified real position cannot establish zero open profit") {
    for identity <- Vector(ujson.Null, ujson.Str(""), ujson.Str(" "), ujson.Num(1)) do
      val data = account(ujson.Str("10"))
      data("assetPositions")(0)("position")("coin") = identity
      val portfolio = PortfolioView.live(data, "XMR", None)
      assertEquals(portfolio.openProfit, None)
      assertEquals(portfolio.tokenValue, None)
      assert(row(portfolio).contains("Open P&L --"))
  }
