package desk

import java.math.{BigDecimal as Exact}
import java.nio.file.Files
import java.time.ZoneOffset

class IndependentOpenProfitSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private val blank = PaperAccount(BigDecimal(1000), deposit = BigDecimal(1000))
  private def exact(text: String): Exact = new Exact(text)
  private def same(actual: Option[BigDecimal], expected: Exact): Unit =
    assert(
      actual.exists(_.bigDecimal.compareTo(expected) == 0),
      s"actual=$actual expected=$expected"
    )
  private def account(position: ujson.Value): ujson.Value =
    ujson.Obj("assetPositions" -> ujson.Arr(ujson.Obj("position" -> position)))
  private def position(coin: String, q: String, profit: ujson.Value): ujson.Obj =
    ujson.Obj(
      "coin" -> coin,
      "szi" -> q,
      "positionValue" -> "500",
      "entryPx" -> "100",
      "unrealizedPnl" -> profit,
      "leverage" -> ujson.Obj("type" -> "cross", "value" -> 5)
    )
  private def frame(
      p: PortfolioView,
      cols: Int,
      rows: Int,
      style: Style,
      mode: String
  ): WorkspaceFrame =
    Workspace.draw(
      Snapshot.initial("XMR", now),
      TradingView("XMR", message = "Ready", ready = true, portfolio = p),
      PanelView(mode = mode),
      cols,
      rows,
      now,
      ZoneOffset.UTC,
      style
    )

  private def colors(text: String): (String, Vector[Option[Int]]) =
    val ansi = "\\u001b\\[([0-9;]*)m".r
    val plain = new StringBuilder
    val result = Vector.newBuilder[Option[Int]]
    var foreground = Option.empty[Int]
    var cursor = 0
    def append(until: Int): Unit =
      text.substring(cursor, until).foreach { ch =>
        plain.append(ch)
        result += foreground
      }
    ansi.findAllMatchIn(text).foreach { m =>
      append(m.start)
      val codes = m.group(1).split(";").toVector
      if codes.contains("0") || codes == Vector("") then foreground = None
      if codes == Vector("31") then foreground = Some(31)
      if codes == Vector("32") then foreground = Some(32)
      cursor = m.end
    }
    append(text.length)
    (plain.result(), result.result())

  test("actual buys, partial sales and full closure separate gross open profit from total net") {
    val dir = Files.createTempDirectory("desk-open-profit-")
    try
      val ledger = PaperLedger.open(dir, "mainnet", BigDecimal(1000), BigDecimal("0.01"))
      def fill(side: Side, q: Int, px: Int): Unit =
        assert(
          ledger
            .fill(OrderPlan("XMR", 0, side, OrderKind.Hard, q, px, px, q, q * px, Some(px), 0), now)
            .isRight
        )
      try
        fill(Side.Buy, 2, 100)
        val first = PortfolioView.paper(ledger.account, "XMR", Some(110))
        same(first.openProfit, exact("20"))
        same(first.net, exact("18"))
        fill(Side.Sell, 1, 120)
        val remaining = PortfolioView.paper(ledger.account, "XMR", Some(90))
        same(remaining.openProfit, exact("-10"))
        same(remaining.net, exact("6.8"))
        fill(Side.Sell, 1, 80)
        val flat = PortfolioView.paper(ledger.account, "XMR", None)
        same(flat.openProfit, exact("0"))
        same(flat.net, exact("-4"))
      finally ledger.close()
    finally
      val paths = Files.walk(dir)
      try paths.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally paths.close()
  }

  test(
    "paper open profit matches independent exact arithmetic and ignores fees and closed results"
  ) {
    val quantities = Vector("1", "0.000001", "12345.123456")
    val entries = Vector("100", "123.45678901234567890123456789")
    val marks = Vector("0.000001", "99", "101", "10000000000000000000000000000000000000000.5")
    for q <- quantities; entry <- entries; mark <- marks do
      val saved = blank.copy(
        cash = BigDecimal("913.456"),
        realized = BigDecimal("321.987"),
        fees = BigDecimal("77.123"),
        positions = Map("XMR" -> PaperPosition(BigDecimal(q), BigDecimal(entry)))
      )
      val p = PortfolioView.paper(saved, "XMR", Some(BigDecimal(mark)))
      val expected = exact(q).multiply(exact(mark).subtract(exact(entry)))
      same(p.openProfit, expected)
      same(p.net, exact("913.456").add(exact(q).multiply(exact(mark))).subtract(exact("1000")))
      val unrelated = PortfolioView.paper(
        saved.copy(cash = 42, fees = 999, realized = -555),
        "XMR",
        Some(BigDecimal(mark))
      )
      same(unrelated.openProfit, expected)
  }

  test(
    "held positions need a valid selected mark while flat and other-market open profit is zero"
  ) {
    val saved = blank.copy(positions = Map("XMR" -> PaperPosition(2, 100)))
    for mark <- Vector(None, Some(BigDecimal(0)), Some(BigDecimal(-1))) do
      assertEquals(PortfolioView.paper(saved, "XMR", mark).openProfit, None)
      assertEquals(PortfolioView.paper(saved, "XMR", mark).net, None)
    same(PortfolioView.paper(blank, "XMR", None).openProfit, exact("0"))
    val other = PortfolioView.paper(saved, "BTC", Some(110))
    same(other.openProfit, exact("0"))
    assertEquals(other.net, None)
  }

  test(
    "live long and short positions use venue profit once without multiplying by side or leverage"
  ) {
    for
      q <- Vector("5", "-5");
      profit <- Vector("13.125", "-17.25", "0", "1000000000000000000000000000000")
    do
      val data = account(position("XMR", q, ujson.Str(profit)))
      val p = PortfolioView.live(data, "XMR", Some(500))
      same(p.openProfit, exact(profit))
      assertEquals(p.net, None)
  }

  test("missing, malformed and ambiguous selected live information remains unknown") {
    val valid = position("XMR", "5", ujson.Str("10"))
    val missingProfit = ujson.read(ujson.write(valid))
    missingProfit.obj.remove("unrealizedPnl")
    val duplicate = ujson.Obj(
      "assetPositions" -> ujson.Arr(ujson.Obj("position" -> valid), ujson.Obj("position" -> valid))
    )
    val cases = Vector(
      ujson.Obj(),
      ujson.Obj("assetPositions" -> ujson.Null),
      account(missingProfit),
      account(position("XMR", "bad", ujson.Str("10"))),
      account(position("XMR", "5", ujson.Null)),
      account(position("XMR", "5", ujson.Num(10))),
      account(position("XMR", "5", ujson.Str("NaN"))),
      account(position("XMR", "5", ujson.Str("1e31"))),
      duplicate
    )
    for data <- cases do
      assertEquals(PortfolioView.live(data, "XMR", None).openProfit, None, ujson.write(data))
  }

  test("malformed position identity does not establish a flat live account") {
    val unknown = position("XMR", "5", ujson.Str("10"))
    unknown.obj.remove("coin")
    for malformed <- Vector(unknown, ujson.Null, ujson.Str("invalid")) do
      val data = account(malformed)
      assertEquals(PortfolioView.live(data, "XMR", None).openProfit, None, ujson.write(data))
  }

  test(
    "confirmed live flat and another correctly identified market show selected zero open profit"
  ) {
    for data <- Vector(
        ujson.Obj("assetPositions" -> ujson.Arr()),
        account(position("XMR", "0", ujson.Null)),
        account(position("BTC", "5", ujson.Str("10")))
      )
    do
      val p = PortfolioView.live(data, "XMR", None)
      same(p.openProfit, exact("0"))
      assertEquals(p.net, None)
  }

  test("Open and Net remain separate and independently colored at 100x30 and 360x99") {
    val scenarios = Vector(
      (Some(BigDecimal(10)), Some(BigDecimal(-30))),
      (Some(BigDecimal(-10)), Some(BigDecimal(20))),
      (None, Some(BigDecimal(10))),
      (Some(BigDecimal(-10)), None),
      (Some(BigDecimal(0)), Some(BigDecimal(0))),
      (Some(BigDecimal("1e40")), Some(BigDecimal("-1e40")))
    )
    for
      (cols, rows) <- Vector((100, 30), (360, 99)); style <- Vector(Style.mono, Style.colour);
      mode <- Vector("PAPER", "LIVE"); (open, net) <- scenarios
    do
      val p =
        PortfolioView(openProfit = open, net = net, tokenValue = Some(500), stables = Some(800))
      val drawn = frame(p, cols, rows, style, mode)
      assertEquals(drawn.lines.size, rows)
      assert(drawn.lines.forall(line => Chart.plain(line).length == cols))
      val raw = drawn.lines(rows - 12)
      val (plain, paint) = colors(raw)
      val start = Layout.workspaceMarketWidth(cols) + 1
      val half = (cols - start) / 2
      val left = plain.substring(start, start + half)
      val right = plain.substring(start + half)
      assert(left.startsWith("Open P&L "), left)
      assert(right.startsWith("Net P&L "), right)
      assertEquals(plain.sliding(8).count(_ == "Open P&L"), 1)
      assertEquals(plain.sliding(7).count(_ == "Net P&L"), 1)
      if open.isEmpty then assert(left.contains("--"), left)
      if net.isEmpty then assert(right.contains("--"), right)
      if style.colour then
        def expected(v: Option[BigDecimal]): Option[Int] = v.map(n => if n >= 0 then 32 else 31)
        assert(paint.slice(start, start + "Open P&L".length).forall(_ == expected(open)))
        assert(
          paint.slice(start + half, start + half + "Net P&L".length).forall(_ == expected(net))
        )
      else assert(!raw.contains("\u001b"))
  }

  test("a market change clears both values instead of retaining the previous market profit") {
    val old = TradingView(
      "BTC",
      message = "Ready",
      portfolio = PortfolioView(openProfit = Some(42), net = Some(99))
    )
    val next = old.forMarket("XMR")
    assertEquals(next.portfolio.openProfit, None)
    assertEquals(next.portfolio.net, None)
    val drawn = Workspace.draw(
      Snapshot.initial("XMR", now),
      old,
      PanelView(mode = "LIVE"),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    val line = drawn.lines(18)
    assert(line.contains("Open P&L --"), line)
    assert(line.contains("Net P&L --"), line)
  }
