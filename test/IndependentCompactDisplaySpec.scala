package desk

import java.time.ZoneOffset

class IndependentCompactDisplaySpec extends munit.FunSuite:
  import CompactDisplayFixtures.*

  private def correlation(frame: WorkspaceFrame): Vector[String] =
    val (start, end) = frame.cols match
      case 100 => (28, 53)
      case 120 => (41, 72)
      case 160 => (65, 96)
      case _   => fail("unlisted fixture width")
    frame.lines.takeRight(12).map(line => Chart.plain(line).slice(start, end).trim)

  for (cols, rows) <- sizes do
    test(s"moving correlation endpoints retain every exact paired count at $cols x $rows") {
      for style <- Vector(Style.mono, Style.colour) do
        val frame = draw(snapshot(cols = cols, rows = rows), panel(), style)
        val readings = correlation(frame)
        val half = if cols == 100 then 1 else 4
        val positive = "." * half + "|" + "." * (half - 1) + Chart.FULL
        val negative = Chart.FULL + "." * (half - 1) + "|" + "." * half
        for ((hz, _, n), i) <- Gauges.horizons.zipWithIndex do
          val btcPrefix = if i == 0 then "BTC " else ""
          val goldPrefix = if i == 0 then "gold " else ""
          assertEquals(readings(i), s"$btcPrefix$hz $positive +1.00 $n/$n")
          assertEquals(readings(i + 6), s"$goldPrefix$hz $negative -1.00 $n/$n")
        if style == Style.colour then
          assert(frame.lines(rows - 12).contains(style.bid + Chart.FULL + Screen.reset))
          assert(frame.lines(rows - 6).contains(style.ask + Chart.FULL + Screen.reset))
    }

  test("zero correlation places its marker in the middle and keeps a numeric zero") {
    val returns = (-7 to 7).map(_.toDouble).toVector
    def candles(values: Vector[Double]): Vector[Candle] =
      values.scanLeft(100.0)((p, r) => p * math.exp(r / 10000.0)).zipWithIndex.map { (p, i) =>
        Candle(now - (16L - i) * 60000L, p, p, p, p, 1)
      }
    val a = candles(returns)
    val b = candles(returns.map(r => r * r))
    val original = snapshot()
    val s = original.copy(
      candles = Map("1m" -> a),
      refs = original.refs.updated("BTC", Ref.empty.copy(candles = Map("1m" -> b)))
    )
    val reading = correlation(draw(s, panel())).head
    assert(reading.startsWith("BTC 15m ." + Chart.FULL + ". "), reading)
    assert(reading.endsWith("0.00 15/15"), reading)
  }

  for (kind, expected) <- Vector(
      "empty" -> "unavailable 0/15",
      "partial" -> "partial 13/15",
      "stale" -> "stale 14/15",
      "flat" -> "flat 15/15"
    )
  do
    test(s"$kind correlation keeps its reason and exact count without an active marker") {
      for style <- Vector(Style.mono, Style.colour) do
        val readings = correlation(draw(inactive(kind), panel(), style))
        assertEquals(readings.head, "BTC 15m " + expected)
        assertEquals(readings(6), "gold 15m " + expected)
        assert(readings.forall(!_.contains(Chart.FULL)), readings.mkString("\n"))
    }

  for coin <- Vector("BTC", "PAXG") do
    test(
      s"selected $coin suppresses all self correlation markers while retaining the other market"
    ) {
      val readings = correlation(draw(snapshot(coin), panel(coin)))
      val own = if coin == "BTC" then readings.take(6) else readings.drop(6)
      val other = if coin == "BTC" then readings.drop(6) else readings.take(6)
      assertEquals(own.size, 6)
      assert(own.forall(_.contains("same market")), own.mkString("\n"))
      assert(own.forall(!_.contains(Chart.FULL)))
      assert(other.forall(_.contains(Chart.FULL)), other.mkString("\n"))
    }

  private def referenceFixture(
      mode: ChartMode,
      history: Boolean,
      coin: String,
      cols: Int,
      rows: Int
  ): Snapshot =
    val ages = if mode == ChartMode.Line then Vector(600, 598, 596, 2, 1, 0)
    else Vector(720, 660, 600, 120, 60, 0)
    val prices = Vector(200.0, 240.0, 215.0, 200.0, 240.0, 210.0)
    def ref(scale: Double): Ref =
      val points = ages.zip(prices).map((age, price) => (now / 1000 - age, price * scale))
      Ref(
        points,
        Map("1m" -> points.map { (second, price) =>
          Candle(second * 1000L, price, price, price, price, 1)
        })
      )
    val offset = if history then (if mode == ChartMode.Line then 596L else 600L) else 0L
    snapshot(coin, mode, cols, rows, offset).copy(refs =
      Map("BTC" -> ref(1.0), "PAXG" -> ref(10.0))
    )

  for
    (cols, rows) <- sizes
    mode <- ChartMode.values
    selected <- Vector("XMR", "BTC", "PAXG")
  do
    test(
      s"$mode reference prices and plotted current agree during live and historical views at $cols x $rows selected $selected"
    ) {
      for history <- Vector(false, true) do
        val s = referenceFixture(mode, history, selected, cols, rows)
        val frame = draw(s, panel(selected))
        val chartWidth = cols - 39
        val cellCount = cols - 78
        val plain = frame.lines.map(Chart.plain)
        val references = Vector("BTC", "PAXG").filterNot(_ == selected)
        for coin <- references do
          val first = plain.indexWhere(_.startsWith(if coin == "BTC" then "BTC " else "PAXG ("))
          assert(first > 0, coin + " missing")
          assert(first + 3 <= rows - 15, "reference crosses trade controls")
          val actual = plain.slice(first, first + 3).map(_.slice(chartWidth - 9, chartWidth).trim)
          val factor = if coin == "BTC" then 1.0 else 10.0
          val wanted =
            Vector(242.0, if history then 215.0 else 210.0, if history then 188.0 else 178.0).map(
              _ * factor
            )
          assertEquals(
            actual(1),
            "<" + BigDecimal(wanted(1)).bigDecimal.stripTrailingZeros().toPlainString
          )
          assertEquals(actual.head.toDouble, wanted.head)
          assertEquals(actual.last.toDouble, wanted.last)
          assertEquals((actual.head.toDouble + actual.last.toDouble) / 2.0, wanted(1))
          val middlePlot = plain(first + 1).slice(30, chartWidth - 9)
          if mode == ChartMode.Line then
            val last = middlePlot.last.toInt - 0x2800
            assert(
              last >= 0 && (last & 0x20) != 0,
              s"$coin latest visible line point is not in middle row subrow2: $middlePlot"
            )
          else
            val start = (cellCount / 4 - 1) * 4
            assertEquals(middlePlot.slice(start, start + 3), "───")
        if selected != "XMR" then
          assert(
            !plain.drop(1).exists(_.startsWith(if selected == "BTC" then "BTC " else "PAXG ("))
          )
        assert(plain(rows - 15).contains("PAPER " + selected))
        assertEquals(plain.size, rows)
        assert(plain.forall(_.length == cols))
    }

  test("empty reference history keeps all reference price labels empty") {
    val original = snapshot()
    val s = original.copy(refs = Map("BTC" -> Ref.empty, "PAXG" -> Ref.empty))
    val frame = draw(s, panel())
    val plain = frame.lines.map(Chart.plain)
    for coin <- Vector("BTC", "PAXG") do
      val first = plain.indexWhere(_.startsWith(if coin == "BTC" then "BTC " else "PAXG ("))
      assert(first > 0)
      assert(plain.slice(first, first + 3).forall(_.slice(52, 61).trim.isEmpty))
  }

  private def requireButton(frame: WorkspaceFrame, action: DeskAction): DeskButton =
    frame.buttons.find(_.action == action).getOrElse(fail(s"missing $action"))

  for (cols, rows) <- sizes do
    test(
      s"Details and Ask retain chart history, complete controls and readable source content at $cols x $rows"
    ) {
      val s = snapshot(cols = cols, rows = rows, offset = 60L)
      val normal = draw(s, panel())
      val chartWidth = cols - 39
      val tradeTop = rows - 15
      val cases = Vector(
        panel().copy(details = true, detailPage = DetailPage.Method),
        panel().copy(details = true, detailPage = DetailPage.Sources),
        panel().copy(details = true, detailPage = DetailPage.Answers),
        panel().copy(
          details = true,
          detailPage = DetailPage.Answers,
          input = Some("A full question with retained input")
        )
      )
      for p <- cases do
        val frame = draw(s, p)
        assertEquals(
          frame.lines.take(tradeTop).map(Chart.plain).map(_.take(chartWidth)),
          normal.lines.take(tradeTop).map(Chart.plain).map(_.take(chartWidth))
        )
        for button <- frame.controls do
          val rendered =
            Chart.plain(frame.lines(button.y - 1)).slice(button.x - 1, button.x - 1 + button.width)
          val enabled = frame.buttons.contains(button)
          assertEquals(
            rendered,
            (if enabled then "[" else "(") + button.label + (if enabled then "]" else ")")
          )
          if enabled then
            for x <- button.x until button.x + button.width do
              assertEquals(frame.hit(x, button.y, cols, rows, "XMR"), Some(button.action))
        val _ = requireButton(frame, DeskAction.Details)
        if p.input.nonEmpty then
          val _ = requireButton(frame, DeskAction.SendQuestion)
          val _ = requireButton(frame, DeskAction.Cancel)
          assert(!frame.buttons.exists(_.action == DeskAction.Status))
        else
          for page <- DetailPage.values do
            val _ = requireButton(frame, DeskAction.Page(page))
          if p.detailPage == DetailPage.Answers then
            val _ = requireButton(frame, DeskAction.Feedback(1L, true))
            val _ = requireButton(frame, DeskAction.Feedback(1L, false))
      val sources = cases(1)
      val first = draw(s, sources)
      val pages =
        Vector(0, first.scrollLimit).distinct.map(scroll => draw(s, sources.copy(scroll = scroll)))
      val text = pages
        .map(_.lines.slice(3, tradeTop).map(Chart.plain).map(_.drop(chartWidth + 1)).mkString)
        .mkString
        .replaceAll("\\s", "")
      assert(text.contains(sourceText.replaceAll("\\s", "")), text)
      assert(text.contains(sourceUrl), text)
      assert(text.contains("Published2027-01-15T07:50:00Z"), text)
      assert(text.contains("Retrieved2027-01-15T07:59:59Z"), text)
    }

    test(s"complete order review keeps exact terms and all send hit cells at $cols x $rows") {
      for details <- Vector(false, true) do
        val p = panel().copy(review = Some(review), details = details)
        val frame = draw(snapshot(cols = cols, rows = rows), p)
        val text = frame.lines.map(Chart.plain).mkString("\n")
        assert(frame.completeReview, text)
        for term <- Vector("5.4321", "101", "548.6421", "1x paper", "Expires in") do
          assert(text.contains(term), text)
        val send = requireButton(frame, DeskAction.Submit)
        val cancel = requireButton(frame, DeskAction.Cancel)
        for button <- Vector(send, cancel); x <- button.x until button.x + button.width do
          assertEquals(frame.hit(x, button.y, cols, rows, "XMR"), Some(button.action))
        assert(send.y > rows - 15)
    }
