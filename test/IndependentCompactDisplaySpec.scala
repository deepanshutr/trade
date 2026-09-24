package desk

import java.time.ZoneOffset

class IndependentCompactDisplaySpec extends munit.FunSuite:
  import CompactDisplayFixtures.*

  private def correlation(frame: WorkspaceFrame): Vector[String] =
    val width = (Layout.workspaceMarketWidth(frame.cols) - 1) / 2
    frame.lines.takeRight(12).map(line => Chart.plain(line).slice(width + 1, width * 2 + 1).trim)

  for (cols, rows) <- sizes do
    test(s"moving correlation endpoints retain every exact paired count at $cols x $rows") {
      for style <- Vector(Style.mono, Style.colour) do
        val frame = draw(snapshot(cols = cols, rows = rows), panel(), style)
        val readings = correlation(frame)
        for ((hz, _, n), i) <- Gauges.horizons.zipWithIndex do
          for (name, index, positive) <- Vector(("BTC", i, true), ("GOLD", i + 6, false)) do
            val reading = readings(index)
            assert(reading.startsWith(name + " " + hz + " "), reading)
            assert(reading.endsWith((if positive then "+1.00 " else "-1.00 ") + s"$n/$n"), reading)
            val track = reading.split(" ")(2)
            assertEquals(track.count(_ == Chart.FULL.head), 1)
            assert(if positive then track.endsWith(Chart.FULL) else track.startsWith(Chart.FULL))
        if style == Style.colour then
          assert(frame.lines(rows - 12).contains(style.bitcoin))
          assert(frame.lines(rows - 6).contains(style.gold))

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
    val track = reading.split(" ")(2)
    assertEquals(track.indexOf(Chart.FULL), track.length / 2)
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
        assertEquals(readings(6), "GOLD 15m " + expected)
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
