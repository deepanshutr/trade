package desk

// Independent fixtures use different turning points and wick schedules.
class OutlookArithmeticSpec extends munit.FunSuite:
  private val step = 300000L
  private val now = 1260L * step

  private def fixture(up: Boolean): Vector[Candle] =
    val initial = Vector.tabulate(1260) { i =>
      val close = if up then
        BigDecimal(2000) - BigDecimal("1.25") * math.min(i, 420) +
          BigDecimal("0.5") * math.max(0, i - 420)
      else
        BigDecimal(500) + BigDecimal("1.6") * math.min(i, 580) -
          BigDecimal("0.5") * math.max(0, i - 580)
      Candle(
        i.toLong * step,
        close.toDouble,
        (close + BigDecimal(".01")).toDouble,
        (close - BigDecimal(".01")).toDouble,
        close.toDouble,
        1
      )
    }
    val altered = (0 until 90).foldLeft(initial) { (candles, period) =>
      val wanted = period % (if up then 7 else 9) == (if up then 0 else 2)
      if !wanted then candles
      else
        val anchor = 60 + period * 12
        val old = candles(anchor + 2)
        val level = BigDecimal(candles(anchor - 1).c) * BigDecimal(if up then "1.06" else ".94")
        candles.updated(
          anchor + 2,
          if up then old.copy(h = level.toDouble)
          else old.copy(l = level.toDouble)
        )
    }
    val last = altered.last
    val target = BigDecimal(last.c) * BigDecimal(if up then "1.04" else ".96")
    altered.updated(
      altered.size - 1,
      if up then last.copy(h = target.toDouble)
      else last.copy(l = target.toDouble)
    )

  test("asymmetric rising fixture has fourteen base hits and nine of sixty-seven matches") {
    val candles = fixture(true)
    val snapshot = Snapshot.initial("XMR", now).copy(candles = Map("5m" -> candles))
    val value = Outlook.compute(snapshot, now).columns.last
    assertEquals(value.direction, OutlookDirection.Up)
    assertEquals(value.status, "ready")
    assertEquals(value.base, HistoricalRate(14, 100, 100))
    assertEquals(value.matched, HistoricalRate(9, 67, 50))
    assertEquals(value.from, Some(60L * step))
    assertEquals(value.to, Some(now))
    assertEqualsDouble(value.target.get / value.reference.get, 1.04, 1e-14)
    assertEqualsDouble(value.matched.percent.get, 900.0 / 67, 1e-10)
  }

  test("asymmetric falling fixture has eleven base hits and six of fifty-four matches") {
    val candles = fixture(false)
    val snapshot = Snapshot.initial("XMR", now).copy(candles = Map("5m" -> candles))
    val value = Outlook.compute(snapshot, now).columns.last
    assertEquals(value.direction, OutlookDirection.Down)
    assertEquals(value.status, "ready")
    assertEquals(value.base, HistoricalRate(11, 100, 100))
    assertEquals(value.matched, HistoricalRate(6, 54, 50))
    assertEqualsDouble(value.target.get / value.reference.get, .96, 1e-14)
    assertEqualsDouble(value.matched.percent.get, 600.0 / 54, 1e-10)
  }
