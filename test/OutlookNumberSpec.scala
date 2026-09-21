package desk

import java.time.ZoneOffset

class OutlookNumberSpec extends munit.FunSuite:
  private def outlook(level: Double): OutlookView =
    Outlook
      .empty("XMR")
      .copy(columns = Outlook.horizons.map { horizon =>
        HorizonOutlook(
          horizon,
          OutlookDirection.Down,
          "ready",
          Some(level),
          Some(533.209),
          Some(level),
          Some(530),
          Some(0L),
          HistoricalRate(50, 100, 100),
          HistoricalRate(25, 50, 50),
          100,
          Some(0L),
          Some(1L)
        )
      })

  private def first(rows: Vector[String], row: Int, label: String): String =
    rows(row).split("\\|", -1).head.stripPrefix(label + " ").trim

  test("ordinary levels share plain rounded precision at the narrowest column width") {
    val rows = OutlookScreen.rows(outlook(525.376), 46, ZoneOffset.UTC)
    val target = first(rows, 2, "Target")
    val support = first(rows, 3, "Support")
    assertEquals(target, support)
    assert(!target.toLowerCase.contains("e"))
    assert(math.abs(target.toDouble - 525.376) < 0.01)
    val move = first(rows, 5, "Test").stripSuffix("%")
    assert(!move.toLowerCase.contains("e"))
    assert(math.abs(move.toDouble - 100 * (525.376 / 530 - 1)) < 0.001)
  }

  test("small nonzero levels remain nonzero instead of being rounded away") {
    val rows = OutlookScreen.rows(outlook(0.000000000357812), 46, ZoneOffset.UTC)
    val shown = first(rows, 2, "Target").toDouble
    assert(shown > 0)
    assert(math.abs(shown / 0.000000000357812 - 1) < 0.01)
  }

  test("large levels stay bounded and numerically close at every supported width") {
    for width <- Vector(46, 47, 63) do
      val rows = OutlookScreen.rows(outlook(3578123456789.0), width, ZoneOffset.UTC)
      assert(rows.forall(_.length <= width))
      val shown = first(rows, 2, "Target").toDouble
      assert(math.abs(shown / 3578123456789.0 - 1) < 0.01)
  }

  test("extreme levels keep one percent precision or explicitly show unavailable") {
    for
      level <- Vector(3.57812e-308, 3.57812e-13, 0.00357812, 357812.0, 3.57812e307)
      width <- Vector(46, 47, 63)
    do
      val rows = OutlookScreen.rows(outlook(level), width, ZoneOffset.UTC)
      assert(rows.forall(_.length <= width))
      val shown = first(rows, 2, "Target")
      assert(
        shown == "--" || math.abs(shown.toDouble / level - 1) < 0.01,
        s"$level displayed as $shown at width $width"
      )
      assert(
        OutlookScreen
          .details(outlook(level))
          .contains("Target " + BigDecimal(level).bigDecimal.stripTrailingZeros().toPlainString)
      )
  }
