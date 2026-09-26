package desk

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

  private def rowsFor(level: Double, width: Int = 46): Vector[String] =
    OutlookScreen.rows(
      outlook(level),
      width,
      Some(Axis(level * 0.99, level * 1.01, 9)),
      9,
      ChartMode.Candles
    )

  private def target(rows: Vector[String]): String =
    rows.map(_.split("\\|", -1).head.trim).find(_.startsWith("─ST ")).get.stripPrefix("─ST ")

  test("ordinary levels share plain rounded precision at the narrowest column width") {
    val rows = rowsFor(525.376)
    val shown = target(rows)
    assert(!shown.toLowerCase.contains("e"))
    assert(math.abs(shown.toDouble - 525.376) < 0.01)
    val move = rows(10).split("\\|", -1).head.trim.stripPrefix("Test ").stripSuffix("%")
    assert(!move.toLowerCase.contains("e"))
    assert(math.abs(move.toDouble - 100 * (525.376 / 530 - 1)) < 0.001)
  }

  test("small nonzero levels remain nonzero instead of being rounded away") {
    val rows = rowsFor(0.000000000357812)
    val shown = target(rows).toDouble
    assert(shown > 0)
    assert(math.abs(shown / 0.000000000357812 - 1) < 0.01)
  }

  test("large levels stay bounded and numerically close at every supported width") {
    for width <- Vector(46, 47, 63) do
      val rows = rowsFor(3578123456789.0, width)
      assert(rows.forall(_.length <= width))
      val shown = target(rows).toDouble
      assert(math.abs(shown / 3578123456789.0 - 1) < 0.01)
  }

  test("extreme levels keep one percent precision or explicitly show unavailable") {
    for
      level <- Vector(3.57812e-308, 3.57812e-13, 0.00357812, 357812.0, 3.57812e307)
      width <- Vector(46, 47, 63)
    do
      val rows = rowsFor(level, width)
      assert(rows.forall(_.length <= width))
      val shown = target(rows)
      assert(
        shown == "--" || math.abs(shown.toDouble / level - 1) < 0.01,
        s"$level displayed as $shown at width $width"
      )
  }
