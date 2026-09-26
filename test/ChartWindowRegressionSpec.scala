package desk

class ChartWindowRegressionSpec extends munit.FunSuite:
  private val now = 20_000_000L

  test(
    "book windows retain distinct readings through the fifteen minute boundary"
  ) {
    val sec = now / 1000
    val current = Book(Vector(Level("100", "1", 1)), Vector.empty, now)
    val ages = Vector(901L, 900L, 600L, 120L, 45L, 20L, 5L)
    val books = ages.map { age =>
      val at = now - age * 1000
      (sec - age, Book(Vector.empty, Vector(Level("101", "1", 1)), at))
    }
    val s =
      Snapshot.initial("XMR", 0L).copy(book = Some(current), books = books)
    val labels = Vector("now", "10s", "30s", "1m", "5m", "15m")
    val readings = Vector("+1.00", "+0.00", "-0.33", "-0.50", "-0.60", "-0.71")
    for cols <- Vector(100, 101, 120) do
      val rows = Workspace
        .indicatorRows(s, now, cols / 2, Style.mono)
        .map(Chart.plain)
        .take(6)
      assertEquals(rows.map(_.slice(5, 8).trim), labels)
      for (row, value) <- rows.zip(readings) do assert(row.contains(s"D$value Q$value"), row)
  }

  test(
    "tape windows retain distinct counts and flow through the four hour boundary"
  ) {
    val ages = Vector(14_400L, 7200L, 1800L, 600L, 120L, 30L, 5L)
    val trades = ages.map { age =>
      Trade(
        now - age * 1000,
        if age == 5 then Side.Buy else Side.Sell,
        "100",
        "1"
      )
    }
    val s = Snapshot.initial("XMR", 0L).copy(tape = trades)
    val labels = Vector("10s", "1m", "5m", "15m", "1h", "4h")
    val readings = Vector("+1.00", "+0.00", "-0.33", "-0.50", "-0.60", "-0.67")
    for cols <- Vector(100, 101, 120) do
      val rows = Workspace
        .indicatorRows(s, now, cols / 2, Style.mono)
        .map(Chart.plain)
        .drop(6)
      assertEquals(rows.map(_.slice(5, 8).trim), labels)
      for ((row, value), index) <- rows.zip(readings).zipWithIndex do
        assert(row.contains(s"F$value n${index + 1}"), row)
  }
