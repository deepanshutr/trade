package desk

class SharedRulesSpec extends munit.FunSuite:
  test("crossed books supply neither a display midpoint nor a sample") {
    val now = 10000L
    val book = Book(Vector(Level("101", "1", 1)), Vector(Level("100", "1", 1)), now)
    val snapshot =
      State.reduce(Snapshot.initial("XMR", now), Event.BookUpdate("XMR", book, true), now)
    assertEquals(State.mid(book), None)
    assertEquals(snapshot.samples, Vector.empty)
    assert(Screen.stats(snapshot, now, 160).contains("mid unavailable"))
    assert(
      Outlook
        .visible(Outlook.empty("XMR").copy(calculatedAt = now), snapshot, now)
        .columns
        .forall(_.status == "invalid price")
    )
    val policy = new AlertPolicy
    assertEquals(policy.market(snapshot, now), Vector.empty)
  }

  test("outlook header derives its period from the horizon") {
    val view = Outlook.empty("XMR")
    val changed = view.copy(columns =
      view.columns.map(c =>
        if c.horizon.label == "MID" then c.copy(horizon = OutlookHorizon("MID", "15m", 32)) else c
      )
    )
    assert(OutlookScreen.rows(changed, 90, None, 5, ChartMode.Line).head.contains("MID 8h"))
  }
