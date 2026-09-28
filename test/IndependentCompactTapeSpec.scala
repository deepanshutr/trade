package desk

import java.time.ZoneOffset

class IndependentCompactTapeSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L

  for
    i <- Gauges.tapeLenses.indices
    delta <- Vector(-1L, 0L)
  do
    test(s"compact tape lens $i preserves observation age at boundary $delta") {
      val (_, seconds) = Gauges.tapeLenses(i)
      val elapsed = seconds * 1000L + delta
      val s = Snapshot.initial("XMR", now).copy(since = now - elapsed)
      val compact = Gauges.compactTapeRow(s, i, now, 1, Style.colour)
      assertEquals(compact.contains(Screen.dim), delta < 0L, compact)
    }

  test("minimum workspace preserves maximum tape count and both book readings") {
    val s = Snapshot
      .initial("XMR", now)
      .copy(
        since = now - 86_400_000L,
        book = Some(Book(Vector(Level("99", "1", 1)), Vector(Level("101", "3", 1)), now)),
        tape = Vector.fill(State.tapeCap)(Trade(now - 1L, Side.Buy, "100", "1"))
      )
    for style <- Vector(Style.mono, Style.colour) do
      val frame = Workspace.draw(
        s,
        TradingView("XMR", message = "View only"),
        PanelView(),
        100,
        30,
        now,
        ZoneOffset.UTC,
        style
      )
      val plain = frame.lines.map(Chart.plain)
      assertEquals(plain.size, 30)
      assert(plain.forall(_.length == 100))
      val tape = plain.filter(_.startsWith("TAPE"))
      assertEquals(tape.size, 6)
      assert(tape.forall(_.take(26).endsWith("n200000")), tape.mkString("\n"))
      assert(tape.forall(_.take(26).contains("F+1.00")))
      val book = plain.filter(_.startsWith("BOOK"))
      assertEquals(book.size, 6)
      assert(book.forall(_.take(26).contains("D-0.50 Q-0.50")), book.mkString("\n"))
  }
