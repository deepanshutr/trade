package desk

import java.time.ZoneOffset

class CompactReferenceLayoutSpec extends munit.FunSuite:
  private val now = 1800000000000L

  for (cols, rows) <- Vector((100, 30), (120, 40), (160, 45)) do
    test(s"compact reversed levels return space to the graph at $cols x $rows") {
      val s = Snapshot
        .initial("XMR", now)
        .copy(book = Some(Book(Vector(Level("99", "10", 1)), Vector(Level("101", "10", 1)), now)))
      val frame = Workspace.draw(
        s,
        TradingView("XMR", message = "View only"),
        PanelView(),
        cols,
        rows,
        now,
        ZoneOffset.UTC,
        Style.mono
      )
      val header = frame.lines.head
      assert(header.indexOf("SHORT") < header.indexOf("MID"), header)
      assert(header.indexOf("MID") < header.indexOf("LONG"), header)
      val first = header.indexOf('|')
      assertEquals(first, cols - 39)
      val columns = header.drop(first + 1).split("\\|", -1)
      assertEquals(columns.map(_.length).toVector, Vector(12, 12, 12))
      assert(frame.lines.forall(_.length == cols))
      assertEquals(frame.lines.size, rows)
      val tradeTop = rows - 15
      assert(frame.lines(tradeTop).contains("VIEW XMR"), frame.lines.mkString("\n"))
      assert(frame.buttons.filter(_.action == DeskAction.Mode("PAPER")).forall(_.y > tradeTop))
    }
