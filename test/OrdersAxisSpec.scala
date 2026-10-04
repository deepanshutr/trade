package desk

class OrdersAxisSpec extends munit.FunSuite:
  private def d(value: String): BigDecimal = BigDecimal(value)
  private val xmr = MarketRules(7, 3, 5)
  private val btc = MarketRules(1, 5, 40)
  private val micro = MarketRules(9, 0, 5)
  private val style = Style.colour
  private val reset = Screen.reset
  private val (lo, hi) = (543.60, 552.40)
  private def order(
      id: String,
      side: Side,
      price: String,
      size: String,
      cancelling: Boolean = false
  ): RestingOrder =
    RestingOrder(id, "XMR", 7, side, d(price), d(size), d(size), false, 1L, cancelling)
  private val sample = Vector(
    order("s1", Side.Sell, "552.00", "0.1"),
    order("b1", Side.Buy, "546.00", "0.2"),
    order("b2", Side.Buy, "544.00", "0.1")
  )
  private val sellTicket = Some((Side.Sell, d("546.10"), d("0.02")))
  private def place(
      rows: Int = 9,
      current: Option[BigDecimal] = Some(d("546.09")),
      entry: Option[(BigDecimal, BigDecimal)] = None,
      resting: Vector[RestingOrder] = sample,
      ticket: Option[(Side, BigDecimal, BigDecimal)] = sellTicket,
      leverage: Option[Int] = Some(5),
      low: Double = lo,
      high: Double = hi
  ): Placed =
    OrdersAxis.marks(current, entry, resting, ticket, leverage, low, high, rows)
  private def draw(
      placed: Placed,
      focus: Option[Int] = None,
      rows: Int = 9,
      stale: Boolean = false,
      positionSide: Option[Side] = None,
      rules: MarketRules = xmr,
      low: Double = lo,
      high: Double = hi
  ): Vector[String] =
    OrdersAxis.lines(placed, focus, rules, low, high, rows, stale, positionSide, style)
  private def seen(placed: Placed): Vector[(BigDecimal, AxisKind, Int, Char)] =
    placed.marks.map(m => (m.price, m.kind, m.row, m.tie))

  test("nine rows place the sample marks around the current price") {
    val placed = place()
    assertEquals(
      seen(placed),
      Vector(
        (d("552.00"), AxisKind.Sell, 0, ' '),
        (d("546.10"), AxisKind.Ticket, 5, '\''),
        (d("546.09"), AxisKind.Current, 6, '>'),
        (d("546.00"), AxisKind.Buy, 7, ','),
        (d("544.00"), AxisKind.Buy, 8, ' ')
      )
    )
    assertEquals(placed.marks.map(_.trueRow), Vector(0, 6, 6, 6, 8))
    assertEquals((placed.above, placed.below), (0, 0))
  }

  test("nineteen rows place the sample marks on rows 0, 12, 13, 14 and 18") {
    val placed = place(rows = 19)
    assertEquals(placed.marks.map(_.row), Vector(0, 12, 13, 14, 18))
    assertEquals(placed.marks.map(_.tie), Vector(' ', '\'', '>', ',', ' '))
    assertEquals(placed.marks.map(_.trueRow), Vector(0, 13, 13, 13, 18))
  }

  test("the farthest marks above are left off and counted when the rows run out") {
    val sells = Vector("550.10", "550.20", "550.30", "552.00").zipWithIndex.map((p, i) =>
      order(s"s$i", Side.Sell, p, "0.1")
    )
    val placed = place(current = Some(d("550.00")), resting = sells, ticket = None)
    assertEquals(
      seen(placed),
      Vector(
        (d("550.20"), AxisKind.Sell, 0, '\''),
        (d("550.10"), AxisKind.Sell, 1, '\''),
        (d("550.00"), AxisKind.Current, 2, '>')
      )
    )
    assertEquals((placed.above, placed.below), (2, 0))
  }

  test("the farthest marks below are left off and counted when the rows run out") {
    val buys = Vector("546.00", "545.90", "545.80", "544.00").zipWithIndex.map((p, i) =>
      order(s"b$i", Side.Buy, p, "0.1")
    )
    val placed = place(resting = buys, ticket = None)
    assertEquals(
      seen(placed),
      Vector(
        (d("546.09"), AxisKind.Current, 6, '>'),
        (d("546.00"), AxisKind.Buy, 7, ','),
        (d("545.90"), AxisKind.Buy, 8, ',')
      )
    )
    assertEquals((placed.above, placed.below), (0, 2))
  }

  test("without a current price the marks go down from the top in one pass") {
    val placed =
      place(current = None, resting = sample :+ order("b3", Side.Buy, "545.00", "0.1"))
    assertEquals(
      seen(placed),
      Vector(
        (d("552.00"), AxisKind.Sell, 0, ' '),
        (d("546.10"), AxisKind.Ticket, 6, ' '),
        (d("546.00"), AxisKind.Buy, 7, ','),
        (d("545.00"), AxisKind.Buy, 8, ',')
      )
    )
    assertEquals((placed.above, placed.below), (0, 1))
  }

  test("orders at one price on one side make one mark with the amounts summed") {
    val resting = Vector(
      order("b1", Side.Buy, "546.00", "0.1"),
      order("b2", Side.Buy, "544.00", "0.1"),
      order("b3", Side.Buy, "546.00", "0.1", cancelling = true)
    )
    val placed = place(resting = resting, ticket = None)
    val buys = placed.marks.filter(_.kind == AxisKind.Buy)
    assertEquals(buys.map(_.price), Vector(d("546.00"), d("544.00")))
    assertEquals(buys.map(_.ids), Vector(Vector("b1", "b3"), Vector("b2")))
    assertEquals(buys.map(_.amount), Vector(Some(d("21.84")), Some(d("10.88"))))
    assertEquals(buys.map(_.cancelling), Vector(true, false))
    assertEquals(buys.map(_.side), Vector(Some(Side.Buy), Some(Side.Buy)))
    assertEquals(placed.marks.count(_.kind == AxisKind.Current), 1)
  }

  test("the entry and the ticket each make one mark with their money put up") {
    val placed = place(entry = Some((d("548.00"), d("-0.2"))))
    val entry = placed.marks.filter(_.kind == AxisKind.Entry)
    assertEquals(
      entry.map(m => (m.price, m.amount, m.row)),
      Vector((d("548.00"), Some(d("21.92")), 4))
    )
    val ticket = placed.marks.filter(_.kind == AxisKind.Ticket)
    assertEquals(ticket.map(m => (m.amount, m.side)), Vector((Some(d("2.18")), Some(Side.Sell))))
    assertEquals(place(ticket = None).marks.count(_.kind == AxisKind.Ticket), 0)
    assertEquals(place().marks.count(_.kind == AxisKind.Entry), 0)
  }

  test("a buy at the current price sits below it and a sell at it sits above it") {
    val resting = Vector(
      order("b1", Side.Buy, "546.09", "0.183"),
      order("s1", Side.Sell, "546.09", "0.1")
    )
    val placed = place(resting = resting, ticket = None)
    assertEquals(
      seen(placed),
      Vector(
        (d("546.09"), AxisKind.Sell, 5, '\''),
        (d("546.09"), AxisKind.Current, 6, '>'),
        (d("546.09"), AxisKind.Buy, 7, ',')
      )
    )
    assertEquals(draw(placed)(7), style.bid + " $19.99   546.09," + reset)
  }

  test("lines draw the nine row case with amounts, prices, ties and colours") {
    val rows = draw(place())
    assertEquals(rows.size, 9)
    assertEquals(rows(0), style.ask + " $11.04   552.00 " + reset)
    assertEquals(rows(5), style.ask + Screen.bold + "  $2.18   546.10'" + reset)
    assertEquals(rows(6), Screen.bold + "          546.09>" + reset)
    assertEquals(rows(7), style.bid + " $21.84   546.00," + reset)
    assertEquals(rows(8), style.bid + " $10.88   544.00 " + reset)
    assert(rows.forall(r => Chart.plain(r).length == 17), rows)
    assert(!rows.exists(_.contains(OrdersAxis.inverse)), rows)
  }

  test("free rows show a faint middle price on every third free row and the last") {
    val rows = draw(place())
    assertEquals(rows(1), " " * 17)
    assertEquals(rows(2), Screen.dim + "          549.96 " + reset)
    assertEquals(rows(3), " " * 17)
    assertEquals(rows(4), Screen.dim + "          548.00 " + reset)
    val bare = draw(place(resting = Vector.empty, ticket = None, current = None))
    val labelled = bare.zipWithIndex.collect { case (r, i) if r.trim.nonEmpty => i }
    assertEquals(labelled, Vector(1, 4, 7, 8))
  }

  test("the entry is bold in the position's side colour") {
    val placed = place(entry = Some((d("548.00"), d("0.2"))))
    assertEquals(
      draw(placed, positionSide = Some(Side.Buy))(4),
      style.bid + Screen.bold + " $21.92   548.00 " + reset
    )
    assertEquals(
      draw(placed, positionSide = Some(Side.Sell))(4),
      style.ask + Screen.bold + " $21.92   548.00 " + reset
    )
  }

  test("a buy ticket is bold in the bid colour") {
    val placed = place(ticket = Some((Side.Buy, d("546.00"), d("0.1"))), resting = Vector.empty)
    assertEquals(draw(placed)(7), style.bid + Screen.bold + " $10.92   546.00," + reset)
  }

  test("an unknown leverage reads -- on every amount and nothing throws") {
    val placed = place(leverage = None, entry = Some((d("548.00"), d("0.2"))))
    assert(placed.marks.forall(_.amount.isEmpty), placed)
    val rows = draw(placed, positionSide = Some(Side.Buy))
    val marked = placed.marks.filter(_.kind != AxisKind.Current).map(m => Chart.plain(rows(m.row)))
    assertEquals(marked.size, 5)
    assert(marked.forall(_.take(7) == "     --"), marked)
    assertEquals(Chart.plain(rows(6)).take(7), " " * 7)
    assertEquals(place(leverage = Some(0)).marks.flatMap(_.amount), Vector.empty)
  }

  test("amounts are dim while stale or while the row is cancelling") {
    val stale = draw(place(), stale = true)
    assertEquals(
      stale(7),
      style.bid + Screen.dim + " $21.84" + reset + style.bid + "   546.00," + reset
    )
    assertEquals(
      stale(5),
      style.ask + Screen.dim + "  $2.18" + reset + style.ask + Screen.bold + "   546.10'" + reset
    )
    val cancelling = sample.map(o => if o.id == "b1" then o.copy(cancelling = true) else o)
    val rows = draw(place(resting = cancelling))
    assertEquals(
      rows(7),
      style.bid + Screen.dim + " $21.84" + reset + style.bid + "   546.00," + reset
    )
    assertEquals(rows(8), style.bid + " $10.88   544.00 " + reset)
  }

  test("the focused selectable row is inverse and bold") {
    val placed = place()
    val rows = draw(placed, focus = Some(1))
    assertEquals(OrdersAxis.inverse, Screen.esc + "[7m")
    assertEquals(rows(7), OrdersAxis.inverse + Screen.bold + " $21.84   546.00," + reset)
    assertEquals(rows.count(_.contains(OrdersAxis.inverse)), 1)
    assertEquals(
      draw(placed, focus = Some(0))(0),
      OrdersAxis.inverse + Screen.bold + " $11.04   552.00 " + reset
    )
    assertEquals(draw(placed, focus = Some(9)), draw(placed))
  }

  test("prices use the market's tick decimals") {
    val buy = Vector(order("b1", Side.Buy, "99000", "0.001"))
    val placed = place(
      current = Some(d("99010")),
      resting = buy,
      ticket = None,
      leverage = Some(40),
      low = 98000,
      high = 100000
    )
    val rows = draw(placed, rules = btc, low = 98000, high = 100000)
    val plain = rows.map(Chart.plain)
    assertEquals(plain(4), " " * 11 + "99010>")
    assertEquals(plain(5), "  $2.48    99000,")
    assert(plain.forall(!_.drop(8).contains(".")), plain)
    val tiny = place(
      current = Some(d("0.012350")),
      resting = Vector(order("b1", Side.Buy, "0.012345", "1000")),
      ticket = None,
      low = 0.0123,
      high = 0.0124
    )
    val small = draw(tiny, rules = micro, low = 0.0123, high = 0.0124).map(Chart.plain)
    assertEquals(small(4), " " * 8 + "0.012350>")
    assertEquals(small(5), "  $2.47 0.012345,")
  }

  test("selectable holds the order and entry marks in row order") {
    val placed = place(entry = Some((d("548.00"), d("0.2"))))
    val chosen = OrdersAxis.selectable(placed)
    assertEquals(
      chosen.map(m => (m.kind, m.row)),
      Vector((AxisKind.Sell, 0), (AxisKind.Entry, 4), (AxisKind.Buy, 7), (AxisKind.Buy, 8))
    )
  }

  test("start is the order mark nearest the current price, never the entry") {
    val placed = place()
    val start = OrdersAxis.start(placed)
    assertEquals(start.map(OrdersAxis.selectable(placed)(_).price), Some(d("546.00")))
    val near = place(entry = Some((d("546.08"), d("0.2"))))
    assertEquals(
      OrdersAxis.start(near).map(OrdersAxis.selectable(near)(_).kind),
      Some(AxisKind.Buy)
    )
    val bare = place(resting = Vector.empty, entry = Some((d("548.00"), d("0.2"))))
    assertEquals(OrdersAxis.start(bare), None)
  }

  test("start takes the order mark above on a tie") {
    val resting = Vector(
      order("b1", Side.Buy, "546.00", "0.1"),
      order("s1", Side.Sell, "546.18", "0.1")
    )
    val placed = place(resting = resting, ticket = None)
    assertEquals(
      OrdersAxis.start(placed).map(OrdersAxis.selectable(placed)(_).price),
      Some(d("546.18"))
    )
  }

  test("held lists every price the range must hold") {
    assertEquals(
      OrdersAxis.held(Some(d("546.09")), Some(d("548.00")), sample, Some(d("546.10"))),
      Vector(546.09, 548.0, 552.0, 546.0, 544.0, 546.10)
    )
    assertEquals(OrdersAxis.held(None, None, Vector.empty, None), Vector.empty)
  }

  test("no rows and an unusable range draw nothing wrong and throw nothing") {
    val none = place(rows = 0)
    assertEquals(none.marks, Vector.empty)
    assertEquals((none.above, none.below), (2, 2))
    assertEquals(draw(none, rows = 0), Vector.empty)
    val odd = place(low = Double.NaN, high = Double.NaN)
    val rows = draw(odd, low = Double.NaN, high = Double.NaN)
    assertEquals(rows.size, 9)
    assert(rows.forall(r => Chart.plain(r).length == 17), rows)
  }
