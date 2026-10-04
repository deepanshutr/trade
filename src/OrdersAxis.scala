package desk

enum AxisKind:
  case Current, Entry, Buy, Sell, Ticket

// One row of the orders axis. The side is the side of the orders or the ticket behind a mark,
// so the ticket row can take its side's colour. The current price and the entry carry none.
final case class AxisMark(
    price: BigDecimal,
    amount: Option[BigDecimal],
    kind: AxisKind,
    ids: Vector[String],
    row: Int,
    trueRow: Int,
    tie: Char,
    cancelling: Boolean,
    side: Option[Side] = None
)

// The marks on the axis and the counts left off above and below the current price.
final case class Placed(marks: Vector[AxisMark], above: Int, below: Int)

object OrdersAxis:
  val inverse: String = Screen.esc + "[7m"

  def held(
      current: Option[BigDecimal],
      entry: Option[BigDecimal],
      resting: Vector[RestingOrder],
      ticket: Option[BigDecimal]
  ): Vector[Double] =
    (current.toVector ++ entry ++ resting.map(_.price) ++ ticket).map(_.toDouble)

  def marks(
      current: Option[BigDecimal],
      entry: Option[(BigDecimal, BigDecimal)],
      resting: Vector[RestingOrder],
      ticket: Option[(Side, BigDecimal, BigDecimal)],
      leverage: Option[Int],
      lo: Double,
      hi: Double,
      rows: Int
  ): Placed =
    def mark(
        price: BigDecimal,
        amount: Option[BigDecimal],
        kind: AxisKind,
        side: Option[Side] = None,
        ids: Vector[String] = Vector.empty,
        cancelling: Boolean = false
    ): AxisMark =
      val row = Math.floor((hi - price.toDouble) / (hi - lo) * rows).toInt.max(0).min(rows - 1)
      AxisMark(price, amount, kind, ids, row, row, ' ', cancelling, side)
    val orders = Vector(Side.Buy -> AxisKind.Buy, Side.Sell -> AxisKind.Sell).flatMap {
      (side, kind) =>
        resting.filter(_.side == side).groupBy(_.price).toVector.map { (price, group) =>
          val amount = group.foldLeft(Option(BigDecimal(0))) { (sum, o) =>
            sum.zip(Trading.money(o.remaining, o.price, leverage)).map(_ + _)
          }
          mark(price, amount, kind, Some(side), group.map(_.id), group.exists(_.cancelling))
        }
    }
    val others = entry.toVector.map((price, size) =>
      mark(price, Trading.money(size.abs, price, leverage), AxisKind.Entry)
    ) ++ orders ++ ticket.map((side, price, quantity) =>
      mark(price, Trading.money(quantity, price, leverage), AxisKind.Ticket, Some(side))
    )
    current match
      case None =>
        val (down, below) = pass(others.sortBy(-_.price), -1, up = false, rows)
        Placed(down, 0, below)
      case Some(c) =>
        val centre = mark(c, None, AxisKind.Current).copy(tie = '>')
        val (upper, lower) =
          others.partition(m => m.price > c || m.price == c && m.side.contains(Side.Sell))
        val (up, above) = pass(upper.sortBy(_.price), centre.row, up = true, rows)
        val (down, below) = pass(lower.sortBy(-_.price), centre.row, up = false, rows)
        val shown = (up :+ centre) ++ down
        Placed(shown.filter(m => m.row >= 0 && m.row < rows).sortBy(_.row), above, below)

  // Places the marks, nearest first, on one side of row `from`. The side keeps as many marks as
  // it has rows and counts the rest. Distances count rows outward from `from`, so one rule serves
  // both sides. Going out, each mark takes its true row or one beyond the mark before it. Coming
  // back from the farthest, each mark stays inside the edge and one nearer than the mark after it.
  private def pass(
      marks: Vector[AxisMark],
      from: Int,
      up: Boolean,
      rows: Int
  ): (Vector[AxisMark], Int) =
    val step = if up then -1 else 1
    val room = if up then from else rows - 1 - from
    val kept = marks.take(room.max(0))
    val outward = kept.scanLeft(0)((previous, m) => ((m.trueRow - from) * step).max(previous + 1))
    val inward = outward.tail.scanRight(room + 1)((distance, next) => distance.min(next - 1))
    val placed = kept.zip(inward).map { (m, distance) =>
      val row = from + step * distance
      val tie = if row == m.trueRow then ' ' else if row > m.trueRow then ',' else '\''
      m.copy(row = row, tie = tie)
    }
    (placed, marks.size - kept.size)

  def lines(
      placed: Placed,
      focus: Option[Int],
      rules: MarketRules,
      lo: Double,
      hi: Double,
      rows: Int,
      stale: Boolean,
      positionSide: Option[Side],
      style: Style
  ): Vector[String] =
    def colour(side: Option[Side]): String =
      side.fold("")(s => if s == Side.Buy then style.bid else style.ask)
    def right(text: String): String = " " * (8 - text.length).max(0) + text
    val byRow = placed.marks.map(m => m.row -> m).toMap
    val chosen = focus.flatMap(selectable(placed).lift).map(_.row)
    val free = (0 until rows).filterNot(byRow.contains)
    val labelled = free.zipWithIndex.collect {
      case (row, i) if i % 3 == 1 || i == free.size - 1 => row
    }.toSet
    (0 until rows).toVector.map { row =>
      byRow.get(row) match
        case Some(m) =>
          val amount = if m.kind == AxisKind.Current then " " * 7 else Trading.dollars(m.amount, 7)
          val rest = " " + right(Trading.priceText(m.price, rules)) + m.tie
          val ink = colour(if m.kind == AxisKind.Entry then positionSide else m.side)
          val weight = if m.kind == AxisKind.Buy || m.kind == AxisKind.Sell then "" else Screen.bold
          if chosen.contains(row) then inverse + Screen.bold + amount + rest + Screen.reset
          else if stale || m.cancelling then
            style.wrap(ink + Screen.dim, amount) + style.wrap(ink + weight, rest)
          else style.wrap(ink + weight, amount + rest)
        case None =>
          val middle = hi - (row + 0.5) * (hi - lo) / rows
          if labelled(row) && java.lang.Double.isFinite(middle) then
            val price = right(Trading.priceText(BigDecimal(middle), rules))
            style.wrap(Screen.dim, " " * 8 + price + " ")
          else " " * 17
    }

  def selectable(placed: Placed): Vector[AxisMark] =
    placed.marks
      .filter(m => m.kind == AxisKind.Buy || m.kind == AxisKind.Sell || m.kind == AxisKind.Entry)
      .sortBy(_.row)

  def start(placed: Placed): Option[Int] =
    val current = placed.marks.find(_.kind == AxisKind.Current).map(_.price)
    selectable(placed).zipWithIndex
      .filter((m, _) => m.kind != AxisKind.Entry)
      .minByOption((m, _) => (current.fold(BigDecimal(0))(c => (m.price - c).abs), m.row))
      .map(_._2)
