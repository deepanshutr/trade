package desk

class RailsSpec extends munit.FunSuite:
  private def dots(cells: Int, at: Map[Int, Char] = Map.empty): String =
    (0 until cells).map(i => at.getOrElse(i, if i == (cells - 1) / 2 then '|' else '·')).mkString
  private def sized(label: Int, values: Int): RailGroup =
    RailGroup(Vector.empty, label, -1, 1, Vector("down", "neutral", "up"), values, Vector.empty)
  private val four = Vector(sized(4, 13), sized(10, 21), sized(10, 21), sized(10, 21))
  private def row(label: String, values: Vector[String], marks: Vector[RailMark] = Vector.empty) =
    RailRow(label, marks, values.map(_ -> ""))
  private val book = RailGroup(
    Vector(("◆ book", ""), ("● tape", ""), ("share of buying", "")),
    4,
    0,
    100,
    Vector("0%", "50%", "100%"),
    13,
    Vector(
      row("5m", Vector("B 62%", "T 55%"), Vector(RailMark(62, '◆', ""), RailMark(55, '●', "")))
    )
      ++ Vector("15m", "1h", "4h", "24h").map(row(_, Vector("B --", "T --")))
  )
  private val market = RailGroup(
    Vector(("XMR", ""), ("▲ change", ""), ("◇ vs average", ""), ("● Polymarket up", "")),
    10,
    -1,
    1,
    Vector("down", "neutral", "up"),
    21,
    Vector(
      row(
        "5m  ±0.5%",
        Vector("▲+0.12%", "◇+0.05%", "●--"),
        Vector(RailMark(0.24, '▲', ""), RailMark(0.1, '◇', ""))
      )
    )
      ++ Vector("15m ±1%", "1h  ±2%", "4h  ±3%", "24h ±6%").map(row(_, Vector("▲--", "◇--", "●--")))
  )

  test("the fit rule") {
    val expected = Vector(
      230 -> (4, 25),
      198 -> (4, 17),
      191 -> (4, 17),
      190 -> (3, 33),
      139 -> (3, 17),
      138 -> (2, 41),
      87 -> (2, 17),
      86 -> (1, 41),
      81 -> (1, 41),
      61 -> (1, 41),
      35 -> (1, 17),
      34 -> (0, 0),
      2000 -> (4, 41)
    )
    expected.foreach { case (width, (count, w)) =>
      val (kept, got) = Rails.fit(four, width)
      assertEquals((kept, got), (four.take(count), w), clue(width))
    }
    assertEquals(Rails.fit(Vector.empty, 230), (Vector.empty, 0))
  }

  test("a rail with no marks is faint dots and a centre bar") {
    assertEquals(
      Rails.rail(17, -1, 1, Vector.empty, Screen.bold),
      Screen.dim + dots(17) + Screen.reset
    )
  }

  test("a marker takes the nearest cell") {
    def at(v: Double) = Rails.rail(17, -1, 1, Vector(RailMark(v, '▲', "")), Screen.bold)
    assertEquals(Chart.plain(at(0.06)), dots(17, Map(8 -> '▲')))
    assertEquals(Chart.plain(at(0.07)), dots(17, Map(9 -> '▲')))
    assertEquals(Chart.plain(at(1.0)), dots(17, Map(16 -> '▲')))
    assert(!at(1.0).contains(Screen.inverse))
  }

  test("a clamped marker is inverse at the end cell") {
    def one(v: Double) = Rails.rail(17, -1, 1, Vector(RailMark(v, '▲', "X")), Screen.bold)
    assertEquals(
      one(1.5),
      Screen.dim + "········|·······" + Screen.reset + Screen.inverse + "X▲" + Screen.reset
    )
    assertEquals(
      one(-1.2),
      Screen.inverse + "X▲" + Screen.reset + Screen.dim + "·······|········" + Screen.reset
    )
    assertEquals(one(1.0), Screen.dim + "········|·······" + Screen.reset + "X▲" + Screen.reset)
    assertEquals(one(-1.0), "X▲" + Screen.reset + Screen.dim + "·······|········" + Screen.reset)
  }

  test("two marks on one cell show ◉ in the overlap ink") {
    assertEquals(
      Rails.rail(17, -1, 1, Vector(RailMark(0.0, '▲', "X"), RailMark(0.01, '◇', "Y")), Screen.bold),
      Screen.dim + "········" + Screen.reset + Screen.bold + "◉" + Screen.reset + Screen.dim +
        "········" + Screen.reset
    )
    assert(
      Rails
        .rail(17, -1, 1, Vector(RailMark(1.5, '▲', "X"), RailMark(1.0, '◆', "Y")), Screen.bold)
        .contains(Screen.inverse + Screen.bold + "◉")
    )
  }

  test("zero sits on the centre bar at every odd width") {
    for w <- 17 to 41 by 2 do
      assertEquals(
        Chart.plain(Rails.rail(w, -1, 1, Vector(RailMark(0.0, '▲', "")), Screen.bold)),
        dots(w, Map((w - 1) / 2 -> '▲')),
        clue(w)
      )
      assertEquals(
        Chart.plain(Rails.rail(w, 0, 100, Vector(RailMark(50.0, '◆', "")), Screen.bold)),
        dots(w, Map((w - 1) / 2 -> '◆')),
        clue(w)
      )
    assertEquals(
      Chart.plain(
        Rails.rail(41, 0, 100, Vector(RailMark(62, '◆', ""), RailMark(55, '●', "")), Screen.bold)
      ),
      dots(41, Map(22 -> '●', 25 -> '◆'))
    )
  }

  test("two groups at 87") {
    val raw = Rails.lines(Vector(book, market), 87, "■ XMR  ■ buy", Screen.bold)
    assertEquals(
      raw.map(Chart.plain),
      Vector(
        "◆ book  ● tape  share of buying" + " " * 7 + "XMR  ▲ change  ◇ vs average  ● Polymarket up" + " " * 5,
        "    0%" + " " * 5 + "50%" + " " * 3 + "100%" + " " * 27 + "down" + " " + "neutral" + " " * 3 + "up" + " " * 22,
        "5m  " + dots(17, Map(9 -> '●', 10 -> '◆')) + " B 62% T 55%" + " " * 5 +
          "5m  ±0.5% " + dots(17, Map(9 -> '◇', 10 -> '▲')) + " ▲+0.12% ◇+0.05% ●--" + " " * 2
      ) ++ Vector("15m ", "1h  ", "4h  ", "24h ")
        .zip(Vector("15m ±1%   ", "1h  ±2%   ", "4h  ±3%   ", "24h ±6%   "))
        .map { (b, m) =>
          b + dots(17) + " B -- T --" + " " * 7 + m + dots(17) + " ▲-- ◇-- ●--" + " " * 10
        } ++ Vector.fill(4)(" " * 87) :+ ("■ XMR  ■ buy" + " " * 75)
    )
    assert(raw(2).startsWith(Screen.dim + "5m  " + Screen.reset))
    assert(raw(1).contains(Screen.dim + "50%" + Screen.reset))
  }

  test("a group that does not fit is not drawn") {
    val narrow = Rails.lines(Vector(book, market), 86, "", Screen.bold).map(Chart.plain)
    assert(!narrow.exists(r => r.contains("XMR") || r.contains("▲")))
    assertEquals(narrow(0), "◆ book  ● tape  share of buying" + " " * 55)
    assertEquals(
      narrow(2),
      "5m  " + dots(41, Map(22 -> '●', 25 -> '◆')) + " B 62% T 55%" + " " * 29
    )
  }

  test("a title is cut at its group's width") {
    val long = market.copy(title =
      Vector(("ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMN", ""), ("▲ change", ""))
    )
    assertEquals(
      Chart.plain(Rails.lines(Vector(book, long), 87, "", Screen.bold)(0)),
      "◆ book  ● tape  share of buying" + " " * 7 + "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMN  ▲ chang"
    )
    assertEquals(
      Chart.plain(Rails.lines(Vector(long, book), 87, "", Screen.bold)(0)),
      "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMN  ▲ chang" + " " * 3 +
        "◆ book  ● tape  share of buying" + " " * 4
    )
  }

  test("long values run into the gap") {
    val market2 = market.copy(rows =
      market.rows.updated(
        0,
        market.rows(0).copy(values = Vector("▲+10.00%", "◇+10.00%", "●100%").map(_ -> ""))
      )
    )
    assertEquals(
      Chart.plain(Rails.lines(Vector(market2, book), 87, "", Screen.bold)(2)).slice(28, 52),
      "▲+10.00% ◇+10.00% ●100% "
    )
  }

  test("twelve rows of the width") {
    for width <- Vector(61, 81, 87, 198, 230) do
      val rows = Rails.lines(Vector(book, market), width, "", Screen.bold).map(Chart.plain)
      assertEquals(rows.map(_.length), Vector.fill(12)(width), clue(width))
    assertEquals(
      Rails.lines(Vector.empty, 61, "■ XMR", Screen.bold).map(Chart.plain),
      Vector.fill(11)(" " * 61) :+ ("■ XMR" + " " * 56)
    )
    assertEquals(
      Rails.lines(four, 230, "", Screen.bold).drop(2).map(Chart.plain),
      Vector.fill(10)(" " * 230)
    )
    val six = market.copy(rows = market.rows :+ row("1w", Vector("▲--")))
    assertEquals(
      Rails.lines(Vector(book, six), 87, "", Screen.bold).slice(7, 11).map(Chart.plain),
      Vector.fill(4)(" " * 87)
    )
  }

  test("inks reach the output") {
    val inked = book.copy(title = book.title.updated(0, ("◆ book", Style.colour.book)))
    assert(
      Rails
        .lines(Vector(inked), 87, "", Screen.bold)(0)
        .startsWith(Style.colour.book + "◆ book" + Screen.reset)
    )
    assertEquals(
      Rails.lines(Vector(book), 87, "", Screen.bold)(0),
      "◆ book  ● tape  share of buying" + " " * 56
    )
  }

  test("value and mark inks reach the rows") {
    val ink = Style.colour.book
    val inked = book.copy(rows =
      book.rows.updated(
        0,
        book
          .rows(0)
          .copy(
            marks = Vector(RailMark(62, '◆', ink)),
            values = Vector(("B 62%", ink), ("T --", ""))
          )
      )
    )
    val raw = Rails.lines(Vector(inked), 87, "", Screen.bold)(2)
    assert(raw.contains(ink + "◆" + Screen.reset), raw)
    assert(raw.contains(" " + ink + "B 62%" + Screen.reset + " T --"), raw)
  }

  test("the overlap ink reaches the rails") {
    val ink = Screen.esc + "[35m"
    val both = book.copy(rows =
      book.rows.updated(
        0,
        book.rows(0).copy(marks = Vector(RailMark(50, '◆', ""), RailMark(50.5, '●', "")))
      )
    )
    assert(Rails.lines(Vector(both), 87, "", ink)(2).contains(ink + "◉" + Screen.reset))
  }

  test("text past the width is cut at the width") {
    val wide = market.copy(rows =
      market.rows.updated(
        0,
        market.rows(0).copy(values = Vector("▲+10.00%", "◇+10.00%", "●100%").map(_ -> ""))
      )
    )
    val rows = Rails.lines(Vector(book, wide), 87, "■ " * 50, Screen.bold).map(Chart.plain)
    assertEquals(
      rows(2).drop(38),
      "5m  ±0.5% " + dots(17, Map(9 -> '◇', 10 -> '▲')) + " ▲+10.00% ◇+10.00% ●10"
    )
    assertEquals(rows(11), ("■ " * 50).take(87))
    val long = book.copy(rows = Vector(row("abcdef", Vector("B --"))))
    assertEquals(
      Chart.plain(Rails.lines(Vector(long), 87, "", Screen.bold)(2)).take(46),
      "abcd" + dots(41) + " "
    )
  }
