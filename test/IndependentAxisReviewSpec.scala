package desk

import java.time.ZoneOffset

class IndependentAxisReviewSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val book = Book(Vector(Level("90", "1", 1)), Vector(Level("110", "1", 1)), now)
  private val view = TradingView("XMR", message = "Ready", ready = true)
  private val horizontal = Vector('⠉', '⠒', '⠤', '⣀')
  private val masks = Vector(Vector(1, 8), Vector(2, 16), Vector(4, 32), Vector(64, 128))

  private def outlook(price: Double): OutlookView =
    Outlook
      .empty("XMR")
      .copy(
        calculatedAt = now,
        columns = Outlook.horizons.map(h =>
          HorizonOutlook(
            h,
            OutlookDirection.Up,
            "ready",
            Some(92),
            Some(price),
            Some(price),
            Some(100),
            Some(now),
            HistoricalRate(60, 100, 100),
            HistoricalRate(35, 50, 50)
          )
        )
      )

  private def frame(mode: ChartMode, price: Double): WorkspaceFrame =
    val state = Snapshot
      .initial("XMR", now)
      .copy(
        book = Some(book),
        samples = Vector(now / 1000 -> price),
        candles = Map("1m" -> Vector(Candle(now, price, price, price, price, 1))),
        chartMode = mode,
        zoom = if mode == ChartMode.Candles then State.candleMinZoom else 0
      )
    Workspace.draw(
      state,
      view,
      PanelView(mode = "PAPER", outlook = outlook(price)),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono
    )

  private def cells(frame: WorkspaceFrame, row: Int): Vector[String] =
    Chart.plain(frame.lines(row)).drop(62).split("\\|", -1).toVector.map(_.trim)

  test("populated line and outlook ticks use the same terminal row and braille subrow") {
    // The 90/110 book fixes the padded scale at 89..111 with six price rows and 44 horizontal dots.
    for (price, row, subrow) <- Vector((110.5, 0, 1), (109.5, 0, 2), (108.5, 0, 3), (107.5, 1, 0))
    do
      val actual = frame(ChartMode.Line, price)
      val chart = actual.lines.slice(1, 7).map(Chart.plain).map(_.slice(30, 52))
      val ink = chart.zipWithIndex.flatMap { (line, r) =>
        line.zipWithIndex.flatMap { (ch, cell) =>
          if ch < 0x2800 || ch > 0x28ff then Vector.empty
          else
            for
              y <- (0 until 4).toVector
              x <- (0 until 2).toVector
              if ((ch.toInt - 0x2800) & masks(y)(x)) != 0
            yield (cell * 2 + x, r, y)
        }
      }
      assertEquals(ink, Vector((43, row, subrow)), s"line point for $price")
      assert(
        cells(actual, row + 1).forall(_.startsWith(horizontal(subrow).toString + "TR ")),
        s"price $price must align in frame row ${row + 1}, braille subrow $subrow:\n" +
          actual.lines.mkString("\n")
      )
      assertEquals(actual.lines.slice(1, 10).count(_.contains("TR ")), 1)
  }

  test("populated candles and outlook ticks use the same terminal row") {
    for (price, row) <- Vector((109.5, 0), (109.0, 0), (108.5, 1), (108.0, 1)) do
      val actual = frame(ChartMode.Candles, price)
      val chart = actual.lines.slice(1, 7).map(Chart.plain).map(_.slice(30, 52))
      assertEquals(chart.zipWithIndex.filter(_._1.trim.nonEmpty).map(_._2), Vector(row))
      assert(chart(row).contains("───"))
      assert(cells(actual, row + 1).forall(_.startsWith("─TR ")), actual.lines.mkString("\n"))
      assertEquals(actual.lines.slice(1, 10).count(_.contains("TR ")), 1)
  }

  private def levels(
      support: Double,
      target: Double,
      resistance: Double,
      mode: ChartMode = ChartMode.Line,
      displayedBook: Book = book
  ): WorkspaceFrame =
    val state = Snapshot.initial("XMR", now).copy(book = Some(displayedBook), chartMode = mode)
    val values = outlook(target).copy(columns =
      outlook(target).columns.map(
        _.copy(support = Some(support), target = Some(target), resistance = Some(resistance))
      )
    )
    Workspace.draw(
      state,
      view,
      PanelView(outlook = values),
      100,
      30,
      now,
      ZoneOffset.UTC,
      Style.mono
    )

  test("levels on different braille subrows keep separate ticks on one terminal row") {
    val actual = levels(109.5, 108.5, 108.5)
    for cell <- cells(actual, 1) do
      assert(cell.contains("⠤S"), cell)
      assert(cell.contains("⣀TR"), cell)
      assert(!cell.exists(_.isDigit), cell)
    assert(!actual.lines.slice(2, 10).exists(_.contains("TR")))
  }

  test("distinct levels on one braille subrow keep all labels without a false price") {
    val actual = levels(109.5, 109.4, 109.4)
    assert(cells(actual, 1).forall(_ == "⠤STR *"), actual.lines.mkString("\n"))
  }

  test("equal levels retain one precise value beside the common tick") {
    val actual = levels(109.5, 109.5, 109.5)
    assert(cells(actual, 1).forall(_ == "⠤STR 109.5"), actual.lines.mkString("\n"))
  }

  test("outside levels keep arrows and exact axis endpoints keep in-range ticks") {
    for mode <- ChartMode.values do
      val outside = levels(80, 130, 130, mode)
      assert(cells(outside, 1).forall(_ == "↑TR 130"), outside.lines.mkString("\n"))
      assert(cells(outside, 6).forall(_ == "↓S 80"), outside.lines.mkString("\n"))
      val edges = levels(89, 150, 111, mode)
      val upper = if mode == ChartMode.Line then "⠉R" else "─R"
      val lower = if mode == ChartMode.Line then "⣀S 89" else "─S 89"
      assert(cells(edges, 1).forall(c => c.contains(upper) && c.contains("↑T")))
      assert(cells(edges, 6).forall(_ == lower), edges.lines.mkString("\n"))
  }

  test("distant outside levels cannot wrap integer coordinates onto the opposite edge") {
    val narrow = Book(Vector(Level("99.9999999", "1", 1)), Vector(Level("100", "1", 1)), now)
    for mode <- ChartMode.values do
      val actual = levels(50, 110, 110, mode, narrow)
      assert(cells(actual, 1).forall(_ == "↑TR 110"), actual.lines.mkString("\n"))
      assert(cells(actual, 6).forall(_ == "↓S 50"), actual.lines.mkString("\n"))
  }
