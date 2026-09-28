package desk

import java.time.ZoneOffset

class AlignedOutlookSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val book = Book(Vector(Level("90", "1", 1)), Vector(Level("110", "1", 1)), now)
  private val market = Snapshot.initial("XMR", now).copy(book = Some(book))
  private val view = TradingView(
    "XMR",
    message = "Ready",
    ready = true,
    rules = Some(MarketRules(0, 2, 5)),
    bid = Some(BigDecimal(90)),
    ask = Some(BigDecimal(110))
  )
  private def outlook: OutlookView = OutlookView(
    "XMR",
    now,
    Outlook.horizons.zipWithIndex.map { (horizon, i) =>
      HorizonOutlook(
        horizon,
        OutlookDirection.Up,
        "ready",
        Some(92 + i * 2),
        Some(108 - i),
        Some(108 - i),
        Some(100),
        None,
        HistoricalRate(60, 100, 100),
        HistoricalRate(35, 50, 50)
      )
    }
  )
  private def draw(
      s: Snapshot = market,
      o: OutlookView = outlook,
      cols: Int = 100,
      rows: Int = 30,
      style: Style = Style.mono,
      panel: PanelView = PanelView()
  ): WorkspaceFrame =
    WorkspaceApp.drawFrame(
      s,
      view.copy(coin = s.coin),
      panel.copy(outlook = o),
      cols,
      rows,
      now,
      ZoneOffset.UTC,
      style,
      new OutlookWorker(() => s, () => now, _ => ())
    )
  private def cells(frame: WorkspaceFrame, row: Int): Vector[String] =
    Chart
      .plain(frame.lines(row))
      .drop(Layout.workspaceChartWidth(frame.cols) + 1)
      .split("\\|", -1)
      .toVector
      .map(_.trim)

  test("three level columns share the graph price rows at every supported size and mode") {
    for
      (cols, rows) <- Vector((100, 30), (120, 40), (160, 45))
      mode <- ChartMode.values
      style <- Vector(Style.mono, Style.colour)
    do
      val frame = draw(market.copy(chartMode = mode), cols = cols, rows = rows, style = style)
      val height = rows - 24
      assert(cells(frame, 0).zip(Outlook.horizons.reverse).forall((c, h) => c.contains(h.label)))
      for (column, i) <- outlook.columns.reverse.zipWithIndex do
        for (label, value) <- Vector("S" -> column.support.get, "TR" -> column.target.get) do
          // Book extent 90..110 plus the existing five percent padding: 89..111.
          val dot = math.floor((111 - value) / 22 * (height * 4 - 1) + 0.5).toInt
          val row = 1 + (if mode == ChartMode.Line then dot / 4
                         else math.floor((111 - value) / 22 * (height - 1) + 0.5).toInt)
          val tick = if mode == ChartMode.Line then Vector("⠉", "⠒", "⠤", "⣀")(dot % 4) else "─"
          assert(
            cells(frame, row)(i).startsWith(tick + label + " "),
            s"$cols x $rows $mode $label $value expected row $row: ${frame.lines.mkString("\n")}"
          )
      assertEquals(frame.lines.size, rows)
      assert(frame.lines.forall(line => Chart.plain(line).length == cols))
  }

  test("off-scale levels are marked as outside rather than silently placed on the edge") {
    val outside = outlook.copy(columns =
      outlook.columns.map(_.copy(support = Some(80), target = Some(130), resistance = Some(130)))
    )
    val frame = draw(o = outside)
    assert(cells(frame, 1).forall(_.startsWith("↑TR ")), frame.lines.mkString("\n"))
    assert(cells(frame, 6).forall(_.startsWith("↓S ")), frame.lines.mkString("\n"))
    assert("[─⠉⠒⠤⣀][STR]".r.findFirstIn(frame.lines.slice(1, 7).mkString).isEmpty)
  }

  test("distinct levels that share one row keep every label and no misleading single price") {
    val close = outlook.copy(columns =
      outlook.columns.map(
        _.copy(support = Some(100.1), target = Some(100.4), resistance = Some(100.4))
      )
    )
    val frame = draw(o = close)
    assert(cells(frame, 3).forall(_ == "⣀STR *"), frame.lines.mkString("\n"))
  }

  test("trade controls stay below the columns when selected size changes") {
    for coin <- Vector("XMR", "BTC", "PAXG") do
      val s = market.copy(coin = coin)
      val normal = draw(s, outlook.copy(coin = coin))
      val details = draw(s, outlook.copy(coin = coin), panel = PanelView(bigOrder = true))
      for action <- Vector(DeskAction.OrderSize(false), DeskAction.Mode("PAPER")) do
        val a = normal.buttons.find(_.action == action).get
        val b = details.buttons.find(_.action == action).get
        assertEquals((a.x, a.y, a.height), (b.x, b.y, b.height))
        assert(a.y >= 17)
        assert(
          Chart.plain(normal.lines(a.y - 1)).substring(a.x - 1).startsWith("[" + a.label + "]")
        )
      assertEquals(normal.lines.size, 30)
      assertEquals(details.lines.size, 30)
  }

  test("missing scale and missing levels never invent price marks") {
    val noScale = draw(market.copy(book = None), outlook)
    assert(noScale.lines.exists(_.contains("No prices")))
    val marker = "[─⠉⠒⠤⣀↑↓][STR]".r
    assert(marker.findFirstIn(noScale.lines.mkString).isEmpty)
    val empty = draw(o = Outlook.empty("XMR").copy(calculatedAt = now))
    assert(empty.lines.exists(_.contains("Waiting")))
    assert(marker.findFirstIn(empty.lines.mkString).isEmpty)
  }

  test("history, zoom and resize keep marks beside the displayed price labels") {
    val samples = Vector.tabulate(1200)(i => (now / 1000 - 1199 + i, 80.0 + i / 20.0))
    for
      width <- Vector(100, 120, 160)
      height <- Vector(30, 40)
      offset <- Vector(0L, 90L, 300L)
      zoom <- Vector(0, 2)
    do
      val s = market.copy(samples = samples, offsetSec = offset, zoom = zoom)
      val frame = draw(s, cols = width, rows = height)
      val priceRows = height - 24
      val labels =
        frame.lines.slice(1, 1 + priceRows).map(line => Chart.plain(line).take(8).trim.toDouble)
      for (column, index) <- outlook.columns.reverse.zipWithIndex do
        val price = column.target.get
        val nearest = math
          .floor(
            (labels.head - price) / (labels.head - labels.last) *
              (priceRows * 4 - 1) + 0.5
          )
          .toInt / 4
        assert(cells(frame, 1 + nearest)(index).contains("TR"), frame.lines.mkString("\n"))
  }

  test("reference panels cannot push the columns or trade controls beyond the screen") {
    for refs <- Vector(Map.empty[String, Ref], market.refs) do
      val frame = draw(market.copy(refs = refs))
      assertEquals(frame.lines.size, 30)
      assert(frame.lines.forall(_.length == 100))
      assert(frame.lines(15).contains("VIEW XMR"))
      assert(cells(frame, 0).head.contains("SHORT"))
  }

  test("edge prices stay in range beside an outside price sharing the edge row") {
    val levels = outlook.copy(columns =
      outlook.columns.map(_.copy(support = Some(89), resistance = Some(111), target = Some(150)))
    )
    val rows = OutlookScreen.rows(levels, 46, Some(Axis(89, 111, 9)), 9, ChartMode.Candles)
    assert(rows(1).split("\\|", -1).forall(_.trim == "─R ↑T"))
    assert(rows(9).split("\\|", -1).forall(_.trim == "─S 89"))
  }

  test("stale quotes retain descriptive levels but never active rates or direction") {
    val stale = draw(market.copy(book = Some(book.copy(time = now - 6000))))
    assert(!cells(stale, 0).exists(c => c.contains("↑") || c.contains("↓")))
    assert(cells(stale, 7).forall(_.contains("Stale price")))
    assert(cells(stale, 8).forall(_ == "Base --"))
    assert(cells(stale, 9).forall(_ == "Match --"))
    assert(stale.lines.slice(1, 7).exists(_.contains("TR ")))
  }
