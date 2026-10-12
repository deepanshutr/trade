package desk

class ScreenSpec extends munit.FunSuite:
  private def market(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): Vector[String] =
    val r = Layout.regions(cols, rows)
    Screen
      .renderMarket(s, now, zone, style, r, AxisDecor.plain, blank(r))
      .lines
  // The band rows the chart block leaves for the band, blank across the chart's width.
  private def blank(r: Regions): Vector[String] = Vector.fill(r.band)(" " * (r.left + r.plot))
  val t0 = 1_700_000_000_000L
  val zone = java.time.ZoneId.of("UTC")
  def lv(px: String, sz: String) = Level(px, sz, 1)
  private def plain(l: String): String = Chart.plain(l)

  test("the escape sequences are built from the escape character") {
    assertEquals(Screen.esc, 27.toChar.toString)
    assertEquals(Screen.bold, Screen.esc + "[1m")
    assertEquals(Screen.dim, Screen.esc + "[2m")
    assertEquals(Screen.reset, Screen.esc + "[0m")
    assertEquals(Screen.inverse, Screen.esc + "[7m")
  }
  test("rounding is half up") {
    assertEquals(Screen.round(0.5), 1L)
    assertEquals(Screen.round(-0.5), 0L)
    assertEquals(Screen.round(2.49), 2L)
  }
  test("the stats line shows the market first, the figures, the ping, bad and the prompt") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot.initial("XMR", t0).copy(ctx = Some(ctx), feed = Health.Live(t0 - 300))
    val line = Screen.stats(s, t0, 110)
    assertEquals(plain(line).length, 110)
    assert(line.startsWith(Screen.bold + "XMR" + Screen.reset + "  mid 522.26"), line)
    assert(plain(line).endsWith("ping 0.3s "), line)
    assert(line.contains(Screen.dim + "ping 0.3s" + Screen.reset), line)
    assert(line.contains("mark 522.28") && line.contains("oracle 521.515"), line)
    val down = s.copy(feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val staleLine = Screen.stats(down, t0, 110)
    assertEquals(plain(staleLine).length, 110)
    assert(plain(staleLine).endsWith("ping down bad 3 "), staleLine)
    assert(Screen.stats(s.copy(prompt = Some("BT")), t0, 110).contains("market: BT_"))
    val bare = Snapshot.initial("XMR", t0).copy(feed = Health.Live(t0))
    assert(plain(Screen.stats(bare, t0, 110)).startsWith("XMR  mid unavailable"))
    assert(plain(Screen.stats(bare, t0, 110)).endsWith("ping 0.0s "))
  }

  test("the stats line prints the book mid, falling back to the context mid without a book") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val book = Book(Vector(lv("500.10", "1")), Vector(lv("500.20", "1")), t0)
    val s = Snapshot.initial("XMR", t0).copy(ctx = Some(ctx), feed = Health.Live(t0))
    assert(Screen.stats(s, t0, 110).contains("mid 522.26"), "no book")
    val withBook = Screen.stats(s.copy(book = Some(book)), t0, 110)
    assert(withBook.contains("mid 500.15"), withBook)
  }
  test("the stats mid uses the trading width format") {
    for value <- Vector("0.000012345678", "123456789.123") do
      val s = Snapshot
        .initial("XMR", t0)
        .copy(ctx =
          Some(
            Ctx("400", "401", "0.0001", "1", "1", "300", Some(value))
          )
        )
      val line = Screen.stats(s, t0, 100)
      assert(line.contains("mid " + Trading.display(BigDecimal(value), 12)), line)
  }
  test("the feed is stale past five seconds, not six") {
    val s = Snapshot.initial("XMR", t0).copy(feed = Health.Live(t0 - 5500))
    val line = Screen.stats(s, t0, 100)
    assert(plain(line).endsWith("ping stale "), line)
  }
  test("the socket age never goes negative, even if the last message is ahead of now") {
    val s = Snapshot.initial("X", t0).copy(feed = Health.Live(t0 + 400))
    val line = Screen.stats(s, t0, 100)
    assert(plain(line).endsWith("ping 0.0s "), line)
  }
  test("a stats line never exceeds the terminal width") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot
      .initial("flx:GOLD", t0)
      .copy(ctx = Some(ctx), feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    for width <- Vector(100, 61, 30, 12, 5) do
      val line = plain(Screen.stats(s, t0, width))
      assertEquals(line.length, width, line)
    assert(plain(Screen.stats(s, t0, 100)).endsWith("ping down bad 3 "))
  }
  test("short numbers use k, M and B with one decimal") {
    assertEquals(Screen.short(31_022_816.0), "31.0M")
    assertEquals(Screen.short(208_410.0), "208.4k")
    assertEquals(Screen.short(12.34), "12.3")
    assertEquals(Screen.short(3_314_300_000.0), "3.3B")
  }

  test("render yields the chart block rows at the chart width at the golden sizes") {
    for (cols, rows) <- Vector((100, 30), (110, 30), (120, 40), (166, 50)) do
      val frame = market(Golden.snapshot, cols, rows, Golden.now, zone, Style.colour)
      assertEquals(frame.size, rows - 12)
      assert(frame.map(plain).forall(_.length == cols - 39), frame.map(plain).map(_.length))
  }

  test("the current price row alone carries the marker and the right scale is gone") {
    val frame = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.colour)
    val priceRows = frame.slice(1, 14)
    val marked = priceRows.zipWithIndex.filter((line, _) => plain(line).charAt(16) == '>')
    assertEquals(marked.size, 1)
    val axis = Chart
      .axis(Chart.columns(Golden.snapshot, 46, Golden.now), Golden.snapshot.book, 13)
      .get
    val mid = Golden.snapshot.book.flatMap(_.mid).get.toDouble
    assertEquals(marked.head._2, axis.trueRow(mid))
    assert(marked.head._1.contains(Screen.bold + ">" + Screen.reset), marked.head._1)
    assert(!priceRows.exists(line => plain(line).contains("<")))
    assert(priceRows.forall(line => plain(line).length == 61))
  }

  test("the colour frame keeps side colours and the rows under the price rows read without them") {
    val colour = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.colour)
    val mono = market(Golden.snapshot, 120, 40, Golden.now, zone, Style.mono)
    assert(colour.exists(_.contains(Style.colour.bid)))
    assert(colour.exists(_.contains(Style.colour.ask)))
    assert(!colour.slice(14, 18).exists(_.contains(Screen.esc + "[3")))
    assert(!mono.exists(_.contains(Screen.esc + "[3")))
  }

  test("the price rows carry braille bars from column 18 and no hash marks") {
    val frame = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.mono).map(plain)
    val bars = frame.slice(1, 14).map(_.slice(17, 38))
    assert(bars.count(_.trim.nonEmpty) >= 10, bars)
    assert(!frame.exists(_.contains("#")))
    assert(bars.exists(_.contains(Chart.FULL)))
    assert(frame.slice(1, 14).forall(_.slice(0, 16).forall(c => c == ' ' || c == '.' || c.isDigit)))
  }

  test("volume and the time axis occupy their own rows") {
    val frame = market(Golden.snapshot, 100, 30, Golden.now, zone, Style.colour).map(plain)
    assertEquals(frame(14).take(38), " " * 31 + "volume ")
    assertEquals(frame(14).length, 61)
    assertEquals(frame(17).take(38), " " * 24 + "c line 2s/col ")
    assert(frame(17).drop(38).matches("[ |0-9:]*"), frame(17))
    assert("""\|\d\d:\d\d:\d\d""".r.findFirstIn(frame(17)).nonEmpty, frame(17))
  }

  test("a coarse zoom draws the minute series, the cell caption and the volume") {
    val minute = Golden.now / 60_000L * 60_000L
    val candles = Vector.tabulate(130) { i =>
      val o = 503.0 + (i % 7) * 0.1
      Candle(minute - (129 - i) * 60_000L, o, o + 0.2, o - 0.2, o + 0.05, 1.0 + i % 5)
    }
    val s = Golden.snapshot.copy(candles = Map("1m" -> candles), zoom = 5)
    val frame = market(s, 100, 30, Golden.now, zone, Style.mono).map(plain)
    assertEquals(frame.size, 18)
    assert(frame.forall(_.length == 61), frame.map(_.length))
    assert(frame(17).take(38).endsWith("c line 2m/col "), frame(17))
    val braille = frame.slice(1, 14).filter(_.drop(38).exists(c => c >= 0x2800 && c <= 0x28ff))
    assert(braille.nonEmpty, frame.slice(1, 14).mkString("\n"))
    assert(frame(14).drop(38).exists(c => c >= 0x2800 && c <= 0x28ff), frame(14))
  }

  test("a day zoom draws history from the series and dates its day ticks") {
    val s = Golden.snapshot.copy(zoom = State.zooms.indexOf(86400))
    val frame = market(s, 100, 30, Golden.now, zone, Style.mono).map(plain)
    assert(frame(17).take(38).endsWith("c line 2d/col "), frame(17))
    assert("""\|\d\d-\d\d""".r.findFirstIn(frame(17).drop(38)).nonEmpty, frame(17))
    assert(frame.slice(1, 14).exists(_.drop(38).exists(c => c >= 0x2800 && c <= 0x28ff)))
  }

  test("candles name the seconds per candle in the caption") {
    val s = Golden.snapshot.copy(chartMode = ChartMode.Candles, zoom = State.candleMinZoom)
    val frame = market(s, 100, 30, Golden.now, zone, Style.mono).map(plain)
    assertEquals(frame(17).take(38), " " * 18 + "c candles 1m/candle ")
    val history = market(s.copy(offsetSec = 600), 100, 30, Golden.now, zone, Style.mono)
    assertEquals(plain(history(17)).take(38), " " * 10 + "c candles 1m/candle history ")
  }

  // The chart block of lane L5. The fixture series spans 544.00 to 552.00 and the book holds
  // three levels a side, so the axis is 543.60 to 552.40 and every expected row can be worked
  // out by hand from the rules. BTC and PAXG hold samples too, and no row of the block draws them.
  private val pinNow = 1_800_000_000_000L
  private def pinned: Snapshot =
    val sec = pinNow / 1000
    Snapshot
      .initial("XMR", pinNow)
      .copy(
        feed = Health.Live(pinNow - 300),
        ctx = Some(Ctx("546.09", "546.08", "0.000013", "1200000", "48000000", "539.62", None)),
        book = Some(
          Book(
            Vector(lv("546.09", "0.5"), lv("545.00", "1"), lv("544.00", "3")),
            Vector(lv("546.10", "0.5"), lv("547.00", "1"), lv("549.00", "3")),
            pinNow
          )
        ),
        samples =
          Vector((sec - 40, 548.0), (sec - 30, 552.0), (sec - 20, 544.0), (sec - 1, 546.095)),
        refs = Map(
          "BTC" -> Ref(Vector((sec - 40, 95000.0), (sec - 1, 96000.0)), Map.empty),
          "PAXG" -> Ref(Vector((sec - 40, 2300.0), (sec - 1, 2310.0)), Map.empty)
        )
      )
  private def chart(
      s: Snapshot,
      cols: Int,
      rows: Int,
      style: Style,
      decor: AxisDecor = AxisDecor.plain
  ): Screen.MarketFrame =
    val r = Layout.regions(cols, rows)
    Screen.renderMarket(s, pinNow, zone, style, r, decor, blank(r))

  test("the regions give the left pane, the plot, the outlook, the price rows and the band") {
    assertEquals(Layout.regions(100, 30), Regions(38, 23, 38, 13, 61, 38, 2))
    assertEquals(Layout.regions(120, 40), Regions(38, 43, 38, 21, 81, 38, 4))
    assertEquals(Layout.regions(237, 71), Regions(38, 160, 38, 52, 198, 38, 4))
    assertEquals(Layout.regions(269, 73), Regions(38, 192, 38, 54, 230, 38, 4))
    assertEquals(Layout.regions(100, 39).band, 2)
    assertEquals(Layout.regions(100, 40).band, 4)
  }

  test("the band rows sit between the volume row and the time axis") {
    val r = Layout.regions(100, 30)
    val band = Vector("<" + " " * 59 + ">", "[" + " " * 59 + "]")
    val text = Screen
      .renderMarket(pinned, pinNow, zone, Style.mono, r, AxisDecor.plain, band)
      .lines
      .map(plain)
    assertEquals(text.size, 18)
    assert(text(14).startsWith(" " * 31 + "volume "), text(14))
    assertEquals(text(15), band(0))
    assertEquals(text(16), band(1))
    assert(text(17).contains("c line 2s/col"), text(17))
    intercept[IllegalArgumentException](
      Screen.renderMarket(pinned, pinNow, zone, Style.mono, r, AxisDecor.plain, band.take(1))
    )
  }

  test("the chart block refuses a band of more rows than the regions give") {
    for (cols, rows) <- Vector((100, 30), (120, 40)) do
      val r = Layout.regions(cols, rows)
      val tall = Vector.fill(r.band + 1)(" " * (r.left + r.plot))
      intercept[IllegalArgumentException](
        Screen.renderMarket(pinned, pinNow, zone, Style.mono, r, AxisDecor.plain, tall)
      )
  }

  test("no frame draws a reference row") {
    // Each reference market holds two samples, so a reference row would have a plot to draw.
    val sec = pinNow / 1000
    val held = Ref(Vector((sec - 40, 100.0), (sec - 1, 101.0)), Map.empty)
    for
      coin <- Vector("XMR", "BTC", "PAXG")
      (cols, rows) <- Vector((100, 30), (120, 40))
    do
      val s = Snapshot.initial(coin, pinNow).copy(refs = Map("BTC" -> held, "PAXG" -> held))
      val text = Workspace
        .draw(
          s,
          TradingView(coin, message = "ready"),
          PanelView(),
          cols,
          rows,
          pinNow,
          zone,
          Style.mono
        )
        .lines
        .map(plain)
      assert(!text.exists(_.contains("PAXG (token tied to gold)")), s"$coin $cols x $rows")
      assert(!text.exists(_.startsWith("BTC" + " " * 27)), s"$coin $cols x $rows")
  }

  test("the header and the left pane match the hand written fixtures") {
    // Each fixture line ends with a bar so that its trailing spaces stay visible. The lines are
    // the header, the left 38 columns of every price row, the volume row and the band rows, and
    // the time axis.
    for (cols, rows) <- Vector((100, 30), (120, 40)) do
      val r = Layout.regions(cols, rows)
      val frame = chart(pinned, cols, rows, Style.mono)
      val text = frame.lines.map(plain)
      val expected = Golden
        .fixture(s"screen-limit-chart-${cols}x$rows.txt")
        .linesIterator
        .toVector
        .map(_.stripSuffix("|"))
      val actual =
        Vector(text.head) ++ text.slice(1, text.size - 1).map(_.take(r.left)) ++ Vector(text.last)
      assertEquals(actual, expected)
      assertEquals(frame.priceRows, r.priceRows)
      assertEquals(text.size, r.priceRows + 3 + r.band)
      assert(text.forall(_.length == r.left + r.plot), text.map(_.length))
  }

  test("the marker sits on the current price's true row") {
    // The mid 545.50 is on true row 10 of the 543.60 to 552.40 axis at 13 rows, where the
    // rounded row is 9.
    val touch = Book(Vector(lv("545.49", "1")), Vector(lv("545.51", "1")), pinNow)
    val frame = chart(pinned.copy(book = Some(touch)), 100, 30, Style.mono)
    assertEqualsDouble(frame.axis.get.lo, 543.6, 1e-9)
    assertEqualsDouble(frame.axis.get.hi, 552.4, 1e-9)
    assertEquals(frame.axis.get.row(545.5), 9)
    val marked = frame.lines.slice(1, 14).map(plain).zipWithIndex.collect {
      case (line, row) if line.charAt(16) == '>' => row
    }
    assertEquals(marked, Vector(10))
  }

  test("the plot takes the cells the right scale held") {
    val text = chart(pinned, 100, 30, Style.mono).lines.map(plain)
    val newest = text.slice(1, 14).map(_.charAt(60))
    assert(newest.exists(c => c >= 0x2800 && c <= 0x28ff), newest)
    assert(
      text.slice(1, 14).forall(_.drop(38).forall(c => c == ' ' || (c >= 0x2800 && c <= 0x28ff)))
    )
  }

  test("the price rows keep the side colours, faint plain numbers and a bold marker") {
    val lines = chart(pinned, 100, 30, Style.colour).lines
    assert(lines(6).contains(Style.colour.ask + Chart.FULL * 21), lines(6))
    assert(lines(8).contains(Style.colour.ask), lines(8))
    assert(lines(10).contains(Style.colour.bid) && lines(11).contains(Style.colour.bid))
    assert(lines(13).contains(Style.colour.bid), lines(13))
    assert(lines(2).startsWith(Screen.dim + " " * 8 + "  551.38 " + Screen.reset), lines(2))
    assert(lines(10).startsWith(" " * 16 + Screen.bold + ">" + Screen.reset), lines(10))
    assert(!lines.slice(1, 14).exists(_.contains(Screen.inverse)))
  }

  test("the header starts with the market in bold and ends with a faint ping before the outlook") {
    val fields = "XMR  mid 546.095  mark 546.09  oracle 546.08"
    for (feed, ping) <- Vector(
        Health.Live(pinNow - 300) -> "ping 0.3s",
        Health.Live(pinNow - 5500) -> "ping stale",
        Health.Down(pinNow - 100, pinNow - 300) -> "ping down"
      )
    do
      val frame = Workspace.draw(
        pinned.copy(feed = feed),
        TradingView("XMR", message = "ready"),
        PanelView(),
        100,
        30,
        pinNow,
        zone,
        Style.colour
      )
      val line = frame.lines(0)
      assert(line.startsWith(Screen.bold + "XMR" + Screen.reset + "  mid 546.095"), line)
      assert(line.contains(Screen.dim + ping + Screen.reset), line)
      val text = plain(line)
      assertEquals(text.take(62), fields.padTo(60 - ping.length, ' ') + ping + " |")
      assertEquals(text.indexOf(ping) + ping.length - 1, 59)
  }

  test("the ticket line fills only the empty cells of its row and a mark replaces its cell") {
    val ink = Style.colour.bid + Screen.bold
    val ticket = Style.colour.ask + Screen.bold
    val marks =
      Vector(PlotMark(3, 2, '▲', ink), PlotMark(23, 2, '▼', ink), PlotMark(0, 13, '▼', ink))
    val decor = AxisDecor(Vector.empty, Chart.plainLabels, _ => marks, _ => Some((4, ticket)))
    val marked = chart(pinned, 100, 30, Style.colour, decor).lines
    val bare = chart(pinned, 100, 30, Style.colour).lines
    val before = plain(bare(5)).drop(38)
    val after = plain(marked(5)).drop(38)
    assert(before.exists(_ == ' ') && before.exists(_ != ' '), before)
    before.zip(after).foreach((b, a) => assertEquals(a, if b == ' ' then '⠒' else b))
    assert(marked(5).contains(ticket + "⠒"), marked(5))
    assertEquals(plain(marked(3)).charAt(41), '▲')
    assert(marked(3).contains(ink + "▲" + Screen.reset), marked(3))
    assertEquals(plain(marked(3)).length, 61)
    assertEquals(marked.map(plain).mkString.count(_ == '▼'), 0)
    assertEquals(marked.drop(14).map(plain), bare.drop(14).map(plain))
  }

  test("held prices from the decor widen the chart's axis") {
    val decor = AxisDecor.plain.copy(held = Vector(500.0))
    val frame = chart(pinned, 100, 30, Style.mono, decor)
    assert(frame.axis.exists(_.lo < 500.0), frame.axis)
    assert(chart(pinned, 100, 30, Style.mono).axis.exists(_.lo > 543.0))
  }

  test("the decor's mark counts give the chart's axis its room") {
    // The series ends at the current price 546.09. A sell resting there sits above it, and its
    // count keeps the top row free for it.
    val sec = pinNow / 1000
    val top = pinned.copy(
      book = Some(Book(Vector(lv("546.08", "1")), Vector(lv("546.10", "1")), pinNow)),
      samples = Vector((sec - 40, 540.0), (sec - 1, 546.09))
    )
    val decor = AxisDecor.plain.copy(held = Vector(546.09))
    val bare = chart(top, 100, 30, Style.mono, decor).axis.get
    assertEquals(bare.trueRow(546.09), 0, bare)
    val counted = chart(top, 100, 30, Style.mono, decor.copy(above = 1)).axis.get
    assertEquals(counted.trueRow(546.09), 1, counted)
  }

  test("the workspace draws the chart block above the twelve row band in every mode") {
    for
      (cols, rows) <- Vector((100, 30), (120, 40))
      mode <- Vector("LIVE", "PROP", "VIEW")
    do
      val tradeTop = rows - 12
      val r = Layout.regions(cols, rows)
      val width = r.left + r.plot
      val text = Workspace
        .draw(
          pinned,
          TradingView("XMR", message = "ready"),
          PanelView(mode = mode),
          cols,
          rows,
          pinNow,
          zone,
          Style.mono
        )
        .lines
      // With no odds the workspace's band reads unreachable on its first row, 50 on its third
      // when it has four, and 0 on its last.
      val label = " " * 10 + "Polymarket unreachable  100 "
      val band =
        (if r.band == 4 then Vector(label, " " * 38, " " * 35 + "50 ", " " * 36 + "0 ")
         else Vector(label, " " * 36 + "0 ")).map(_ + " " * r.plot)
      val block = Screen
        .renderMarket(
          pinned,
          pinNow,
          zone,
          Style.mono,
          r,
          AxisDecor.plain,
          band,
          Some(width)
        )
        .lines
        .map(plain)
      // The view has no market rules, so every mode draws the plain axis. No row is left blank.
      assertEquals(block.size, tradeTop)
      assertEquals(text.take(tradeTop).map(_.take(width)), block, mode)
      assert(text.take(tradeTop).forall(_.charAt(width) == '|'))
      assertEquals(text(tradeTop - 1).take(38), " " * 24 + "c line 2s/col ")
  }

  test("the plot draws from the oldest sample's cell to the last") {
    // The main market holds samples 40 seconds to one second back. At two dots a cell the oldest
    // is dot 2 * plot - 41, so the plot draws from that cell to the last.
    for (cols, rows) <- Vector((100, 30), (120, 40)) do
      val r = Layout.regions(cols, rows)
      val text = chart(pinned, cols, rows, Style.mono).lines.map(plain)
      def inked(lines: Vector[String]): Vector[Int] =
        lines
          .flatMap(_.zipWithIndex.collect { case (c, i) if c >= 0x2800 && c <= 0x28ff => i })
          .filter(_ >= r.left)
          .distinct
          .sorted
      val expected = (r.left + (2 * r.plot - 41) / 2 until r.left + r.plot).toVector
      assertEquals(inked(text.slice(1, r.priceRows + 1)), expected)
      assert(text.forall(_.length == r.left + r.plot), text.map(_.length))
  }

  test("the scrub step is a quarter of the plot the owner sees") {
    for (cols, step) <- Vector((100, 11L), (101, 12L), (120, 21L)) do
      val s = Snapshot.initial("XMR", t0).copy(cols = cols, rows = 30)
      assertEquals(State.reduce(s, Event.Key(Command.ScrubBack), t0).offsetSec, step, cols)
      assertEquals(step, Layout.regions(cols, 30).plot * 2 / 4L)
  }
