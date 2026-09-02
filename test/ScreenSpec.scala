package desk

class ScreenSpec extends munit.FunSuite:
  val t0 = 1_700_000_000_000L
  def lv(px: String, sz: String) = Level(px, sz, 1)
  def plain(x: String) =
    x.replace(Screen.bold, "").replace(Screen.dim, "").replace(Screen.reset, "")

  test("the escape sequences are built from the escape character") {
    assertEquals(Screen.esc, 27.toChar.toString)
    assertEquals(Screen.bold, Screen.esc + "[1m")
    assertEquals(Screen.dim, Screen.esc + "[2m")
    assertEquals(Screen.reset, Screen.esc + "[0m")
  }
  // the research snapshot at 09:00:41Z: u = 0.964, r = 1.696, bar width 20
  test("bar lengths reproduce the research numbers") {
    val (u, r) = (0.964, 1.696)
    assertEquals(Screen.barLen(0.547, u, r), 9L)
    assertEquals(Screen.barLen(0.740, u, r), 11L)
    assertEquals(Screen.barLen(0.964, u, r), 14L)
    assertEquals(Screen.barLen(1.221, u, r), 16L)
    assertEquals(Screen.barLen(1.696, u, r), 20L)
    assert(Screen.barLen(2.235, u, r) > 20L)
    assertEquals(Screen.barLen(0, u, r), 0L)
    assertEquals(Screen.barLen(1, 0, r), 0L)
  }
  test("rounding is half up") {
    assertEquals(Screen.round(0.5), 1L)
    assertEquals(Screen.round(-0.5), 0L)
    assertEquals(Screen.round(2.49), 2L)
  }
  test("lower median for even counts, plain median for odd") {
    assertEquals(Screen.lowerMedian(Vector(4, 1, 3, 2)), 2.0)
    assertEquals(Screen.lowerMedian(Vector(3, 1, 2)), 2.0)
    assertEquals(Screen.lowerMedian(Vector.empty), 0.0)
  }
  test("the reference is the second largest on screen until the window holds 40 sizes") {
    val onScreen = Vector(0.547, 0.740, 2.235, 1.696, 0.964)
    assertEquals(Screen.reference(Vector.empty, onScreen), 1.696)
    assertEquals(Screen.reference(Vector.empty, Vector(5.0)), 5.0)
    val window = Vector((t0, (1 to 40).map(_.toDouble).toVector))
    assertEquals(Screen.reference(window, onScreen), 38.0)
  }
  test("cumulative sums from the best level outward") {
    val ls = Vector(lv("1", "0.5"), lv("2", "1.5"), lv("3", "1"))
    assertEquals(Screen.cumulative(ls), Vector(0.5, 2.0, 3.0))
  }
  test("the gauge track places the mark by tenths and the centre bar survives") {
    assertEquals(Screen.track(0.0923), "..........|#.........")
    assertEquals(Screen.track(-0.049), "..........#..........")
    assertEquals(Screen.track(1.0), "..........|.........#")
    assertEquals(Screen.track(-1.0), "#.........|..........")
    assertEquals(Screen.track(0.0), "..........#..........")
  }
  test("queue and distance weighted imbalance reproduce the research numbers") {
    val asks = Vector(
      "518.40" -> "0.547",
      "518.43" -> "0.740",
      "518.44" -> "1.221",
      "518.47" -> "0.584",
      "518.49" -> "0.771",
      "518.50" -> "0.732",
      "518.53" -> "0.732",
      "518.56" -> "1.696",
      "518.58" -> "2.235",
      "518.59" -> "1.696"
    ).map(lv)
    val bids = (0 until 10).map(i => lv(f"${518.36 - 0.01 * i}%.2f", "0.964")).toVector
    val (q, d) = Screen.imbalance(Book(bids, asks, 0))
    assertEqualsDouble(q, 0.2760, 0.0005)
    assertEqualsDouble(d, 0.0923, 0.0005)
    assertEquals(Screen.imbalance(Book(Vector.empty, asks, 0)), (-1.0, -1.0))
  }
  test("trade flow over the last sixty seconds") {
    val tape = Vector(
      Trade(t0, Side.Buy, "1", "19.4"),
      Trade(t0 - 59_000, Side.Sell, "1", "21.4"),
      Trade(t0 - 61_000, Side.Buy, "1", "1000")
    )
    val (f, n) = Screen.flow(tape, t0)
    assertEqualsDouble(f, -0.049, 0.001)
    assertEquals(n, 2)
    assertEquals(Screen.flow(Vector.empty, t0), (0.0, 0))
  }
  test("the spread rule shows spread, basis points and book age, padded to 46") {
    val b = Book(Vector(lv("518.36", "1")), Vector(lv("518.40", "1")), t0 - 400)
    val r = Screen.rule(b, t0)
    assertEquals(r.length, 46)
    assert(r.startsWith("--------  spread 0.04  0.8bp  book 0.4s  ----"), r)
    val empty = Screen.rule(Book(Vector.empty, Vector.empty, t0), t0)
    assertEquals(empty.length, 46)
    assert(empty.contains("spread n/a"))
  }
  test("ladder rows are 46 wide, asks above the rule highest first, bids below best first") {
    val b = Book(Vector(lv("100", "1"), lv("99", "2")), Vector(lv("101", "3"), lv("102", "4")), t0)
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(b))
    val rows = Screen.ladder(s, 27, t0)
    assertEquals(rows.size, 27)
    assert(rows.forall(r => plain(r).length == 46), rows.map(plain).map(_.length))
    assert(plain(rows(11)).startsWith("     102"))
    assert(plain(rows(12)).startsWith("     101"))
    assert(rows(13).startsWith("--------"))
    assert(plain(rows(14)).startsWith("     100"))
    assert(plain(rows(15)).startsWith("      99"))
    assert(plain(rows(0)).trim.isEmpty)
    assert(plain(rows(26)).trim.isEmpty)
  }
  test("the best rows are bold, a clamped row carries the marker and bold, outer rows dim") {
    val asks = (0 until 13).map(i => lv((101 + i).toString, "1")).toVector
    val bids = (0 until 13).map(i => lv((100 - i).toString, if i == 5 then "40" else "1")).toVector
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(Book(bids, asks, t0)))
    val rows = Screen.ladder(s, 27, t0)
    assert(rows(12).startsWith(Screen.bold), "best ask bold")
    assert(rows(14).startsWith(Screen.bold), "best bid bold")
    assert(rows(19).startsWith(Screen.bold) && rows(19).contains("#>"), "clamped bid row 5")
    assert(rows(0).startsWith(Screen.dim), "outer ask dim")
    assert(rows(26).startsWith(Screen.dim), "outer bid dim")
    assert(!rows(7).startsWith(Screen.dim) && !rows(7).startsWith(Screen.bold), "middle ask plain")
  }
  test("an even ladder height leaves one blank row at the bottom") {
    val b = Book(Vector(lv("100", "1"), lv("99", "1")), Vector(lv("101", "1"), lv("102", "1")), t0)
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(b))
    val rows = Screen.ladder(s, 28, t0)
    assertEquals(rows.size, 28)
    assert(rows(13).startsWith("--------"))
    assert(plain(rows(12)).startsWith("     101"))
    assert(plain(rows(14)).startsWith("     100"))
    assert(plain(rows(27)).trim.isEmpty)
    assert(plain(rows(26)).trim.isEmpty)
  }
  test("gauge lines are 46 wide and read the book and the tape") {
    val b = Book(Vector(lv("100", "3")), Vector(lv("101", "1")), t0)
    val tape = Vector(Trade(t0, Side.Buy, "1", "2"))
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(b), tape = tape)
    val g = Screen.gauges(s, t0)
    assertEquals(g.size, 2)
    assert(g.forall(_.length == 46))
    assert(g(0).startsWith("BOOK ..........|....#..... D+0.50 Q+0.50"), g(0))
    assert(g(1).startsWith("TAPE ..........|.........# F+1.00 60s n=1"), g(1))
    val none = Screen.gauges(Snapshot.initial("X", "5m", t0), t0)
    assert(none(0).startsWith("BOOK ..........|.........."), none(0))
    assert(none(1).startsWith("TAPE ..........|.......... n=0"), none(1))
  }

  val utc = java.time.ZoneId.of("UTC")

  test("wide sizes never push a ladder row past 46 columns") {
    val b = Book(
      Vector(lv("76984.0", "123456.789")),
      Vector(lv("76984.0", "16.10381")),
      t0
    )
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(b))
    val rows = Screen.ladder(s, 27, t0)
    assert(rows.forall(r => plain(r).length == 46), rows.map(plain).map(_.length))
  }
  test("the chart maps prices to rows, draws bodies over wicks, and bolds the newest column") {
    def c(o: Double, h: Double, l: Double, cl: Double, t: Long) = Candle(t, o, h, l, cl, 1)
    val candles = Vector(c(10, 20, 0, 10, 1), c(5, 15, 5, 15, 2))
    val s = Snapshot.initial("X", "5m", t0).copy(candles = candles)
    val lines = Screen.chart(s, 54)
    assertEquals(lines.size, 17)
    assert(lines.forall(l => plain(l).length == 54), lines.map(plain).map(_.length))
    val col0 = lines.map(l => plain(l).charAt(44))
    val col1 = lines.map(l => plain(l).charAt(45))
    assertEquals(col0(0), '|')
    assertEquals(col0(8), '#')
    assertEquals(col0(16), '|')
    assertEquals(col1(4), '#')
    assertEquals(col1(12), '#')
    assertEquals(col1(0), ' ')
    assert(lines(4).contains(Screen.bold + "#" + Screen.reset), "newest column bold")
    assert(plain(lines(0)).endsWith("   20.00"))
    assert(plain(lines(8)).endsWith("   10.00"))
    assert(plain(lines(16)).endsWith("    0.00"))
  }
  test("a flat range draws every candle on the middle row and no candles says so") {
    val s = Snapshot.initial("X", "5m", t0).copy(candles = Vector(Candle(1, 5, 5, 5, 5, 1)))
    val lines = Screen.chart(s, 54)
    assertEquals(plain(lines(8)).charAt(45), '#')
    assert((0 until 17).filter(_ != 8).forall(r => plain(lines(r)).charAt(45) == ' '))
    assert(Screen.chart(Snapshot.initial("X", "5m", t0), 54).head.startsWith("no candles"))
  }
  test("the tape lists newest first with the zone's time, bolds the unusual size, pads to width") {
    val tape = Vector(
      Trade(t0, Side.Buy, "518.40", "1.650"),
      Trade(t0 - 1000, Side.Sell, "518.36", "0.400"),
      Trade(t0 - 2000, Side.Buy, "518.40", "0.300")
    )
    val s = Snapshot.initial("X", "5m", t0).copy(tape = tape)
    val lines = Screen.tape(s, 5, 54, utc)
    assertEquals(lines.size, 5)
    assert(lines.forall(l => plain(l).length == 54))
    assertEquals(plain(lines(0)), " 22:13:20  buy    518.40   1.650".padTo(54, ' '))
    assert(lines(0).startsWith(Screen.bold), "largest trade bold")
    assert(!lines(1).startsWith(Screen.bold))
    assert(plain(lines(3)).trim.isEmpty)
  }
  test("the stats line shows the figures, the age, stale, bad and the prompt") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot.initial("XMR", "5m", t0).copy(ctx = Some(ctx), feed = Health.Live(t0 - 300))
    val line = Screen.stats(s, t0, 100)
    assertEquals(line.length, 100)
    val expected = "XMR  522.26  mark 522.28  orc 521.515  24h -1.4%  fund 0.0130%  " +
      "OI 210.2k  vol 31.2M  5m  live 0.3s"
    assert(line.startsWith(expected), line)
    val down = s.copy(feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val staleLine = Screen.stats(down, t0, 100)
    assertEquals(staleLine.length, 100)
    assert(staleLine.contains("STALE 12s"), staleLine)
    assert(staleLine.contains("bad 3"), staleLine)
    assert(Screen.stats(s.copy(prompt = Some("BT")), t0, 100).startsWith("market: BT_"))
    val bare = Snapshot.initial("XMR", "5m", t0).copy(feed = Health.Live(t0))
    assert(Screen.stats(bare, t0, 100).startsWith("XMR  -  5m  live 0.0s"))
  }
  test("the feed is stale past five seconds, not six") {
    val s = Snapshot.initial("XMR", "5m", t0).copy(feed = Health.Live(t0 - 5500))
    val line = Screen.stats(s, t0, 100)
    assert(line.contains("STALE"), line)
  }
  test("a stats line never exceeds the terminal width") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot
      .initial("flx:GOLD", "5m", t0)
      .copy(ctx = Some(ctx), feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val line = Screen.stats(s, t0, 100)
    assertEquals(line.length, 100)
    assert(line.contains("STALE 12s"), line)
  }
  test("short numbers use k and M with one decimal") {
    assertEquals(Screen.short(31_022_816.0), "31.0M")
    assertEquals(Screen.short(208_410.0), "208.4k")
    assertEquals(Screen.short(12.34), "12.3")
  }
  test("render refuses a small terminal with one line") {
    val frame = Screen.render(Golden.snapshot, 80, 24, Golden.now, utc)
    assertEquals(frame, Vector("need 100x30, have 80x24"))
  }
  test("write the golden frames when asked") {
    assume(sys.env.contains("WRITE_GOLDEN"), "set WRITE_GOLDEN=1 to regenerate the frames")
    java.nio.file.Files.createDirectories(java.nio.file.Paths.get("golden"))
    for (cols, rows) <- Seq((100, 30), (120, 40)) do
      val frame = Screen.render(Golden.snapshot, cols, rows, Golden.now, utc)
      val path = java.nio.file.Paths.get("golden", s"frame_${cols}x$rows.txt")
      java.nio.file.Files.writeString(path, frame.mkString("\n") + "\n")
  }
  test("render matches the golden frames at 100 by 30 and 120 by 40") {
    for (cols, rows) <- Seq((100, 30), (120, 40)) do
      val frame = Screen.render(Golden.snapshot, cols, rows, Golden.now, utc)
      assertEquals(frame.size, rows)
      assert(frame.forall(l => plain(l).length == cols), frame.map(plain).map(_.length))
      val path = java.nio.file.Paths.get("golden", s"frame_${cols}x$rows.txt")
      assertEquals(frame.mkString("\n") + "\n", java.nio.file.Files.readString(path))
  }
