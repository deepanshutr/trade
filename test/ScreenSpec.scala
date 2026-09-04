package desk

class ScreenSpec extends munit.FunSuite:
  val t0 = 1_700_000_000_000L
  val zone = java.time.ZoneId.of("UTC")
  def lv(px: String, sz: String) = Level(px, sz, 1)
  private def plain(l: String): String = Chart.plain(l)

  test("the escape sequences are built from the escape character") {
    assertEquals(Screen.esc, 27.toChar.toString)
    assertEquals(Screen.bold, Screen.esc + "[1m")
    assertEquals(Screen.dim, Screen.esc + "[2m")
    assertEquals(Screen.reset, Screen.esc + "[0m")
  }
  test("rounding is half up") {
    assertEquals(Screen.round(0.5), 1L)
    assertEquals(Screen.round(-0.5), 0L)
    assertEquals(Screen.round(2.49), 2L)
  }
  test("the stats line shows the figures, the age, stale, bad and the prompt") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot.initial("XMR", t0).copy(ctx = Some(ctx), feed = Health.Live(t0 - 300))
    val line = Screen.stats(s, t0, 110, None)
    assertEquals(line.length, 110)
    val expected = "XMR  522.26  mark 522.28  orc 521.515  24h -1.4%  fund 0.0130%  " +
      "OI 210.2k  vol 31.2M  big --  live 0.3s"
    assert(line.startsWith(expected), line)
    assert(Screen.stats(s, t0, 110, Some(1234.0)).contains("big>$1.2k"), line)
    val down = s.copy(feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val staleLine = Screen.stats(down, t0, 110, None)
    assertEquals(staleLine.length, 110)
    assert(staleLine.contains("STALE 12s"), staleLine)
    assert(staleLine.contains("bad 3"), staleLine)
    assert(Screen.stats(s.copy(prompt = Some("BT")), t0, 110, None).startsWith("market: BT_"))
    val bare = Snapshot.initial("XMR", t0).copy(feed = Health.Live(t0))
    assert(Screen.stats(bare, t0, 110, None).startsWith("XMR  -  big --  live 0.0s"))
  }
  test("the stats line prints the book mid, falling back to the context mid without a book") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val book = Book(Vector(lv("500.10", "1")), Vector(lv("500.20", "1")), t0)
    val s = Snapshot.initial("XMR", t0).copy(ctx = Some(ctx), feed = Health.Live(t0))
    assert(Screen.stats(s, t0, 110, None).startsWith("XMR  522.26  "), "no book")
    val withBook = Screen.stats(s.copy(book = Some(book)), t0, 110, None)
    assert(withBook.startsWith("XMR  500.15  "), withBook)
  }
  test("the feed is stale past five seconds, not six") {
    val s = Snapshot.initial("XMR", t0).copy(feed = Health.Live(t0 - 5500))
    val line = Screen.stats(s, t0, 100, None)
    assert(line.contains("STALE"), line)
  }
  test("the socket age never goes negative, even if the last message is ahead of now") {
    val s = Snapshot.initial("X", t0).copy(feed = Health.Live(t0 + 400))
    val line = Screen.stats(s, t0, 100, None)
    assert(line.contains("live 0.0s"), line)
  }
  test("a stats line never exceeds the terminal width") {
    val ctx =
      Ctx("522.28", "521.515", "0.0001302411", "210160.16", "31202156.16", "529.78", Some("522.26"))
    val s = Snapshot
      .initial("flx:GOLD", t0)
      .copy(ctx = Some(ctx), feed = Health.Down(t0 - 12_000, t0 - 12_300), bad = 3)
    val line = Screen.stats(s, t0, 100, None)
    assertEquals(line.length, 100)
    assert(line.contains("STALE 12s"), line)
  }
  test("short numbers use k, M and B with one decimal") {
    assertEquals(Screen.short(31_022_816.0), "31.0M")
    assertEquals(Screen.short(208_410.0), "208.4k")
    assertEquals(Screen.short(12.34), "12.3")
    assertEquals(Screen.short(3_314_300_000.0), "3.3B")
  }
  test("render keeps the small terminal message") {
    assertEquals(
      Screen.render(Golden.snapshot, 80, 24, Golden.now, zone),
      Vector("need 100x30, have 80x24")
    )
    assertEquals(
      Screen.render(Golden.snapshot, 100, 29, Golden.now, zone),
      Vector("need 100x30, have 100x29")
    )
  }

  test("render yields rows lines of the width at both golden sizes") {
    for (cols, rows) <- Vector((100, 30), (110, 30), (120, 40), (166, 50)) do
      val frame = Screen.render(Golden.snapshot, cols, rows, Golden.now, zone)
      assertEquals(frame.size, rows)
      assert(frame.map(plain).forall(_.length == cols), frame.map(plain).map(_.length))
  }

  test("the price rows carry the mid marker on one row with the bold exact mid in the scale") {
    val frame = Screen.render(Golden.snapshot, 100, 30, Golden.now, zone)
    val priceRows = frame.slice(1, 15)
    val marked = priceRows.filter(l => plain(l).charAt(8) == '>')
    assertEquals(marked.size, 1)
    assert(
      marked.head.contains(Screen.bold + "  <503.325" + Screen.reset) || marked.head.contains(
        Screen.bold + " <503.325" + Screen.reset
      ),
      plain(marked.head)
    )
    assert(priceRows.count(_.contains("<503.325")) == 1)
  }

  test(
    "the volume rows carry the caption and the largest volume, the axis row the zoom and clocks"
  ) {
    val frame = Screen.render(Golden.snapshot, 100, 30, Golden.now, zone).map(plain)
    assert(frame(15).startsWith(" " * 23 + "volume "))
    assert(frame(15).drop(91).trim.startsWith("v "), frame(15))
    assert(frame(16).startsWith(" " * 30))
    assert(frame(17).startsWith(" " * 23 + "1s/col "))
    assert(frame(17).drop(30).take(8).matches("\\d\\d:\\d\\d:\\d\\d"))
    assert(frame(17).drop(83).take(8).matches("\\d\\d:\\d\\d:\\d\\d"))
  }

  test("a coarse zoom draws the candles, the minute clocks and the candle volume") {
    val minute = Golden.now / 60_000L * 60_000L
    val candles = Vector.tabulate(130) { i =>
      val o = 503.0 + (i % 7) * 0.1
      Candle(minute - (129 - i) * 60_000L, o, o + 0.2, o - 0.2, o + 0.05, 1.0 + i % 5)
    }
    val s = Golden.snapshot.copy(candles = candles, zoom = 5)
    val frame = Screen.render(s, 100, 30, Golden.now, zone).map(plain)
    assertEquals(frame.size, 30)
    assert(frame.forall(_.length == 100), frame.map(_.length))
    assert(frame(17).contains("60s/col"), frame(17))
    assertEquals("""\d\d:\d\d""".r.findAllIn(frame(17)).size, 3, frame(17))
    val braille = frame.slice(1, 15).filter(_.exists(c => c >= 0x2800 && c <= 0x28ff))
    assert(braille.nonEmpty, frame.slice(1, 15).mkString("\n"))
    assert(frame(15).endsWith("v 5.0"), frame(15))
  }

  test("the gauge rows fill the last twelve lines") {
    val frame = Screen.render(Golden.snapshot, 100, 30, Golden.now, zone).map(plain)
    assertEquals(frame.drop(18).map(_.take(4)), Vector.fill(6)("BOOK") ++ Vector.fill(6)("TAPE"))
  }

  test("the stats line carries the big threshold and no interval") {
    val line = Screen.render(Golden.snapshot, 100, 30, Golden.now, zone).head
    assert(line.contains("big>$"), line)
    assert(!line.contains(" 1m "), line)
    assert(line.indexOf("big>$") < line.indexOf("live"), line)
    assertEquals(Screen.bigToken(None), "big --")
    assertEquals(Screen.bigToken(Some(410.29)), "big>$410")
    assertEquals(Screen.bigToken(Some(1234.0)), "big>$1.2k")
  }
  test("write the golden frames when asked") {
    assume(sys.env.contains("WRITE_GOLDEN"), "set WRITE_GOLDEN=1 to regenerate the frames")
    java.nio.file.Files.createDirectories(java.nio.file.Paths.get("golden"))
    for (cols, rows) <- Seq((100, 30), (120, 40)) do
      val frame = Screen.render(Golden.snapshot, cols, rows, Golden.now, zone)
      val path = java.nio.file.Paths.get("golden", s"frame_${cols}x$rows.txt")
      java.nio.file.Files.writeString(path, frame.mkString("\n") + "\n")
  }
  test("render matches the golden frames at 100 by 30 and 120 by 40") {
    for (cols, rows) <- Seq((100, 30), (120, 40)) do
      val frame = Screen.render(Golden.snapshot, cols, rows, Golden.now, zone)
      assertEquals(frame.size, rows)
      assert(frame.forall(l => plain(l).length == cols), frame.map(plain).map(_.length))
      val path = java.nio.file.Paths.get("golden", s"frame_${cols}x$rows.txt")
      assertEquals(frame.mkString("\n") + "\n", java.nio.file.Files.readString(path))
  }
