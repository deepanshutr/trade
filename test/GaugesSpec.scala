package desk

class GaugesSpec extends munit.FunSuite:
  private def lvl(px: String, sz: String): Level = Level(px, sz, 1)
  private def book(bidSz: String, askSz: String, t: Long = 1L): Book =
    Book(Vector(lvl("100", bidSz)), Vector(lvl("101", askSz)), t)
  private val now = 1_700_000_000_000L
  private def trade(agoMs: Long, side: Side, px: String, sz: String): Trade =
    Trade(now - agoMs, side, px, sz)
  private def s0: Snapshot = Snapshot.initial("XMR", now)
  private val FULL = (0x28ff).toChar.toString
  private def tr(v: Option[Double]): String = Gauges.track(v, 13, "")
  private def series(closes: Vector[Double], t0: Long = 0L): Vector[Candle] =
    closes.zipWithIndex.map((c, i) => Candle(t0 + i * 60_000L, c, c, c, c, 1))

  test("imbalance keeps the part one formulas") {
    val (q, d) = Gauges.imbalance(book("1", "3"))
    assertEqualsDouble(q, -0.5, 1e-9)
    assertEqualsDouble(d, -0.5, 1e-9)
    assertEquals(Gauges.weights.size, 10)
    assertEqualsDouble(Gauges.weights(1), 0.7, 1e-9)
  }

  test("flow over a window of trades and undefined when the window is empty") {
    val lens = Vector(10L)
    assertEquals(Gauges.tapeStats(Vector.empty, now, lens).head, (Option.empty[Double], 0))
    val ts = Vector(trade(1L, Side.Buy, "1", "3"), trade(2L, Side.Sell, "1", "1"))
    assertEquals(Gauges.tapeStats(ts, now, lens).head, (Some(0.5), 2))
    val zero = Vector(trade(1L, Side.Buy, "1", "0"))
    assertEquals(Gauges.tapeStats(zero, now, lens).head, (Option.empty[Double], 1))
  }

  test("pearson of returns and its edge cases") {
    assertEquals(Gauges.pearson(Vector(1, 2, 3), Vector(2, 4, 6)).map(_.round), Some(1L))
    assertEqualsDouble(Gauges.pearson(Vector(1, 2, 3), Vector(3, 2, 1)).get, -1.0, 1e-9)
    assertEquals(Gauges.pearson(Vector(1, 1, 1), Vector(1, 2, 3)), None)
    assertEquals(Gauges.pearson(Vector(1, 2), Vector(1, 2)), None)
  }

  test("correlation pairs candles by start time over the last n returns") {
    val main = series(Vector(1, 2, 4, 7, 16))
    val same = series(Vector(2, 4, 8, 14, 32))
    assertEqualsDouble(Gauges.correlation(main, same, 4).get, 1.0, 1e-9)
    val shifted = series(Vector(9, 2, 4, 8, 14, 32), t0 = -60_000L)
    assertEqualsDouble(Gauges.correlation(main, shifted, 4).get, 1.0, 1e-9)
    assertEquals(Gauges.correlation(main, series(Vector(1, 2)), 4), None)
    val flat = series(Vector(1, 1, 1, 1, 1))
    assertEquals(Gauges.correlation(main, flat, 4), None)
  }

  test("correlation reads the tails and returns what the whole series would") {
    def whole(main: Vector[Candle], ref: Vector[Candle], n: Int): Option[Double] =
      val a = main.map(c => c.t -> c.c).toMap
      val b = ref.map(c => c.t -> c.c).toMap
      val ts = (a.keySet & b.keySet).toVector.sorted.takeRight(n + 1)
      Gauges.pearson(
        ts.zip(ts.drop(1)).map((t1, t2) => math.log(a(t2) / a(t1))),
        ts.zip(ts.drop(1)).map((t1, t2) => math.log(b(t2) / b(t1)))
      )
    val main = series((0 until 300).toVector.map(i => 100.0 + math.sin(i / 3.0)))
    val ref = series((0 until 300).toVector.map(i => 50.0 + math.cos(i / 5.0)))
    Vector(3, 15, 48, 96, 168).foreach { n =>
      assertEqualsDouble(
        Gauges.correlation(main, ref, n).get,
        whole(main, ref, n).get,
        1e-12,
        s"n=$n"
      )
    }
    val gapped = main.filterNot(_.t == 250 * 60_000L)
    assertEquals(gapped.size, 299)
    assertEqualsDouble(Gauges.correlation(gapped, ref, 15).get, whole(gapped, ref, 15).get, 1e-12)
    assertEquals(Gauges.correlation(main, ref.filterNot(_.t == 290 * 60_000L), 15), None)
  }

  test("correlation reads n returns from n plus one closes") {
    val main = series(Vector(1, 2, 4, 7, 16))
    val same = series(Vector(2, 4, 8, 14, 32))
    assertEqualsDouble(Gauges.correlation(main, same, 3).get, 1.0, 1e-9)
    val threeCloses = Vector(4.0, 7.0, 16.0)
    val twoReturns = threeCloses.zip(threeCloses.drop(1)).map((x, y) => math.log(y / x))
    assertEquals(twoReturns.size, 2)
    assertEquals(Gauges.pearson(twoReturns, twoReturns), None)
  }

  test("correlation pairs only the tails of both series") {
    def minute(i: Int, c: Double): Candle = Candle(i * 60_000L, c, c, c, c, 1)
    val main = (0 until 50).toVector.map(i => minute(i, 100 + (i % 7) * 1.5))
    val sparse =
      main.filter(c => (c.t / 60_000L) % 3 == 0).map(c => c.copy(c = c.c * 2, o = c.o * 2))
    assertEquals(Gauges.correlation(main, sparse, 3), None)
    val dense = main.map(c => c.copy(c = c.c * 2, o = c.o * 2))
    assert(Gauges.correlation(main, dense, 3).isDefined)
  }

  test("a lens reads the horizon's series and is undefined for the main market itself") {
    val closes = Vector(1.0, 1.1, 1.2, 1.1, 1.3, 1.4, 1.2, 1.5)
    val cs = closes.zipWithIndex.map((c, i) => Candle(i * 60_000L, c, c, c, c, 1))
    val s = s0.copy(
      candles = Map("1m" -> cs),
      refs = Map("BTC" -> Ref(Vector.empty, Map("1m" -> cs)), "PAXG" -> Ref.empty)
    )
    assertEqualsDouble(Gauges.lens(s, "BTC", "1m", 6).get, 1.0, 1e-9)
    assertEquals(Gauges.lens(s, "PAXG", "1m", 6), None)
    assertEquals(Gauges.lens(s.copy(coin = "BTC"), "BTC", "1m", 6), None)
    assertEquals(Gauges.horizons.map(_._1), Vector("15m", "1h", "4h", "1d", "1w", "1M"))
    assertEquals(Gauges.horizons.map(_._2), Vector("1m", "1m", "5m", "15m", "1h", "4h"))
    assertEquals(Gauges.horizons.map(_._3), Vector(15, 60, 48, 96, 168, 180))
  }

  test("a track marks with a braille cell in the given style") {
    assertEquals(Gauges.track(Some(0.5), 12, ""), "............|....." + FULL + "......")
    assertEquals(
      Gauges.track(Some(0.5), 12, "S"),
      "............|....." + "S" + FULL + Screen.reset + "......"
    )
    assertEquals(Gauges.track(None, 12, "S"), "............|............")
    assert(!Gauges.track(Some(1.0), 4, "").contains("#"))
  }

  test("book rows carry the depth and quote readings") {
    val sec = now / 1000
    val s = s0.copy(
      book = Some(book("1", "1")),
      books = Vector((sec - 20, book("1", "3")), (sec - 5, book("1", "1"))),
      since = now - 15_000
    )
    val row = Gauges.bookRow(s, 0, now, 13, Style.mono)
    assertEquals(row, s"BOOK  now ${tr(Some(0.0))} D+0.00 Q+0.00 ")
    assertEquals(
      Gauges.bookRow(s, 2, now, 13, Style.mono),
      s"BOOK ${Screen.dim} 30s${Screen.reset} ${tr(Some(-0.25))} D-0.25 Q-0.25 "
    )
    val coloured = Gauges.bookRow(s, 0, now, 13, Style.colour)
    assert(
      coloured.contains(Style.colour.bid + FULL + Screen.reset) || coloured.contains(
        Style.colour.ask + FULL + Screen.reset
      ),
      coloured
    )
  }

  test("a lens adds the current book once when the newest kept book is an older one") {
    val sec = now / 1000
    val s = s0.copy(
      book = Some(book("1", "1")),
      books = Vector((sec - 5, book("3", "1"))),
      since = now - 100_000
    )
    assertEquals(
      Gauges.bookRow(s, 1, now, 13, Style.mono),
      s"BOOK  10s ${tr(Some(0.25))} D+0.25 Q+0.25 "
    )
  }

  test("a lens label is bright, not dim, exactly when its window equals the time since since") {
    val s = s0.copy(book = Some(book("1", "1")), since = now - 10_000)
    val line = Gauges.bookRow(s, 1, now, 13, Style.mono)
    assert(!line.contains(Screen.dim), line)
  }

  test("tape rows carry the flow and trade count") {
    val s = s0.copy(
      tape = Vector(trade(5_000L, Side.Buy, "100", "1"), trade(30_000L, Side.Sell, "100", "3")),
      since = now - 1_000_000L
    )
    assertEquals(
      Gauges.tapeRow(s, 0, now, 13, Style.mono),
      s"TAPE  10s ${tr(Some(1.0))} F+1.00 n=1    "
    )
    assertEquals(
      Gauges.tapeRow(s, 1, now, 13, Style.mono),
      s"TAPE   1m ${tr(Some(-0.5))} F-0.50 n=2    "
    )
    val coloured = Gauges.tapeRow(s, 1, now, 13, Style.colour)
    assert(coloured.contains(Style.colour.ask + FULL + Screen.reset), coloured)
  }

  test("the tape window edge is exclusive: a trade exactly secs old is left out") {
    val s = s0.copy(tape = Vector(trade(10_000L, Side.Buy, "100", "1")), since = now - 1_000_000L)
    assertEquals(
      Gauges.tapeRow(s, 0, now, 13, Style.mono),
      s"TAPE  10s ${tr(None)} F   -- n=0    "
    )
  }

  test("six rows retain every book and tape lens in separate halves") {
    val rows = Gauges.rows(s0, now, 100, Style.colour).map(Chart.plain)
    assertEquals(rows.size, 6)
    assertEquals(rows.map(_.slice(5, 9).trim), Gauges.bookLenses.map(_._1))
    assertEquals(rows.map(_.slice(55, 59).trim), Gauges.tapeLenses.map(_._1))
    assert(rows.forall(_.length == 100))
    assert(rows.forall(r => r.take(50).contains("D   --") && r.take(50).contains("Q   --")))
    assert(rows.forall(_.drop(50).contains("F   --")))
  }

  test("each reference has six correlation readings in three label rows") {
    val labels = Gauges.correlationRows(s0, "BTC")
    assertEquals(labels.size, 3)
    assert(labels.forall(_.length == 30))
    assert(labels.head.startsWith("bitcoin 15m "))
    assertEquals(labels.map(l => "r   --".r.findAllIn(l).size), Vector(2, 2, 2))
    assert(labels(0).contains("15m") && labels(0).contains("1h"))
    assert(labels(1).contains("4h") && labels(1).contains("1d"))
    assert(labels(2).contains("1w") && labels(2).contains("1M"))
    assert(Gauges.correlationRows(s0, "PAXG").head.startsWith("gold"))
    assert(Gauges.correlationRows(s0, "ETH").head.startsWith("ETH"))
  }

  test("paired gauges fit every width and retain a six-digit trade count") {
    val deep = Book(
      (1 to 20).toVector.map(i => lvl((100 - i).toString, "10")),
      (1 to 20).toVector.map(i => lvl((101 + i).toString, "10")),
      1L
    )
    val tape =
      Vector.tabulate(100_000)(i => Trade(now - i.toLong * 9_999L / 100_000L, Side.Buy, "100", "1"))
    val s = s0.copy(
      book = Some(deep),
      books = Vector(now / 1000 -> deep),
      tape = tape,
      since = now - 100_000_000L
    )
    for cols <- 100 to 170 do
      val rows = Gauges.rows(s, now, cols, Style.colour).map(Chart.plain)
      assertEquals(rows.size, 6)
      assertEquals(rows.map(_.length).distinct, Vector(cols))
      assert(rows.head.drop(cols / 2).contains("n=100000"), rows.head)
  }

  test("a row too long for the width is cut and keeps its escapes whole") {
    val s = s0.copy(book = Some(book("1", "1")), since = now)
    val rows = Gauges.rows(s, now, 50, Style.colour)
    assertEquals(rows.map(r => Chart.plain(r).length).distinct, Vector(50))
    assert(rows(1).contains(Screen.dim) && rows(1).contains(Screen.reset), rows(1))
  }

  test("gauges and correlations over full retained data render within the existing limit") {
    val tape = Vector.tabulate(200_000)(i =>
      Trade(now - i.toLong * 70L, if i % 2 == 0 then Side.Buy else Side.Sell, "100", "1")
    )
    def full(seed: Double): Map[String, Vector[Candle]] =
      Vector("1m", "5m", "15m", "1h", "4h").map { interval =>
        val ms = State.intervalMs(interval)
        interval -> Vector.tabulate(State.seriesCap) { k =>
          val c = 100.0 + math.sin((k + seed) / 7.0)
          Candle(k * ms, c, c, c, c, 1.0)
        }
      }.toMap
    val s = s0.copy(
      book = Some(book("1", "1")),
      tape = tape,
      since = now - 100_000_000L,
      candles = full(0),
      refs = Map("BTC" -> Ref(Vector.empty, full(1)), "PAXG" -> Ref(Vector.empty, full(2)))
    )
    def render(): Vector[String] =
      Gauges.rows(s, now, 100, Style.colour) ++
        Gauges.correlationRows(s, "BTC") ++ Gauges.correlationRows(s, "PAXG")
    val _ = render()
    val _ = render()
    val started = System.nanoTime()
    val rows = render()
    val elapsed = System.nanoTime() - started
    assertEquals(rows.size, 12)
    assertEquals(rows.drop(6).map(l => "r[+-]\\d\\.\\d\\d".r.findAllIn(l).size).sum, 12)
    assert(elapsed < 2e8, s"took ${elapsed / 1e6} ms")
  }
