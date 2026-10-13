package desk

class SharesSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val M = now / 60_000L
  private val day = 86_400_000L

  private def near(v: Option[Double], x: Double): Unit =
    assert(v.exists(y => math.abs(y - x) <= 1e-9), s"$v is not $x")

  private val dayMinutes =
    (0 until 720).toVector.map(k => MinuteBook(M - k, 0.0, 1)).reverse

  private val firstBook = Book(Vector(Level("100", "3", 1)), Vector(Level("101", "1", 1)), 1L)
  private val bookEvent = Event.BookUpdate("XMR", firstBook, fromSocket = true)
  private val bboEvent = Event.BboUpdate("XMR", Level("100", "1", 1), Level("101", "3", 1), 2L)

  private def batch(ids: Vector[Long]) =
    Event.Trades("XMR", ids.map(id => Trade(now, Side.Buy, "100", "2", Some(id))))

  test("a book update adds to its minute") {
    val one = Shares.addBook(Vector.empty, now, 0.5)
    assertEquals(one, Vector(MinuteBook(M, 0.5, 1)))
    val two = Shares.addBook(one, now + 30_000L, -0.1)
    assertEquals(two.map(b => (b.minute, b.count)), Vector((M, 2)))
    assertEqualsDouble(two.head.sum, 0.4, 1e-12)
    val three = Shares.addBook(two, now + 60_000L, 0.2)
    assertEquals(three.map(_.minute), Vector(M, M + 1))
    assertEquals(Shares.addBook(three, now, 0.9), three)
    assertEquals(Shares.addBook(three, now + 60_000L, Double.NaN), three)
  }

  test("the book ring holds 1440 minutes") {
    val ring = (0 to 1440).foldLeft(Vector.empty[MinuteBook]) { (r, k) =>
      Shares.addBook(r, now + k * 60_000L, 0.0)
    }
    assertEquals(ring.size, 1440)
    assertEquals(ring.head.minute, M + 1)
    assertEquals(ring.last.minute, M + 1440)
  }

  test("trades add to their minutes in order and odd sizes are skipped") {
    val trades = Vector(
      Trade(now + 1_000L, Side.Buy, "100", "3"),
      Trade(now + 2_000L, Side.Sell, "100", "1"),
      Trade(now - 1_000L, Side.Buy, "100", "2"),
      Trade(now + 3_000L, Side.Buy, "100", "1e400"),
      Trade(now + 4_000L, Side.Sell, "100", "-1")
    )
    assertEquals(
      Shares.addTrades(Vector.empty, trades),
      Vector(MinuteTape(M - 1, 2.0, 0.0), MinuteTape(M, 3.0, 1.0))
    )
    val days = (0 to 1440).toVector.map(k => Trade(now - k * 60_000L, Side.Buy, "1", "1"))
    val ring = Shares.addTrades(Vector.empty, days)
    assertEquals(ring.size, 1440)
    assertEquals(ring.head.minute, M - 1439)
  }

  test("the shares over a window") {
    val s = Snapshot
      .initial("XMR", now)
      .copy(
        bookMinutes =
          Vector(MinuteBook(M - 2, 0.6, 2), MinuteBook(M - 1, -0.2, 1), MinuteBook(M, 0.2, 1)),
        tapeMinutes = Vector(MinuteTape(M - 2, 3.0, 1.0), MinuteTape(M, 0.0, 4.0))
      )
    val at = now + 30_000L
    near(Shares.book(s, 300_000L, at), 57.5)
    near(Shares.tape(s, 300_000L, at), 37.5)
    assertEquals(Shares.book(s, 900_000L, at), None)
    assertEquals(Shares.tape(s, 900_000L, at), None)
    val thin = s.copy(bookMinutes = s.bookMinutes.tail)
    assertEquals(Shares.book(thin, 300_000L, at), None)
    assertEquals(Shares.tape(thin, 300_000L, at), None)
    assertEquals(Shares.tape(s.copy(tapeMinutes = Vector.empty), 300_000L, at), None)
    assertEquals(
      Shares.tape(s.copy(tapeMinutes = Vector(MinuteTape(M, 0.0, 0.0))), 300_000L, at),
      None
    )
  }

  test("the window holds its own minutes only and an idle minute does not cover it") {
    val s = Snapshot
      .initial("XMR", now)
      .copy(
        bookMinutes = Vector(
          MinuteBook(M - 5, 1.0, 1),
          MinuteBook(M - 2, 0.6, 2),
          MinuteBook(M - 1, -0.2, 1),
          MinuteBook(M, 0.2, 1),
          MinuteBook(M + 1, 1.0, 1)
        ),
        tapeMinutes = Vector(
          MinuteTape(M - 5, 10.0, 0.0),
          MinuteTape(M - 2, 3.0, 1.0),
          MinuteTape(M, 0.0, 4.0),
          MinuteTape(M + 1, 10.0, 0.0)
        )
      )
    val at = now + 30_000L
    near(Shares.book(s, 300_000L, at), 57.5)
    near(Shares.tape(s, 300_000L, at), 37.5)
    val idle = s.copy(bookMinutes = s.bookMinutes.updated(1, MinuteBook(M - 2, 0.0, 0)))
    assertEquals(Shares.book(idle, 300_000L, at), None)
  }

  test("the 24h share needs twelve hours") {
    val s = Snapshot.initial("XMR", now).copy(bookMinutes = dayMinutes)
    near(Shares.book(s, day, now), 50.0)
    assertEquals(Shares.book(s.copy(bookMinutes = dayMinutes.tail), day, now), None)
  }

  private def fedState: Snapshot =
    val booked = State.reduce(Snapshot.initial("XMR", now), bookEvent, now)
    State.reduce(booked, bboEvent, now + 1_000L)

  test("the fold feeds the book ring from the selected market only") {
    val start = Snapshot.initial("XMR", now)
    val booked = State.reduce(start, bookEvent, now)
    assertEquals(booked.bookMinutes, Vector(MinuteBook(M, 0.5, 1)))
    val other = Event.BookUpdate("BTC", firstBook, fromSocket = true)
    assertEquals(State.reduce(start, other, now).bookMinutes, Vector.empty)
    val bbo = State.reduce(booked, bboEvent, now + 1_000L)
    assertEquals(bbo.bookMinutes.map(b => (b.minute, b.count)), Vector((M, 2)))
    assertEqualsDouble(bbo.bookMinutes.head.sum, 0.0, 1e-12)
  }

  test("a replayed batch is counted once") {
    val first = State.reduce(Snapshot.initial("XMR", now), batch(Vector(11L, 12L)), now)
    assertEquals(first.tapeMinutes, Vector(MinuteTape(M, 4.0, 0.0)))
    val second = State.reduce(first, batch(Vector(12L, 13L)), now)
    assertEquals(second.tapeMinutes, Vector(MinuteTape(M, 6.0, 0.0)))
    val other = Event.Trades("BTC", Vector(Trade(now, Side.Buy, "100", "2", Some(14L))))
    assertEquals(State.reduce(second, other, now).tapeMinutes, second.tapeMinutes)
  }

  test("a replayed batch counts its new, repeated and anonymous trades as the tape does") {
    val first = State.reduce(Snapshot.initial("XMR", now), batch(Vector(11L)), now)
    val replay = Vector(
      Trade(now, Side.Buy, "100", "2", Some(11L)),
      Trade(now, Side.Sell, "100", "1", Some(13L)),
      Trade(now, Side.Sell, "100", "1", Some(13L)),
      Trade(now, Side.Sell, "100", "5"),
      Trade(now, Side.Sell, "100", "5"),
      Trade(now - State.tapeMs - 1L, Side.Sell, "100", "7", Some(14L))
    )
    val second = State.reduce(first, Event.Trades("XMR", replay), now)
    assertEquals(second.tape.size, 4)
    assertEquals(second.tapeMinutes, Vector(MinuteTape(M, 2.0, 11.0)))
  }

  test("a market change resets both rings") {
    val primed = fedState.copy(tapeMinutes = Vector(MinuteTape(M, 1.0, 0.0)), prompt = Some("BTC"))
    assert(primed.bookMinutes.nonEmpty, "the book ring was not fed")
    val moved = State.reduce(primed, Event.Key(Command.PromptSubmit), now + 2_000L)
    assertEquals(moved.coin, "BTC")
    assertEquals(moved.bookMinutes, Vector.empty)
    assertEquals(moved.tapeMinutes, Vector.empty)
    val kept = primed.copy(prompt = Some("XMR"))
    val stayed = State.reduce(kept, Event.Key(Command.PromptSubmit), now + 2_000L)
    assertEquals(stayed.coin, "XMR")
    assertEquals(stayed.bookMinutes, kept.bookMinutes)
    assertEquals(stayed.tapeMinutes, kept.tapeMinutes)
  }

  test("huge sizes stay finite") {
    val huge = Vector(
      Trade(now, Side.Buy, "100", "1e100", Some(1L)),
      Trade(now, Side.Sell, "100", "1e100", Some(2L))
    )
    val s = State.reduce(Snapshot.initial("XMR", now), Event.Trades("XMR", huge), now)
    assertEquals(s.tapeMinutes.map(m => (m.buy.isFinite, m.sell.isFinite)), Vector((true, true)))
    val share = Shares.tape(s.copy(bookMinutes = dayMinutes), day, now)
    assert(share.exists(_.isFinite), s"$share is not finite")
  }

  test("the oldest minute of the window counts toward coverage and the shares") {
    val s = Snapshot
      .initial("XMR", now)
      .copy(
        bookMinutes =
          Vector(MinuteBook(M - 4, 1.0, 1), MinuteBook(M - 1, 0.0, 1), MinuteBook(M, 0.0, 1)),
        tapeMinutes = Vector(MinuteTape(M - 4, 4.0, 0.0), MinuteTape(M, 0.0, 1.0))
      )
    val at = now + 30_000L
    assertEqualsDouble(Shares.book(s, 300_000L, at).getOrElse(-1.0), 200.0 / 3, 1e-9)
    assertEqualsDouble(Shares.tape(s, 300_000L, at).getOrElse(-1.0), 80.0, 1e-9)
  }

  test("the book ring takes the depth weighted imbalance") {
    val book =
      Book(Vector(Level("100", "1", 1), Level("99", "10", 1)), Vector(Level("101", "1", 1)), 1L)
    val fed = State.reduce(Snapshot.initial("XMR", now), Event.BookUpdate("XMR", book, true), now)
    assertEquals(fed.bookMinutes.map(b => (b.minute, b.count)), Vector((M, 1)))
    assertEqualsDouble(fed.bookMinutes.head.sum, 7.0 / 9, 1e-12)
  }

  test("reference and stale books leave the book ring") {
    val book = Book(Vector(Level("100", "3", 1)), Vector(Level("101", "1", 1)), 5L)
    val fed = State.reduce(Snapshot.initial("XMR", now), Event.BookUpdate("XMR", book, true), now)
    val ref = Event.BboUpdate("BTC", Level("100", "1", 1), Level("101", "3", 1), 9L)
    assertEquals(State.reduce(fed, ref, now + 1_000L).bookMinutes, fed.bookMinutes)
    assertEquals(
      State.reduce(fed, Event.BookUpdate("XMR", book, true), now).bookMinutes,
      fed.bookMinutes
    )
    val older = Event.BookUpdate("XMR", book.copy(time = 4L), fromSocket = true)
    assertEquals(State.reduce(fed, older, now).bookMinutes, fed.bookMinutes)
    val staleBbo = Event.BboUpdate("XMR", Level("100", "1", 1), Level("101", "3", 1), 5L)
    assertEquals(State.reduce(fed, staleBbo, now).bookMinutes, fed.bookMinutes)
  }

  test("a fresh batch counts its repeats once and leaves expired trades out") {
    val batch = Vector(
      Trade(now, Side.Buy, "100", "2", Some(1L)),
      Trade(now, Side.Buy, "100", "2", Some(1L)),
      Trade(now - State.tapeMs - 1, Side.Buy, "100", "7", Some(2L)),
      Trade(now - State.tapeMs - 1, Side.Sell, "100", "5")
    )
    val s = State.reduce(Snapshot.initial("XMR", now), Event.Trades("XMR", batch), now)
    assertEquals(s.tape.size, 1)
    assertEquals(s.tapeMinutes, Vector(MinuteTape(M, 2.0, 0.0)))
    val replay = Vector(Trade(now - State.tapeMs - 1, Side.Sell, "100", "5"))
    assertEquals(State.reduce(s, Event.Trades("XMR", replay), now).tapeMinutes, s.tapeMinutes)
  }

  test("a trade lands in its own minute when the ring holds several") {
    val ring = Vector(MinuteTape(M - 1, 1.0, 0.0), MinuteTape(M, 1.0, 0.0))
    assertEquals(
      Shares.addTrades(ring, Vector(Trade(now, Side.Buy, "100", "1"))),
      Vector(MinuteTape(M - 1, 1.0, 0.0), MinuteTape(M, 2.0, 0.0))
    )
  }

  test("a window under a minute has no share") {
    val s = Snapshot.initial("XMR", now)
    assertEquals(Shares.book(s, 0L, now), None)
    assertEquals(Shares.book(s, 59_999L, now), None)
    assertEquals(Shares.tape(s, 59_999L, now), None)
  }

  test("sizes whose sum overflows have no tape share") {
    def share(trades: Vector[Trade]) =
      val ring = Shares.addTrades(Vector.empty, trades)
      Shares.tape(
        Snapshot.initial("XMR", now).copy(bookMinutes = dayMinutes, tapeMinutes = ring),
        day,
        now
      )
    val infinite =
      Vector(Trade(now, Side.Buy, "100", "1e308"), Trade(now, Side.Buy, "100", "1e308"))
    assertEquals(share(infinite), None)
    val overflowing = Vector(
      Trade(now, Side.Buy, "100", "1e308"),
      Trade(now, Side.Buy, "100", "5e307"),
      Trade(now, Side.Sell, "100", "1e308")
    )
    assertEquals(share(overflowing), None)
  }
