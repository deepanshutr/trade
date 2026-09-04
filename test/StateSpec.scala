package desk

class StateSpec extends munit.FunSuite:
  val t0 = 1_700_000_000_000L
  def lv(px: String, sz: String) = Level(px, sz, 1)
  def book(time: Long, bids: Seq[(String, String)], asks: Seq[(String, String)]) =
    Book(bids.map(lv).toVector, asks.map(lv).toVector, time)
  val b1 = book(1000, Seq("100" -> "1", "99" -> "2"), Seq("101" -> "3", "102" -> "4"))
  val s0 = Snapshot.initial("XMR", "5m", t0)
  private def bookAt(bid: String, ask: String, t: Long): Book =
    book(t, Seq(bid -> "1"), Seq(ask -> "1"))
  private def press(s: Snapshot, c: Command, now: Long = t0): Snapshot =
    State.reduce(s, Event.Key(c), now)

  test("the initial snapshot seeds the feed six seconds stale so the screen starts honest") {
    assertEquals(State.lastMsgAt(Snapshot.initial("X", "5m", t0).feed), t0 - 6000)
  }
  test("a socket book marks the socket live and replaces the book") {
    val s = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0 + 10)
    assertEquals(s.book, Some(b1))
    assertEquals(s.feed, Health.Live(t0 + 10))
  }
  test("a REST book replaces the book but leaves the socket clock alone") {
    val s = State.reduce(s0, Event.BookUpdate(b1, fromSocket = false), t0 + 10)
    assertEquals(s.book, Some(b1))
    assertEquals(s.feed, s0.feed)
  }
  test("an older book is ignored, a newer one wins, from either source") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val older = b1.copy(time = 999)
    val s2 = State.reduce(s1, Event.BookUpdate(older, fromSocket = false), t0 + 1)
    assertEquals(s2.book, Some(b1))
    val newer = b1.copy(time = 1001)
    val s3 = State.reduce(s2, Event.BookUpdate(newer, fromSocket = false), t0 + 2)
    assertEquals(s3.book, Some(newer))
  }
  test("a book at the same time as the current book is ignored, not just an older one") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val sameTime = b1.copy(bids = Vector(lv("999", "999")))
    val s2 = State.reduce(s1, Event.BookUpdate(sameTime, fromSocket = false), t0 + 1)
    assertEquals(s2.book, Some(b1))
  }
  test("a bbo newer than the book replaces the best levels and drops crossed ones") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val s2 = State.reduce(s1, Event.BboUpdate(lv("100.5", "7"), lv("101.5", "8"), 2000), t0 + 1)
    val b = s2.book.get
    assertEquals(b.bids.map(_.px), Vector("100.5", "99"))
    assertEquals(b.asks.map(_.px), Vector("101.5", "102"))
    assertEquals(b.time, 2000L)
    assertEquals(s2.feed, Health.Live(t0 + 1))
  }
  test("a bbo update drops old book levels crossed by the new best price") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val s2 = State.reduce(s1, Event.BboUpdate(lv("98", "5"), lv("103", "6"), 2000), t0 + 1)
    val b = s2.book.get
    assertEquals(b.bids.map(_.px), Vector("98"))
    assertEquals(b.asks.map(_.px), Vector("103"))
  }
  test("a bbo at the same best prices replaces the sizes only") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val s2 = State.reduce(s1, Event.BboUpdate(lv("100", "9"), lv("101", "9"), 2000), t0 + 1)
    assertEquals(s2.book.get.bids.map(l => (l.px, l.sz)), Vector("100" -> "9", "99" -> "2"))
    assertEquals(s2.book.get.asks.map(l => (l.px, l.sz)), Vector("101" -> "9", "102" -> "4"))
  }
  test("an older bbo is ignored and a bbo with no book creates a one level book") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val s2 = State.reduce(s1, Event.BboUpdate(lv("50", "1"), lv("60", "1"), 500), t0 + 1)
    assertEquals(s2.book, Some(b1))
    val s3 = State.reduce(s0, Event.BboUpdate(lv("50", "1"), lv("60", "1"), 500), t0 + 1)
    assertEquals(s3.book, Some(Book(Vector(lv("50", "1")), Vector(lv("60", "1")), 500)))
  }
  test("a bbo update on a book with an empty side does not throw") {
    val thin = book(1000, Seq.empty, Seq("101" -> "3", "102" -> "4"))
    val s1 = State.reduce(s0, Event.BookUpdate(thin, fromSocket = true), t0)
    val s2 = State.reduce(s1, Event.BboUpdate(lv("100.5", "7"), lv("101.5", "8"), 2000), t0 + 1)
    val b = s2.book.get
    assertEquals(b.bids, Vector(lv("100.5", "7")))
    assertEquals(b.asks, Vector(lv("101.5", "8"), lv("102", "4")))
  }
  test("trades go to the front newest first, kept uncapped well under two hundred thousand") {
    val batch = Vector(Trade(t0 + 1, Side.Buy, "1", "1"), Trade(t0 + 2, Side.Sell, "1", "1"))
    val s1 = State.reduce(s0, Event.Trades(batch), t0)
    assertEquals(s1.tape.map(_.time), Vector(t0 + 2, t0 + 1))
    val many = (3 to 300).map(i => Trade(t0 + i, Side.Buy, "1", "1")).toVector
    val s2 = State.reduce(s1, Event.Trades(many), t0)
    assertEquals(s2.tape.size, 300)
    assertEquals(s2.tape.head.time, t0 + 300)
  }
  test("a candle replaces the one with the same start or appends, capped at 1200, in time order") {
    def c(t: Long, close: Double) = Candle(t, 1, 1, 1, close, 1)
    val s1 = State.reduce(s0, Event.CandleUpdate(c(10, 1)), t0)
    val s2 = State.reduce(s1, Event.CandleUpdate(c(10, 2)), t0)
    assertEquals(s2.candles, Vector(c(10, 2)))
    val s3 = State.reduce(s2, Event.CandleUpdate(c(5, 3)), t0)
    assertEquals(s3.candles.map(_.t), Vector(5L, 10L))
    val s4 =
      (11 to 1220).foldLeft(s3)((s, i) => State.reduce(s, Event.CandleUpdate(c(i.toLong, 0)), t0))
    assertEquals(s4.candles.size, 1200)
    assertEquals(s4.candles.last.t, 1220L)
  }
  test(
    "history replaces all candles and a failed history counts as bad without touching the clock"
  ) {
    def c(t: Long) = Candle(t, 1, 1, 1, 1, 1)
    val s1 = State.reduce(s0, Event.CandleUpdate(c(1)), t0)
    val s2 = State.reduce(s1, Event.CandleHistory(Vector(c(7), c(6))), t0 + 5)
    assertEquals(s2.candles.map(_.t), Vector(6L, 7L))
    val s3 = State.reduce(s2, Event.CandleHistoryFailed, t0 + 9)
    assertEquals(s3.bad, 1)
    assertEquals(s3.feed, s1.feed)
  }
  test("context, pong and bad all mark the socket live, bad also counts") {
    val ctx = Ctx("1", "1", "0", "0", "0", "1", None)
    val s1 = State.reduce(s0, Event.CtxUpdate(ctx), t0 + 1)
    assertEquals(s1.ctx, Some(ctx))
    assertEquals(s1.feed, Health.Live(t0 + 1))
    val s2 = State.reduce(s1, Event.Pong, t0 + 2)
    assertEquals(s2.feed, Health.Live(t0 + 2))
    val s3 = State.reduce(s2, Event.Bad("x"), t0 + 3)
    assertEquals(s3.bad, 1)
    assertEquals(s3.feed, Health.Live(t0 + 3))
  }
  test("socket down keeps the last message time, socket up marks live") {
    val s1 = State.reduce(s0, Event.Pong, t0 + 1)
    val s2 = State.reduce(s1, Event.SocketDown, t0 + 5)
    assertEquals(s2.feed, Health.Down(t0 + 5, t0 + 1))
    assertEquals(State.lastMsgAt(s2.feed), t0 + 1)
    val s3 = State.reduce(s2, Event.SocketUp, t0 + 9)
    assertEquals(s3.feed, Health.Live(t0 + 9))
  }
  test("quit sets the flag") {
    assert(State.reduce(s0, Event.Key(Command.Quit), t0).quit)
  }
  test("next interval cycles, wraps, enters the cycle from outside, and clears candles") {
    val withCandle = s0.copy(candles = Vector(Candle(1, 1, 1, 1, 1, 1)))
    val s1 = State.reduce(withCandle, Event.Key(Command.NextInterval), t0)
    assertEquals(s1.interval, "15m")
    assertEquals(s1.candles, Vector.empty)
    val s2 = State.reduce(s0.copy(interval = "1d"), Event.Key(Command.NextInterval), t0)
    assertEquals(s2.interval, "1m")
    val s3 = State.reduce(s0.copy(interval = "3m"), Event.Key(Command.NextInterval), t0)
    assertEquals(s3.interval, "1m")
  }
  test("the prompt opens, takes characters, backspaces, cancels") {
    val s1 = State.reduce(s0, Event.Key(Command.StartMarketPrompt), t0)
    assertEquals(s1.prompt, Some(""))
    val s2 = State.reduce(s1, Event.Key(Command.PromptChar('B')), t0)
    val s3 = State.reduce(s2, Event.Key(Command.PromptChar('T')), t0)
    assertEquals(s3.prompt, Some("BT"))
    val s4 = State.reduce(s3, Event.Key(Command.PromptBackspace), t0)
    assertEquals(s4.prompt, Some("B"))
    val s5 = State.reduce(s4, Event.Key(Command.PromptCancel), t0)
    assertEquals(s5.prompt, None)
    assertEquals(State.reduce(s0, Event.Key(Command.PromptChar('x')), t0).prompt, None)
  }
  test("submit switches the coin and clears exactly book, tape, candles and ctx") {
    val withBook = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val withTape = State.reduce(withBook, Event.Trades(Vector(Trade(1, Side.Buy, "1", "1"))), t0)
    val full = State.reduce(withTape, Event.CtxUpdate(Ctx("1", "1", "0", "0", "0", "1", None)), t0)
    val opened = State.reduce(full, Event.Key(Command.StartMarketPrompt), t0)
    val typed = State.reduce(opened, Event.Key(Command.PromptChar('B')), t0)
    val s = State.reduce(typed, Event.Key(Command.PromptSubmit), t0)
    assertEquals(s.coin, "B")
    assertEquals(s.prompt, None)
    assertEquals(s.book, None)
    assertEquals(s.tape, Vector.empty)
    assertEquals(s.candles, Vector.empty)
    assertEquals(s.ctx, None)
    assertEquals(s.interval, full.interval)
    assertEquals(s.feed, full.feed)
    assertEquals(s.bad, full.bad)
    val empty = State.reduce(opened, Event.Key(Command.PromptSubmit), t0)
    assertEquals(empty.coin, "XMR")
    assertEquals(empty.prompt, None)
    assertEquals(empty.book, full.book)
  }
  test("submitting the current coin only closes the prompt, changing nothing else") {
    val withBook = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    val withTape = State.reduce(withBook, Event.Trades(Vector(Trade(1, Side.Buy, "1", "1"))), t0)
    val full = State.reduce(withTape, Event.CtxUpdate(Ctx("1", "1", "0", "0", "0", "1", None)), t0)
    val opened = State.reduce(full, Event.Key(Command.StartMarketPrompt), t0)
    val typed =
      "XMR".foldLeft(opened)((s, c) => State.reduce(s, Event.Key(Command.PromptChar(c)), t0))
    assertEquals(typed.prompt, Some("XMR"))
    val s = State.reduce(typed, Event.Key(Command.PromptSubmit), t0)
    assertEquals(s.coin, "XMR")
    assertEquals(s.prompt, None)
    assertEquals(s.book, full.book)
    assertEquals(s.tape, full.tape)
    assertEquals(s.candles, full.candles)
    assertEquals(s.ctx, full.ctx)
  }

  test(
    "a book update samples the mid for its second and a later one in the same second replaces it"
  ) {
    val a =
      State.reduce(s0, Event.BookUpdate(bookAt("100", "102", t0 + 1), fromSocket = true), t0 + 100)
    assertEquals(a.samples, Vector((t0 / 1000, 101.0)))
    val b =
      State.reduce(a, Event.BookUpdate(bookAt("100", "104", t0 + 2), fromSocket = true), t0 + 900)
    assertEquals(b.samples, Vector((t0 / 1000, 102.0)))
    assertEquals(b.books.map(_._1), Vector(t0 / 1000))
    assertEquals(b.books.head._2.asks.head.px, "104")
    val c =
      State.reduce(b, Event.BookUpdate(bookAt("100", "106", t0 + 3), fromSocket = true), t0 + 1500)
    assertEquals(c.samples.map(_._1), Vector(t0 / 1000, t0 / 1000 + 1))
    assertEquals(c.books.map(_._1), Vector(t0 / 1000, t0 / 1000 + 1))
  }

  test("a best bid and ask update samples too") {
    val a = State.reduce(s0, Event.BboUpdate(lv("10", "1"), lv("12", "1"), t0 + 1), t0 + 1)
    assertEquals(a.samples, Vector((t0 / 1000, 11.0)))
    assertEquals(a.books.size, 1)
  }

  test("samples older than six hours and books older than fifteen minutes are dropped") {
    val a = State.reduce(s0, Event.BookUpdate(bookAt("100", "102", t0 + 1), fromSocket = true), t0)
    val later = t0 + State.samplesMs + 1000
    val b = State.reduce(a, Event.BookUpdate(bookAt("100", "102", later), fromSocket = true), later)
    assertEquals(b.samples.map(_._1), Vector(later / 1000))
    val later2 = t0 + State.booksMs + 1000
    val c =
      State.reduce(a, Event.BookUpdate(bookAt("100", "102", later2), fromSocket = true), later2)
    assertEquals(c.books.map(_._1), Vector(later2 / 1000))
    assertEquals(c.samples.map(_._1), Vector(t0 / 1000, later2 / 1000))
  }

  test("trades older than four hours are dropped and the cap is two hundred thousand") {
    val old = Trade(t0 - State.tapeMs - 1, Side.Buy, "1", "1")
    val fresh = Trade(t0, Side.Sell, "1", "1")
    val a = State.reduce(s0, Event.Trades(Vector(old, fresh)), t0)
    assertEquals(a.tape, Vector(fresh))
    assertEquals(State.tapeCap, 200_000)
  }

  test("candle history keeps the last twelve hundred") {
    val cs = (0 until 1300).toVector.map(i => Candle(i * 60_000L, 1, 1, 1, 1, 1))
    val a = State.reduce(s0, Event.CandleHistory(cs), t0)
    assertEquals(a.candles.size, 1200)
    assertEquals(a.candles.head.t, 100 * 60_000L)
  }

  test("a resize is stored") {
    val a = State.reduce(s0, Event.Resize(120, 40), t0)
    assertEquals((a.cols, a.rows), (120, 40))
    assertEquals((s0.cols, s0.rows), (100, 30))
  }

  test("zoom walks the table and stops at both ends") {
    assertEquals(press(s0, Command.ZoomIn).zoom, 0)
    val out = (1 to 9).foldLeft(s0)((s, _) => press(s, Command.ZoomOut))
    assertEquals(out.zoom, State.zooms.size - 1)
    assertEquals(State.zooms(out.zoom), 300)
    assertEquals(State.zooms, Vector(1, 2, 5, 10, 30, 60, 120, 300))
    assertEquals(press(out, Command.ZoomIn).zoom, State.zooms.size - 2)
  }

  test("scrub back adds a quarter window at the zoom and scrub forward clamps at zero") {
    val back = press(s0, Command.ScrubBack)
    assertEquals(back.offsetSec, 30L)
    val wide = press(State.reduce(s0, Event.Resize(120, 40), t0), Command.ZoomOut)
    assertEquals(press(wide, Command.ScrubBack).offsetSec, 80L)
    val fwd = press(back, Command.ScrubForward)
    assertEquals(fwd.offsetSec, 0L)
    assertEquals(press(fwd, Command.ScrubForward).offsetSec, 0L)
  }

  test("a zoom change keeps the offset") {
    val back = press(s0, Command.ScrubBack)
    assertEquals(press(back, Command.ZoomOut).offsetSec, 30L)
  }

  test("a market switch clears the rings, resets the offset and since, and keeps the zoom") {
    val busy = State.reduce(
      press(press(s0, Command.ScrubBack), Command.ZoomOut),
      Event.BookUpdate(bookAt("1", "2", t0 + 1), fromSocket = true),
      t0
    )
    assert(busy.samples.nonEmpty && busy.books.nonEmpty)
    val typed =
      "BTC".foldLeft(press(busy, Command.StartMarketPrompt))((s, c) =>
        press(s, Command.PromptChar(c))
      )
    val switched = press(typed, Command.PromptSubmit, t0 + 5000)
    assertEquals(switched.coin, "BTC")
    assert(switched.samples.isEmpty && switched.books.isEmpty)
    assertEquals(switched.offsetSec, 0L)
    assertEquals(switched.since, t0 + 5000)
    assertEquals(switched.zoom, 1)
    assertEquals(s0.since, t0)
  }
