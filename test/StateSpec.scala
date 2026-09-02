package desk

class StateSpec extends munit.FunSuite:
  val t0 = 1_700_000_000_000L
  def lv(px: String, sz: String) = Level(px, sz, 1)
  def book(time: Long, bids: Seq[(String, String)], asks: Seq[(String, String)]) =
    Book(bids.map(lv).toVector, asks.map(lv).toVector, time)
  val b1 = book(1000, Seq("100" -> "1", "99" -> "2"), Seq("101" -> "3", "102" -> "4"))
  val s0 = Snapshot.initial("XMR", "5m", t0)

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
  test("a book adds its sizes to the window, one sample per second, pruned at 60 seconds") {
    val s1 = State.reduce(s0, Event.BookUpdate(b1, fromSocket = true), t0)
    assertEquals(s1.sizes, Vector((t0, Vector(1.0, 2.0, 3.0, 4.0))))
    val s2 = State.reduce(s1, Event.BookUpdate(b1.copy(time = 1001), fromSocket = true), t0 + 500)
    assertEquals(s2.sizes.size, 1)
    val s3 = State.reduce(s2, Event.BookUpdate(b1.copy(time = 1002), fromSocket = true), t0 + 1000)
    assertEquals(s3.sizes.size, 2)
    val s4 =
      State.reduce(s3, Event.BookUpdate(b1.copy(time = 1003), fromSocket = true), t0 + 61_000)
    assertEquals(s4.sizes.map(_._1), Vector(t0 + 1000, t0 + 61_000))
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
  test("trades go to the front newest first and the tape is capped at 200") {
    val batch = Vector(Trade(1, Side.Buy, "1", "1"), Trade(2, Side.Sell, "1", "1"))
    val s1 = State.reduce(s0, Event.Trades(batch), t0)
    assertEquals(s1.tape.map(_.time), Vector(2L, 1L))
    val many = (3 to 300).map(i => Trade(i.toLong, Side.Buy, "1", "1")).toVector
    val s2 = State.reduce(s1, Event.Trades(many), t0)
    assertEquals(s2.tape.size, 200)
    assertEquals(s2.tape.head.time, 300L)
  }
  test("a candle replaces the one with the same start or appends, capped at 200, in time order") {
    def c(t: Long, close: Double) = Candle(t, 1, 1, 1, close, 1)
    val s1 = State.reduce(s0, Event.CandleUpdate(c(10, 1)), t0)
    val s2 = State.reduce(s1, Event.CandleUpdate(c(10, 2)), t0)
    assertEquals(s2.candles, Vector(c(10, 2)))
    val s3 = State.reduce(s2, Event.CandleUpdate(c(5, 3)), t0)
    assertEquals(s3.candles.map(_.t), Vector(5L, 10L))
    val s4 =
      (11 to 220).foldLeft(s3)((s, i) => State.reduce(s, Event.CandleUpdate(c(i.toLong, 0)), t0))
    assertEquals(s4.candles.size, 200)
    assertEquals(s4.candles.last.t, 220L)
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
  test("submit switches the coin and clears exactly book, tape, candles, ctx and sizes") {
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
    assertEquals(s.sizes, Vector.empty)
    assertEquals(s.interval, full.interval)
    assertEquals(s.feed, full.feed)
    assertEquals(s.bad, full.bad)
    val empty = State.reduce(opened, Event.Key(Command.PromptSubmit), t0)
    assertEquals(empty.coin, "XMR")
    assertEquals(empty.prompt, None)
    assertEquals(empty.book, full.book)
  }
