package desk

import java.nio.file.{Files, Paths}

class EventsSpec extends munit.FunSuite:
  def fixture(name: String): String = Files.readString(Paths.get("fixtures", name))

  test("a null input is bad, not an exception") {
    assert(Events.parse(null).exists(_.isInstanceOf[Event.Bad]))
  }
  test("l2Book parses to a socket book with 20 levels a side") {
    Events.parse(fixture("l2Book.json")) match
      case Some(Event.BookUpdate("XMR", b, true)) =>
        assertEquals(b.bids.size, 20)
        assertEquals(b.asks.size, 20)
        assertEquals(b.bids.head, Level("522.24", "0.766", 1))
        assertEquals(b.asks.head, Level("522.28", "5.0", 1))
        assertEquals(b.time, 1788345725175L)
      case other => fail(s"unexpected $other")
  }
  test("bbo parses both sides and the time") {
    assertEquals(
      Events.parse(fixture("bbo.json")),
      Some(
        Event.BboUpdate(
          "XMR",
          Level("522.24", "0.766", 1),
          Level("522.28", "5.487", 2),
          1788345725728L
        )
      )
    )
  }
  test("bbo with an empty side is ignored") {
    val text =
      """{"channel":"bbo","data":{"coin":"XMR","time":1,"bbo":[null,{"px":"1","sz":"1","n":1}]}}"""
    assertEquals(Events.parse(text), None)
  }
  test("trades parse in order with B as buy and A as sell") {
    Events.parse(fixture("trades.json")) match
      case Some(Event.Trades("XMR", ts)) =>
        assertEquals(ts.size, 30)
        assertEquals(
          ts.head,
          Trade(1788345676023L, Side.Buy, "522.05", "0.421", Some(0L))
        )
        assert(ts.exists(_.side == Side.Sell))
        assertEquals(ts.last.time, 1788345719062L)
      case other => fail(s"unexpected $other")
  }
  test("a trade with an unknown side is bad") {
    val text =
      """{"channel":"trades","data":[{"coin":"XMR","side":"X","px":"1","sz":"1","time":1}]}"""
    assert(Events.parse(text).exists(_.isInstanceOf[Event.Bad]))
  }
  test("candle parses its text prices to numbers") {
    assertEquals(
      Events.parse(fixture("candle.json")),
      Some(
        Event.CandleUpdate(
          "XMR",
          "1m",
          Candle(1788345720000L, 522.27, 522.27, 522.27, 522.27, 0.493)
        )
      )
    )
  }
  test("candle events carry their own interval, not always 1m") {
    val text = fixture("candle.json").replace("\"i\":\"1m\"", "\"i\":\"5m\"")
    Events.parse(text) match
      case Some(Event.CandleUpdate(_, i, _)) => assertEquals(i, "5m")
      case other                             => fail(s"unexpected $other")
  }
  test("activeAssetCtx keeps every figure as text") {
    assertEquals(
      Events.parse(fixture("activeAssetCtx.json")),
      Some(
        Event.CtxUpdate(
          "XMR",
          Ctx(
            "522.28",
            "521.515",
            "0.0001302411",
            "210160.1640000001",
            "31202156.1599900015",
            "529.78",
            Some("522.26")
          )
        )
      )
    )
  }
  test("a context with a null mid keeps mid empty") {
    val text = fixture("activeAssetCtx.json").replace("\"midPx\":\"522.26\"", "\"midPx\":null")
    Events.parse(text) match
      case Some(Event.CtxUpdate(_, c)) => assertEquals(c.mid, None)
      case other                       => fail(s"unexpected $other")
  }
  test("pong parses and subscriptionResponse is ignored") {
    assertEquals(Events.parse(fixture("pong.json")), Some(Event.Pong))
    assertEquals(Events.parse(fixture("subscriptionResponse.json")), None)
  }
  test("empty, truncated, non JSON and off channel text are bad") {
    val texts = Seq(
      "",
      "{",
      "not json",
      fixture("l2Book.json").take(200),
      """{"channel":"weird","data":1}""",
      """{"data":1}"""
    )
    for text <- texts do assert(Events.parse(text).exists(_.isInstanceOf[Event.Bad]), text)
  }
  test("an unknown extra field still parses") {
    val text = fixture("bbo.json").replace("\"coin\":\"XMR\"", "\"coin\":\"XMR\",\"extra\":[1,2]")
    assert(Events.parse(text).exists(_.isInstanceOf[Event.BboUpdate]))
  }
  test("market events carry their market and candles their interval") {
    val bbo = Events.parse(fixture("bbo.json")).get
    assert(
      bbo match { case Event.BboUpdate("XMR", _, _, _) => true; case _ => false },
      bbo.toString
    )
    val one = Events.parse(fixture("candle.json")).get
    assert(
      one match { case Event.CandleUpdate("XMR", "1m", _) => true; case _ => false },
      one.toString
    )
    val text = fixture("bbo.json").replace("\"XMR\"", "\"BTC\"")
    assert(Events.parse(text).get match {
      case Event.BboUpdate("BTC", _, _, _) => true; case _ => false
    })
  }
  test("a trades message with no trades yields no event") {
    assertEquals(Events.parse("""{"channel":"trades","data":[]}"""), None)
  }
  test("a REST book reply parses to a book") {
    val b = Events.parseRestBook(fixture("rest_l2Book.json")).get
    assertEquals(b.bids.size, 20)
    assertEquals(b.asks.head, Level("522.27", "0.273", 1))
    assertEquals(b.time, 1788345727341L)
  }
  test("a REST candle reply parses in order") {
    val cs = Events.parseRestCandles(fixture("rest_candles.json")).get
    assertEquals(cs.size, 41)
    assertEquals(cs.head.t, 1788327600000L)
    assertEquals(cs.head.o, 520.71)
    assertEquals(cs.last.t, 1788339600000L)
  }
  test("REST parse failures are None") {
    assertEquals(Events.parseRestBook("{"), None)
    assertEquals(Events.parseRestBook("""{"coin":"X"}"""), None)
    assertEquals(Events.parseRestCandles("nope"), None)
  }
