package desk

import java.nio.file.{Files, Paths}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.LinkedBlockingQueue

class OnchainSpec extends munit.FunSuite:
  // Made-up values in BNB Chain's shape: the token/quote pair reserves and the USDT/quote pool price.
  private val reserveToken = BigInt("20000000000000000000000000")
  private val reserveQuote = BigInt("5000000000000000000000")
  private val sqrtPrice = BigInt("19807040628566084398385987584")
  private val token = "0x1111111111111111111111111111111111111111"
  private val quote = "0x9999999999999999999999999999999999999999"
  private val stable = "0x55d398326f99059ff775485246999027b3197955"
  private val pair = "0x3333333333333333333333333333333333333333"
  private val pool = "0x4444444444444444444444444444444444444444"
  private val pools = Pools(token, quote, stable, 18, 18, 18, true, true)
  private val market =
    OnchainMarket("DEMO", pair, pool, Vector("https://a.example", "https://b.example"))

  private def hexWord(n: BigInt): String = n.toString(16).reverse.padTo(64, '0').reverse
  private def hexAddress(a: String): String = "0x" + ("0" * 24) + a.stripPrefix("0x")
  private def within(actual: Double, expected: Double, tolerance: Double): Boolean =
    math.abs(actual - expected) <= expected.abs * tolerance
  private def socketEvents(events: Vector[Event]): Vector[Event] =
    events.filter(e => e == Event.SocketUp || e == Event.SocketDown)
  private def drain(queue: LinkedBlockingQueue[Event]): Vector[Event] =
    Iterator.continually(queue.poll()).takeWhile(_ != null).toVector

  test("the dollar price multiplies quote per token by dollars per quote") {
    val dollars = Onchain.dollarsPerQuote(sqrtPrice, pools)
    assert(within(dollars, 16.0, 0.002), dollars.toString)
    val price = Onchain.price(reserveToken, reserveQuote, sqrtPrice, pools)
    assert(within(price, 0.004, 0.002), price.toString)
    val flipped = pools.copy(tokenIsPairToken0 = false)
    assert(within(Onchain.price(reserveQuote, reserveToken, sqrtPrice, flipped), price, 1e-9))
    val quoteFirst = pools.copy(quoteIsPoolToken1 = false)
    assert(within(Onchain.dollarsPerQuote(sqrtPrice, quoteFirst), 1 / 16.0, 0.002))
  }

  test("decimals scale the price on every side and empty pools are refused") {
    val sixDecimalToken = pools.copy(tokenDecimals = 6)
    val price =
      Onchain.price(reserveToken / BigInt(10).pow(12), reserveQuote, sqrtPrice, sixDecimalToken)
    assert(within(price, 0.004, 0.002), price.toString)
    // a six decimal stable against an eighteen decimal quote scales the dollar price by 1e12
    val sixDecimalStable = Onchain.dollarsPerQuote(sqrtPrice, pools.copy(stableDecimals = 6))
    assert(within(sixDecimalStable, 16.0e12, 0.002), sixDecimalStable.toString)
    intercept[IllegalArgumentException](Onchain.price(BigInt(0), reserveQuote, sqrtPrice, pools))
    intercept[IllegalArgumentException](Onchain.dollarsPerQuote(BigInt(0), pools))
  }

  test("impact sizes grow with the impact and follow the constant product") {
    val reserve = 20_000_000.0
    val bids = Onchain.impacts.map(Onchain.bidSize(reserve, _))
    val asks = Onchain.impacts.map(Onchain.askSize(reserve, _))
    assert(bids == bids.sorted && asks == asks.sorted)
    assert(bids.forall(_ > 0) && asks.forall(_ > 0))
    assert(within(Onchain.bidSize(reserve, 0.01), 20_000_000 * 0.0050378, 0.001))
    assert(within(Onchain.askSize(reserve, 0.01), 20_000_000 * 0.0049629, 0.001))
    assert(Onchain.bidSize(reserve, 0.01) > Onchain.askSize(reserve, 0.01))
  }

  test("the book has per level sizes, bids below and asks above, the fee as the first spread") {
    val reserve = 20_000_000.0
    val book = Onchain.book(0.004, reserve, 1000L)
    assertEquals(book.time, 1000L)
    assertEquals(book.bids.size, Onchain.impacts.size)
    assertEquals(book.bids.head.px, "0.00399")
    assertEquals(book.asks.head.px, "0.00401")
    val bidPrices = book.bids.map(_.px.toDouble)
    val askPrices = book.asks.map(_.px.toDouble)
    assert(bidPrices == bidPrices.sorted.reverse && askPrices == askPrices.sorted)
    assert(book.bids.forall(l => l.sz.toDouble > 0 && l.n == 1))
    assert(within(State.mid(book).get, 0.004, 1e-9))
    // each level is the band between two impacts, so the levels sum to the deepest impact
    assert(within(book.bids.head.sz.toDouble, Onchain.bidSize(reserve, 0.0025), 1e-5))
    val secondBand = Onchain.bidSize(reserve, 0.005) - Onchain.bidSize(reserve, 0.0025)
    assert(within(book.bids(1).sz.toDouble, secondBand, 1e-5))
    assert(within(book.bids.map(_.sz.toDouble).sum, Onchain.bidSize(reserve, 0.1), 1e-5))
    assert(within(book.asks.map(_.sz.toDouble).sum, Onchain.askSize(reserve, 0.1), 1e-5))
  }

  test("swap logs decode into dollar priced trades with stable ids") {
    def log(block: Long, index: Long, in0: BigInt, in1: BigInt, out0: BigInt, out1: BigInt) =
      ujson.Obj(
        "blockNumber" -> ("0x" + block.toHexString),
        "logIndex" -> ("0x" + index.toHexString),
        "data" -> ("0x" + Vector(in0, in1, out0, out1).map(hexWord).mkString)
      )
    val tokens = BigInt(10).pow(18) * 5000
    val quotes = BigInt("1250000000000000000")
    val sell = log(1000L, 7L, tokens, 0, 0, quotes)
    val buy = log(1008L, 2L, 0, quotes, tokens, 0)
    val mint = log(1009L, 0L, 0, 0, 0, 0)
    val trades = Onchain.swaps(ujson.Arr(sell, buy, mint), pools, 16.0, 5000L)
    assertEquals(trades.map(_.side), Vector(Side.Sell, Side.Buy))
    assertEquals(trades.head.time, 5000L)
    assertEquals(trades.head.sz, "5000")
    assert(within(trades.head.px.toDouble, 0.00025 * 16.0, 0.001), trades.head.px)
    assertEquals(trades.head.id, Some(1000L * 100_000L + 7L))
    assertEquals(trades(1).id, Some(1008L * 100_000L + 2L))
    val flipped = Onchain.swaps(
      ujson.Arr(log(1L, 0L, 0, tokens, quotes, 0)),
      pools.copy(tokenIsPairToken0 = false),
      16.0,
      1L
    )
    assertEquals(flipped.map(_.side), Vector(Side.Sell))
  }

  test("candles per interval open, extend and roll over by bucket") {
    val builder = new Onchain.CandleBuilder
    val first = builder.add(60_000L * 10 + 5_000L, 2.0, 1.0).toMap
    assertEquals(first("1m"), Candle(600_000L, 2.0, 2.0, 2.0, 2.0, 1.0))
    val second = builder.add(60_000L * 10 + 30_000L, 3.0, 2.0).toMap
    assertEquals(second("1m"), Candle(600_000L, 2.0, 3.0, 2.0, 3.0, 3.0))
    assertEquals(second("5m").t, 600_000L)
    val third = builder.add(60_000L * 11 + 1_000L, 1.0, 0.0).toMap
    assertEquals(third("1m"), Candle(660_000L, 1.0, 1.0, 1.0, 1.0, 0.0))
    assertEquals(third("5m"), Candle(600_000L, 2.0, 3.0, 1.0, 1.0, 3.0))
    assertEquals(third.keySet, State.intervals.toSet)
  }

  test("a batch body numbers calls and results come back in call order") {
    val calls = Vector(
      Onchain.call(pair, Onchain.getReserves),
      Onchain.call(pool, Onchain.slot0),
      ("eth_blockNumber", ujson.Arr())
    )
    val body = ujson.read(Onchain.batchBody(calls)).arr.toVector
    assertEquals(body.map(_("id").num.toInt), Vector(0, 1, 2))
    assertEquals(body(0)("method").str, "eth_call")
    assertEquals(body(0)("params")(0)("to").str, pair)
    val results =
      Onchain.batchResults(Files.readString(Paths.get("fixtures", "onchain_batch.json")), 3).get
    assertEquals(Onchain.word(results(0).str, 0), BigInt("20000000000000000000000000"))
    assertEquals(Onchain.word(results(0).str, 1), BigInt("5000000000000000000000"))
    assertEquals(Onchain.word(results(1).str, 0), BigInt(1))
    assertEquals(Onchain.quantity(results(2).str), 1234567L)
    assertEquals(
      Onchain.batchResults("""[{"id":0,"error":{"code":-32000}},{"id":1,"result":"0x1"}]""", 2),
      None
    )
    // an error member disqualifies a reply even when a result member sits beside it
    assertEquals(
      Onchain.batchResults("""[{"id":0,"error":{"code":-32000},"result":"0x1"}]""", 1),
      None
    )
    assertEquals(Onchain.batchResults("""[{"id":0,"result":"0x1"}]""", 2), None)
    assertEquals(Onchain.batchResults("not json", 1), None)
  }

  test("log windows follow the last known head and never exceed the span") {
    assertEquals(Onchain.logWindow(None, Some(100L)), None)
    assertEquals(Onchain.logWindow(Some(100L), None), None)
    assertEquals(Onchain.logWindow(Some(100L), Some(100L)), None)
    assertEquals(Onchain.logWindow(Some(100L), Some(101L)), Some((101L, 101L)))
    assertEquals(Onchain.logWindow(Some(100L), Some(105L)), Some((101L, 105L)))
    assertEquals(Onchain.logWindow(Some(100L), Some(1000L)), Some((801L, 1000L)))
    val (method, params) = Onchain.logs(pair, 101L, 105L)
    assertEquals(method, "eth_getLogs")
    assertEquals(params(0)("fromBlock").str, "0x65")
    assertEquals(params(0)("toBlock").str, "0x69")
  }

  /** A scripted client: each batch returns the next reply. None means a failed batch. */
  private final class FakeRpc(var replies: Vector[Option[Vector[ujson.Value]]]) extends Rpc:
    var seen = Vector.empty[Vector[(String, ujson.Value)]]
    def batch(calls: Vector[(String, ujson.Value)]): Option[Vector[ujson.Value]] =
      seen = seen :+ calls
      val reply = replies.headOption.flatten
      replies = replies.drop(1)
      reply

  private def discovery = Vector(
    Some(
      Vector(hexAddress(token), hexAddress(quote), hexAddress(stable), hexAddress(quote))
        .map(ujson.Str(_))
    ),
    Some(Vector.fill(3)(ujson.Str("0x" + hexWord(BigInt(18)))))
  )

  private def reading(block: Long, swaps: ujson.Arr = ujson.Arr()) = Some(
    Vector(
      ujson.Str(
        "0x" + hexWord(reserveToken) + hexWord(reserveQuote) + hexWord(BigInt(1700000000L))
      ),
      ujson.Str("0x" + hexWord(sqrtPrice) + hexWord(BigInt(0))),
      ujson.Str("0x" + block.toHexString),
      swaps
    )
  )

  test("the feed discovers the pools, then pushes book and candles each poll, never health") {
    val queue = new LinkedBlockingQueue[Event]()
    val rpc = new FakeRpc(
      discovery ++ Vector(reading(100L).map(_.take(3)), reading(101L).map(_.take(3)), reading(102L))
    )
    var clock = 1_000_000L
    var observed = Vector.empty[Event]
    val feed =
      new OnchainFeed(Vector(market), _ => rpc, queue, () => clock, "DEMO", observed :+= _)
    feed.poll()
    val first = drain(queue)
    assertEquals(socketEvents(first), Vector.empty)
    val book = first.collectFirst { case Event.BookUpdate(coin, b, fromSocket) =>
      (coin, b, fromSocket)
    }.get
    assertEquals(book._1, "DEMO")
    assert(book._3)
    assert(within(State.mid(book._2).get, 0.004, 0.002))
    assertEquals(
      first.count { case Event.CandleUpdate("DEMO", _, _) => true; case _ => false },
      State.intervals.size
    )
    assertEquals(observed, first)
    assertEquals(feed.status, "Connected to a.example")
    assertEquals(rpc.seen(2).map(_._1), Vector("eth_call", "eth_call", "eth_blockNumber"))
    clock += 2000L
    feed.poll()
    // the second poll learned head 101 but its logs window (101 to 100) is empty
    assertEquals(rpc.seen(3).map(_._1), Vector("eth_call", "eth_call", "eth_blockNumber"))
    feed.poll()
    val third = rpc.seen(4)
    assertEquals(third.map(_._1), Vector("eth_call", "eth_call", "eth_blockNumber", "eth_getLogs"))
    assertEquals(third(3)._2(0)("fromBlock").str, "0x65")
    assertEquals(third(3)._2(0)("toBlock").str, "0x65")
    assertEquals(socketEvents(drain(queue)), Vector.empty)
  }

  test("a failed read reports, rotates to the next endpoint, re-anchors, and pushes no health") {
    val queue = new LinkedBlockingQueue[Event]()
    val a = new FakeRpc(discovery ++ Vector(reading(100L).map(_.take(3)), None))
    val b = new FakeRpc(
      Vector(reading(103L).map(_.take(3)), reading(104L).map(_.take(3)), reading(105L))
    )
    val byUrl = Map("https://a.example" -> a, "https://b.example" -> b)
    val feed = new OnchainFeed(Vector(market), byUrl(_), queue, () => 5_000L, "DEMO")
    feed.poll()
    assertEquals(feed.status, "Connected to a.example")
    queue.clear()
    feed.poll()
    assertEquals(queue.poll(), null)
    assertEquals(feed.status, "Read failed at a.example. Retrying")
    feed.poll()
    assertEquals(feed.status, "Connected to b.example")
    // the poll after a failure re-anchors without a log query, so a wide range can never wedge it
    assertEquals(b.seen.head.map(_._1), Vector("eth_call", "eth_call", "eth_blockNumber"))
    feed.poll() // learns head 104, the window after the anchor is still empty
    assertEquals(b.seen(1).map(_._1), Vector("eth_call", "eth_call", "eth_blockNumber"))
    feed.poll() // asks for block 104's swaps
    assertEquals(
      b.seen(2).map(_._1),
      Vector("eth_call", "eth_call", "eth_blockNumber", "eth_getLogs")
    )
    assertEquals(b.seen(2)(3)._2(0)("fromBlock").str, "0x68")
    assertEquals(b.seen(2)(3)._2(0)("toBlock").str, "0x68")
    assertEquals(socketEvents(drain(queue)), Vector.empty)
  }

  test("a node that refuses log queries keeps serving prices and is asked again later") {
    val queue = new LinkedBlockingQueue[Event]()
    var clock = 1_000L
    final class TapelessRpc extends Rpc:
      var seen = Vector.empty[Vector[(String, ujson.Value)]]
      def batch(calls: Vector[(String, ujson.Value)]): Option[Vector[ujson.Value]] =
        seen = seen :+ calls
        if seen.size <= 2 then discovery(seen.size - 1) // the two discovery batches
        else if calls.size == 4 then None // every log query is refused
        else reading(100L + seen.size).map(_.take(3)) // prices always work, the head advances
    val rpc = new TapelessRpc
    val feed = new OnchainFeed(Vector(market), _ => rpc, queue, () => clock, "DEMO")
    feed.poll() // discovers and anchors
    feed.poll() // head moves, window still empty
    feed.poll() // first log window: refused, retried without logs at once
    val events = drain(queue)
    assertEquals(socketEvents(events), Vector.empty)
    assertEquals(rpc.seen.takeRight(2).map(_.size), Vector(4, 3))
    assertEquals(feed.status, "Connected to a.example, no swaps from it")
    feed.poll()
    assertEquals(rpc.seen.last.size, 3) // not asked for logs again for a while
    clock += Onchain.logRetryMs + 1
    feed.poll()
    assertEquals(rpc.seen.takeRight(2).map(_.size), Vector(4, 3)) // asked again, refused again
  }

  test("a pool lookup that fails, mismatches or throws is reported and nothing is pushed") {
    def statusFor(reply: Option[Vector[ujson.Value]]): String =
      val queue = new LinkedBlockingQueue[Event]()
      val rpc = new FakeRpc(Vector(reply, Some(Vector.fill(3)(ujson.Str("0x")))))
      val feed = new OnchainFeed(Vector(market), _ => rpc, queue, () => 1L, "DEMO")
      feed.poll()
      assertEquals(queue.poll(), null)
      feed.status
    val neither = Vector(token, quote, stable, pair).map(a => ujson.Str(hexAddress(a)))
    assertEquals(statusFor(Some(neither)), "Pool lookup failed at a.example. Retrying")
    val both = Vector(token, quote, token, quote).map(a => ujson.Str(hexAddress(a)))
    assertEquals(statusFor(Some(both)), "Pool lookup failed at a.example. Retrying")
    assertEquals(statusFor(None), "Pool lookup failed at a.example. Retrying")
    // a decimals reply too short to hold a word must not escape the poll thread
    val good = Vector(token, quote, stable, quote).map(a => ujson.Str(hexAddress(a)))
    assertEquals(statusFor(Some(good)), "Pool lookup failed at a.example. Retrying")
  }

  test("the composite tells the on-chain feed every name and the venue only venue names") {
    val queue = new LinkedBlockingQueue[Event]()
    val rest = new Rest { def post(body: String): Option[String] = None }
    val connector = new Connector {
      def connect(onText: String => Unit, onClose: () => Unit): Socket =
        throw new RuntimeException("no venue")
    }
    val venue = new Feed(connector, rest, queue, () => 1L, "BTC")
    val onchain =
      new OnchainFeed(Vector(market), _ => new FakeRpc(Vector.empty), queue, () => 1L, "DEMO")
    var shown = "DEMO"
    val composite = new CompositeFeed(venue, onchain, () => shown)
    assertEquals(composite.status, onchain.status)
    shown = "BTC"
    assertEquals(composite.status, venue.status)
    composite.setMarket("XMR")
    assertEquals(venue.coin, "XMR")
    assertEquals(onchain.coin, "XMR") // the on-chain feed now serves nothing and polls nothing
    onchain.poll()
    assertEquals(queue.poll(), null)
    composite.setMarket("DEMO")
    assertEquals(onchain.coin, "DEMO")
    assertEquals(venue.coin, "XMR")
  }

  test("the composite answers the market check for on-chain names and asks the venue for others") {
    val queue = new LinkedBlockingQueue[Event]()
    var asked = Vector.empty[String]
    val rest = new Rest {
      def post(body: String): Option[String] =
        asked = asked :+ ujson.read(body)("coin").str
        Some("null")
    }
    val connector = new Connector {
      def connect(onText: String => Unit, onClose: () => Unit): Socket =
        throw new RuntimeException("no venue")
    }
    val venue = new Feed(connector, rest, queue, () => 1L, "BTC")
    val onchain =
      new OnchainFeed(Vector(market), _ => new FakeRpc(Vector.empty), queue, () => 1L, "DEMO")
    val composite: MarketFeed = new CompositeFeed(venue, onchain, () => "DEMO")
    assertEquals(composite.checkMarket("DEMO"), Right(()))
    assertEquals(asked, Vector.empty) // an on-chain name never reaches the venue
    assertEquals(onchain.checkMarket("XMR"), Left(Feed.unknownMarket))
    assertEquals(composite.checkMarket("XMR"), Left(Feed.unknownMarket))
    assertEquals(asked, Vector("XMR"))
    assertEquals(queue.poll(), null) // a check sends no event by itself
  }

  test("settings list on-chain markets and refuse bad names, addresses, endpoints and references") {
    val directory = Files.createTempDirectory("desk-onchain-")
    val path = directory.resolve("config.json")
    def load(text: String) =
      Files.writeString(path, text)
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------"))
      TradingConfig.load(Some(path))
    val good =
      s"""{"onchain": [{"name": "DEMO", "pair": "$pair", "quotePool": "$pool", "rpc": ["https://rpc-bsc.48.club", "https://bsc-dataseed.binance.org"]}]}"""
    val loaded = load(good).toOption.get
    assertEquals(
      loaded.onchain,
      Vector(
        OnchainMarket(
          "DEMO",
          pair,
          pool,
          Vector("https://rpc-bsc.48.club", "https://bsc-dataseed.binance.org")
        )
      )
    )
    assertEquals(loaded.onchainMarket("DEMO").map(_.pair), Some(pair))
    assertEquals(loaded.onchainMarket("XMR"), None)
    assert(load("{}").toOption.get.onchain.isEmpty)
    assert(load(good.replace("DEMO", "demo")).isLeft)
    assert(load(good.replace(pair, "0x123")).isLeft)
    assert(load(good.replace("https://rpc-bsc.48.club", "http://rpc-bsc.48.club")).isLeft)
    assert(load(good.replace("DEMO", "BTC")).isLeft)
    assert(load(good.replace("]}]}", "]}, " + good.drop(12))).isLeft)
    Files.deleteIfExists(path)
    Files.deleteIfExists(directory)
  }
