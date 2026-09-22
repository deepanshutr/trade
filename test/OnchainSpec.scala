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
    OnchainMarket("DEMO", pair, pool, Vector("https://rpc.example/a", "https://rpc.example/b"))

  private def hexWord(n: BigInt): String = n.toString(16).reverse.padTo(64, '0').reverse
  private def hexAddress(a: String): String = "0x" + ("0" * 24) + a.stripPrefix("0x")
  private def within(actual: Double, expected: Double, tolerance: Double): Boolean =
    math.abs(actual - expected) <= expected * tolerance

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

  test("decimals scale the price and empty pools are refused") {
    val sixDecimalToken = pools.copy(tokenDecimals = 6)
    val price =
      Onchain.price(reserveToken / BigInt(10).pow(12), reserveQuote, sqrtPrice, sixDecimalToken)
    assert(within(price, 0.004, 0.002), price.toString)
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

  test("the book has bids below and asks above the price with the fee as the first spread") {
    val book = Onchain.book(0.004, 20_000_000.0, 1000L)
    assertEquals(book.time, 1000L)
    assertEquals(book.bids.size, Onchain.impacts.size)
    assertEquals(book.bids.head.px, "0.00399")
    assertEquals(book.asks.head.px, "0.00401")
    val bidPrices = book.bids.map(_.px.toDouble)
    val askPrices = book.asks.map(_.px.toDouble)
    assert(bidPrices == bidPrices.sorted.reverse && askPrices == askPrices.sorted)
    assert(book.bids.forall(l => l.sz.toDouble > 0 && l.n == 1))
    assert(within(State.mid(book).get, 0.004, 1e-9))
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
    assertEquals(Onchain.batchResults("""[{"id":0,"result":"0x1"}]""", 2), None)
    assertEquals(Onchain.batchResults("not json", 1), None)
  }

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

  test("the feed discovers the pools, then pushes book, candles and health each poll") {
    val queue = new LinkedBlockingQueue[Event]()
    val rpc = new FakeRpc(discovery ++ Vector(reading(100L).map(_.take(3)), reading(101L)))
    var clock = 1_000_000L
    val feed = new OnchainFeed(Vector(market), _ => rpc, queue, () => clock, "DEMO")
    feed.poll()
    val first = Iterator.continually(queue.poll()).takeWhile(_ != null).toVector
    assertEquals(first.head, Event.SocketUp)
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
    assertEquals(feed.status, "Connected to rpc.example")
    assertEquals(rpc.seen(2).map(_._1), Vector("eth_call", "eth_call", "eth_blockNumber"))
    clock += 2000L
    feed.poll()
    val second = Iterator.continually(queue.poll()).takeWhile(_ != null).toVector
    assert(!second.contains(Event.SocketUp))
    assertEquals(
      rpc.seen(3).map(_._1),
      Vector("eth_call", "eth_call", "eth_blockNumber", "eth_getLogs")
    )
    assertEquals(rpc.seen(3)(3)._2(0)("fromBlock").str, "0x65")
  }

  test("a failed read marks the feed down once, rotates the endpoint, and recovery marks it up") {
    val queue = new LinkedBlockingQueue[Event]()
    val rpc =
      new FakeRpc(discovery ++ Vector(reading(100L).map(_.take(3)), None, None, reading(103L)))
    val feed = new OnchainFeed(Vector(market), _ => rpc, queue, () => 5_000L, "DEMO")
    feed.poll()
    queue.clear()
    feed.poll()
    assertEquals(queue.poll(), Event.SocketDown)
    assertEquals(queue.poll(), null)
    assertEquals(feed.status, "Read failed at rpc.example. Retrying")
    feed.poll()
    assertEquals(queue.poll(), null)
    feed.poll()
    assertEquals(queue.poll(), Event.SocketUp)
  }

  test("a market whose quote is in neither pool is reported and nothing is pushed") {
    val queue = new LinkedBlockingQueue[Event]()
    val wrong = Some(
      Vector(hexAddress(token), hexAddress(quote), hexAddress(stable), hexAddress(pair)).map(
        ujson.Str(_)
      )
    )
    val feed =
      new OnchainFeed(Vector(market), _ => new FakeRpc(Vector(wrong)), queue, () => 1L, "DEMO")
    feed.poll()
    assertEquals(queue.poll(), null)
    assertEquals(feed.status, "Pool lookup failed at rpc.example. Retrying")
  }

  test("the composite routes by name and reports the status of the shown market's feed") {
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
    composite.setMarket("DEMO")
    assertEquals(onchain.coin, "DEMO")
    assertEquals(venue.coin, "BTC")
    composite.setMarket("XMR")
    assertEquals(venue.coin, "XMR")
    assertEquals(onchain.coin, "DEMO")
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
