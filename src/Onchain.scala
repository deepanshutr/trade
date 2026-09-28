package desk

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.util.Try

/** A market that trades on a BNB Chain automated market maker: a PancakeSwap V2 pair, the V3 pool
  * that prices the pair's quote asset in a dollar stable, and the JSON-RPC endpoints to read from.
  */
final case class OnchainMarket(name: String, pair: String, quotePool: String, rpc: Vector[String])

/** What the feed learns once about a market's pools. */
final case class Pools(
    token: String,
    quote: String,
    stable: String,
    tokenDecimals: Int,
    quoteDecimals: Int,
    stableDecimals: Int,
    tokenIsPairToken0: Boolean,
    quoteIsPoolToken1: Boolean
)

/** JSON-RPC over HTTP. One batch per poll keeps a reading to one round trip. */
trait Rpc:
  def batch(calls: Vector[(String, ujson.Value)]): Option[Vector[ujson.Value]]

final class HttpRpc(val url: String, waitMs: Long = 5000L) extends Rpc:
  private val client = HttpClient.newHttpClient()
  def batch(calls: Vector[(String, ujson.Value)]): Option[Vector[ujson.Value]] = Try {
    val request = HttpRequest
      .newBuilder(URI.create(url))
      .timeout(java.time.Duration.ofMillis(waitMs))
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(Onchain.batchBody(calls)))
      .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    if response.statusCode() != 200 then
      throw new RuntimeException(s"http ${response.statusCode()}")
    response.body()
  }.toOption.flatMap(Onchain.batchResults(_, calls.size))

trait MarketFeed:
  def status: String
  def setMarket(newCoin: String): Unit
  def checkMarket(name: String): Either[String, Unit]

object Onchain:
  val pollMs = 2000L
  val fee = 0.0025
  val impacts: Vector[Double] = Vector(0.0025, 0.005, 0.01, 0.02, 0.05, 0.1)
  val swapTopic = "0xd78ad95fa46c994b6551d0da85fc275fe613ce37657fb8d5e3d130840159d822"
  val getReserves = "0x0902f1ac"
  val slot0 = "0x3850c7bd"
  val token0 = "0x0dfe1681"
  val token1 = "0xd21220a7"
  val decimals = "0x313ce567"

  def call(to: String, data: String): (String, ujson.Value) =
    ("eth_call", ujson.Arr(ujson.Obj("to" -> to, "data" -> data), "latest"))

  val logSpan = 200L
  val logRetryMs = 600_000L

  /** Swap logs over an explicit block range. Nodes refuse an open upper bound. */
  def logs(pair: String, fromBlock: Long, toBlock: Long): (String, ujson.Value) =
    (
      "eth_getLogs",
      ujson.Arr(
        ujson.Obj(
          "address" -> pair,
          "fromBlock" -> ("0x" + fromBlock.toHexString),
          "toBlock" -> ("0x" + toBlock.toHexString),
          "topics" -> ujson.Arr(swapTopic)
        )
      )
    )

  /** The blocks to ask logs for: after the anchor, up to the last known head, never more than
    * logSpan blocks so a gap after an outage cannot exceed a node's range limit.
    */
  def logWindow(anchor: Option[Long], head: Option[Long]): Option[(Long, Long)] =
    for
      a <- anchor
      h <- head
      if h > a
    yield ((a + 1).max(h - logSpan + 1), h)

  def batchBody(calls: Vector[(String, ujson.Value)]): String =
    ujson.write(ujson.Arr.from(calls.zipWithIndex.map { case ((method, params), i) =>
      ujson.Obj("jsonrpc" -> "2.0", "id" -> i, "method" -> method, "params" -> params)
    }))

  /** Results in request order, or None when any reply is missing or an error. */
  def batchResults(text: String, count: Int): Option[Vector[ujson.Value]] = Try {
    val replies = ujson.read(text).arr.toVector
    require(replies.size == count && replies.forall(r => !r.obj.contains("error")))
    val byId = replies.map(r => r("id").num.toInt -> r("result")).toMap
    (0 until count).toVector.map(byId)
  }.toOption

  def word(hex: String, i: Int): BigInt =
    val body = hex.stripPrefix("0x")
    require(body.length >= (i + 1) * 64, "short reply")
    BigInt(body.slice(i * 64, (i + 1) * 64), 16)

  def address(hex: String, i: Int): String =
    "0x" + hex.stripPrefix("0x").slice(i * 64 + 24, (i + 1) * 64)

  def quantity(hex: String): Long = BigInt(hex.stripPrefix("0x"), 16).toLong

  /** Dollar price of one whole token: quote per token from the pair's reserves, times dollars per
    * quote from the V3 pool's square root price.
    */
  def price(reserve0: BigInt, reserve1: BigInt, sqrtPriceX96: BigInt, p: Pools): Double =
    val (tokens, quotes) =
      if p.tokenIsPairToken0 then (reserve0, reserve1) else (reserve1, reserve0)
    require(tokens.signum > 0 && quotes.signum > 0, "empty pool")
    val quotePerToken =
      quotes.doubleValue / tokens.doubleValue * math.pow(10, p.tokenDecimals - p.quoteDecimals)
    quotePerToken * dollarsPerQuote(sqrtPriceX96, p)

  def dollarsPerQuote(sqrtPriceX96: BigInt, p: Pools): Double =
    val ratio = sqrtPriceX96.doubleValue / math.pow(2, 96)
    val token1PerToken0 = ratio * ratio
    require(token1PerToken0 > 0, "empty pool")
    val raw = if p.quoteIsPoolToken1 then 1 / token1PerToken0 else token1PerToken0
    raw * math.pow(10, p.quoteDecimals - p.stableDecimals)

  /** Tokens the pool absorbs before its price falls by the impact, when selling. */
  def bidSize(reserveTokens: Double, impact: Double): Double =
    reserveTokens * (1 / math.sqrt(1 - impact) - 1)

  /** Tokens the pool gives up before its price rises by the impact, when buying. */
  def askSize(reserveTokens: Double, impact: Double): Double =
    reserveTokens * (1 - 1 / math.sqrt(1 + impact))

  def figure(x: Double, digits: Int): String =
    BigDecimal(x)
      .round(new java.math.MathContext(digits))
      .bigDecimal
      .stripTrailingZeros
      .toPlainString

  /** The pool's own liquidity as a book: levels at fixed impact steps, the fee as the first spread.
    * Each level holds what the pool absorbs between its impact and the one before, so sizes are per
    * level as the depth gauges expect, and they sum to the deepest impact.
    */
  def book(price: Double, reserveTokens: Double, time: Long): Book =
    def bands(size: Double => Double): Vector[Double] =
      val cumulative = impacts.map(size)
      cumulative.zip(0.0 +: cumulative.init).map((to, from) => to - from)
    val bids = impacts.zip(bands(bidSize(reserveTokens, _))).map { (d, size) =>
      Level(figure(price * (1 - d), 8), figure(size, 6), 1)
    }
    val asks = impacts.zip(bands(askSize(reserveTokens, _))).map { (d, size) =>
      Level(figure(price * (1 + d), 8), figure(size, 6), 1)
    }
    Book(bids, asks, time)

  /** Swaps from the pair's logs as trades, priced in dollars, timed at the poll that saw them. */
  def swaps(logs: ujson.Value, p: Pools, dollarsPerQuote: Double, time: Long): Vector[Trade] =
    logs.arr.toVector.flatMap { l =>
      val data = l("data").str
      val (in0, in1, out0, out1) = (word(data, 0), word(data, 1), word(data, 2), word(data, 3))
      val (tokenIn, quoteIn, tokenOut, quoteOut) =
        if p.tokenIsPairToken0 then (in0, in1, out0, out1) else (in1, in0, out1, out0)
      val id = Some(quantity(l("blockNumber").str) * 100_000L + quantity(l("logIndex").str))
      def px(quote: BigInt, tokens: BigInt): String =
        figure(
          quote.doubleValue / tokens.doubleValue * math.pow(10, p.tokenDecimals - p.quoteDecimals) *
            dollarsPerQuote,
          8
        )
      def sz(tokens: BigInt): String = figure(tokens.doubleValue / math.pow(10, p.tokenDecimals), 6)
      if tokenIn.signum > 0 && quoteOut.signum > 0 then
        Some(Trade(time, Side.Sell, px(quoteOut, tokenIn), sz(tokenIn), id))
      else if tokenOut.signum > 0 && quoteIn.signum > 0 then
        Some(Trade(time, Side.Buy, px(quoteIn, tokenOut), sz(tokenOut), id))
      else None
    }

  /** Candles per interval from the feed's own samples and observed volume. */
  final class CandleBuilder:
    private var open = Map.empty[String, Candle]
    def add(ms: Long, price: Double, volume: Double): Vector[(String, Candle)] =
      State.intervals.map { interval =>
        val width = State.intervalMs(interval)
        val t = ms / width * width
        val candle = open.get(interval) match
          case Some(c) if c.t == t =>
            c.copy(h = c.h.max(price), l = c.l.min(price), c = price, v = c.v + volume)
          case _ => Candle(t, price, price, price, price, volume)
        open = open.updated(interval, candle)
        (interval, candle)
      }

/** Polls the shown on-chain market. Its books, trades and candles mark that market fresh in the
  * snapshot's health field, which Screen and Alerts read. Its own connection state supplies its
  * status line.
  */
final class OnchainFeed(
    markets: Vector[OnchainMarket],
    rpcFor: String => Rpc,
    queue: java.util.Queue[Event],
    clock: () => Long,
    coin0: String,
    observer: Event => Unit = _ => ()
) extends MarketFeed:
  @volatile var coin: String = coin0
  private var pools = Map.empty[String, Pools]
  private var anchor = Map.empty[String, Long] // the last block whose swaps were read
  private var head = Map.empty[String, Long] // the last block number a poll learned
  private var builders = Map.empty[String, Onchain.CandleBuilder]
  private var endpoint = Map.empty[String, Int]
  private var noLogs = Map.empty[String, Long] // endpoints that refused a log query, and when
  private var connectionStatus = "Connecting"

  def status: String = synchronized(connectionStatus)
  def market(name: String): Option[OnchainMarket] = markets.find(_.name == name)
  def serves(name: String): Boolean = market(name).nonEmpty
  def setMarket(newCoin: String): Unit = coin = newCoin
  def checkMarket(name: String): Either[String, Unit] =
    if serves(name) then Right(()) else Left(Feed.unknownMarket)

  private def push(e: Event): Unit =
    observer(e)
    val _ = queue.add(e)

  private def rpc(m: OnchainMarket): (Rpc, String) =
    val url = m.rpc(endpoint.getOrElse(m.name, 0) % m.rpc.size)
    (rpcFor(url), url)

  private def host(url: String): String = Try(URI.create(url).getHost).toOption.getOrElse(url)

  private[desk] def discover(m: OnchainMarket, rpc: Rpc): Option[Pools] = Try {
    for
      first <- rpc.batch(
        Vector(
          Onchain.call(m.pair, Onchain.token0),
          Onchain.call(m.pair, Onchain.token1),
          Onchain.call(m.quotePool, Onchain.token0),
          Onchain.call(m.quotePool, Onchain.token1)
        )
      )
      addresses = first.map(v => Onchain.address(v.str, 0).toLowerCase(java.util.Locale.ROOT))
      Vector(pair0, pair1, pool0, pool1) = addresses
      inPool = Vector(pair0, pair1).filter(a => a == pool0 || a == pool1)
      if inPool.size == 1
      quote = inPool.head
      token = if quote == pair0 then pair1 else pair0
      stable = if quote == pool0 then pool1 else pool0
      decimals <- rpc.batch(Vector(token, quote, stable).map(Onchain.call(_, Onchain.decimals)))
      digits = decimals.map(v => Onchain.word(v.str, 0).toInt)
      if digits.forall(d => d >= 0 && d <= 36)
    yield Pools(
      token,
      quote,
      stable,
      digits(0),
      digits(1),
      digits(2),
      token == pair0,
      quote == pool1
    )
  }.toOption.flatten

  private def fail(m: OnchainMarket, url: String, reason: String): Unit = synchronized {
    connectionStatus = s"$reason at ${host(url)}. Retrying"
    endpoint = endpoint.updated(m.name, endpoint.getOrElse(m.name, 0) + 1)
    // Forget the log anchors: the next poll re-anchors without a log query.
    anchor = anchor - m.name
    head = head - m.name
  }

  /** One reading of the shown market. Nothing here may escape: a feed thread must not stop Desk, so
    * any surprise counts as a failed read.
    */
  def poll(): Unit =
    val name = coin
    market(name).foreach { m =>
      val (client, url) = rpc(m)
      Try(read(m, name, client, url)).failed.foreach(_ => fail(m, url, "Read failed"))
    }

  private def read(m: OnchainMarket, name: String, client: Rpc, url: String): Unit =
    pools
      .get(name)
      .orElse(discover(m, client).map { p =>
        pools = pools.updated(name, p)
        p
      }) match
      case None    => fail(m, url, "Pool lookup failed")
      case Some(p) =>
        def attempt(window: Option[(Long, Long)]): Option[Vector[Event]] =
          val calls = Vector(
            Onchain.call(m.pair, Onchain.getReserves),
            Onchain.call(m.quotePool, Onchain.slot0),
            ("eth_blockNumber", ujson.Arr())
          ) ++ window.map((from, to) => Onchain.logs(m.pair, from, to)).toVector
          client.batch(calls).flatMap(events(m, p, _, window))
        val now = clock()
        val tapeless = noLogs.get(url).exists(at => now - at < Onchain.logRetryMs)
        val window = if tapeless then None else Onchain.logWindow(anchor.get(name), head.get(name))
        // A node that serves prices but refuses log queries keeps serving prices: the poll
        // is retried once without logs and the node is left alone about logs for a while.
        val outcome = attempt(window).orElse(window.flatMap { _ =>
          noLogs = noLogs.updated(url, now)
          attempt(None)
        })
        outcome match
          case None         => fail(m, url, "Read failed")
          case Some(pushed) =>
            val quiet = noLogs.get(url).exists(at => now - at < Onchain.logRetryMs)
            synchronized {
              connectionStatus =
                "Connected to " + host(url) + (if quiet then ", no swaps from it" else "")
            }
            pushed.foreach(push)

  private def events(
      m: OnchainMarket,
      p: Pools,
      results: Vector[ujson.Value],
      window: Option[(Long, Long)]
  ): Option[Vector[Event]] = Try {
    val now = clock()
    val reserves = results(0).str
    val (reserve0, reserve1) = (Onchain.word(reserves, 0), Onchain.word(reserves, 1))
    val sqrt = Onchain.word(results(1).str, 0)
    val block = Onchain.quantity(results(2).str)
    val dollars = Onchain.dollarsPerQuote(sqrt, p)
    val price = Onchain.price(reserve0, reserve1, sqrt, p)
    val tokens = (if p.tokenIsPairToken0 then reserve0 else reserve1).doubleValue /
      math.pow(10, p.tokenDecimals)
    val trades = window.fold(Vector.empty[Trade])(_ => Onchain.swaps(results(3), p, dollars, now))
    val builder = builders.getOrElse(m.name, new Onchain.CandleBuilder)
    builders = builders.updated(m.name, builder)
    // Swaps are read one poll behind: this poll's head bounds the next poll's query.
    anchor = anchor.updated(m.name, window.fold(anchor.getOrElse(m.name, block))(_._2))
    head = head.updated(m.name, block)
    val candles = builder.add(now, price, trades.map(_.displaySize).sum)
    Vector(Event.BookUpdate(m.name, Onchain.book(price, tokens, now), fromSocket = true)) ++
      Option.when(trades.nonEmpty)(Event.Trades(m.name, trades)).toVector ++
      candles.map((i, c) => Event.CandleUpdate(m.name, i, c))
  }.toOption

/** Routes a market by name. The on-chain feed always learns the shown name, so it polls nothing
  * once a venue market is shown; the venue feed hears venue names only.
  */
final class CompositeFeed(venue: Feed, onchain: OnchainFeed, shown: () => String)
    extends MarketFeed:
  def status: String = if onchain.serves(shown()) then onchain.status else venue.status
  def setMarket(newCoin: String): Unit =
    val returning =
      onchain.serves(onchain.coin) && !onchain.serves(newCoin) && venue.coin == newCoin
    onchain.setMarket(newCoin)
    if !onchain.serves(newCoin) then
      venue.setMarket(newCoin)
      if returning then venue.reloadHistory()
  def checkMarket(name: String): Either[String, Unit] =
    if onchain.serves(name) then Right(()) else venue.checkMarket(name)
