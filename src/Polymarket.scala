package desk

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{Duration, Instant, ZoneId}
import java.time.format.TextStyle
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.control.NonFatal

object Polymarket:
  enum Window:
    case M5, M15, H1, H4, D1
  import Window.*

  // Times are epoch milliseconds. A coin is BTC or PAXG, the gold token.
  final case class Market(
      coin: String,
      window: Window,
      slug: String,
      upToken: String,
      startsAt: Long,
      endsAt: Long
  )
  final case class Point(at: Long, up: Double) // percent
  enum State:
    case Unreachable
    case NoMarket
    case Ready(points: Vector[Point], at: Long)

  val maxTokens = 11
  val maxBody = 524288
  val staleAfterMs = 120000L
  val ringSize = 720
  val perMinute = 6
  val midpointEveryMs = 30000L
  val timeout: Duration = Duration.ofSeconds(3)
  private val gamma = "https://gamma-api.polymarket.com/markets"
  private val clob = "https://clob.polymarket.com/midpoints"
  private val newYork = ZoneId.of("America/New_York")

  private val requestCount = new AtomicLong(0)
  private val failureCounts = new ConcurrentHashMap[Int, Long]()
  def requests: Long = requestCount.get

  /** Failed reads by status. 0 is no reply, -1 is a reply that could not be used. */
  def failures: Map[Int, Long] = failureCounts.asScala.toMap
  private[desk] def countRequest(): Unit = { val _ = requestCount.incrementAndGet() }
  private[desk] def countFailure(status: Int): Unit =
    val _ = failureCounts.merge(status, 1L, (a, b) => a + b)

  def slugs(now: Long, coins: Set[String]): Vector[(String, Window, String)] =
    val at = Instant.ofEpochMilli(now).atZone(newYork)
    val seconds = Math.floorDiv(now, 1000L)
    def start(length: Long): Long = seconds / length * length
    def day(prefix: String): Vector[String] =
      Vector(at.toLocalDate, at.toLocalDate.plusDays(1)).map { date =>
        val month = date.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH).toLowerCase
        s"$prefix-on-$month-${date.getDayOfMonth}-${date.getYear}"
      }
    val month = at.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH).toLowerCase
    val hour = s"${if at.getHour % 12 == 0 then 12 else at.getHour % 12}${
        if at.getHour < 12 then "am" else "pm"
      }"
    val btc =
      if coins.contains("BTC") then
        Vector(
          ("BTC", M5, s"btc-updown-5m-${start(300)}"),
          ("BTC", M15, s"btc-updown-15m-${start(900)}"),
          ("BTC", H1, s"bitcoin-up-or-down-$month-${at.getDayOfMonth}-${at.getYear}-$hour-et"),
          ("BTC", H4, s"btc-updown-4h-${start(14400)}")
        ) ++ day("bitcoin-up-or-down").map(("BTC", D1, _))
      else Vector.empty
    val gold =
      if coins.contains("PAXG") then day("xauusd-up-or-down").map(("PAXG", D1, _))
      else Vector.empty
    btc ++ gold

  def marketsRequest(slugs: Vector[String]): String =
    gamma + "?" + slugs.map("slug=" + _).mkString("&")

  def parseMarkets(
      body: String,
      wanted: Vector[(String, Window, String)],
      now: Long
  ): Either[String, Map[(String, Window), Option[Market]]] =
    if body.length > maxBody then Left("too large")
    else
      Try {
        val keyOf = wanted.map((coin, window, slug) => slug -> (coin, window)).toMap
        val found = ujson.read(body).arr.toVector.flatMap { item =>
          val slug = item.obj("slug").str
          keyOf.get(slug).map { (coin, window) =>
            val tokens = ujson.read(item.obj("clobTokenIds").str).arr.map(_.str)
            val outcomes = ujson.read(item.obj("outcomes").str).arr.map(_.str)
            val up = tokens(outcomes.indexOf("Up").ensuring(_ >= 0))
            Market(
              coin,
              window,
              slug,
              up,
              Instant.parse(item.obj("eventStartTime").str).toEpochMilli,
              Instant.parse(item.obj("endDate").str).toEpochMilli
            )
          }
        }
        wanted
          .map(w => (w._1, w._2))
          .distinct
          .map { key =>
            key -> found
              .filter(m => (m.coin, m.window) == key && m.endsAt > now)
              .minByOption(_.endsAt)
          }
          .toMap
      }.toEither.left.map(_ => "bad markets reply")

  def midpointsRequest(tokens: Vector[String]): (String, String) =
    (clob, ujson.write(ujson.Arr.from(tokens.take(maxTokens).map(t => ujson.Obj("token_id" -> t)))))

  def parseMidpoints(body: String): Either[String, Map[String, Double]] =
    if body.length > maxBody then Left("too large")
    else
      Try {
        val fields = ujson.read(body).obj
        require(fields.nonEmpty)
        // A price outside 0 to 1 is no price.
        fields
          .map((token, price) => token -> BigDecimal(price.str))
          .collect { case (token, p) if p >= 0 && p <= 1 => token -> (p * 100).toDouble }
          .toMap
      }.toEither.left.map(_ => "bad midpoints reply")

  /** Reads at most maxBody bytes. A reply over the cap is a failure with status -1. */
  def readCapped(in: java.io.InputStream): Either[Int, String] =
    val bytes = in.readNBytes(maxBody + 1)
    if bytes.length > maxBody then Left(-1)
    else Right(new String(bytes, java.nio.charset.StandardCharsets.UTF_8))

  /** Lets at most `limit` requests start in any sliding minute, waiting with `sleep`. */
  final class Pacer(limit: Int, now: () => Long, sleep: Long => Unit):
    private val sent = mutable.Queue.empty[Long]
    private def drop(): Unit = while sent.nonEmpty && sent.head <= now() - 60000L do sent.dequeue()
    def take(): Unit =
      drop()
      while sent.size >= limit do
        sleep(sent.head + 60000L - now())
        drop()
      sent.enqueue(now())

  // No header is set here. The JVM adds its own User-Agent when the request is sent.
  def getRequest(url: String): HttpRequest =
    HttpRequest.newBuilder(URI.create(url)).timeout(timeout).GET().build()

  def postRequest(url: String, body: String): HttpRequest =
    HttpRequest
      .newBuilder(URI.create(url))
      .timeout(timeout)
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()

object PolymarketReader:
  private def realSend(client: HttpClient)(request: HttpRequest): Either[Int, String] =
    val reply = client.send(request, HttpResponse.BodyHandlers.ofInputStream())
    val stream = reply.body
    try if reply.statusCode == 200 then Polymarket.readCapped(stream) else Left(reply.statusCode)
    finally stream.close()

  /** The two calls. Requests are built only by getRequest and postRequest. A status other than 200,
    * no reply, or a reply over the size cap is a Left.
    */
  def http(
      send: HttpRequest => Either[Int, String] = realSend(
        HttpClient
          .newBuilder()
          .connectTimeout(Polymarket.timeout)
          .followRedirects(HttpClient.Redirect.NEVER)
          .build()
      )
  ): (String => Either[Int, String], (String, String) => Either[Int, String]) =
    (
      url => send(Polymarket.getRequest(url)),
      (url, body) => send(Polymarket.postRequest(url, body))
    )

final class PolymarketReader(
    get: String => Either[Int, String],
    post: (String, String) => Either[Int, String],
    coins: () => Set[String],
    now: () => Long,
    sleep: Long => Unit
):
  import Polymarket.*

  private type Key = (String, Window)
  private final case class Snapshot(
      markets: Map[Key, Option[Market]],
      rings: Map[Key, Vector[Point]],
      newest: Map[Key, String], // the token whose price is the newest point of each ring
      discovered: Boolean,
      discoveryOk: Boolean,
      midpointsOk: Boolean
  )
  @volatile private var snapshot = Snapshot(Map.empty, Map.empty, Map.empty, false, false, true)
  @volatile private var thread = Option.empty[Thread]

  // Everything below runs on the reader thread only.
  private val pacer = new Pacer(perMinute, now, sleep)
  private var nextDiscovery = 0L
  private var nextMidpoints = 0L
  private var discoveryFailures = 0
  private var midpointFailures = 0

  private def backoff(failed: Int): Long =
    math.min(midpointEveryMs * (1L << math.min(failed, 10)), 300000L)

  private def boundary(at: Long): Long = (at / 300000L + 1) * 300000L + 5000L

  private def call(request: => Either[Int, String]): Either[Int, String] =
    pacer.take()
    countRequest()
    try request
    catch case NonFatal(_) => Left(0)

  private def discover(): Unit =
    val followed = coins()
    val wanted = slugs(now(), followed)
    if wanted.isEmpty then
      snapshot = snapshot.copy(
        markets = Map.empty,
        rings = snapshot.rings.filter((key, _) => followed.contains(key._1)),
        discovered = true,
        discoveryOk = true
      )
      nextDiscovery = boundary(now())
    else
      val result = call(get(marketsRequest(wanted.map(_._3).distinct))).flatMap { body =>
        parseMarkets(body, wanted, now()).left.map(_ => -1)
      }
      result match
        case Left(status) =>
          countFailure(status)
          discoveryFailures += 1
          nextDiscovery = now() + backoff(discoveryFailures)
          snapshot = snapshot.copy(discoveryOk = false)
        case Right(found) =>
          discoveryFailures = 0
          nextDiscovery = boundary(now())
          val old = snapshot
          val kept = old.rings.filter((key, _) => followed.contains(key._1))
          snapshot = old.copy(markets = found, rings = kept, discovered = true, discoveryOk = true)

  private def midpoints(): Unit =
    val started = now()
    val live = snapshot.markets.collect { case (key, Some(m)) if m.endsAt > started => key -> m }
    if live.isEmpty then nextMidpoints = started + midpointEveryMs
    else
      val tokens = live.values.toVector.sortBy(_.slug).map(_.upToken).distinct.take(maxTokens)
      val (url, body) = midpointsRequest(tokens)
      val result = call(post(url, body)).flatMap(parseMidpoints(_).left.map(_ => -1))
      result match
        case Left(status) =>
          countFailure(status)
          midpointFailures += 1
          nextMidpoints = now() + backoff(midpointFailures)
          snapshot = snapshot.copy(midpointsOk = false)
        case Right(prices) =>
          midpointFailures = 0
          nextMidpoints = started + midpointEveryMs
          val at = now()
          val old = snapshot
          val rings = live.foldLeft(old.rings) { case (rings, (key, market)) =>
            prices.get(market.upToken) match
              case Some(up) =>
                rings.updated(
                  key,
                  (rings.getOrElse(key, Vector.empty) :+ Point(at, up)).takeRight(ringSize)
                )
              case None => rings
          }
          val newest = live.collect {
            case (key, market) if prices.contains(market.upToken) => key -> market.upToken
          }
          snapshot = old.copy(rings = rings, newest = old.newest ++ newest, midpointsOk = true)

  private def loop(): Unit =
    nextDiscovery = now()
    nextMidpoints = nextDiscovery
    while !Thread.currentThread.isInterrupted do
      if now() >= nextDiscovery then discover()
      if now() >= nextMidpoints then midpoints()
      sleep(math.max(1L, math.min(nextDiscovery, nextMidpoints) - now()))

  def start(): Unit = synchronized {
    if thread.isEmpty then
      val started = Threads.daemon("polymarket-reader")(loop())
      thread = Some(started)
      started.start()
  }

  def state(coin: String, window: Window): State =
    val s = snapshot
    if !s.discovered then State.Unreachable
    else
      s.markets.get((coin, window)).flatten match
        case None                                   => State.NoMarket
        case Some(market) if market.endsAt <= now() => State.NoMarket
        // Ready only once the newest point is the open market's own price.
        case Some(market) =>
          val own = s.newest.get((coin, window)).contains(market.upToken)
          s.rings.get((coin, window)).filter(_.nonEmpty && own) match
            case Some(points) => State.Ready(points, points.last.at)
            case None         => State.Unreachable

  // The ring of a coin and window whatever its market's state, oldest first.
  def points(coin: String, window: Window): Vector[Point] =
    snapshot.rings.getOrElse((coin, window), Vector.empty)

  def isStale(coin: String, window: Window): Boolean =
    state(coin, window) match
      case State.Ready(_, at) => now() - at > staleAfterMs
      case _                  => false

  def unreachable: Boolean = !snapshot.discoveryOk || !snapshot.midpointsOk

  def alive: Boolean = thread.exists(_.isAlive)

  def close(): Unit = thread.foreach(_.interrupt())
