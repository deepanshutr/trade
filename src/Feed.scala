package desk

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.util.concurrent.{CompletionStage, CountDownLatch}
import scala.util.Try

trait Socket:
  def send(text: String): Unit
  def close(): Unit

trait Connector:
  def connect(onText: String => Unit, onClose: () => Unit): Socket

trait Rest:
  def post(body: String): Option[String]

final class Joiner:
  private val buffer = new StringBuilder
  def append(piece: String, last: Boolean): Option[String] =
    buffer.append(piece)
    if last then
      val whole = buffer.toString
      buffer.clear()
      Some(whole)
    else None

object Feed:
  val url = "wss://api.hyperliquid.xyz/ws"
  val restUrl = "https://api.hyperliquid.xyz/info"
  val ping = """{"method":"ping"}"""

  def backoffMs(attempt: Int): Long = attempt match
    case 1 => 1000L
    case 2 => 2000L
    case 3 => 4000L
    case _ => 8000L

  private def sub(kind: String, c: String, extra: String = ""): String =
    s"""{"method":"subscribe","subscription":{"type":"$kind","coin":"$c"$extra}}"""

  def subscriptions(c: String): Vector[String] =
    val main =
      Vector(sub("l2Book", c), sub("bbo", c), sub("trades", c), sub("activeAssetCtx", c)) ++
        State.intervals.map(i => sub("candle", c, s""","interval":"$i""""))
    val refs = State.refCoins.flatMap(r =>
      sub("bbo", r) +: State.intervals.map(i => sub("candle", r, s""","interval":"$i""""))
    )
    (main ++ refs).distinct

  def unsubscriptions(c: String): Vector[String] =
    subscriptions(c).map(_.replace("\"method\":\"subscribe\"", "\"method\":\"unsubscribe\""))

  def historyBody(coin: String, interval: String, now: Long): String =
    val start = now - 5000L * State.intervalMs(interval)
    s"""{"type":"candleSnapshot","req":{"coin":"$coin","interval":"$interval","startTime":$start,"endTime":$now}}"""

  def coarseBody(coin: String, k: Int): String =
    s"""{"type":"l2Book","coin":"$coin","nSigFigs":$k}"""

final class Feed(
    connector: Connector,
    rest: Rest,
    queue: java.util.Queue[Event],
    clock: () => Long,
    coin0: String
):
  @volatile var coin: String = coin0
  @volatile private var socket: Option[Socket] = None
  private val sendLock = new Object

  private def push(e: Event): Unit =
    val _ = queue.add(e)

  def send(text: String): Unit = sendLock.synchronized {
    socket.foreach(_.send(text))
  }

  /** One connection. True once the socket opened and later closed, false if it never opened. */
  def connectOnce(): Boolean =
    val closed = new CountDownLatch(1)
    val opened = Try(
      connector.connect(text => Events.parse(text).foreach(push), () => closed.countDown())
    ).toOption
    opened match
      case None    => false
      case Some(s) =>
        socket = Some(s)
        Feed.subscriptions(coin).foreach(send)
        push(Event.SocketUp)
        fetchHistory()
        closed.await()
        socket = None
        push(Event.SocketDown)
        true

  def runForever(sleep: Long => Unit): Unit =
    var attempt = 0
    while true do
      val opened = connectOnce()
      attempt = if opened then 1 else attempt + 1
      sleep(Feed.backoffMs(attempt))

  def heartbeat(): Unit = send(Feed.ping)

  def poll(): Unit =
    val c = coin
    rest
      .post(s"""{"type":"l2Book","coin":"$c"}""")
      .flatMap(Events.parseRestBook)
      .foreach(b => push(Event.BookUpdate(c, b, fromSocket = false)))

  def pollCoarse(): Unit =
    val c = coin
    Vector(4, 3, 2).foreach { k =>
      rest
        .post(Feed.coarseBody(c, k))
        .flatMap(Events.parseRestBook)
        .foreach(b => push(Event.CoarseBook(c, k, b)))
    }

  def fetchHistoryFor(c: String): Unit =
    State.intervals.foreach { i =>
      rest.post(Feed.historyBody(c, i, clock())).flatMap(Events.parseRestCandles) match
        case Some(cs) => push(Event.CandleHistory(c, i, cs))
        case None     => push(Event.CandleHistoryFailed)
    }

  def fetchHistory(): Unit = (coin +: State.refCoins.filterNot(_ == coin)).foreach(fetchHistoryFor)

  def setMarket(newCoin: String): Unit =
    if newCoin != coin then
      val old = coin
      coin = newCoin
      Feed.unsubscriptions(old).foreach(send)
      Feed.subscriptions(newCoin).foreach(send)
      fetchHistoryFor(newCoin)

object JdkConnector extends Connector:
  private val client = HttpClient.newHttpClient()

  def connect(onText: String => Unit, onClose: () => Unit): Socket =
    val joiner = new Joiner
    val deliver = onText
    val closed = onClose
    val listener = new WebSocket.Listener:
      override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
        joiner.append(data.toString, last).foreach(deliver)
        ws.request(1)
        null
      override def onClose(ws: WebSocket, code: Int, reason: String): CompletionStage[?] =
        closed()
        null
      override def onError(ws: WebSocket, error: Throwable): Unit = closed()
    val ws = client.newWebSocketBuilder().buildAsync(URI.create(Feed.url), listener).join()
    new Socket:
      def send(text: String): Unit =
        val _ = ws.sendText(text, true).join()
      def close(): Unit =
        val _ = ws.sendClose(WebSocket.NORMAL_CLOSURE, "bye")

object JdkRest extends Rest:
  private val client = HttpClient.newHttpClient()

  def post(body: String): Option[String] = Try {
    val request = HttpRequest
      .newBuilder(URI.create(Feed.restUrl))
      .timeout(java.time.Duration.ofSeconds(5))
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    if response.statusCode() == 200 then response.body()
    else throw new RuntimeException(s"http ${response.statusCode()}")
  }.toOption
