package desk

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.util.concurrent.{
  ArrayBlockingQueue,
  CompletableFuture,
  CompletionStage,
  CountDownLatch,
  TimeUnit
}
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
  val ping = """{"method":"ping"}"""
  val historyEveryMs = 60_000L

  def backoffMs(attempt: Int): Long = attempt match
    case 1 => 1000L
    case 2 => 2000L
    case 3 => 4000L
    case _ => 8000L

  private def sub(kind: String, c: String, extra: String = ""): String =
    s"""{"method":"subscribe","subscription":{"type":"$kind","coin":"$c"$extra}}"""

  def subscriptions(c: String, referenceCoins: Vector[String] = State.refCoins): Vector[String] =
    val main =
      Vector(
        sub("l2Book", c, ""","fast":true"""),
        sub("bbo", c),
        sub("trades", c),
        sub("activeAssetCtx", c)
      ) ++
        State.intervals.map(i => sub("candle", c, s""","interval":"$i""""))
    val refs = referenceCoins.distinct.flatMap(r =>
      sub("bbo", r) +: State.intervals.map(i => sub("candle", r, s""","interval":"$i""""))
    )
    (main ++ refs).distinct

  def unsubscriptions(c: String, referenceCoins: Vector[String] = State.refCoins): Vector[String] =
    subscriptions(c, referenceCoins)
      .map(_.replace("\"method\":\"subscribe\"", "\"method\":\"unsubscribe\""))

  def historyBody(coin: String, interval: String, now: Long, since: Option[Long] = None): String =
    val start = (now - 5000L * State.intervalMs(interval)).max(since.getOrElse(Long.MinValue))
    s"""{"type":"candleSnapshot","req":{"coin":"$coin","interval":"$interval","startTime":$start,"endTime":$now}}"""

  def coarseBody(coin: String, k: Int): String =
    s"""{"type":"l2Book","coin":"$coin","nSigFigs":$k}"""

final class Feed(
    connector: Connector,
    rest: Rest,
    queue: java.util.Queue[Event],
    clock: () => Long,
    coin0: String,
    referenceCoins: Vector[String] = State.refCoins,
    observer: Event => Unit = _ => ()
):
  @volatile var coin: String = coin0
  private final class Connection:
    val closed = new CountDownLatch(1)
    var socket: Option[Socket] = None
  private var current: Option[Connection] = None
  private var historyAt = Map.empty[(String, String), Long]
  private var historySuccess = Map.empty[(String, String), Long]
  private var historyInFlight = Set.empty[(String, String)]
  @volatile private var marketGeneration = 0L
  private val historyWakeup = new ArrayBlockingQueue[java.lang.Boolean](1)
  private val sendLock = new Object

  private val references = referenceCoins.distinct
  require(references.size <= 3, "choose at most three reference markets")

  private def push(e: Event): Unit =
    observer(e)
    val _ = queue.add(e)

  private def end(c: Connection): Unit = synchronized {
    if current.contains(c) then
      current = None
      try push(Event.SocketDown)
      finally
        c.closed.countDown()
        c.socket.foreach(s => { val _ = Try(s.close()) })
  }

  private def transmit(c: Connection, text: String): Unit =
    val socket = synchronized { if current.contains(c) then c.socket else None }
    socket.foreach(s => if Try(s.send(text)).isFailure then end(c))

  def connectOnce(): Boolean =
    val c = new Connection
    synchronized {
      current.foreach(end)
      current = Some(c)
    }
    def receive(text: String): Unit = synchronized {
      if current.contains(c) && c.socket.nonEmpty then Events.parse(text).foreach(push)
    }
    try
      Try(connector.connect(receive, () => end(c))).toOption match
        case None    => false
        case Some(s) =>
          val opened = sendLock.synchronized {
            val accepted = synchronized {
              if current.contains(c) then
                c.socket = Some(s)
                push(Event.SocketUp)
                true
              else false
            }
            if accepted then Feed.subscriptions(coin, references).foreach(transmit(c, _))
            else { val _ = Try(s.close()) }
            accepted
          }
          if synchronized(current.contains(c)) then wakeHistory()
          c.closed.await()
          opened
    finally end(c)

  def runForever(sleep: Long => Unit): Unit =
    var attempt = 0
    while true do
      val opened = connectOnce()
      attempt = if opened then 1 else attempt + 1
      sleep(Feed.backoffMs(attempt))

  def heartbeat(): Unit = sendLock.synchronized {
    synchronized(current).foreach(transmit(_, Feed.ping))
  }

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

  private def history(
      c: String,
      i: String,
      force: Boolean,
      relevant: () => Boolean = () => true
  ): Unit =
    val requestedAt = clock()
    val key = (c, i)
    val accepted = synchronized {
      val wait =
        if historySuccess.get(key).nonEmpty && historySuccess.get(key) == historyAt.get(key) then
          State.intervalMs(i).max(Feed.historyEveryMs)
        else Feed.historyEveryMs
      val due = historyAt.get(key).forall(at => requestedAt < at || requestedAt - at >= wait)
      if !relevant() || historyInFlight.contains(key) || (!force && !due) then false
      else
        historyAt = historyAt.updated(key, requestedAt)
        historyInFlight += key
        true
    }
    if accepted then
      try
        val since = synchronized(historySuccess.get(key).map(_ - 2L * State.intervalMs(i)))
        def attempt(): Option[Vector[Candle]] =
          if relevant() then
            rest.post(Feed.historyBody(c, i, clock(), since)).flatMap(Events.parseRestCandles)
          else None
        attempt().orElse(attempt()) match
          case Some(cs) =>
            synchronized {
              if relevant() then
                if cs.nonEmpty then historySuccess = historySuccess.updated(key, requestedAt)
                push(Event.CandleHistory(c, i, cs, requestedAt))
            }
          case None => if relevant() then push(Event.CandleHistoryFailed)
      finally synchronized { historyInFlight -= key }

  def fetchHistoryFor(c: String): Unit = State.intervals.foreach(i => history(c, i, true))

  def fetchHistory(): Unit =
    val c = coin
    (c +: references.filterNot(_ == c)).foreach(fetchHistoryFor)

  def repairHistory(): Unit =
    val (generation, c) = synchronized { (marketGeneration, coin) }
    (c +: references.filterNot(_ == c)).foreach(market =>
      State.intervals.foreach(i => history(market, i, false, () => generation == marketGeneration))
    )

  private def wakeHistory(): Unit = { val _ = historyWakeup.offer(java.lang.Boolean.TRUE) }

  def awaitHistoryRepair(): Unit =
    val _ = historyWakeup.poll(Feed.historyEveryMs, TimeUnit.MILLISECONDS)
    repairHistory()

  def setMarket(newCoin: String): Unit =
    val changed = sendLock.synchronized {
      if newCoin == coin then false
      else
        val old = coin
        synchronized {
          coin = newCoin
          marketGeneration += 1L
          historyAt = historyAt.filterNot(_._1._1 == newCoin)
          historySuccess = historySuccess.filterNot(_._1._1 == newCoin)
        }
        synchronized(current).foreach { c =>
          Feed.unsubscriptions(old, references).foreach(transmit(c, _))
          Feed.subscriptions(newCoin, references).foreach(transmit(c, _))
        }
        true
    }
    if changed then wakeHistory()

final class SocketConnector(val url: String, waitMs: Long = 5000L) extends Connector:
  private val client = HttpClient.newHttpClient()

  def connect(onText: String => Unit, onClose: () => Unit): Socket =
    connectWith(
      onText,
      onClose,
      listener => client.newWebSocketBuilder().buildAsync(URI.create(url), listener)
    )

  private def await[A](future: CompletableFuture[A]): A = future.get(waitMs, TimeUnit.MILLISECONDS)

  private[desk] def connectWith(
      deliver: String => Unit,
      closed: () => Unit,
      open: WebSocket.Listener => CompletableFuture[WebSocket]
  ): Socket =
    val joiner = new Joiner
    val listener = new WebSocket.Listener:
      override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
        joiner.append(data.toString, last).foreach(deliver)
        ws.request(1)
        null
      override def onClose(ws: WebSocket, code: Int, reason: String): CompletionStage[?] =
        closed()
        null
      override def onError(ws: WebSocket, error: Throwable): Unit = closed()
    val pending = open(listener)
    var accepted = false
    try
      val ws = await(pending)
      accepted = true
      new Socket:
        def send(text: String): Unit = { val _ = await(ws.sendText(text, true)) }
        def close(): Unit = ws.abort()
    finally if !accepted then { val _ = pending.thenAccept(_.abort()) }

final class EndpointRest(val url: String) extends Rest:
  private val client = HttpClient.newHttpClient()

  def post(body: String): Option[String] = Try {
    PublicRequestBudget.execute(body) {
      val request = HttpRequest
        .newBuilder(URI.create(url))
        .timeout(java.time.Duration.ofSeconds(5))
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(body))
        .build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofString())
      if response.statusCode() == 200 then response.body()
      else throw new RuntimeException(s"http ${response.statusCode()}")
    }
  }.toOption
