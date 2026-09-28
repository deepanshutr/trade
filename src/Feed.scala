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
  def connectReporting(onText: String => Unit, onClose: String => Unit): Socket =
    connect(onText, () => onClose("Connection closed"))

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
  val unknownMarket = "Unknown market. Press m to retry"
  val ping = """{"method":"ping"}"""
  val historyEveryMs = 60_000L
  val heartbeatEveryMs = 500L
  val pingEveryMs = 5000L
  val replyWaitMs = 5000L

  def failure(error: Throwable): String = error match
    case e: java.util.concurrent.ExecutionException if e.getCause != null => failure(e.getCause)
    case _: java.util.concurrent.TimeoutException | _: java.net.http.HttpTimeoutException =>
      "Connection timed out"
    case e: java.net.http.WebSocketHandshakeException =>
      s"Connection refused (${e.getResponse.statusCode()})"
    case _: javax.net.ssl.SSLException    => "Secure connection failed"
    case _: java.net.UnknownHostException => "Address unavailable"
    case _: java.net.ConnectException     => "Network unavailable"
    case _: java.io.IOException           => "Network error"
    case _                                => "Connection failed"

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
    observer: Event => Unit = _ => (),
    monotonic: () => Long = () => System.nanoTime() / 1_000_000L
) extends MarketFeed:
  @volatile var coin: String = coin0
  private final class Connection:
    val closed = new CountDownLatch(1)
    var socket: Option[Socket] = None
    var openedAt = 0L
    var received = false
    var pingAt = Option.empty[Long]
    var lastPingAt = Option.empty[Long]
  private var current: Option[Connection] = None
  private var lastHealthy = false
  private var connectionStatus = "Connecting"
  def status: String = synchronized(connectionStatus)
  private var historyAt = Map.empty[(String, String), Long]
  private var historySuccess = Map.empty[(String, String), Long]
  private var historyInFlight = Set.empty[(String, String)]
  @volatile private var marketGeneration = 0L
  private var selectionGeneration = 0L
  private val historyWakeup = new ArrayBlockingQueue[java.lang.Boolean](1)
  private val sendLock = new Object

  private final class StartupLookup(val selection: (String, Long)):
    val done = new CountDownLatch(1)
    @volatile var result = Option.empty[Either[String, Unit]]
    val thread = new Thread(
      () =>
        try
          val found = checkMarket(selection._1)
          Feed.this.synchronized {
            result = Some(found)
            if relevant && found == Left(Feed.unknownMarket) then
              current.foreach(c => end(c, Feed.unknownMarket))
              connectionStatus = Feed.unknownMarket
              push(Event.MarketUnavailable(selection._1, Feed.unknownMarket))
          }
        catch case _: InterruptedException => ()
        finally done.countDown(),
      "market-check"
    )
    thread.setDaemon(true)
    def relevant: Boolean = selection == ((coin, selectionGeneration))
    def permitsConnection: Boolean = !relevant || !result.contains(Left(Feed.unknownMarket))
    def close(): Unit =
      thread.interrupt()
      thread.join(5000L)

  private val references = referenceCoins.distinct
  require(references.size <= 3, "choose at most three reference markets")

  private def push(e: Event): Unit =
    observer(e)
    val _ = queue.add(e)

  private def end(c: Connection, reason: String = "Connection closed"): Unit = synchronized {
    if current.contains(c) then
      current = None
      lastHealthy = c.received && monotonic() - c.openedAt >= 10_000L
      connectionStatus = reason
      try push(Event.SocketDown)
      finally
        c.closed.countDown()
        c.socket.foreach(s => { val _ = Try(s.close()) })
  }

  private def transmit(c: Connection, text: String): Unit =
    val socket = synchronized { if current.contains(c) then c.socket else None }
    socket.foreach(s => Try(s.send(text)).failed.foreach(e => end(c, Feed.failure(e))))

  def connectOnce(permitted: () => Boolean = () => true): Boolean =
    val c = new Connection
    val begin = synchronized {
      if !permitted() then false
      else
        current.foreach(c => end(c))
        current = Some(c)
        lastHealthy = false
        connectionStatus = "Connecting"
        true
    }
    if !begin then return false
    def receive(text: String): Unit = synchronized {
      if current.contains(c) && c.socket.nonEmpty then
        Events.parse(text).foreach { event =>
          event match
            case Event.Pong =>
              c.pingAt = None
              c.received = true
            case Event.Bad => ()
            case _         => c.received = true
          push(event)
        }
    }
    try
      Try(connector.connectReporting(receive, reason => end(c, reason))) match
        case scala.util.Failure(error) =>
          end(c, Feed.failure(error))
          false
        case scala.util.Success(s) =>
          val opened = sendLock.synchronized {
            val accepted = synchronized {
              if current.contains(c) then
                c.socket = Some(s)
                c.openedAt = monotonic()
                connectionStatus = "Connected"
                push(Event.SocketUp)
                true
              else false
            }
            if accepted then Feed.subscriptions(coin, references).foreach(transmit(c, _))
            else { val _ = Try(s.close()) }
            accepted
          }
          synchronized {
            if current.contains(c) then
              val now = clock()
              historyAt = historyAt.filter((_, at) => now - at < Feed.historyEveryMs)
              wakeHistory()
          }
          c.closed.await()
          opened
    finally end(c)

  def runForever(sleep: Long => Unit): Unit =
    var attempt = 0
    var verified = Option.empty[(String, Long)]
    var lookup = Option.empty[StartupLookup]
    try
      while true do
        val selected = synchronized((coin, selectionGeneration))
        if !verified.contains(selected) then
          val replace = lookup.forall(l =>
            !l.thread.isAlive && (l.selection != selected || l.result.exists(_.isLeft))
          )
          if replace then
            val next = new StartupLookup(selected)
            lookup = Some(next)
            next.thread.start()
          lookup.filter(_.selection == selected).foreach { check =>
            val _ = check.done.await(1000L, TimeUnit.MILLISECONDS)
            if check.result.contains(Right(())) then verified = Some(selected)
          }
        // A slow public lookup must not hold the independent socket path.
        // Its eventual rejection still closes only the matching selection.
        val _ = connectOnce(() => lookup.forall(_.permitsConnection))
        attempt = if synchronized(lastHealthy) then 1 else (attempt + 1).min(4)
        val delay = Feed.backoffMs(attempt)
        synchronized { connectionStatus = s"$connectionStatus. Retrying in ${delay / 1000}s" }
        sleep(delay)
    finally lookup.foreach(_.close())

  def heartbeat(): Unit = sendLock.synchronized {
    val send = synchronized {
      current.filter(_.socket.nonEmpty).flatMap { c =>
        val now = monotonic()
        if c.pingAt.exists(at => now - at >= Feed.replyWaitMs) then
          end(c, "No heartbeat reply")
          None
        else if c.pingAt.isEmpty && c.lastPingAt.forall(at => now - at >= Feed.pingEveryMs) then
          c.pingAt = Some(now)
          c.lastPingAt = Some(now)
          Some(c)
        else None
      }
    }
    send.foreach(transmit(_, Feed.ping))
  }

  def poll(): Unit =
    val c = coin
    rest
      .post(s"""{"type":"l2Book","coin":"$c"}""")
      .flatMap(Events.parseRestBook)
      .foreach(b => push(Event.BookUpdate(c, b, fromSocket = false)))

  def checkMarket(name: String): Either[String, Unit] =
    val unavailable = "Cannot check market. Press m to retry"
    Try {
      rest.post(ujson.write(ujson.Obj("type" -> "l2Book", "coin" -> name))) match
        case Some(text) if ujson.read(text).isNull =>
          Left(Feed.unknownMarket)
        case Some(text)
            if ujson.read(text)("coin").str == name && Events.parseRestBook(text).nonEmpty =>
          Right(())
        case _ => Left(unavailable)
    }.getOrElse(Left(unavailable))

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
      if !relevant() || historyInFlight.contains(key) || !due then false
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
      finally
        synchronized { historyInFlight -= key }
        if !relevant() then wakeHistory()

  def repairHistory(): Unit =
    val (generation, c) = synchronized { (marketGeneration, coin) }
    (c +: references.filterNot(_ == c)).foreach(market =>
      State.intervals.foreach(i => history(market, i, () => generation == marketGeneration))
    )

  private def wakeHistory(): Unit = { val _ = historyWakeup.offer(java.lang.Boolean.TRUE) }

  def awaitHistoryRepair(): Unit =
    val _ = historyWakeup.poll(Feed.historyEveryMs, TimeUnit.MILLISECONDS)
    repairHistory()

  def reloadHistory(): Unit =
    synchronized {
      marketGeneration += 1L
      historyAt = historyAt.filterNot(_._1._1 == coin)
      historySuccess = historySuccess.filterNot(_._1._1 == coin)
    }
    wakeHistory()

  def setMarket(newCoin: String): Unit =
    val changed = sendLock.synchronized {
      if newCoin == coin then false
      else
        val old = coin
        synchronized {
          coin = newCoin
          selectionGeneration += 1L
          marketGeneration += 1L
          historyAt = historyAt.filterNot(_._1._1 == newCoin)
          historySuccess = historySuccess.filterNot(_._1._1 == newCoin)
        }
        synchronized(current).foreach { c =>
          Feed
            .unsubscriptions(old, references)
            .diff(Feed.unsubscriptions(newCoin, references))
            .foreach(transmit(c, _))
          Feed
            .subscriptions(newCoin, references)
            .diff(Feed.subscriptions(old, references))
            .foreach(transmit(c, _))
        }
        true
    }
    if changed then wakeHistory()

final class SocketConnector(val url: String, waitMs: Long = 5000L) extends Connector:
  private val client =
    HttpClient.newBuilder().connectTimeout(java.time.Duration.ofMillis(waitMs)).build()

  def connect(onText: String => Unit, onClose: () => Unit): Socket =
    connectWith(
      onText,
      onClose,
      listener =>
        client
          .newWebSocketBuilder()
          .connectTimeout(java.time.Duration.ofMillis(waitMs))
          .buildAsync(URI.create(url), listener)
    )

  override def connectReporting(onText: String => Unit, onClose: String => Unit): Socket =
    connectWith(
      onText,
      () => (),
      listener =>
        client
          .newWebSocketBuilder()
          .connectTimeout(java.time.Duration.ofMillis(waitMs))
          .buildAsync(URI.create(url), listener),
      onClose
    )

  private def await[A](future: CompletableFuture[A]): A = future.get(waitMs, TimeUnit.MILLISECONDS)

  private[desk] def connectWith(
      deliver: String => Unit,
      closed: () => Unit,
      open: WebSocket.Listener => CompletableFuture[WebSocket],
      report: String => Unit = _ => ()
  ): Socket =
    val joiner = new Joiner
    val listener = new WebSocket.Listener:
      override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
        joiner.append(data.toString, last).foreach(deliver)
        ws.request(1)
        null
      override def onClose(ws: WebSocket, code: Int, reason: String): CompletionStage[?] =
        report(s"Server closed connection ($code)")
        closed()
        null
      override def onError(ws: WebSocket, error: Throwable): Unit =
        report(Feed.failure(error))
        closed()
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
