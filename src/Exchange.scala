package desk

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.time.Duration
import java.util.concurrent.{CompletionStage, TimeUnit}
import scala.util.Try

trait Exchange:
  def info(body: ujson.Value): Option[ujson.Value]
  def post(body: ujson.Value): Option[ujson.Value]
  def watch(
      coin: String,
      owner: String,
      generation: Long,
      receive: Option[ujson.Value] => Unit
  ): Unit
  def watchTimed(
      coin: String,
      owner: String,
      generation: Long,
      clock: () => Long,
      receive: (Option[ujson.Value], Long) => Unit
  ): Unit = watch(coin, owner, generation, data => receive(data, clock()))
  def warm(): Unit = ()
  def close(): Unit = ()

private[desk] final class StreamGuard:
  private var serial = 0L
  def begin(): Long = synchronized {
    serial += 1
    serial
  }
  def deliver(id: Long)(body: => Unit): Boolean = synchronized {
    if serial != id then false
    else
      body
      true
  }
  def invalidate(): Unit = synchronized { serial += 1 }

private[desk] final case class ActiveDataSession(
    coin: String,
    owner: String,
    generation: Long,
    connection: Long
)

private[desk] final class ActiveDataReads:
  private final case class Pending(
      id: Long,
      at: Long,
      superseded: Boolean = false
  )
  private var current = Option.empty[ActiveDataSession]
  private var pending = Option.empty[Pending]
  private var serial = 0L
  private var attemptedAt = Long.MinValue
  private var receivedAt = Long.MinValue
  private def fresh(at: Long, now: Long, age: Long): Boolean =
    at != Long.MinValue && now >= at && now - at <= age
  def open(session: ActiveDataSession): Unit = synchronized {
    current = Some(session)
    pending = None
    attemptedAt = Long.MinValue
    receivedAt = Long.MinValue
  }
  def close(): Unit = synchronized {
    current = None
    pending = None
  }
  def expire(session: ActiveDataSession, now: Long): Boolean = synchronized {
    if current.contains(session) && pending.exists(p => !fresh(p.at, now, 2000L))
    then
      current = None
      pending = None
      true
    else false
  }
  def request(session: ActiveDataSession, now: Long): Option[ujson.Value] =
    synchronized {
      if !current.contains(session) || pending.nonEmpty || fresh(
          attemptedAt,
          now,
          999L
        ) ||
        fresh(receivedAt, now, 999L)
      then None
      else
        serial += 1L
        require(serial <= 9007199254740991L)
        pending = Some(Pending(serial, now))
        attemptedAt = now
        Some(
          ujson.Obj(
            "method" -> "post",
            "id" -> ujson.Num(serial.toDouble),
            "request" -> ujson.Obj(
              "type" -> "info",
              "payload" -> ujson.Obj(
                "type" -> "activeAssetData",
                "user" -> session.owner,
                "coin" -> session.coin
              )
            )
          )
        )
    }
  def accept(
      session: ActiveDataSession,
      message: ujson.Value,
      now: Long
  ): Option[(Option[ujson.Value], Long)] = synchronized {
    def matching(value: ujson.Value): Boolean = Try {
      value("user").str.toLowerCase(java.util.Locale.ROOT) == session.owner &&
      value("coin").str == session.coin
    }.getOrElse(false)
    if !current.contains(session) then None
    else
      Try(message("channel").str).toOption match
        case Some("activeAssetData") =>
          message.obj.get("data").filter(matching).map { value =>
            pending = pending.map(_.copy(superseded = true))
            receivedAt = now
            (Some(value), now)
          }
        case Some("post") =>
          pending
            .filter(p => Try(message("data")("id").num == p.id.toDouble).getOrElse(false))
            .flatMap { p =>
              pending = None
              if p.superseded then None
              else
                val value = Try {
                  require(fresh(p.at, now, 2000L))
                  val response = message("data")("response")
                  require(response("type").str == "info")
                  val payload = response("payload")
                  require(payload("type").str == "activeAssetData")
                  val data = payload("data")
                  require(matching(data))
                  data
                }.toOption
                if value.nonEmpty then receivedAt = p.at
                Some((value, p.at))
            }
        case _ => None
  }

final class JdkExchange(
    base: String,
    connectSocket: Option[(HttpClient, URI, WebSocket.Listener) => WebSocket] = None
) extends Exchange:
  private def client(): HttpClient = HttpClient
    .newBuilder()
    .connectTimeout(Duration.ofSeconds(2))
    .followRedirects(HttpClient.Redirect.NEVER)
    .build()
  private val reads = client()
  private val orders = client()
  private var stream = Option.empty[WebSocket]
  private var watched = Option.empty[ActiveDataSession]
  private var receivedAt = 0L
  private var pingAt = 0L
  private val streamGuard = new StreamGuard
  private val activeReads = new ActiveDataReads
  override def watch(
      coin: String,
      owner: String,
      generation: Long,
      receive: Option[ujson.Value] => Unit
  ): Unit = watchTimed(
    coin,
    owner,
    generation,
    () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime()),
    (data, _) => receive(data)
  )
  override def watchTimed(
      coin: String,
      owner: String,
      generation: Long,
      clock: () => Long,
      receive: (Option[ujson.Value], Long) => Unit
  ): Unit =
    val now = System.nanoTime()
    def selected(session: ActiveDataSession): Boolean =
      session.coin == coin && session.owner == owner && session.generation == generation
    val reopen = synchronized {
      !watched.exists(
        selected
      ) || stream.isEmpty || now - receivedAt > TimeUnit.SECONDS.toNanos(30)
    }
    if reopen then
      val id = streamGuard.begin()
      val session = ActiveDataSession(coin, owner, generation, id)
      synchronized {
        watched = Some(session)
        stream.foreach(_.abort())
        stream = None
        receivedAt = now
      }
      val _ = streamGuard.deliver(id) {
        activeReads.open(session)
        receive(None, clock())
      }
      val text = new StringBuilder
      def disconnected(): Unit =
        val _ = streamGuard.deliver(id) {
          synchronized { stream = None }
          activeReads.close()
          receive(None, clock())
        }
      val listener = new WebSocket.Listener:
        override def onOpen(socket: WebSocket): Unit =
          if !streamGuard.deliver(id) { socket.request(1) } then socket.abort()
        override def onText(
            socket: WebSocket,
            data: CharSequence,
            last: Boolean
        ): CompletionStage[?] =
          text.append(data)
          if text.length > 2000000 then
            socket.abort()
            disconnected()
          else if last then
            val parsed = Try(ujson.read(text.result())).toOption
            text.clear()
            val _ = streamGuard.deliver(id) {
              JdkExchange.this.synchronized { receivedAt = System.nanoTime() }
              parsed.flatMap(activeReads.accept(session, _, clock())).foreach { case (value, at) =>
                receive(value, at)
              }
            }
          socket.request(1)
          null
        override def onClose(
            socket: WebSocket,
            code: Int,
            reason: String
        ): CompletionStage[?] =
          disconnected()
          null
        override def onError(socket: WebSocket, error: Throwable): Unit =
          disconnected()
      val address = URI.create(base.replace("https://", "wss://") + "/ws")
      val socket = connectSocket.fold(
        reads
          .newWebSocketBuilder()
          .connectTimeout(Duration.ofSeconds(2))
          .buildAsync(address, listener)
          .get(3, TimeUnit.SECONDS)
      )(open => open(reads, address, listener))
      if !streamGuard.deliver(id) { synchronized { stream = Some(socket) } }
      then
        socket.abort()
        throw new IllegalStateException("Account connection was replaced")
      val message = ujson.Obj(
        "method" -> "subscribe",
        "subscription" -> ujson.Obj(
          "type" -> "activeAssetData",
          "user" -> owner,
          "coin" -> coin
        )
      )
      val _ =
        socket.sendText(ujson.write(message), true).get(3, TimeUnit.SECONDS)
    else if now - pingAt > TimeUnit.SECONDS.toNanos(20) then
      val socket = synchronized(stream)
      socket.foreach(s => {
        val _ =
          s.sendText("{\"method\":\"ping\"}", true).get(3, TimeUnit.SECONDS)
      })
      pingAt = now
    val connection = synchronized {
      for session <- watched.filter(selected); socket <- stream
      yield (session, socket)
    }
    connection.foreach { case (session, socket) =>
      var request = Option.empty[ujson.Value]
      var timedOut = false
      val _ = streamGuard.deliver(session.connection) {
        val at = clock()
        if activeReads.expire(session, at) then
          synchronized { stream = None }
          receive(None, at)
          timedOut = true
        else request = activeReads.request(session, at)
      }
      if timedOut then socket.abort()
      request.foreach { body =>
        val _ =
          socket.sendText(ujson.write(body), true).get(3, TimeUnit.SECONDS)
      }
    }
  private def request(
      client: HttpClient,
      path: String,
      body: ujson.Value
  ): Option[ujson.Value] =
    Try {
      val request = HttpRequest
        .newBuilder(URI.create(base + path))
        .timeout(Duration.ofSeconds(3))
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
        .build()
      val reply = client.send(request, HttpResponse.BodyHandlers.ofString())
      require(reply.statusCode() == 200 && reply.body().length <= 2000000)
      ujson.read(reply.body())
    }.toOption
  def info(body: ujson.Value): Option[ujson.Value] =
    request(reads, "/info", body)
  def post(body: ujson.Value): Option[ujson.Value] =
    request(orders, "/exchange", body)
  override def warm(): Unit =
    val _ = request(orders, "/info", ujson.Obj("type" -> "meta"))
  override def close(): Unit =
    streamGuard.invalidate()
    activeReads.close()
    synchronized {
      watched = None
      stream.foreach(_.abort())
      stream = None
    }
    reads.shutdownNow()
    orders.shutdownNow()

object Exchange:
  // Assumes the venue refuses actions after expiresAfter (unproven until a test-network order).
  // Local and venue clocks are within this margin: the venue checks expiry on its clock, Desk on local time.
  // Assumes order status lookup reflects every action applied before expiry within this margin.
  // Assumes an err reply plus unknownOid after this margin means the action was not placed.
  private val ExpiryMarginMs = 60000L
  // The local clock may step, so release also needs this much monotonic time since Desk
  // first saw the order: the five-second signed lifetime plus the margin.
  private val ReleaseWaitMs = 5000L + ExpiryMarginMs

  private val terminal = Set(
    "filled",
    "canceled",
    "rejected",
    "marginCanceled",
    "vaultWithdrawalCanceled",
    "openInterestCapCanceled",
    "selfTradeCanceled",
    "reduceOnlyCanceled",
    "siblingFilledCanceled",
    "delistedCanceled",
    "liquidatedCanceled",
    "scheduledCancel",
    "tickRejected",
    "minTradeNtlRejected",
    "perpMarginRejected",
    "reduceOnlyRejected",
    "badAloPxRejected",
    "iocCancelRejected",
    "badTriggerPxRejected",
    "marketOrderNoLiquidityRejected",
    "positionIncreaseAtOpenInterestCapRejected",
    "positionFlipAtOpenInterestCapRejected",
    "tooAggressiveAtOpenInterestCapRejected",
    "openInterestIncreaseRejected",
    "insufficientSpotBalanceRejected",
    "oracleRejected",
    "perpMaxPositionRejected"
  )

  private[desk] def integer(value: ujson.Value): Int =
    val n = value.num
    require(n.isWhole && n >= 0 && n <= Int.MaxValue)
    n.toInt

  private[desk] def validMetadata(value: ujson.Value, coin: String): Boolean = Try {
    val names = value("universe").arr.toVector.map(_("name").str)
    require(names.distinct.size == names.size)
    value("universe").arr.find(_("name").str == coin) match
      case None                                                      => true
      case Some(asset) if asset.obj.get("isDelisted").exists(_.bool) => true
      case Some(_)                                                   =>
        market(value, coin)
        true
  }.getOrElse(false)

  def market(meta: ujson.Value, coin: String): MarketRules =
    require(State.validMarket(coin))
    val found = meta("universe").arr.toVector.zipWithIndex
      .filter(_._1("name").str == coin)
    require(found.size == 1)
    val (asset, index) = found.head
    require(!asset.obj.get("isDelisted").exists(_.bool))
    val rules = MarketRules(
      index,
      integer(asset("szDecimals")),
      integer(asset("maxLeverage"))
    )
    require(rules.szDecimals <= 6 && rules.maxLeverage >= 1)
    rules

  def action(p: SavedOrder): ujson.Value = ujson.Obj(
    "type" -> "order",
    "orders" -> ujson.Arr(
      ujson.Obj(
        "a" -> p.asset,
        "b" -> (p.side == Side.Buy),
        "p" -> p.limit,
        "s" -> p.quantity,
        "r" -> false,
        "t" -> ujson.Obj("limit" -> ujson.Obj("tif" -> "Ioc")),
        "c" -> p.id
      )
    ),
    "grouping" -> "na"
  )

  private def status(reply: Option[ujson.Value]): Option[String] =
    reply.flatMap(_.objOpt.flatMap(_.get("status")).flatMap(_.strOpt))

  def isErrReply(reply: Option[ujson.Value]): Boolean = status(reply).contains("err")

  def outcome(reply: Option[ujson.Value], saved: SavedOrder): Option[String] =
    reply.flatMap(j =>
      Try {
        require(j("status").str == "ok" && j("response")("type").str == "order")
        val statuses = j("response")("data")("statuses").arr
        require(statuses.size == 1)
        val status = statuses.head
        if status.obj.keySet == Set("error") then
          val _ = status("error").str
          saved.coin + " order rejected"
        else
          require(status.obj.keySet == Set("filled"))
          val f = status("filled")
          val quantity = BigDecimal(f("totalSz").str)
          val price = BigDecimal(f("avgPx").str)
          require(
            f("oid").num.isWhole && f(
              "oid"
            ).num >= 0 && quantity > 0 && quantity <= BigDecimal(
              saved.quantity
            ) && price > 0
          )
          require(if saved.side == Side.Buy then price <= BigDecimal(saved.limit)
          else price >= BigDecimal(saved.limit))
          s"${saved.coin} filled ${Trading.display(quantity, 6)} at ${Trading.display(price, 6)}"
      }.toOption
    )

  def recovered(
      reply: Option[ujson.Value],
      saved: SavedOrder,
      now: Long,
      elapsedMs: Long,
      allowUnknown: Boolean,
      errReply: Boolean
  ): Option[String] =
    if status(reply).contains("unknownOid") && elapsedMs >= ReleaseWaitMs &&
      BigInt(now) > BigInt(saved.expiresAfter) + ExpiryMarginMs
    then
      if errReply then return Some(saved.coin + " order rejected")
      if allowUnknown then return Some(saved.coin + " not placed")
    reply.flatMap(j =>
      Try {
        require(j("status").str == "order")
        val detail = j("order")
        val order = detail("order")
        val status = detail("status").str
        require(terminal.contains(status) && order("cloid").str == saved.id)
        require(
          order("coin").str == saved.coin && order(
            "side"
          ).str == (if saved.side == Side.Buy then "B"
                    else "A")
        )
        require(BigDecimal(order("origSz").str) == BigDecimal(saved.quantity))
        require(BigDecimal(order("limitPx").str) == BigDecimal(saved.limit))
        s"${saved.coin} recovered $status"
      }.toOption
    )

object ProtectedKey:
  def read(config: TradingConfig): Array[Byte] =
    val process = new ProcessBuilder(
      "/usr/bin/security",
      "find-generic-password",
      "-s",
      config.keyService,
      "-a",
      config.agent.get,
      "-w"
    )
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    var raw = Array.emptyByteArray
    var key = Array.emptyByteArray
    var success = false
    try
      require(
        process.waitFor(8, TimeUnit.SECONDS) && process.exitValue() == 0,
        "protected signing key is unavailable"
      )
      raw = process.getInputStream.readNBytes(129)
      var end = raw.length
      while end > 0 && Set(10, 13, 32).contains(raw(end - 1).toInt) do end -= 1
      val start = if end >= 2 && raw(0) == 48 && raw(1) == 120 then 2 else 0
      require(end - start == 64, "protected signing key is invalid")
      key = new Array[Byte](32)
      for i <- key.indices do
        val hi = Character.digit(raw(start + i * 2).toChar, 16)
        val lo = Character.digit(raw(start + i * 2 + 1).toChar, 16)
        require(hi >= 0 && lo >= 0, "protected signing key is invalid")
        key(i) = ((hi << 4) | lo).toByte
      require(
        OrderSigning.address(key) == config.agent.get,
        "protected signing address differs"
      )
      success = true
      key
    finally
      if process.isAlive then
        process.destroyForcibly()
        val _ = process.waitFor(1, TimeUnit.SECONDS)
      java.util.Arrays.fill(raw, 0.toByte)
      if !success then java.util.Arrays.fill(key, 0.toByte)
      process.getInputStream.close()
