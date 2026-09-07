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

final class JdkExchange(base: String) extends Exchange:
  private def client(): HttpClient = HttpClient
    .newBuilder()
    .connectTimeout(Duration.ofSeconds(2))
    .followRedirects(HttpClient.Redirect.NEVER)
    .build()
  private val reads = client()
  private val orders = client()
  private var stream = Option.empty[WebSocket]
  private var watched = Option.empty[(String, String, Long)]
  private var receivedAt = 0L
  private var pingAt = 0L
  private val streamGuard = new StreamGuard
  override def watch(
      coin: String,
      owner: String,
      generation: Long,
      receive: Option[ujson.Value] => Unit
  ): Unit =
    val key = (coin, owner, generation)
    val now = System.nanoTime()
    val reopen = synchronized {
      watched != Some(key) || stream.isEmpty || now - receivedAt > TimeUnit.SECONDS.toNanos(30)
    }
    if reopen then
      val id = streamGuard.begin()
      synchronized {
        watched = Some(key)
        stream.foreach(_.abort())
        stream = None
        receivedAt = now
      }
      val _ = streamGuard.deliver(id) { receive(None) }
      val text = new StringBuilder
      def disconnected(): Unit =
        val _ = streamGuard.deliver(id) {
          synchronized { stream = None }
          receive(None)
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
              parsed
                .filter(_.obj.get("channel").exists(_.str == "activeAssetData"))
                .foreach(j => receive(j.obj.get("data")))
            }
          socket.request(1)
          null
        override def onClose(socket: WebSocket, code: Int, reason: String): CompletionStage[?] =
          disconnected()
          null
        override def onError(socket: WebSocket, error: Throwable): Unit = disconnected()
      val socket = reads
        .newWebSocketBuilder()
        .connectTimeout(Duration.ofSeconds(2))
        .buildAsync(URI.create(base.replace("https://", "wss://") + "/ws"), listener)
        .get(3, TimeUnit.SECONDS)
      if !streamGuard.deliver(id) { synchronized { stream = Some(socket) } } then
        socket.abort()
        throw new IllegalStateException("Account connection was replaced")
      val message = ujson.Obj(
        "method" -> "subscribe",
        "subscription" -> ujson.Obj("type" -> "activeAssetData", "user" -> owner, "coin" -> coin)
      )
      val _ = socket.sendText(ujson.write(message), true).get(3, TimeUnit.SECONDS)
    else if now - pingAt > TimeUnit.SECONDS.toNanos(20) then
      val socket = synchronized(stream)
      socket.foreach(s => {
        val _ = s.sendText("{\"method\":\"ping\"}", true).get(3, TimeUnit.SECONDS)
      })
      pingAt = now
  private def request(client: HttpClient, path: String, body: ujson.Value): Option[ujson.Value] =
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
  def info(body: ujson.Value): Option[ujson.Value] = request(reads, "/info", body)
  def post(body: ujson.Value): Option[ujson.Value] = request(orders, "/exchange", body)
  override def warm(): Unit =
    val _ = request(orders, "/info", ujson.Obj("type" -> "meta"))
  override def close(): Unit =
    streamGuard.invalidate()
    synchronized {
      watched = None
      stream.foreach(_.abort())
      stream = None
    }
    reads.shutdownNow()
    orders.shutdownNow()

object Exchange:
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

  def market(meta: ujson.Value, coin: String): MarketRules =
    require(coin.matches("[A-Z0-9][A-Z0-9._-]{0,19}"))
    val found = meta("universe").arr.toVector.zipWithIndex.filter(_._1("name").str == coin)
    require(found.size == 1)
    val (asset, index) = found.head
    require(!asset.obj.get("isDelisted").exists(_.bool))
    val rules = MarketRules(index, integer(asset("szDecimals")), integer(asset("maxLeverage")))
    require(rules.szDecimals <= 6 && rules.maxLeverage >= 5)
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
            f("oid").num.isWhole && f("oid").num >= 0 && quantity > 0 && quantity <= BigDecimal(
              saved.quantity
            ) && price > 0
          )
          require(if saved.side == Side.Buy then price <= BigDecimal(saved.limit)
          else price >= BigDecimal(saved.limit))
          s"${saved.coin} filled ${Trading.display(quantity, 6)} at ${Trading.display(price, 6)}"
      }.toOption
    )

  def recovered(reply: Option[ujson.Value], saved: SavedOrder): Option[String] =
    reply.flatMap(j =>
      Try {
        require(j("status").str == "order")
        val detail = j("order")
        val order = detail("order")
        val status = detail("status").str
        require(terminal.contains(status) && order("cloid").str == saved.id)
        require(
          order("coin").str == saved.coin && order("side").str == (if saved.side == Side.Buy then
                                                                     "B"
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
      require(OrderSigning.address(key) == config.agent.get, "protected signing address differs")
      success = true
      key
    finally
      if process.isAlive then
        process.destroyForcibly()
        val _ = process.waitFor(1, TimeUnit.SECONDS)
      java.util.Arrays.fill(raw, 0.toByte)
      if !success then java.util.Arrays.fill(key, 0.toByte)
      process.getInputStream.close()
