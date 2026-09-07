package desk

import java.nio.file.Path
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try

final case class TradingView(
    coin: String,
    position: Option[BigDecimal] = None,
    message: String,
    ready: Boolean = false,
    pending: Boolean = false,
    rules: Option[MarketRules] = None,
    bid: Option[BigDecimal] = None,
    ask: Option[BigDecimal] = None,
    lastPlan: Option[OrderPlan] = None
):
  def reference(side: Side): Option[BigDecimal] = if side == Side.Buy then ask else bid
  def forMarket(current: String): TradingView =
    if coin == current then this
    else TradingView(current, message = "changing market", pending = true)

object TradingWorker:
  def create(
      config: TradingConfig,
      enabled: Boolean,
      coin: String,
      journalDirectory: Path
  ): TradingWorker =
    new TradingWorker(
      config,
      enabled,
      coin,
      new JdkExchange(config.baseUrl),
      () => ProtectedKey.read(config),
      () => OrderJournal.open(journalDirectory, config.identity),
      () => System.currentTimeMillis(),
      () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime())
    )

final class TradingWorker private[desk] (
    config: TradingConfig,
    enabled: Boolean,
    coin0: String,
    exchange: Exchange,
    loadKey: () => Array[Byte],
    openJournal: () => OrderJournal,
    wall: () => Long,
    monotonic: () => Long,
    afterReserve: () => Unit = () => (),
    beforeOrderSign: () => Unit = () => (),
    warmSigning: () => Unit = () => OrderSigning.warm()
) extends AutoCloseable:
  private final case class Seen(book: Book, at: Long)
  private final case class Touch(bid: BigDecimal, ask: BigDecimal, time: Long, at: Long)
  private final case class Press(
      plan: OrderPlan,
      generation: Long,
      quoteAt: Long,
      bookAt: Long,
      accountAt: Long,
      activeAt: Long
  )
  private val busy = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val executing = new AtomicBoolean(false)
  private val requests = new ArrayBlockingQueue[Press](1)
  private var workers = Vector.empty[Thread]
  private var coin = coin0
  private var generation = 0L
  private var connected = false
  private var exact = Option.empty[Seen]
  private var touch = Option.empty[Touch]
  private var rules = Option.empty[MarketRules]
  private var position = Option.empty[BigDecimal]
  private var accountAt = Long.MinValue
  private var leverageReady = false
  private var activeAt = Long.MinValue
  private var activeMode = "cross"
  private var activeLeverage = 0
  private var roleAt = Long.MinValue
  private var failedRoleAt = Long.MinValue
  private var roleGeneration = Option.empty[Long]
  private var maximums = Vector(BigDecimal(0), BigDecimal(0))
  private var lastPlan = Option.empty[OrderPlan]
  private var message = if enabled then "Preparing trading" else "View only"
  private var fatal = false
  private var waitingRefresh = false
  private var accountVersion = 0L
  private var metadata = Option.empty[ujson.Value]
  private var metadataAt = Long.MinValue
  private var saved = Option.empty[SavedOrder]
  private var journal = Option.empty[OrderJournal]
  private var secret = Option.empty[Array[Byte]]
  private var leverageAttempt = Option.empty[Long]

  private def fresh(at: Long, age: Long): Boolean =
    val now = monotonic()
    at != Long.MinValue && now >= at && now - at <= age

  private def marketReady: Boolean =
    connected && rules.nonEmpty && exact.exists(e => fresh(e.at, config.quoteAgeMs)) &&
      touch.exists(t => t.bid > 0 && t.ask > t.bid && fresh(t.at, config.quoteAgeMs)) &&
      exact.exists(e =>
        touch.exists(t => (BigInt(t.time) - BigInt(e.book.time)).abs <= config.quoteAgeMs)
      )

  private def accountReady: Boolean =
    position.nonEmpty && leverageReady && fresh(accountAt, config.positionAgeMs) &&
      fresh(activeAt, config.positionAgeMs) && (!enabled ||
        (roleGeneration.contains(generation) && fresh(roleAt, 60000L)))

  def view: TradingView = synchronized {
    val shown = if message != "Ready" then message
    else if !marketReady then "Waiting for fresh selected-market depth"
    else if !accountReady then "Waiting for fresh account data"
    else message
    TradingView(
      coin,
      position,
      shown,
      enabled && !fatal && !busy
        .get() && !waitingRefresh && secret.nonEmpty && marketReady && accountReady,
      busy.get() || saved.nonEmpty,
      rules,
      touch.map(_.bid),
      touch.map(_.ask),
      lastPlan
    )
  }

  def select(next: String): Unit = synchronized {
    if next != coin then
      coin = next
      generation += 1
      exact = None
      touch = None
      rules = None
      position = None
      accountAt = Long.MinValue
      leverageReady = false
      activeAt = Long.MinValue
      roleGeneration = None
      leverageAttempt = None
      lastPlan = None
  }

  def observe(event: Event): Unit = synchronized {
    val now = monotonic()
    def quote(b: Book): Unit =
      if touch.forall(_.time <= b.time) then
        Try(Touch(BigDecimal(b.bids.head.px), BigDecimal(b.asks.head.px), b.time, now)).toOption
          .foreach(t => touch = Some(t))
    event match
      case Event.SocketUp =>
        if !connected then
          exact = None
          touch = None
        connected = true
      case Event.SocketDown =>
        connected = false
        generation += 1
        exact = None
        touch = None
        accountAt = Long.MinValue
        leverageReady = false
        activeAt = Long.MinValue
        roleGeneration = None
      case Event.BookUpdate(c, b, _) if c == coin =>
        val better = exact.forall { old =>
          b.time > old.book.time || (b.time == old.book.time &&
            b.bids.size >= old.book.bids.size && b.asks.size >= old.book.asks.size &&
            b.bids.size + b.asks.size > old.book.bids.size + old.book.asks.size)
        }
        if better then
          exact = Some(Seen(b, now))
          quote(b)
      case Event.BboUpdate(c, bid, ask, time) if c == coin && touch.forall(_.time <= time) =>
        Try(Touch(BigDecimal(bid.px), BigDecimal(ask.px), time, now)).toOption
          .foreach(t => touch = Some(t))
      case _ => ()
  }

  private def estimate(intent: OrderIntent): Either[String, OrderPlan] =
    if !marketReady then Left("Waiting for fresh selected-market depth")
    else
      Trading.plan(
        coin,
        rules.get,
        exact.get.book,
        if intent.side == Side.Buy then touch.get.ask else touch.get.bid,
        position.getOrElse(BigDecimal(0)),
        intent,
        config.sizes
      )

  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized(estimate(intent))

  def submit(intent: OrderIntent, expectedCoin: String): Boolean = synchronized {
    if expectedCoin != coin || !view.ready || !busy.compareAndSet(false, true) then false
    else
      estimate(intent) match
        case Left(reason) =>
          message = reason
          busy.set(false)
          false
        case Right(plan) =>
          val maximum = maximums(if intent.side == Side.Buy then 0 else 1)
          if plan.quantity > maximum then
            message = "Requested quantity exceeds confirmed account capacity"
            busy.set(false)
            false
          else
            val press = Press(plan, generation, touch.get.at, exact.get.at, accountAt, activeAt)
            lastPlan = Some(plan)
            message = coin + " submitting"
            if requests.offer(press) then true
            else
              busy.set(false)
              false
  }

  private def signed(action: ujson.Value, nonce: Long, expiry: Option[Long]): ujson.Value =
    val key = synchronized(secret.get)
    val signature = OrderSigning.sign(key, action, nonce, config.network == "mainnet", expiry)
    val request =
      ujson.Obj("action" -> action, "nonce" -> ujson.Num(nonce.toDouble), "signature" -> signature)
    expiry.foreach(t => request("expiresAfter") = ujson.Num(t.toDouble))
    request

  private def storageFailure(): Unit = synchronized {
    fatal = true
    busy.set(true)
    message = "Saved order state failed. Restart to recover"
  }

  private def complete(p: SavedOrder, text: String): Unit =
    try
      journal.get.finish(p.id)
      synchronized {
        saved = None
        message = text
        waitingRefresh = true
        accountVersion += 1
        accountAt = Long.MinValue
      }
    catch case _: Exception => storageFailure()

  private def execute(press: Press): Unit =
    executing.set(true)
    try executeOrder(press)
    finally executing.set(false)

  private def executeOrder(press: Press): Unit =
    val p = try
      val reserved = journal.get.reserve(press.plan, wall())
      afterReserve()
      reserved
    catch
      case _: Exception =>
        storageFailure()
        return
    synchronized { saved = Some(p) }
    val request = Try {
      beforeOrderSign()
      signed(Exchange.action(p), p.nonce, Some(p.expiresAfter))
    }
    val canSend = synchronized {
      !closed.get() && !fatal && press.generation == generation && press.plan.coin == coin &&
      marketReady && accountReady && fresh(press.quoteAt, config.quoteAgeMs) &&
      fresh(press.bookAt, config.quoteAgeMs) && fresh(press.accountAt, config.positionAgeMs) &&
      fresh(press.activeAt, config.positionAgeMs) &&
      press.plan.quantity <= maximums(if press.plan.side == Side.Buy then 0 else 1) &&
      wall() < p.expiresAfter
    }
    if !canSend || request.isFailure then complete(p, p.coin + " canceled before sending")
    else
      val response = Try(exchange.post(request.get)).toOption.flatten
      Exchange.outcome(response, p) match
        case Some(text) => complete(p, text)
        case None       => synchronized { message = p.coin + " waiting for order result" }

  private def initializeSigning(): Unit =
    if journal.isEmpty then
      try
        val opened = openJournal()
        journal = Some(opened)
        synchronized {
          saved = opened.pending
          if saved.nonEmpty then
            busy.set(true)
            message = saved.get.coin + " waiting for order result"
        }
      catch case _: Exception => storageFailure()
    if !synchronized(fatal || saved.nonEmpty || secret.nonEmpty) then
      var loaded = Array.emptyByteArray
      try
        loaded = loadKey()
        require(OrderSigning.address(loaded) == config.agent.get)
        synchronized { secret = Some(loaded) }
        exchange.warm()
      catch
        case _: Exception =>
          java.util.Arrays.fill(loaded, 0.toByte)
          synchronized {
            fatal = true
            message = "Protected signing key is unavailable or differs"
          }

  private def prepareLeverage(g: Long, market: MarketRules, cross: Boolean): Unit =
    val attempt = synchronized {
      if fatal || generation != g || leverageAttempt.contains(g) || secret.isEmpty ||
        !busy.compareAndSet(false, true)
      then false
      else
        leverageAttempt = Some(g)
        leverageReady = false
        true
    }
    if attempt then
      try
        val nonce = journal.get.nextNonce(wall())
        val action = ujson.Obj(
          "type" -> "updateLeverage",
          "asset" -> market.asset,
          "isCross" -> cross,
          "leverage" -> 5
        )
        val expiry = Math.addExact(wall(), 5000L)
        val request = signed(action, nonce, Some(expiry))
        val canSend = synchronized {
          !closed.get() && !fatal && generation == g &&
          fresh(activeAt, config.positionAgeMs) && (activeMode == "cross") == cross &&
          activeLeverage != 5 && wall() < expiry
        }
        if canSend then
          val _ = exchange.post(request)
          synchronized {
            if generation == g then
              roleGeneration = None
              message = "Waiting for confirmed five times leverage"
          }
        else
          synchronized {
            if generation == g && leverageAttempt.contains(g) then leverageAttempt = None
          }
      catch case _: Exception => storageFailure()
      finally if !synchronized(fatal) then busy.set(false)

  private def receiveActive(g: Long, current: String, value: Option[ujson.Value]): Unit =
    val parsed = value.flatMap(active =>
      Try {
        require(
          active("user").str.toLowerCase(java.util.Locale.ROOT) == config.owner.get && active(
            "coin"
          ).str == current
        )
        val mode = active("leverage")("type").str
        require(mode == "cross" || mode == "isolated")
        val leverage = Exchange.integer(active("leverage")("value"))
        val limits = active("maxTradeSzs").arr.toVector.map { v =>
          if v.isInstanceOf[ujson.Str] then BigDecimal(v.str) else BigDecimal(v.num.toString)
        }
        require(limits.size == 2 && limits.forall(_ >= 0))
        (mode, leverage, limits)
      }.toOption
    )
    synchronized {
      if generation == g then
        parsed match
          case Some((mode, leverage, limits)) =>
            activeMode = mode
            activeLeverage = leverage
            leverageReady = leverage == 5
            maximums = limits
            activeAt = monotonic()
          case None =>
            leverageReady = false
            activeAt = Long.MinValue
            roleGeneration = None
    }

  private def refresh(): Unit =
    val (current, g, version) = synchronized((coin, generation, accountVersion))
    val startedAt = monotonic()
    try
      if metadata.isEmpty || !fresh(metadataAt, 60000L) then
        metadata = Some(exchange.info(ujson.Obj("type" -> "meta")).get)
        metadataAt = monotonic()
      val matched = Try(Exchange.market(metadata.get, current)).toOption
      synchronized { if generation == g then rules = matched }
      val market = matched.get
      if config.owner.nonEmpty then
        if enabled then
          require(config.configured, "owner")
          if synchronized(!roleGeneration.contains(g) || !fresh(roleAt, 60000L)) then
            if fresh(failedRoleAt, 10000L) then return
            try
              val role =
                exchange.info(ujson.Obj("type" -> "userRole", "user" -> config.agent.get)).get
              require(
                role("role").str == "agent" && role("data")("user").str
                  .toLowerCase(java.util.Locale.ROOT) == config.owner.get,
                "owner"
              )
            catch
              case error: Exception =>
                failedRoleAt = monotonic()
                throw error
            synchronized {
              if generation == g then
                roleGeneration = Some(g)
                roleAt = startedAt
            }
          initializeSigning()
        exchange.watch(current, config.owner.get, g, data => receiveActive(g, current, data))
        val account =
          exchange.info(ujson.Obj("type" -> "clearinghouseState", "user" -> config.owner.get)).get
        val positions = account("assetPositions").arr.toVector
          .map(_("position"))
          .filter(_("coin").str == current)
        require(positions.size <= 1)
        val quantity =
          positions.headOption.map(p => BigDecimal(p("szi").str)).getOrElse(BigDecimal(0))
        synchronized {
          if generation == g && version == accountVersion then
            position = Some(quantity)
            accountAt = startedAt
            if waitingRefresh && !fatal && saved.isEmpty && !executing.get() then
              waitingRefresh = false
              busy.set(false)
            if Set("Preparing trading", "Waiting for confirmed five times leverage").contains(
                message
              ) &&
              leverageReady && secret.nonEmpty
            then message = "Ready"
        }
        val preparation = synchronized {
          if enabled && fresh(activeAt, config.positionAgeMs) && activeLeverage != 5 then
            Some(activeMode == "cross")
          else None
        }
        preparation.foreach(cross => prepareLeverage(g, market, cross))
        val unresolved = synchronized(if !fatal && !executing.get() then saved else None)
        unresolved.foreach { p =>
          val reply = exchange.info(
            ujson.Obj("type" -> "orderStatus", "user" -> config.owner.get, "oid" -> p.id)
          )
          Exchange.recovered(reply, p).foreach(text => complete(p, text))
        }
      else if enabled then synchronized { message = "Configure owner and delegated signer" }
    catch
      case error: Exception =>
        synchronized {
          accountAt = Long.MinValue
          leverageReady = false
          if !fatal && saved.isEmpty && !busy.get() then
            message = if Option(error.getMessage).exists(_.endsWith(": owner")) then
              "Signer does not match the configured owner"
            else "Waiting for market or account data"
        }

  def start(): Unit =
    if started.compareAndSet(false, true) then
      def daemon(name: String)(body: => Unit): Thread =
        val thread = new Thread(
          () => {
            try body
            catch case _: InterruptedException => Thread.currentThread().interrupt()
          },
          name
        )
        thread.setDaemon(true)
        thread
      workers = Vector(
        daemon("orders") { while !closed.get() do execute(requests.take()) },
        daemon("account") {
          val prepared =
            if !enabled then true
            else
              try
                warmSigning()
                true
              catch
                case error: InterruptedException => throw error
                case _: Exception                =>
                  synchronized {
                    fatal = true
                    message = "Order signing could not be prepared. Restart to retry"
                  }
                  false
          if prepared then
            while !closed.get() do
              refresh()
              Thread.sleep(1000)
        }
      )
      workers.foreach(_.start())

  def close(): Unit =
    if closed.compareAndSet(false, true) then
      workers.foreach(_.interrupt())
      exchange.close()
      workers.foreach(_.join(4000))
      if workers.forall(!_.isAlive) then
        synchronized { secret.foreach(key => java.util.Arrays.fill(key, 0.toByte)); secret = None }
        journal.foreach(_.close())
