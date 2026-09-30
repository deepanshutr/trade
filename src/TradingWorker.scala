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
    lastPlan: Option[OrderPlan] = None,
    block: Option[String] = None,
    outcome: Option[String] = None,
    leverage: Option[Int] = None,
    marginMode: Option[String] = None,
    outcomeId: Long = 0L,
    portfolio: PortfolioView = PortfolioView(),
    blockCode: BlockCode = BlockCode.Blocked,
    usableMargin: Option[BigDecimal] = None
):
  def reference(side: Side): Option[BigDecimal] =
    if side == Side.Buy then ask else bid
  def forMarket(current: String): TradingView =
    if coin == current then this
    else TradingView(current, message = "changing market", pending = true)

object TradingWorker:
  val ownerMismatchMessage: String = "Signer does not match the configured owner"
  val savedOwnerMismatchMessage: String = "Saved order belongs to another owner"
  val maximumReviewLifetimeMs: Long = 30000L

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
      () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime()),
      orderPermission = run =>
        AccountGate.withOrder(
          AccountGate.directory,
          config.network,
          config.owner.get
        )(run()),
      accountPaused = () =>
        config.owner.exists(
          AccountGate.paused(AccountGate.directory, config.network, _)
        )
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
    warmSigning: () => Unit = () => OrderSigning.warm(),
    orderPermission: (() => Unit) => Unit = run => run(),
    accountPaused: () => Boolean = () => false
) extends AutoCloseable:
  private final case class Seen(book: Book, at: Long)
  private final case class Touch(
      bid: BigDecimal,
      ask: BigDecimal,
      time: Long,
      at: Long
  )
  private final case class Press(
      plan: OrderPlan,
      generation: Long,
      quoteAt: Long,
      bookAt: Long,
      accountAt: Long,
      activeAt: Long,
      leverage: Int,
      marginMode: String,
      review: Option[OrderReview]
  )
  private val busy = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val disabled = new AtomicBoolean(false)
  private val retired = new AtomicBoolean(false)
  private val refreshWake = new ArrayBlockingQueue[Boolean](1)
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
  private var portfolio = PortfolioView()
  private var marginCash = Option.empty[BigDecimal]
  private var balance = Option.empty[(String, Option[BigDecimal], Long)]
  private var abstraction = Option.empty[(String, Long)]
  private var abstractionAttemptAt = Long.MinValue
  private var accountAt = Long.MinValue
  private var leverageReady = false
  private var activeAt = Long.MinValue
  private var activeFloor = Long.MinValue
  private var activeMode = "cross"
  private var activeLeverage = 0
  private var roleAt = Long.MinValue
  private var failedRoleAt = Long.MinValue
  private var roleGeneration = Option.empty[Long]
  private var usableMargin = Option.empty[BigDecimal]
  private var maximums = Vector(BigDecimal(0), BigDecimal(0))
  private var lastPlan = Option.empty[OrderPlan]
  private var message = if enabled then "Preparing trading" else "View only"
  private var outcome = Option.empty[String]
  private var outcomeId = 0L
  private var fatal = false
  private var waitingRefresh = false
  private var accountVersion = 0L
  private var metadata = Option.empty[ujson.Value]
  private var metadataAt = Long.MinValue
  private[desk] def acceptedMetadata: Option[(ujson.Value, Long)] = synchronized {
    metadata.filter(_ => fresh(metadataAt, 60000L)).map(_ -> metadataAt)
  }
  private var saved = Option.empty[SavedOrder]
  private var journal = Option.empty[OrderJournal]
  private var secret = Option.empty[Array[Byte]]

  private def fresh(at: Long, age: Long): Boolean =
    val now = monotonic()
    at != Long.MinValue && now >= at && now - at <= age

  private def marketReady: Boolean =
    connected && rules.nonEmpty && exact.exists(e => fresh(e.at, config.quoteAgeMs)) &&
      touch.exists(t => t.bid > 0 && t.ask > t.bid && fresh(t.at, config.quoteAgeMs)) &&
      exact.exists(e =>
        touch.exists(t => (BigInt(t.time) - BigInt(e.book.time)).abs <= config.quoteAgeMs)
      )

  private def requiredLeverage: Boolean = coin != "XMR" || activeLeverage == 5

  private def wakeAccount(): Unit =
    val _ = refreshWake.offer(true)

  private def accountReady: Boolean =
    position.nonEmpty && leverageReady && requiredLeverage &&
      rules.exists(m => activeLeverage >= 1 && activeLeverage <= m.maxLeverage) &&
      fresh(accountAt, config.positionAgeMs) &&
      fresh(activeAt, config.positionAgeMs) && (!enabled ||
        (roleGeneration.contains(generation) && fresh(roleAt, 60000L)))

  def view: TradingView = synchronized {
    val paused = enabled && accountPaused()
    val block =
      if disabled.get() then Some(ExecutionBlock(BlockCode.Blocked, "Trading stopped"))
      else if !enabled then Some(ExecutionBlock(BlockCode.Blocked, "View only"))
      else if paused then
        Some(ExecutionBlock(BlockCode.Blocked, "Wallet change paused this account"))
      else if fatal || !config.configured then Some(ExecutionBlock(BlockCode.Blocked, message))
      else if !connected then Some(ExecutionBlock(BlockCode.Connection, "connection disconnected"))
      else if rules.isEmpty then Some(ExecutionBlock(BlockCode.Market, "waiting for market data"))
      else if exact.isEmpty || touch.isEmpty then
        Some(ExecutionBlock(BlockCode.Prices, "waiting for selected depth"))
      else if !touch.exists(t => t.bid > 0 && t.ask > t.bid) then
        Some(ExecutionBlock(BlockCode.Prices, "invalid selected quote"))
      else if !marketReady then Some(ExecutionBlock(BlockCode.Prices, "selected depth stale"))
      else if secret.isEmpty then Some(ExecutionBlock(BlockCode.Blocked, message))
      else if leverageReady && !requiredLeverage then
        Some(ExecutionBlock(BlockCode.Leverage, "Set XMR leverage to 5x"))
      else if !accountReady || waitingRefresh then
        Some(ExecutionBlock(BlockCode.Account, "account data stale"))
      else None
    TradingView(
      coin,
      position,
      message,
      enabled && !disabled.get() && !paused && !fatal && !busy
        .get() && !waitingRefresh && secret.nonEmpty && marketReady && accountReady,
      busy.get() || saved.nonEmpty,
      rules,
      touch.map(_.bid),
      touch.map(_.ask),
      lastPlan,
      block.map(_.text),
      outcome,
      Option.when(leverageReady)(activeLeverage),
      Option.when(leverageReady)(activeMode),
      outcomeId,
      if fresh(accountAt, config.positionAgeMs) && !waitingRefresh then
        portfolio.copy(stables = balance.filter(b => fresh(b._3, config.positionAgeMs)).flatMap {
          case ("disabled", _, _)                              => marginCash
          case ("unifiedAccount" | "portfolioMargin", cash, _) => cash
          case _                                               => None
        })
      else PortfolioView(),
      blockCode = block.fold(BlockCode.Blocked)(_.code),
      usableMargin = usableMargin.filter(_ => fresh(activeAt, config.positionAgeMs))
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
      portfolio = PortfolioView()
      marginCash = None
      accountAt = Long.MinValue
      leverageReady = false
      activeAt = Long.MinValue
      roleGeneration = None
      lastPlan = None
      metadata = metadata.filter(Exchange.validMetadata(_, coin))
      wakeAccount()
  }

  def observe(event: Event): Unit = synchronized {
    val now = monotonic()
    def quote(b: Book): Unit =
      if touch.forall(_.time <= b.time) then
        Try(
          Touch(
            BigDecimal(b.bids.head.px),
            BigDecimal(b.asks.head.px),
            b.time,
            now
          )
        ).toOption
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

  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized(
    estimate(intent)
  )

  private def reviewCurrent(review: OrderReview): Boolean =
    val now = wall()
    now >= review.createdAt && now < review.expiresAt &&
    BigInt(review.expiresAt) - BigInt(
      review.createdAt
    ) <= TradingWorker.maximumReviewLifetimeMs &&
    review.leverage.contains(activeLeverage) && review.marginMode.contains(
      activeMode
    )

  def submitReviewed(review: OrderReview): Boolean = synchronized {
    submitPlan(review.intent, review.plan.coin, Some(review))
  }

  def submit(intent: OrderIntent, expectedCoin: String): Boolean =
    synchronized {
      submitPlan(intent, expectedCoin, None)
    }

  private def submitPlan(
      intent: OrderIntent,
      expectedCoin: String,
      review: Option[OrderReview]
  ): Boolean =
    if disabled.get() || closed
        .get() || expectedCoin != coin || !view.ready || !busy.compareAndSet(
        false,
        true
      )
    then false
    else
      outcome = None
      estimate(intent) match
        case Left(reason) =>
          message = reason
          busy.set(false)
          false
        case Right(plan) =>
          val maximum = maximums(if intent.side == Side.Buy then 0 else 1)
          if review.exists(r => plan != r.plan || !reviewCurrent(r)) then
            message = "Order review changed or expired. Review again before sending"
            busy.set(false)
            false
          else if plan.quantity > maximum then
            message = "Requested quantity exceeds confirmed account capacity"
            busy.set(false)
            false
          else
            val press = Press(
              plan,
              generation,
              touch.get.at,
              exact.get.at,
              accountAt,
              activeAt,
              activeLeverage,
              activeMode,
              review
            )
            lastPlan = Some(plan)
            message = coin + " submitting"
            if requests.offer(press) then true
            else
              busy.set(false)
              false

  private def signed(
      action: ujson.Value,
      nonce: Long,
      expiry: Option[Long]
  ): ujson.Value =
    val key = synchronized(secret.get)
    val signature =
      OrderSigning.sign(key, action, nonce, config.network == "mainnet", expiry)
    val request =
      ujson.Obj(
        "action" -> action,
        "nonce" -> ujson.Num(nonce.toDouble),
        "signature" -> signature
      )
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
        outcome = Some(text)
        outcomeId = OrderUpdates.nextId()
        waitingRefresh = true
        accountVersion += 1
        accountAt = Long.MinValue
        activeAt = Long.MinValue
        activeFloor = monotonic()
        balance = None
      }
      wakeAccount()
    catch case _: Exception => storageFailure()

  private def execute(press: Press): Unit =
    executing.set(true)
    try orderPermission(() => executeOrder(press))
    catch
      case _: Exception =>
        synchronized {
          busy.set(saved.nonEmpty)
          message = "Wallet change paused this account or account state is unavailable"
        }
    finally
      executing.set(false)
      if synchronized(waitingRefresh) then wakeAccount()

  private def executeOrder(press: Press): Unit =
    val p = try
      val reserved = journal.get.reserve(press.plan, config.owner.get, wall())
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
      !disabled.get() && !closed
        .get() && !fatal && press.generation == generation && press.plan.coin == coin &&
      marketReady && accountReady && fresh(press.quoteAt, config.quoteAgeMs) &&
      fresh(press.bookAt, config.quoteAgeMs) && fresh(
        press.accountAt,
        config.positionAgeMs
      ) &&
      fresh(press.activeAt, config.positionAgeMs) &&
      press.leverage == activeLeverage && press.marginMode == activeMode &&
      press.review.forall(r => reviewCurrent(r) && estimate(r.intent).contains(r.plan)) &&
      press.plan.quantity <= maximums(if press.plan.side == Side.Buy then 0
      else 1) &&
      wall() < p.expiresAfter
    }
    if !canSend || request.isFailure then complete(p, p.coin + " canceled before sending")
    else
      val response = Try(exchange.post(request.get)).toOption.flatten
      Exchange.outcome(response, p) match
        case Some(text) => complete(p, text)
        case None       =>
          synchronized { message = p.coin + " waiting for order result" }

  private def initializeSigning(): Unit =
    if !accountPaused() && !synchronized(
        fatal || saved.nonEmpty || secret.nonEmpty
      )
    then
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

  private def receiveActive(
      g: Long,
      current: String,
      value: Option[ujson.Value],
      at: Long
  ): Unit =
    val parsed = value.flatMap(active =>
      Try {
        require(
          active("user").str.toLowerCase(
            java.util.Locale.ROOT
          ) == config.owner.get && active(
            "coin"
          ).str == current
        )
        val mode = active("leverage")("type").str
        require(mode == "cross" || mode == "isolated")
        val leverage = Exchange.integer(active("leverage")("value"))
        val limits = active("maxTradeSzs").arr.toVector.map { v =>
          if v.isInstanceOf[ujson.Str] then BigDecimal(v.str)
          else BigDecimal(v.num.toString)
        }
        require(limits.size == 2 && limits.forall(_ >= 0))
        val available = Try {
          val sides = active("availableToTrade").arr.toVector.map { v =>
            if v.isInstanceOf[ujson.Str] then BigDecimal(v.str)
            else BigDecimal(v.num.toString)
          }
          require(sides.size == 2 && sides.forall(_ >= 0))
          sides.min
        }.toOption
        (mode, leverage, limits, available)
      }.toOption
    )
    synchronized {
      if generation == g && at >= activeFloor then
        parsed match
          case Some((mode, leverage, limits, available)) =>
            activeMode = mode
            activeLeverage = leverage
            leverageReady = rules.exists(m => leverage >= 1 && leverage <= m.maxLeverage)
            maximums = limits
            usableMargin = available
            activeAt = at
          case None =>
            leverageReady = false
            activeAt = Long.MinValue
    }

  private def refresh(): Unit =
    if enabled && !config.configured then
      synchronized { message = "Configure owner and delegated signer" }
      return
    val (current, g, version) = synchronized((coin, generation, accountVersion))
    val startedAt = monotonic()
    try
      if enabled && !synchronized(fatal) && journal.isEmpty then
        try
          val opened = openJournal()
          journal = Some(opened)
          synchronized {
            saved = opened.pending
            if saved.nonEmpty then
              busy.set(true)
              message =
                if saved.get.owner.exists(owner => !config.owner.contains(owner)) then
                  TradingWorker.savedOwnerMismatchMessage
                else saved.get.coin + " waiting for order result"
          }
        catch case _: Exception => storageFailure()
      val unresolved = synchronized(
        if !fatal && !executing.get() then saved else None
      )
      unresolved.foreach { p =>
        val reply = exchange.info(
          ujson.Obj(
            "type" -> "orderStatus",
            "user" -> config.owner.get,
            "oid" -> p.id
          )
        )
        Exchange
          .recovered(
            reply,
            p,
            wall(),
            allowUnknown = p.owner.nonEmpty && p.owner == config.owner &&
              synchronized(roleGeneration.contains(g) && fresh(roleAt, 60000L))
          )
          .foreach(text => complete(p, text))
      }
      val cached = synchronized {
        metadata.filter(_ => fresh(metadataAt, 60000L)).filter(Exchange.validMetadata(_, current))
      }
      val accepted = cached.getOrElse {
        val value = exchange
          .info(ujson.Obj("type" -> "meta"))
          .filter(Exchange.validMetadata(_, current))
          .get
        synchronized {
          if generation == g then
            metadata = Some(value)
            metadataAt = monotonic()
        }
        value
      }
      val matched = Try(Exchange.market(accepted, current)).toOption
      synchronized { if generation == g then rules = matched }
      require(matched.nonEmpty)
      if config.owner.nonEmpty then
        if enabled then
          if synchronized(!roleGeneration.contains(g) || !fresh(roleAt, 60000L))
          then
            if fresh(failedRoleAt, 10000L) then return
            try
              val role =
                exchange
                  .info(
                    ujson.Obj("type" -> "userRole", "user" -> config.agent.get)
                  )
                  .get
              require(
                role("role").str == "agent" && role("data")("user").str
                  .toLowerCase(java.util.Locale.ROOT) == config.owner.get,
                "owner"
              )
            catch
              case error: Exception =>
                synchronized {
                  failedRoleAt = monotonic()
                }
                throw error
            synchronized {
              if generation == g then
                roleGeneration = Some(g)
                roleAt = startedAt
                if message == TradingWorker.ownerMismatchMessage then
                  message = saved.fold("Preparing trading")(_.coin + " waiting for order result")
            }
          initializeSigning()
        exchange.watchTimed(
          current,
          config.owner.get,
          g,
          monotonic,
          (data, at) => receiveActive(g, current, data, at)
        )
        val accountStartedAt = monotonic()
        val account =
          exchange
            .info(
              ujson
                .Obj("type" -> "clearinghouseState", "user" -> config.owner.get)
            )
            .get
        val positions = account("assetPositions").arr.toVector
          .map(_("position"))
          .filter(_("coin").str == current)
        require(positions.size <= 1)
        val quantity =
          positions.headOption
            .map(p => BigDecimal(p("szi").str))
            .getOrElse(BigDecimal(0))
        synchronized {
          if generation == g && version == accountVersion then
            position = Some(quantity)
            portfolio = PortfolioView.live(account, current, None)
            marginCash = PortfolioView.marginCash(account)
            accountAt = accountStartedAt
            if waitingRefresh && !fatal && saved.isEmpty && !executing.get()
            then
              waitingRefresh = false
              busy.set(false)
            if message == "Preparing trading" && leverageReady && secret.nonEmpty
            then message = "Ready"
        }
    catch
      case error: Exception =>
        synchronized {
          accountAt = Long.MinValue
          leverageReady = false
          if !fatal then
            if Option(error.getMessage).exists(_.endsWith(": owner")) then
              message =
                if saved.exists(p => p.owner.exists(owner => !config.owner.contains(owner))) then
                  TradingWorker.savedOwnerMismatchMessage
                else TradingWorker.ownerMismatchMessage
            else if saved.isEmpty && !busy.get() then message = "Waiting for market or account data"
        }

  // Optional display reads run separately. Their failure or delay cannot hold
  // the account refresh needed to submit and reconcile orders.
  private def refreshBalance(): Unit =
    val at = monotonic()
    val version = synchronized(accountVersion)
    val value = Try {
      if !abstraction.exists(m => fresh(m._2, 60000L)) && !fresh(
          abstractionAttemptAt,
          10000L
        )
      then
        abstractionAttemptAt = at
        abstraction = exchange
          .info(
            ujson.Obj("type" -> "userAbstraction", "user" -> config.owner.get)
          )
          .flatMap(_.strOpt)
          .map(_ -> at)
      val mode = abstraction.filter(m => fresh(m._2, 60000L)).get._1
      val cash = if mode == "unifiedAccount" || mode == "portfolioMargin" then
        exchange
          .info(
            ujson.Obj(
              "type" -> "spotClearinghouseState",
              "user" -> config.owner.get
            )
          )
          .flatMap(PortfolioView.usdcCash)
      else None
      (mode, cash, at)
    }.toOption
    synchronized { if version == accountVersion then balance = value }

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
              val _ = refreshWake.poll(1000L, TimeUnit.MILLISECONDS)
        }
      ) ++ config.owner.toVector.map { _ =>
        daemon("balance-display") {
          while !closed.get() do
            refreshBalance()
            Thread.sleep(1000L)
        }
      }
      workers.foreach(_.start())

  private[desk] def disable(): Unit = disabled.set(true)

  private[desk] def isRetired: Boolean = retired.get()

  private def finishRetirement(): Unit = synchronized {
    if !retired.get() && workers.forall(!_.isAlive) then
      secret.foreach(key => java.util.Arrays.fill(key, 0.toByte))
      secret = None
      journal.foreach(_.close())
      retired.set(true)
  }

  private[desk] def awaitRetirement(): Unit =
    require(closed.get())
    workers.foreach(_.join())
    finishRetirement()

  def close(): Unit =
    disable()
    if closed.compareAndSet(false, true) then
      workers.foreach(_.interrupt())
      exchange.close()
      workers.foreach(_.join(4000))
      finishRetirement()
