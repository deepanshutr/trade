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
    usableMargin: Option[BigDecimal] = None,
    resting: Vector[RestingOrder] = Vector.empty,
    fills: Vector[Fill] = Vector.empty,
    stale: Boolean = false,
    ignoredOrderUpdates: Long = 0L,
    notFoundOrders: Long = 0L
):
  def reference(side: Side): Option[BigDecimal] =
    if side == Side.Buy then ask else bid
  // The diagnostics counters belong to the worker, not the market, so a market change keeps them.
  def forMarket(current: String): TradingView =
    if coin == current then this
    else
      TradingView(
        current,
        message = "changing market",
        pending = true,
        ignoredOrderUpdates = ignoredOrderUpdates,
        notFoundOrders = notFoundOrders
      )

object TradingWorker:
  val ownerMismatchMessage: String = "Signer does not match the configured owner"
  val savedOwnerMismatchMessage: String = "Saved order belongs to another owner"
  // The workspace shows this sentence with the key bound to settings, s by default.
  def setupFor(key: String): String = s"Live setup required. Press $key for settings"
  val setupMessage: String = setupFor("s")
  val maximumReviewLifetimeMs: Long = 30000L
  val openingPauseMs: Long = 2000L
  val openingPausedMessage: String = "Opening paused for 2 s after closing"
  val cancelPendingMessage: String = "Wait for the pending order"
  val closeNextMessage: String = "Close goes after this cancel"
  val waitingDepthMessage: String = "Waiting for fresh depth"
  // Two fatal texts. Each fits the 38 column panel, where a ticket shows it on its size row.
  val storageFailedMessage: String = "Order state failed. Restart to recover"
  val signingFailedMessage: String = "Signing not prepared. Restart to retry"
  // Order updates waiting for the account thread, fills kept per market and the finished
  // client ids whose late updates are dropped without a count.
  private[desk] val updateLimit: Int = 256
  private[desk] val fillLimit: Int = 200
  private[desk] val finishedLimit: Int = 200

  // The loader hands this worker its own copy of the key. The worker wipes it on close.
  def create(
      config: TradingConfig,
      enabled: Boolean,
      coin: String,
      journalDirectory: Path,
      loadKey: () => Array[Byte]
  ): TradingWorker =
    withExchange(
      config,
      enabled,
      coin,
      journalDirectory,
      loadKey,
      new JdkExchange(config.baseUrl)
    )

  private[desk] def withExchange(
      config: TradingConfig,
      enabled: Boolean,
      coin: String,
      journalDirectory: Path,
      loadKey: () => Array[Byte],
      exchange: Exchange
  ): TradingWorker =
    new TradingWorker(
      config,
      enabled,
      coin,
      exchange,
      loadKey,
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
      review: OrderReview
  )
  private enum Request:
    case Order(press: Press)
    case Cancel(ids: Vector[String])
  // An order update with the sign of its market's position from the last account read, or
  // none when that position was unknown.
  private final case class Queued(update: VenueOrders.OrderUpdate, sign: Option[Int])
  // A resting order waiting for a status lookup: the last attempt, the unknownOid answers in a
  // row after the margin, and whether the last open orders read left it out.
  private final case class Check(
      attemptAt: Long = Long.MinValue,
      unknowns: Int = 0,
      missing: Boolean = false
  )
  private enum Route:
    case Hold, Drop, Ignore
    case Lost(order: SavedOrder)
    case Apply(order: SavedOrder)
  private enum Gone:
    case Venue(status: String)
    case NotFound
  private val busy = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val disabled = new AtomicBoolean(false)
  private val retired = new AtomicBoolean(false)
  private val refreshWake = new ArrayBlockingQueue[Boolean](1)
  private val executing = new AtomicBoolean(false)
  private val requests = new ArrayBlockingQueue[Request](1)
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
  // Monotonic time at which this process first saw the saved order.
  private var savedSeenAt = 0L
  private var errOrder = Option.empty[String]
  // Monotonic time at which the last reduce-only order completed.
  private var reducedAt = Option.empty[Long]
  private var journal = Option.empty[OrderJournal]
  private var secret = Option.empty[Array[Byte]]
  private var queued = Vector.empty[Queued]
  private var held = Vector.empty[Queued]
  // The journal's resting orders, read again after every change to them.
  private var restingOrders = Vector.empty[SavedOrder]
  private var cancelling = Option.empty[String]
  // A row cancel runs from its request to the end of its loop. A CLOSE pressed meanwhile stops
  // the loop before its next order, so CLOSE goes next.
  private var cancelRunning = false
  private var closeWanted = false
  private var fills = Vector.empty[Fill]
  private var finished = Vector.empty[String]
  private var ignoredUpdates = 0L
  private var notFound = 0L
  private var checks = Map.empty[String, Check]
  private var ordersDue = false
  private var ordersAt = Long.MinValue
  private var ordersFailedAt = Long.MinValue
  private val journalLock = new Object

  private def fresh(at: Long, age: Long): Boolean =
    val now = monotonic()
    at != Long.MinValue && now >= at && now - at <= age

  private def marketReady: Boolean =
    connected && rules.nonEmpty && exact.exists(e => fresh(e.at, config.quoteAgeMs)) &&
      touch.exists(t => t.bid > 0 && t.ask > t.bid && fresh(t.at, config.quoteAgeMs)) &&
      exact.exists(e =>
        touch.exists(t => (BigInt(t.time) - BigInt(e.book.time)).abs <= config.quoteAgeMs)
      )

  // Amounts are sized at five times, so live needs the venue to hold every market at 5x.
  private def requiredLeverage: Boolean = activeLeverage == 5

  private def wakeAccount(): Unit =
    val _ = refreshWake.offer(true)

  private def waited(at: Long, ms: Long): Boolean =
    at == Long.MinValue || monotonic() - at >= ms

  private def ownOrder(r: SavedOrder): Boolean = r.owner.nonEmpty && r.owner == config.owner

  // A resting order of another owner can be neither reconciled nor cancelled with this key.
  private def foreign: Boolean = restingOrders.exists(r => !ownOrder(r))

  private def left(r: SavedOrder): BigDecimal = BigDecimal(r.remaining.getOrElse(r.quantity))

  private def restingView(r: SavedOrder): RestingOrder = RestingOrder(
    r.id,
    r.coin,
    r.asset,
    r.side,
    BigDecimal(r.limit),
    left(r),
    BigDecimal(r.quantity),
    r.reduceOnly,
    r.createdAt,
    cancelling.contains(r.id)
  )

  // Money put up at the active leverage, the unknown mark while it is unknown. Callers hold
  // the lock.
  private def dollars(quantity: BigDecimal, price: BigDecimal): String =
    Trading.dollars(Trading.money(quantity, price, Option.when(leverageReady)(activeLeverage)), 0)

  private def priceText(price: BigDecimal): String = Trading.display(price, 8)

  // The next three run under the lock.
  private def announce(text: String): Unit =
    outcome = Some(text)
    outcomeId = OrderUpdates.nextId()

  private def remember(id: String): Unit =
    finished = (finished.filterNot(_ == id) :+ id).takeRight(TradingWorker.finishedLimit)

  private def addFill(fill: Fill): Unit =
    val added = fills :+ fill
    fills =
      if added.count(_.coin == fill.coin) <= TradingWorker.fillLimit then added
      else added.patch(added.indexWhere(_.coin == fill.coin), Nil, 1)

  // What a completed order does to the account state. A read that started before it cannot
  // restore the account, and active data must arrive again after it. The order state, busy
  // and waitingRefresh, is not touched here.
  private def accountChanged(): Unit =
    accountVersion += 1
    accountAt = Long.MinValue
    activeAt = Long.MinValue
    activeFloor = monotonic()
    balance = None

  private def mirror(): Unit = restingOrders = journal.fold(Vector.empty[SavedOrder])(_.resting)

  // A fill reduced the position when its side faced the stamped sign. With no stamp it counts
  // as a reduce, so the opening pause errs on the safe side.
  private def facing(sign: Option[Int], side: Side): Boolean =
    sign.forall(s => (s > 0 && side == Side.Sell) || (s < 0 && side == Side.Buy))

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
      else if foreign then
        Some(ExecutionBlock(BlockCode.Blocked, TradingWorker.savedOwnerMismatchMessage))
      else if !connected then Some(ExecutionBlock(BlockCode.Connection, "connection disconnected"))
      else if rules.isEmpty then Some(ExecutionBlock(BlockCode.Market, "waiting for market data"))
      else if exact.isEmpty || touch.isEmpty then
        Some(ExecutionBlock(BlockCode.Prices, "waiting for selected depth"))
      else if !touch.exists(t => t.bid > 0 && t.ask > t.bid) then
        Some(ExecutionBlock(BlockCode.Prices, "invalid selected quote"))
      else if !marketReady then Some(ExecutionBlock(BlockCode.Prices, "selected depth stale"))
      else if secret.isEmpty then Some(ExecutionBlock(BlockCode.Blocked, message))
      else if leverageReady && !requiredLeverage then
        Some(ExecutionBlock(BlockCode.Leverage, s"Set $coin leverage to 5x"))
      else if !accountReady || waitingRefresh then
        Some(ExecutionBlock(BlockCode.Account, "account data stale"))
      else None
    TradingView(
      coin,
      position,
      message,
      enabled && !disabled.get() && !paused && !fatal && !foreign && !busy
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
      usableMargin = usableMargin.filter(_ => fresh(activeAt, config.positionAgeMs)),
      resting = restingOrders.map(restingView),
      fills = fills,
      stale = !fresh(activeAt, config.positionAgeMs),
      ignoredOrderUpdates = ignoredUpdates,
      notFoundOrders = notFound
    )
  }

  // One request cancels the given resting orders in turn. It needs the key and a running
  // account, not the current market or fresh depth, so the owner can always reach the orders.
  def cancel(ids: Vector[String]): Boolean = synchronized {
    val known = ids.distinct.filter(id => restingOrders.exists(_.id == id))
    val savedForeign = saved.exists(_.owner.exists(owner => !config.owner.contains(owner)))
    if !enabled || disabled.get() || closed.get() || fatal || secret.isEmpty || foreign ||
      savedForeign || accountPaused()
    then false
    else if busy.get() || saved.nonEmpty then
      message = TradingWorker.cancelPendingMessage
      false
    else if known.isEmpty || !busy.compareAndSet(false, true) then false
    else
      outcome = None
      if requests.offer(Request.Cancel(known)) then
        cancelRunning = true
        closeWanted = false
        true
      else
        busy.set(false)
        false
  }

  // True when a row cancel runs. It then stops before its next order, so CLOSE goes next.
  def closeFirst(): Boolean = synchronized {
    if cancelRunning then
      closeWanted = true
      message = TradingWorker.closeNextMessage
    cancelRunning
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

  // An unknown position refuses every plan. Planning it as flat could turn a reduce or a
  // Close into an opening order.
  // For a short time after a reduce-only order completes, only reduce-only plans pass. So a
  // press repeated on the reducing side cannot run through flat into the other side.
  private def estimate(intent: OrderIntent, terms: TradeTerms): Either[String, OrderPlan] =
    if !marketReady then Left(TradingWorker.waitingDepthMessage)
    else if intent.price.nonEmpty && restingOrders.size >= Trading.restingCap then
      Left(Trading.restingCapText)
    else
      position
        .toRight("Position unknown")
        .flatMap(held =>
          Trading.plan(
            coin,
            rules.get,
            exact.get.book,
            if intent.side == Side.Buy then touch.get.ask else touch.get.bid,
            held,
            intent,
            terms.sizes,
            activeLeverage,
            terms.slippage,
            Trading.reserved(held, restingOrders.filter(_.coin == coin).map(restingView))
          )
        )
        .filterOrElse(
          plan =>
            plan.reduceOnly ||
              reducedAt.forall(at => monotonic() - at >= TradingWorker.openingPauseMs),
          TradingWorker.openingPausedMessage
        )

  def preview(intent: OrderIntent, terms: TradeTerms): Either[String, OrderPlan] = synchronized(
    estimate(intent, terms)
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

  // The worker plans again with the sizes and slippage the review carries, so a change in
  // settings after the review cannot alter what is sent.
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if disabled.get() || closed
        .get() || review.plan.coin != coin || !view.ready || !busy.compareAndSet(
        false,
        true
      )
    then false
    else
      outcome = None
      estimate(review.intent, review.terms) match
        case Left(reason) =>
          message = reason
          busy.set(false)
          false
        case Right(plan) =>
          val maximum = maximums(if review.intent.side == Side.Buy then 0 else 1)
          if plan != review.plan || !reviewCurrent(review) then
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
              review
            )
            lastPlan = Some(plan)
            message = coin + " submitting"
            if requests.offer(Request.Order(press)) then true
            else
              busy.set(false)
              false
  }

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
    message = TradingWorker.storageFailedMessage
  }

  // Ends the order in flight. With an oid it moves into the resting list instead. The pause
  // follows a reduce-only order unless the caller decides it.
  private def complete(
      p: SavedOrder,
      text: String,
      rest: Option[Long] = None,
      fill: Option[Fill] = None,
      pause: Option[Boolean] = None
  ): Unit =
    try
      rest match
        case Some(oid) => val _ = journal.get.rest(p.id, oid)
        case None      => journal.get.finish(p.id)
      synchronized {
        saved = None
        message = text
        announce(text)
        waitingRefresh = true
        if pause.getOrElse(p.reduceOnly) then reducedAt = Some(monotonic())
        fill.foreach(addFill)
        if rest.isEmpty then remember(p.id)
        mirror()
        accountChanged()
      }
      wakeAccount()
    catch case _: Exception => storageFailure()

  // Changes to resting orders run one at a time and check the order is still there, because
  // the account thread and the orders thread may both reach the same order.
  private def journalChange[A](change: OrderJournal => Option[A]): Option[A] =
    try journalLock.synchronized(journal.flatMap(change))
    catch
      case _: Exception =>
        storageFailure()
        None

  // Removes a resting order the venue no longer holds and says why.
  private def settle(order: SavedOrder, gone: Gone, at: Option[Long], reduced: Boolean): Unit =
    journalChange { j =>
      j.resting.find(_.id == order.id).map { r =>
        j.settle(r.id)
        r
      }
    }.foreach { r =>
      synchronized {
        val limit = BigDecimal(r.limit)
        val money = dollars(left(r), limit)
        val price = priceText(limit)
        val side = if r.side == Side.Buy then "buy" else "sell"
        announce(gone match
          case Gone.Venue("filled")            => s"${r.coin} filled $money at $price"
          case Gone.Venue("canceled")          => s"${r.coin} canceled $money at $price"
          case Gone.Venue("selfTradeCanceled") =>
            s"${r.coin} resting $side $money at $price canceled by self-trade prevention"
          case Gone.Venue(status) => s"${r.coin} canceled by the venue $money at $price ($status)"
          case Gone.NotFound      => s"${r.coin} $money at $price not found at the venue")
        if gone == Gone.Venue("filled") then
          at.foreach(time => addFill(Fill(r.coin, r.side, time, limit, left(r))))
          if reduced then reducedAt = Some(monotonic())
        if gone == Gone.NotFound then notFound += 1
        remember(r.id)
        checks -= r.id
        mirror()
        accountChanged()
      }
      wakeAccount()
    }

  // Lowers a resting order's remaining size, never raises it, and names the part that filled.
  // A part fill that reduced starts the opening pause, as a whole fill does.
  private def shrink(
      order: SavedOrder,
      size: BigDecimal,
      at: Option[Long],
      reduced: Boolean
  ): Unit =
    journalChange { j =>
      j.resting.find(_.id == order.id).filter(r => size > 0 && size < left(r)).map { r =>
        j.remaining(r.id, Trading.format(size))
        r
      }
    }.foreach { r =>
      synchronized {
        val limit = BigDecimal(r.limit)
        val part = left(r) - size
        announce(
          s"${r.coin} part filled ${dollars(part, limit)} of " +
            s"${dollars(BigDecimal(r.quantity), limit)} at ${priceText(limit)}"
        )
        at.foreach(time => addFill(Fill(r.coin, r.side, time, limit, part)))
        if reduced then reducedAt = Some(monotonic())
        mirror()
        accountChanged()
      }
      wakeAccount()
    }

  // The socket thread only queues. The account thread applies.
  private def receiveOrders(event: OrderEvent): Unit =
    synchronized {
      event match
        case OrderEvent.Opened(_)          => ordersDue = true
        case OrderEvent.Updates(values, _) =>
          values.foreach { value =>
            VenueOrders.orderUpdate(value) match
              case Some(update) =>
                if queued.size < TradingWorker.updateLimit then
                  val sign = if update.coin == coin then position.map(_.signum) else None
                  queued :+= Queued(update, sign)
                // A full queue drops the update. The open orders read catches it up.
                else ordersDue = true
              case None => ignoredUpdates += 1
          }
    }
    wakeAccount()

  // Runs at the top of every refresh, before any network call, so an info outage cannot hold
  // fills back. Updates for the order in flight wait until it rests or finishes. A fill for it
  // is also its outcome when the reply was lost, but never while the orders thread may still
  // be inside the post.
  private def drain(): Unit =
    val batch = synchronized {
      if fatal || journal.isEmpty then Vector.empty
      else
        val all = held ++ queued
        held = Vector.empty
        queued = Vector.empty
        all
    }
    batch.foreach { item =>
      val u = item.update
      val route = synchronized {
        saved.filter(_.id == u.cloid) match
          case Some(p) =>
            if u.status == "filled" && u.coin == p.coin && !fatal && !executing.get() then
              Route.Lost(p)
            else Route.Hold
          case None =>
            restingOrders.find(_.id == u.cloid) match
              case Some(r) if r.coin == u.coin && r.oid.contains(u.oid) => Route.Apply(r)
              case Some(_)                                              => Route.Ignore
              case None => if finished.contains(u.cloid) then Route.Drop else Route.Ignore
      }
      route match
        case Route.Hold =>
          synchronized {
            if held.size < TradingWorker.updateLimit then held :+= item else ordersDue = true
          }
        case Route.Lost(p) =>
          val quantity = BigDecimal(p.quantity)
          val limit = BigDecimal(p.limit)
          val text =
            synchronized(s"${p.coin} filled ${dollars(quantity, limit)} at ${priceText(limit)}")
          complete(
            p,
            text,
            fill = Some(Fill(p.coin, p.side, u.at, limit, quantity)),
            pause = Some(facing(item.sign, p.side))
          )
        case Route.Apply(r) =>
          // Assumption A1: an open update's sz is the size still resting, and a partial fill
          // sends one with a smaller sz. The venue docs list the field without saying so. If it
          // were not, only the shown remaining size would be wrong, because shrink never raises
          // a size and sends nothing. The owner checks it on the first live partial fill.
          if u.status == "open" then shrink(r, u.remaining, Some(u.at), facing(item.sign, r.side))
          else if VenueOrders.terminal.contains(u.status) then
            settle(r, Gone.Venue(u.status), Some(u.at), facing(item.sign, r.side))
        case Route.Drop   => ()
        case Route.Ignore => synchronized { ignoredUpdates += 1 }
    }

  private def lookup(order: SavedOrder): Option[ujson.Value] =
    exchange.info(
      ujson.Obj("type" -> "orderStatus", "user" -> config.owner.get, "oid" -> order.id)
    )

  // Applies a status lookup of a resting order. Anything but a clear answer keeps the order.
  // A lookup carries no stamped position, so a part fill it finds counts as a reduce.
  // An unknownOid answer counts toward the bound only for the spaced reconciliation lookups,
  // while the open orders read missed the order, after its creation plus the margin, with the
  // signer role proven.
  private def looked(
      r: SavedOrder,
      reply: Option[ujson.Value],
      lookupAt: Long,
      counting: Boolean
  ): Unit =
    VenueOrders.lookup(reply) match
      case Some(VenueOrders.Lookup.Open(_, size)) if Exchange.sameOrder(reply, r) =>
        synchronized { checks -= r.id }
        shrink(r, size, None, facing(None, r.side))
      case Some(VenueOrders.Lookup.Settled(status, _)) if Exchange.sameOrder(reply, r) =>
        settle(r, Gone.Venue(status), None, reduced = true)
      case Some(VenueOrders.Lookup.Unknown) if counting =>
        val gone = synchronized {
          val proven = roleGeneration.contains(generation) && fresh(roleAt, 60000L)
          checks
            .get(r.id)
            .filter(c =>
              c.missing && proven && BigInt(lookupAt) > BigInt(
                r.createdAt
              ) + Exchange.ExpiryMarginMs
            ) match
            case Some(c) =>
              checks = checks.updated(r.id, c.copy(unknowns = c.unknowns + 1))
              c.unknowns + 1 >= 3
            case None => false
        }
        if gone then settle(r, Gone.NotFound, None, reduced = false)
      case _ => ()

  // The safety net for missed updates: an open orders read after every socket open and once a
  // minute while orders rest, then a status lookup for each order missing from it, each no
  // sooner than ten seconds after its last try.
  private def reconcile(): Unit =
    val now = monotonic()
    val own = synchronized {
      val mine = restingOrders.filter(ownOrder)
      checks = checks.filter((id, _) => mine.exists(_.id == id))
      // With nothing resting there is nothing to read. The minute starts with the first order.
      if mine.isEmpty then
        ordersDue = false
        ordersAt = Long.MinValue
      else if ordersAt == Long.MinValue then ordersAt = now
      mine
    }
    val read = own.nonEmpty && synchronized(
      (ordersDue || !fresh(ordersAt, 60000L)) && waited(ordersFailedAt, 10000L)
    )
    if read then
      val reply = exchange.info(ujson.Obj("type" -> "openOrders", "user" -> config.owner.get))
      Exchange.openOrders(reply) match
        case None         => synchronized { ordersFailedAt = now }
        case Some(listed) =>
          synchronized {
            ordersDue = false
            ordersAt = now
          }
          own.foreach { r =>
            listed.find(o => r.oid.contains(o.oid) && o.coin == r.coin) match
              case Some(o) =>
                synchronized { checks -= r.id }
                shrink(r, o.size, None, facing(None, r.side))
              case None =>
                synchronized {
                  checks =
                    checks.updated(r.id, checks.getOrElse(r.id, Check()).copy(missing = true))
                }
          }
    val due = synchronized(
      own.filter(r =>
        restingOrders
          .exists(_.id == r.id) && checks.get(r.id).exists(c => waited(c.attemptAt, 10000L))
      )
    )
    due.foreach { r =>
      val lookupAt = wall()
      synchronized {
        checks.get(r.id).foreach(c => checks = checks.updated(r.id, c.copy(attemptAt = now)))
      }
      looked(r, lookup(r), lookupAt, counting = true)
    }

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
    synchronized {
      saved = Some(p)
      savedSeenAt = monotonic()
    }
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
      reviewCurrent(press.review) &&
      estimate(press.review.intent, press.review.terms).contains(press.review.plan) &&
      press.plan.quantity <= maximums(if press.plan.side == Side.Buy then 0
      else 1) &&
      wall() < p.expiresAfter
    }
    if !canSend || request.isFailure then complete(p, p.coin + " canceled before sending")
    else
      val response = Try(exchange.post(request.get)).toOption.flatten
      if Exchange.isErrReply(response) then
        synchronized {
          if saved.contains(p) then errOrder = Some(p.id)
        }
      Exchange.placed(response, p) match
        case Some(VenueOrders.Placed.Resting(oid)) =>
          val limit = BigDecimal(p.limit)
          val text = synchronized(
            s"${p.coin} resting ${dollars(BigDecimal(p.quantity), limit)} at ${priceText(limit)}"
          )
          complete(p, text, rest = Some(oid), pause = Some(false))
        case Some(VenueOrders.Placed.Filled(quantity, price, _)) =>
          val text = synchronized(
            s"${p.coin} filled ${dollars(quantity, price)} at ${priceText(price)}"
          )
          complete(p, text, fill = Some(Fill(p.coin, p.side, wall(), price, quantity)))
        case Some(VenueOrders.Placed.Rejected(_)) =>
          complete(
            p,
            if press.plan.kind == OrderKind.Close then
              p.coin + " order rejected. Resting reduce orders may block it, press o"
            else p.coin + " order rejected"
          )
        case None =>
          synchronized { message = p.coin + " waiting for order result" }

  // Cancels run on the orders thread, one signed action per order, each under the account gate.
  // The loop stops at the first cancel whose reply is not a clear success, and before its next
  // order once CLOSE is pressed. The rest wait for the next Enter.
  private def executeCancel(ids: Vector[String]): Unit =
    executing.set(true)
    try
      var stopped = false
      ids.zipWithIndex.foreach { (id, index) =>
        // A cancel names only an order the journal holds, never an id from outside it.
        val order =
          if stopped || synchronized(closeWanted || disabled.get() || closed.get() || fatal) then
            None
          else journal.flatMap(_.resting.find(_.id == id))
        order.foreach { r =>
          synchronized {
            message = s"${r.coin} canceling ${index + 1} of ${ids.size}"
            cancelling = Some(r.id)
          }
          try orderPermission(() => if !cancelOne(r) then stopped = true)
          catch
            case _: Exception =>
              stopped = true
              synchronized {
                message = "Wallet change paused this account or account state is unavailable"
              }
          finally synchronized { cancelling = None }
        }
      }
    finally
      executing.set(false)
      synchronized {
        cancelRunning = false
        closeWanted = false
        if !fatal then busy.set(false)
      }

  // True only when the venue answered the cancel with a clear success.
  private def cancelOne(r: SavedOrder): Boolean =
    journalChange(j => Some(j.nextNonce(wall()))).exists { nonce =>
      val request = Try(
        signed(VenueOrders.cancelAction(r.asset, r.id), nonce, Some(Math.addExact(wall(), 5000L)))
      ).toOption
      val reply = request.flatMap(body => Try(exchange.post(body)).toOption.flatten)
      val success = request.nonEmpty && VenueOrders.cancelOutcome(reply).contains(Right(()))
      if request.isEmpty then synchronized { announce(r.coin + " cancel not sent") }
      else if success then settle(r, Gone.Venue("canceled"), None, reduced = false)
      else
        // The venue documents no error text for a cancel by cloid, so any error or unclear
        // reply is settled only from one status lookup. An order still open was not canceled,
        // and an order the lookup cannot place stays for the next reconciliation.
        val status = lookup(r)
        looked(r, status, wall(), counting = false)
        val open = VenueOrders.lookup(status) match
          case Some(VenueOrders.Lookup.Open(_, _)) => Exchange.sameOrder(status, r)
          case _                                   => false
        synchronized {
          if restingOrders.exists(_.id == r.id) then
            announce(r.coin + (if open then " cancel rejected" else " cancel result unknown"))
        }
      success
    }

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
            message = SessionKey.unavailable
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
      synchronized { message = TradingWorker.setupMessage }
      return
    try
      if enabled && !synchronized(fatal) && journal.isEmpty then
        try
          val opened = openJournal()
          journal = Some(opened)
          synchronized {
            saved = opened.pending
            savedSeenAt = monotonic()
            restingOrders = opened.resting
            // Each resting order is looked up once at start.
            checks = restingOrders.filter(ownOrder).map(_.id -> Check()).toMap
            if saved.nonEmpty then
              busy.set(true)
              message =
                if saved.get.owner.exists(owner => !config.owner.contains(owner)) then
                  TradingWorker.savedOwnerMismatchMessage
                else saved.get.coin + " waiting for order result"
            else if foreign then message = TradingWorker.savedOwnerMismatchMessage
          }
        catch case _: Exception => storageFailure()
      drain()
      val (current, g, version) = synchronized((coin, generation, accountVersion))
      val startedAt = monotonic()
      val unresolved = synchronized(
        if !fatal && !executing.get() then saved.map(p => (p, errOrder.contains(p.id), savedSeenAt))
        else None
      )
      unresolved.foreach { case (p, gotErrReply, seenAt) =>
        // Time the lookup before sending it, so a late reply cannot count as past expiry.
        val lookupAt = wall()
        val elapsed = monotonic() - seenAt
        val reply = exchange.info(
          ujson.Obj(
            "type" -> "orderStatus",
            "user" -> config.owner.get,
            "oid" -> p.id
          )
        )
        VenueOrders.lookup(reply) match
          case Some(VenueOrders.Lookup.Unknown) =>
            Exchange
              .released(
                p,
                lookupAt,
                elapsed,
                allowUnknown = p.owner.nonEmpty && p.owner == config.owner &&
                  synchronized(roleGeneration.contains(g) && fresh(roleAt, 60000L)),
                errReply = gotErrReply
              )
              .foreach(text => complete(p, text))
          case Some(VenueOrders.Lookup.Settled(status, _)) if Exchange.sameOrder(reply, p) =>
            complete(p, s"${p.coin} recovered $status")
          // The order is on the book. It moves into the resting list with the venue's size.
          case Some(VenueOrders.Lookup.Open(oid, size)) if Exchange.sameOrder(reply, p) =>
            val limit = BigDecimal(p.limit)
            val text = synchronized(
              s"${p.coin} resting ${dollars(BigDecimal(p.quantity), limit)} at ${priceText(limit)}"
            )
            complete(p, text, rest = Some(oid), pause = Some(false))
            synchronized(restingOrders.find(_.id == p.id))
              .foreach(r => shrink(r, size, None, facing(None, r.side)))
          case _ => ()
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
        if enabled && !synchronized(fatal) then
          try reconcile()
          catch case _: Exception => ()
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
      if enabled then exchange.watchOrders(receiveOrders)
      workers = Vector(
        daemon("orders") {
          while !closed.get() do
            requests.take() match
              case Request.Order(press) => execute(press)
              case Request.Cancel(ids)  => executeCancel(ids)
        },
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
                    message = TradingWorker.signingFailedMessage
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
