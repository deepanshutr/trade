package desk

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try
import scala.util.control.NonFatal

object PropWorker:
  // Prop's preview answers with these while its depth is not fit to plan from.
  val staleMessage: String = "Prop prices stale. Waiting for depth"
  val behindMessage: String = "Prop depth behind quote. Waiting"
  val clockFailedMessage: String = "Prop clock failed. Restart to recover"
  private val hourMs = 3600000L

final class PropWorker(
    config: TradingConfig,
    initialCoin: String,
    ledger: PropLedger,
    rest: Rest,
    wall: () => Long,
    monotonic: () => Long,
    initialMetadata: Option[(ujson.Value, Long)] = None
) extends AutoCloseable:
  private final case class Prepared(
      intent: OrderIntent,
      plan: OrderPlan,
      terms: TradeTerms,
      rules: MarketRules,
      accountCount: Long,
      at: Long
  )
  private final case class Touch(bid: Level, ask: Level, time: Long, receivedAt: Long):
    val bidPrice = BigDecimal(bid.px)
    val askPrice = BigDecimal(ask.px)
    def quote(side: Side): BigDecimal = if side == Side.Buy then askPrice else bidPrice
    val bidSize = BigDecimal(bid.sz)
    val askSize = BigDecimal(ask.sz)
    val midpoint = BigDecimal(
      bidPrice.bigDecimal.add(askPrice.bigDecimal).divide(java.math.BigDecimal.valueOf(2L))
    )

  // A quote can reduce known capacity, but cannot add depth or renew its age.
  private def capBook(previous: Book, quote: Touch): Option[Book] = Try {
    def cap(levels: Vector[Level], latest: Level): Vector[Level] = levels.map { level =>
      if BigDecimal(level.px) == BigDecimal(latest.px) then
        latest.copy(sz = Trading.format(BigDecimal(level.sz).min(BigDecimal(latest.sz))))
      else level
    }
    previous.copy(bids = cap(previous.bids, quote.bid), asks = cap(previous.asks, quote.ask))
  }.toOption

  private var coin = initialCoin
  private var connected = false
  private var backup = false
  private var book = Option.empty[(Book, Long)]
  private var touch = Option.empty[Touch]
  private def validMetadata(value: ujson.Value): Boolean = Exchange.validMetadata(value, coin)
  private var metadata = initialMetadata.map(_._1).filter(validMetadata)
  private var metadataAt = initialMetadata.map(_._2).getOrElse(Long.MinValue)
  private var rules = metadata.flatMap(m => Try(Exchange.market(m, coin)).toOption)
  private var outcome = Option.empty[String]
  private var outcomeId = 0L
  private var outcomes = Vector.empty[(Long, String)]
  private var pendingAt = 0L
  private var pendingCoin = initialCoin
  private var stage = "queued"
  private var lastPlan = Option.empty[OrderPlan]
  private var prepared = Option.empty[Prepared]
  private var reducedAt = Option.empty[Long]
  private var fills = Map.empty[String, Vector[Fill]]
  private var failures = 0L
  // The selected market's latest context with its monotonic and wall clock receipt times, and
  // the last hour funding settled, charged or skipped. Hours while Desk was closed are never
  // charged.
  private var context = Option.empty[(Ctx, Long, Long)]
  private var settledHour = Math.floorDiv(wall(), PropWorker.hourMs)
  // The (time, id) pairs of the trades already counted, kept for the tape window.
  private val seenTrades = scala.collection.mutable.TreeSet.empty[(Long, Long)]
  private val pending = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val orders = new ArrayBlockingQueue[OrderReview](1)
  private var threads = Vector.empty[Thread]
  private def fresh(at: Long): Boolean = monotonic() >= at && monotonic() - at <= config.quoteAgeMs
  private def metadataFresh(age: Long): Boolean =
    val elapsed = BigInt(monotonic()) - metadataAt
    metadata.nonEmpty && elapsed >= 0 && elapsed <= age
  private def blockReason: Option[ExecutionBlock] =
    if closed.get() then Some(ExecutionBlock(BlockCode.Blocked, "Prop closed. Restart Desk"))
    else if !ledger.available then Some(ExecutionBlock(BlockCode.Storage, PropLedger.unavailable))
    else if !connected && !backup then
      Some(ExecutionBlock(BlockCode.Connection, "Reconnecting. Waiting for fresh prices"))
    else if rules.isEmpty || !metadataFresh(60000L) then
      Some(ExecutionBlock(BlockCode.Market, "Prop waiting for market details"))
    else if book.isEmpty || touch.isEmpty then
      Some(ExecutionBlock(BlockCode.Prices, "Prop waiting for selected-market depth"))
    else if !touch.exists(t =>
        t.bidPrice > 0 && t.askPrice > t.bidPrice && t.bidSize > 0 && t.askSize > 0
      )
    then Some(ExecutionBlock(BlockCode.Prices, "Prop quote invalid. Waiting for prices"))
    else if !book.exists(b =>
        fresh(b._2) && (!backup || (BigInt(wall()) - b._1.time).abs <= config.quoteAgeMs)
      ) || !touch.exists(t => fresh(t.receivedAt))
    then Some(ExecutionBlock(BlockCode.Prices, PropWorker.staleMessage))
    else if !book.exists(b =>
        touch.exists(t => (BigInt(b._1.time) - BigInt(t.time)).abs <= config.quoteAgeMs)
      )
    then Some(ExecutionBlock(BlockCode.Prices, PropWorker.behindMessage))
    else None
  private def block: Option[String] = blockReason.map(_.text)
  private def ready: Boolean = block.isEmpty
  private def held(account: PropAccount): BigDecimal =
    account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0))
  def account: PropAccount = ledger.account
  private def resting(account: PropAccount): Vector[RestingOrder] = account.resting.map { order =>
    val asset =
      metadata.flatMap(m => Try(Exchange.market(m, order.coin)).toOption).fold(-1)(_.asset)
    RestingOrder(
      order.id,
      order.coin,
      asset,
      order.side,
      order.price,
      order.quantity,
      order.quantity,
      order.reduceOnly,
      order.createdAt
    )
  }
  // The price keeps its tick decimals, so the texts read it as the ticket showed it.
  private def order(plan: OrderPlan, id: String, at: Long): PropOrder = PropOrder(
    id,
    plan.coin,
    plan.side,
    BigDecimal(Trading.priceText(plan.limit, rules.get)),
    plan.quantity,
    plan.reduceOnly,
    at
  )
  // The same list of outcomes as live keeps, so none is lost between two frames.
  private def announce(text: String): Unit =
    outcome = Some(text)
    outcomeId = OrderUpdates.nextId()
    outcomes = (outcomes :+ (outcomeId -> text)).takeRight(TradingWorker.outcomeLimit)
  private def record(fill: Fill): Unit =
    fills = fills.updated(
      fill.coin,
      (fills.getOrElse(fill.coin, Vector.empty) :+ fill).takeRight(200)
    )
  def view: TradingView = synchronized {
    val account = ledger.account
    val reason = blockReason
    val dataReady = reason.isEmpty
    val working = pending.get()
    val elapsed = ((BigInt(monotonic()) - pendingAt).max(BigInt(0)) / 1000)
    val message = if working then s"PROP $stage ${elapsed}s | $pendingCoin"
    else if dataReady then (if backup then "Prop ready: backup prices" else "Prop ready")
    else reason.get.text
    // The mark is the touch's midpoint while prop is ready. Free collateral values the position
    // there, so it is known while nothing is held or the selected market's position has a mark.
    val mark = Option.when(dataReady)(touch.map(_.midpoint)).flatten
    // Like live's, the figure shown never reads below zero. The ledger's checks keep the sign.
    val free =
      if !ledger.available then None
      else if account.positions.isEmpty then Some(PropMargin.free(account, BigDecimal(0)))
      else if account.positions.keySet == Set(coin) then mark.map(PropMargin.free(account, _))
      else None
    val usable = free.map(_.max(BigDecimal(0)))
    TradingView(
      coin,
      Some(held(account)),
      message,
      dataReady && !working,
      working,
      rules,
      touch.map(_.bidPrice),
      touch.map(_.askPrice),
      lastPlan,
      reason.map(_.text),
      outcome,
      outcomeId = outcomeId,
      portfolio = if ledger.available then
        PortfolioView.prop(account, coin, mark, rules.map(PropMargin.maintenance))
      else PortfolioView(),
      blockCode = reason.fold(BlockCode.Blocked)(_.code),
      usableMargin = usable,
      leverage = Some(PropMargin.leverage),
      marginMode = Some(PropMargin.mode),
      resting = resting(account),
      fills = fills.valuesIterator.flatten.toVector,
      stale = !touch.exists(t => fresh(t.receivedAt)),
      outcomes = outcomes
    )
  }
  def select(next: String): Unit = synchronized {
    if coin != next then
      coin = next
      book = None
      touch = None
      backup = false
      metadata = metadata.filter(validMetadata)
      rules = metadata.flatMap(m => Try(Exchange.market(m, next)).toOption)
      lastPlan = None
      prepared = None
      context = None
  }
  def observe(event: Event): Unit = synchronized {
    val at = monotonic()
    event match
      case Event.SocketUp =>
        if !connected && !backup then
          book = None
          touch = None
        connected = true
      case Event.SocketDown =>
        connected = false
        if !backup then
          book = None
          touch = None
          prepared = None
      case Event.BookUpdate(c, b, fromSocket)
          if c == coin &&
            (fromSocket || (BigInt(wall()) - b.time).abs <= config.quoteAgeMs) =>
        val better = book.forall { (old, _) =>
          b.time > old.time || (b.time == old.time && b.bids.size >= old.bids.size && b.asks.size >= old.asks.size && b.bids.size + b.asks.size > old.bids.size + old.asks.size)
        }
        if better then
          backup = !fromSocket
          book = touch
            .filter(_.time > b.time)
            .fold(Option((b, at)))(quote => capBook(b, quote).map((_, at)))
          if touch.forall(_.time <= b.time) then
            touch = Try(Touch(b.bids.head, b.asks.head, b.time, at)).toOption
      case Event.BboUpdate(c, bid, ask, time) if c == coin && touch.forall(_.time <= time) =>
        touch = Try(Touch(bid, ask, time, at)).toOption
        book = for
          (previous, receivedAt) <- book
          quote <- touch
          capped <- capBook(previous, quote)
        yield (capped, receivedAt)
      case Event.Trades(c, batch) if c == coin && !backup && !closed.get() => fillThrough(batch)
      case Event.CtxUpdate(c, ctx) if c == coin => context = Some((ctx, at, wall()))
      case _                                    => ()
  }

  // A trade fills every resting buy priced above it and every resting sell priced below it,
  // once, and only when it printed after the order rested. A trade at the price does not fill,
  // since the queue place is unknown. Batches on backup prices may replay and count nothing.
  private def fillThrough(batch: Vector[Trade]): Unit =
    val cutoff = wall() - State.tapeMs
    while seenTrades.headOption.exists(_._1 < cutoff) do seenTrades -= seenTrades.head
    val counted = batch
      .filter(t => t.time >= cutoff && t.id.exists(id => seenTrades.add((t.time, id))))
      .sortBy(t => (t.time, t.id))
    val texts = counted.flatMap { trade =>
      Try(BigDecimal(trade.px)).toOption.toVector.flatMap { price =>
        ledger.account.resting
          .filter(o =>
            o.coin == coin && o.createdAt < trade.time &&
              (if o.side == Side.Buy then o.price > price else o.price < price)
          )
          .sortBy(o => ((o.price - price).abs, o.createdAt))
          .flatMap(o => settle(o, trade.time))
      }
    }
    if texts.nonEmpty then announce(texts.mkString("\n"))

  // A fill that faced the position pauses opening, as a live resting fill does. The position is
  // read under the ledger's lock, so no market fill lands between the read and the fill. A
  // liquidation saving under the ledger's lock alone may remove the order after the trade read
  // it, and an order already gone says nothing.
  private def settle(order: PropOrder, at: Long): Option[String] =
    val settled = ledger.synchronized {
      Option.when(ledger.account.resting.exists(_.id == order.id)) {
        val before = ledger.account.positions.get(order.coin).fold(BigDecimal(0))(_.quantity)
        (before.signum == (if order.side == Side.Buy then -1 else 1), ledger.resolve(order.id, at))
      }
    }
    settled.map { (facing, resolved) =>
      resolved match
        case Left(error)             => error
        case Right((text, quantity)) =>
          if quantity > 0 then
            record(Fill(order.coin, order.side, at, order.price, quantity))
            if facing then reducedAt = Some(monotonic())
          text
    }
  def preview(intent: OrderIntent, terms: TradeTerms): Either[String, OrderPlan] = synchronized {
    if closed.get() then Left("Prop worker is closed")
    else if pending.get() then Left("Wait for the pending prop order")
    else
      val account = ledger.account
      val position = held(account)
      val reserved = Trading.reserved(position, resting(account).filter(_.coin == coin))
      val result = block match
        case Some(reason) => Left(reason)
        case None if intent.price.nonEmpty && account.resting.size >= Trading.restingCap =>
          Left(Trading.restingCapText)
        case None =>
          Trading
            .plan(
              coin,
              rules.get,
              book.get._1,
              touch.get.quote(intent.side),
              position,
              intent,
              terms.sizes,
              PropMargin.leverage,
              terms.slippage,
              reserved
            )
            // For a short time after a fill reduced the position, only reducing plans pass.
            .filterOrElse(
              plan =>
                plan.reduceOnly ||
                  reducedAt.forall(at => monotonic() - at >= TradingWorker.openingPauseMs),
              TradingWorker.openingPausedMessage
            )
            // A posted order has no depth inside its price. It rests if the collateral holds it.
            .flatMap(plan =>
              if plan.post then ledger.checkResting(order(plan, "", 0L), account).map(_ => plan)
              else
                Trading
                  .fillAtLimit(
                    plan,
                    rules.get,
                    book.get._1,
                    touch.get.quote(intent.side),
                    position
                  )
                  .flatMap(plan => ledger.check(plan, account).map(_ => plan))
            )
      prepared =
        result.toOption.map(plan => Prepared(intent, plan, terms, rules.get, account.count, wall()))
      result
  }
  private def matches(review: OrderReview): Boolean =
    def validRequest: Boolean =
      review.plan.coin == coin && review.createdAt <= wall() && wall() < review.expiresAt &&
        BigInt(review.expiresAt) - BigInt(review.createdAt) <= 30000L &&
        review.leverage.forall(_ == PropMargin.leverage) &&
        review.marginMode.forall(_ == PropMargin.mode)
    def samePreparation: Boolean = prepared.exists(p =>
      p.intent == review.intent && p.plan == review.plan && p.terms == review.terms &&
        rules.contains(p.rules) &&
        ledger.account.count == p.accountCount && p.at <= review.createdAt &&
        BigInt(wall()) - p.at <= TradingWorker.maximumReviewLifetimeMs
    )
    !closed.get() && ready && validRequest && samePreparation
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if !matches(review) || pending.get() then false
    else if review.plan.post then
      // A posted order rests at once. Nothing waits for its fill. A price the touch reached
      // since the review would take liquidity, so it is refused as the venue refuses it.
      val id = "0x" + java.util.UUID.randomUUID().toString.replace("-", "")
      val plan = review.plan
      val result = touch
        .flatMap(t => Trading.crossing(plan.side, plan.limit, t.bidPrice, t.askPrice))
        .toLeft(())
        .flatMap(_ => ledger.rest(order(plan, id, wall()), prepared.get.accountCount))
      lastPlan = Some(review.plan)
      prepared = None
      announce(result.fold(identity, identity))
      true
    else if !pending.compareAndSet(false, true) then false
    else
      lastPlan = Some(review.plan)
      outcome = None
      pendingAt = monotonic()
      pendingCoin = review.plan.coin
      stage = "queued"
      if orders.offer(review) then true
      else
        pending.set(false)
        false
  }
  // A reset waits for the pending order, and the review, the last plan and the pause go with the
  // old account.
  def reset(balance: BigDecimal): Either[String, String] = synchronized {
    if closed.get() then Left("Prop worker is closed")
    else if pending.get() then Left("Wait for the pending prop order")
    else
      val result = ledger.reset(balance, wall())
      result.foreach { text =>
        prepared = None
        lastPlan = None
        reducedAt = None
        announce(text)
      }
      result
  }
  // One clock step. It decides under the worker lock and saves outside it, so the view never
  // waits behind a save, and the ledger decides each change again under its own lock. Funding
  // takes the oracle and rate of a context from the last hour on both clocks, since a sleep may
  // pause the monotonic one. Nothing runs while an order is pending.
  private[desk] def tick(): Unit =
    val (market, funding, check) = synchronized {
      if closed.get() || pending.get() then (coin, None, None)
      else
        val now = wall()
        val hour = Math.floorDiv(now, PropWorker.hourMs)
        val due = hour > settledHour
        if due then settledHour = hour
        val funding = context
          .filter((_, at, wallAt) =>
            due && monotonic() - at <= PropWorker.hourMs && now - wallAt <= PropWorker.hourMs
          )
          .flatMap((ctx, _, _) => Try((BigDecimal(ctx.oracle), BigDecimal(ctx.funding))).toOption)
          .filter(_._1 > 0)
        val check =
          if ready then touch.map(t => (t.midpoint, PropMargin.maintenance(rules.get))) else None
        (coin, funding, check)
    }
    funding.foreach((oracle, rate) => ledger.fund(market, oracle, rate))
    check
      .flatMap((mark, maintenance) =>
        ledger.liquidate(market, mark, maintenance, wall()).toOption.flatten
      )
      .foreach(text =>
        synchronized {
          announce(text)
          prepared = None
        }
      )
  // A tick that throws keeps only what the ledger saved before the throw. The first failure
  // shows, and the clock goes on to the next tick.
  private[desk] def clockFailures: Long = synchronized(failures)
  private def clockFailed(): Unit = synchronized {
    failures += 1
    if failures == 1 then announce(PropWorker.clockFailedMessage)
  }
  // Prop has no venue to ask, so a cancel removes the orders at once.
  def cancel(ids: Vector[String]): Boolean = synchronized {
    if closed.get() || pending.get() then false
    else
      val texts = ids.distinct.flatMap(id => ledger.cancel(id).toOption)
      if texts.nonEmpty then announce(texts.mkString("\n"))
      texts.nonEmpty
  }
  def start(): Unit =
    if started.compareAndSet(false, true) then
      def spawn(name: String)(body: => Unit): Unit =
        val thread = new Thread(
          () => {
            try body
            catch case _: InterruptedException => ()
          },
          name
        )
        thread.setDaemon(true)
        threads :+= thread
        thread.start()
      spawn("prop-metadata") {
        while !closed.get() do
          if synchronized(!metadataFresh(30000L)) then
            val result = Try(rest.post("""{"type":"meta"}""")).toOption.flatten.flatMap(s =>
              Try(ujson.read(s)).toOption
            )
            synchronized {
              result.filter(validMetadata).foreach { value =>
                metadata = Some(value)
                metadataAt = monotonic()
                rules = Try(Exchange.market(value, coin)).toOption
              }
            }
          Thread.sleep(1000L)
      }
      spawn("prop-clock") {
        while !closed.get() do
          try tick()
          catch case NonFatal(_) => clockFailed()
          Thread.sleep(1000L)
      }
      spawn("prop-orders") {
        while !closed.get() do
          val review = orders.take()
          val result = try
            val fill = synchronized {
              stage = "checking"
              if matches(review) then
                val position =
                  held(ledger.account)
                val currentQuote =
                  touch.get.quote(review.plan.side)
                Trading
                  .fillAtLimit(review.plan, rules.get, book.get._1, currentQuote, position)
                  .map(plan => (plan, prepared.get.accountCount))
              else Left("Prop canceled: reviewed request is no longer current")
            }
            fill.flatMap { (plan, count) =>
              synchronized { stage = "saving" }
              val at = wall()
              ledger.fill(plan, at, Some(count)).map(text => (text, plan, at))
            }
          catch
            case scala.util.control.NonFatal(_) => Left("Prop order failed. Review before retry")
          synchronized {
            result.foreach { (_, plan, at) =>
              plan.average.foreach(price =>
                record(Fill(plan.coin, plan.side, at, price, plan.estimatedQuantity))
              )
              // A reduce-only fill, a CLOSE among them, pauses opening, as in live.
              if plan.reduceOnly then reducedAt = Some(monotonic())
            }
            prepared = None
            announce(result.fold(identity, _._1))
            pending.set(false)
          }
      }
  // Desk's failure handler may close the worker on one of its own threads, so the close never
  // interrupts or waits for the thread it runs on.
  def close(): Unit =
    if closed.compareAndSet(false, true) then
      val others = threads.filterNot(_ eq Thread.currentThread())
      others.foreach(_.interrupt())
      others.foreach(_.join(1000L))
      ledger.close()
