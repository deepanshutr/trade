package desk

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try

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
  private var pendingAt = 0L
  private var pendingCoin = initialCoin
  private var stage = "queued"
  private var lastPlan = Option.empty[OrderPlan]
  private var prepared = Option.empty[Prepared]
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
    else if !ledger.available then
      Some(ExecutionBlock(BlockCode.Storage, "Prop storage unavailable. Restart to recover"))
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
    then Some(ExecutionBlock(BlockCode.Prices, "Prop prices stale. Waiting for fresh depth"))
    else if !book.exists(b =>
        touch.exists(t => (BigInt(b._1.time) - BigInt(t.time)).abs <= config.quoteAgeMs)
      )
    then Some(ExecutionBlock(BlockCode.Prices, "Prop depth behind quote. Waiting for depth"))
    else None
  private def block: Option[String] = blockReason.map(_.text)
  private def ready: Boolean = block.isEmpty
  private def held(account: PropAccount): BigDecimal =
    account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0))
  def account: PropAccount = ledger.account
  def view: TradingView = synchronized {
    val account = ledger.account
    val reason = blockReason
    val dataReady = reason.isEmpty
    val working = pending.get()
    val elapsed = ((BigInt(monotonic()) - pendingAt).max(BigInt(0)) / 1000)
    val message = if working then s"PROP $stage ${elapsed}s | $pendingCoin"
    else if dataReady then (if backup then "Prop ready: backup prices" else "Prop ready")
    else reason.get.text
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
        PortfolioView.prop(
          account,
          coin,
          Option.when(dataReady)(touch.map(_.midpoint)).flatten
        )
      else PortfolioView(),
      blockCode = reason.fold(BlockCode.Blocked)(_.code)
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
      case _ => ()
  }
  def preview(intent: OrderIntent, terms: TradeTerms): Either[String, OrderPlan] = synchronized {
    if closed.get() then Left("Prop worker is closed")
    else if pending.get() then Left("Wait for the pending prop order")
    else
      val account = ledger.account
      val position = held(account)
      val result = block match
        case Some(reason)                                      => Left(reason)
        case None if intent.side == Side.Sell && position <= 0 => Left("No " + coin + " to sell")
        case None                                              =>
          Trading
            .plan(
              coin,
              rules.get,
              book.get._1,
              touch.get.quote(intent.side),
              position,
              intent,
              terms.sizes,
              1,
              terms.slippage
            )
            .map(plan =>
              if intent.side == Side.Sell then plan.copy(quantity = plan.quantity.min(position))
              else plan
            )
            .flatMap(plan =>
              Trading.fillAtLimit(
                plan,
                rules.get,
                book.get._1,
                touch.get.quote(intent.side),
                position
              )
            )
            .flatMap(plan => ledger.check(plan, account).map(_ => plan))
      prepared =
        result.toOption.map(plan => Prepared(intent, plan, terms, rules.get, account.count, wall()))
      result
  }
  private def matches(review: OrderReview): Boolean =
    def validRequest: Boolean =
      review.plan.coin == coin && review.createdAt <= wall() && wall() < review.expiresAt &&
        BigInt(review.expiresAt) - BigInt(review.createdAt) <= 30000L &&
        review.leverage.isEmpty && review.marginMode.isEmpty
    def samePreparation: Boolean = prepared.exists(p =>
      p.intent == review.intent && p.plan == review.plan && p.terms == review.terms &&
        rules.contains(p.rules) &&
        ledger.account.count == p.accountCount && p.at <= review.createdAt &&
        BigInt(wall()) - p.at <= TradingWorker.maximumReviewLifetimeMs
    )
    !closed.get() && ready && validRequest && samePreparation
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if !matches(review) || !pending.compareAndSet(false, true) then false
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
              ledger.fill(plan, wall(), Some(count))
            }
          catch
            case scala.util.control.NonFatal(_) => Left("Prop order failed. Review before retry")
          synchronized {
            prepared = None
            outcome = Some(result.fold(identity, identity))
            outcomeId = OrderUpdates.nextId()
            pending.set(false)
          }
      }
  def close(): Unit =
    if closed.compareAndSet(false, true) then
      threads.foreach(_.interrupt())
      threads.foreach(_.join(1000L))
      ledger.close()
