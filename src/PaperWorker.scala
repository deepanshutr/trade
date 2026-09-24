package desk

import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.Try

final class PaperWorker(
    config: TradingConfig,
    initialCoin: String,
    ledger: PaperLedger,
    rest: Rest,
    wall: () => Long,
    monotonic: () => Long
) extends AutoCloseable:
  private final case class Prepared(
      intent: OrderIntent,
      plan: OrderPlan,
      rules: MarketRules,
      accountCount: Long,
      at: Long
  )
  private final case class Touch(bid: Level, ask: Level, time: Long, receivedAt: Long):
    val bidPrice = BigDecimal(bid.px)
    val askPrice = BigDecimal(ask.px)
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
  private var metadata = Option.empty[ujson.Value]
  private var rules = Option.empty[MarketRules]
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
  private def block: Option[String] =
    if closed.get() then Some("Paper closed. Restart Desk")
    else if !ledger.available then Some("Paper storage unavailable. Restart to recover")
    else if !connected && !backup then Some("Reconnecting. Waiting for fresh prices")
    else if rules.isEmpty then Some("Paper waiting for market details")
    else if book.isEmpty || touch.isEmpty then Some("Paper waiting for selected-market depth")
    else if !touch.exists(t =>
        t.bidPrice > 0 && t.askPrice > t.bidPrice && t.bidSize > 0 && t.askSize > 0
      )
    then Some("Paper quote invalid. Waiting for prices")
    else if !book.exists(b =>
        fresh(b._2) && (!backup || (BigInt(wall()) - b._1.time).abs <= config.quoteAgeMs)
      ) || !touch.exists(t => fresh(t.receivedAt))
    then Some("Paper prices stale. Waiting for fresh depth")
    else if !book.exists(b =>
        touch.exists(t => (BigInt(b._1.time) - BigInt(t.time)).abs <= config.quoteAgeMs)
      )
    then Some("Paper depth behind quote. Waiting for depth")
    else None
  private def ready: Boolean = block.isEmpty
  def account: PaperAccount = ledger.account
  def accountLabel: String = synchronized {
    val a = ledger.account
    val position = a.positions.get(coin)
    val mark = touch.map(_.midpoint)
    val unrealized = for p <- position; px <- mark yield p.quantity * (px - p.entry)
    "Cash $" + Trading.display(a.cash) + " realized $" + Trading.display(a.realized) + unrealized
      .fold("")(p => " open $" + Trading.display(p))
  }
  def view: TradingView = synchronized {
    val account = ledger.account
    val reason = block
    val dataReady = reason.isEmpty
    val working = pending.get()
    val elapsed = ((BigInt(monotonic()) - pendingAt).max(BigInt(0)) / 1000)
    val message = if working then s"PAPER $stage ${elapsed}s | $pendingCoin"
    else if dataReady then (if backup then "Paper ready: backup prices" else "Paper ready")
    else reason.get
    TradingView(
      coin,
      Some(account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0))),
      message,
      dataReady && !working,
      working,
      rules,
      touch.map(_.bidPrice),
      touch.map(_.askPrice),
      lastPlan,
      reason,
      outcome,
      outcomeId = outcomeId,
      portfolio = if ledger.available then
        PortfolioView.paper(
          account,
          coin,
          Option.when(dataReady)(touch.map(_.midpoint)).flatten
        )
      else PortfolioView()
    )
  }
  def select(next: String): Unit = synchronized {
    if coin != next then
      coin = next
      book = None
      touch = None
      backup = false
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
  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized {
    if closed.get() then Left("Paper worker is closed")
    else if pending.get() then Left("Wait for the pending paper order")
    else
      val account = ledger.account
      val position = account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0))
      val result = if !ready then Left(block.get)
      else if intent.side == Side.Sell && position <= 0 then Left("No " + coin + " to sell")
      else
        Trading
          .plan(
            coin,
            rules.get,
            book.get._1,
            if intent.side == Side.Buy then touch.get.askPrice else touch.get.bidPrice,
            position,
            intent,
            config.sizes
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
              if intent.side == Side.Buy then touch.get.askPrice else touch.get.bidPrice,
              position
            )
          )
          .flatMap(plan => ledger.check(plan, account).map(_ => plan))
      prepared =
        result.toOption.map(plan => Prepared(intent, plan, rules.get, account.count, wall()))
      result
  }
  private def matches(review: OrderReview): Boolean =
    !closed
      .get() && ready && review.plan.coin == coin && review.createdAt <= wall() && wall() < review.expiresAt && BigInt(
      review.expiresAt
    ) - BigInt(
      review.createdAt
    ) <= 30000L && review.leverage.isEmpty && review.marginMode.isEmpty && prepared.exists(p =>
      p.intent == review.intent && p.plan == review.plan && rules.contains(p.rules) &&
        ledger.account.count == p.accountCount && p.at <= review.createdAt &&
        BigInt(wall()) - p.at <= TradingWorker.maximumReviewLifetimeMs
    )
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
      spawn("paper-metadata") {
        while !closed.get() do
          val result = rest.post("""{"type":"meta"}""").flatMap(s => Try(ujson.read(s)).toOption)
          synchronized {
            metadata = result
            rules = result.flatMap(m => Try(Exchange.market(m, coin)).toOption)
          }
          Thread.sleep(60000L)
      }
      spawn("paper-orders") {
        while !closed.get() do
          val review = orders.take()
          val result = try
            val fill = synchronized {
              stage = "checking"
              if matches(review) then
                val position =
                  ledger.account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0))
                val currentQuote =
                  if review.plan.side == Side.Buy then touch.get.askPrice else touch.get.bidPrice
                Trading
                  .fillAtLimit(review.plan, rules.get, book.get._1, currentQuote, position)
                  .map(plan => (plan, prepared.get.accountCount))
              else Left("Paper canceled: reviewed request is no longer current")
            }
            fill.flatMap { (plan, count) =>
              synchronized { stage = "saving" }
              ledger.fill(plan, wall(), Some(count))
            }
          catch
            case scala.util.control.NonFatal(_) => Left("Paper order failed. Review before retry")
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
