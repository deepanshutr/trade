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
  private var coin = initialCoin
  private var connected = false
  private var book = Option.empty[(Book, Long)]
  private var touch = Option.empty[(BigDecimal, BigDecimal, Long, Long)]
  private var metadata = Option.empty[ujson.Value]
  private var rules = Option.empty[MarketRules]
  private var outcome = Option.empty[String]
  private var lastPlan = Option.empty[OrderPlan]
  private val pending = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val orders = new ArrayBlockingQueue[OrderReview](1)
  private var threads = Vector.empty[Thread]
  private def fresh(at: Long): Boolean = monotonic() >= at && monotonic() - at <= config.quoteAgeMs
  private def ready: Boolean = connected && rules.nonEmpty && ledger.available && book.exists(b =>
    fresh(b._2)
  ) && touch.exists(t => t._1 > 0 && t._2 > t._1 && fresh(t._4)) && book.exists(b =>
    touch.exists(t => (BigInt(b._1.time) - BigInt(t._3)).abs <= config.quoteAgeMs)
  )
  def account: PaperAccount = ledger.account
  def accountLabel: String = synchronized {
    val a = ledger.account
    val position = a.positions.get(coin)
    val mark = touch.map(t => (t._1 + t._2) / 2)
    val unrealized = for p <- position; px <- mark yield p.quantity * (px - p.entry)
    "Cash $" + Trading.display(a.cash) + " realized $" + Trading.display(a.realized) + unrealized
      .fold("")(p => " open $" + Trading.display(p))
  }
  def view: TradingView = synchronized {
    val dataReady = ready
    val working = pending.get()
    val message = if working then "Paper order pending"
    else if dataReady then "Paper ready"
    else "Paper needs fresh depth and available storage"
    TradingView(
      coin,
      Some(ledger.account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0))),
      message,
      dataReady && !working,
      working,
      rules,
      touch.map(_._1),
      touch.map(_._2),
      lastPlan,
      Option.unless(dataReady)("Paper needs fresh depth and available storage"),
      outcome
    )
  }
  def select(next: String): Unit = synchronized {
    if coin != next then
      coin = next
      book = None
      touch = None
      rules = metadata.flatMap(m => Try(Exchange.market(m, next)).toOption)
      lastPlan = None
  }
  def observe(event: Event): Unit = synchronized {
    val at = monotonic()
    event match
      case Event.SocketUp =>
        if !connected then
          book = None
          touch = None
        connected = true
      case Event.SocketDown =>
        connected = false
        book = None
        touch = None
      case Event.BookUpdate(c, b, _) if c == coin =>
        val better = book.forall { (old, _) =>
          b.time > old.time || (b.time == old.time && b.bids.size >= old.bids.size && b.asks.size >= old.asks.size && b.bids.size + b.asks.size > old.bids.size + old.asks.size)
        }
        if better then
          book = Some((b, at))
          if touch.forall(_._3 <= b.time) then
            touch =
              Try((BigDecimal(b.bids.head.px), BigDecimal(b.asks.head.px), b.time, at)).toOption
      case Event.BboUpdate(c, bid, ask, time) if c == coin && touch.forall(_._3 <= time) =>
        touch = Try((BigDecimal(bid.px), BigDecimal(ask.px), time, at)).toOption
      case _ => ()
  }
  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized {
    if !ready then Left("Paper needs fresh selected-market depth")
    else
      Trading.plan(
        coin,
        rules.get,
        book.get._1,
        if intent.side == Side.Buy then touch.get._2 else touch.get._1,
        ledger.account.positions.get(coin).map(_.quantity).getOrElse(BigDecimal(0)),
        intent,
        config.sizes
      )
  }
  private def matches(review: OrderReview): Boolean =
    !closed
      .get() && ready && review.plan.coin == coin && review.createdAt <= wall() && wall() < review.expiresAt && BigInt(
      review.expiresAt
    ) - BigInt(
      review.createdAt
    ) <= 30000L && review.leverage.isEmpty && review.marginMode.isEmpty && preview(review.intent)
      .contains(review.plan)
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if !matches(review) || !pending.compareAndSet(false, true) then false
    else
      lastPlan = Some(review.plan)
      outcome = None
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
          synchronized {
            val result = if matches(review) then ledger.fill(review.plan, wall())
            else Left("Paper canceled: reviewed market data changed")
            outcome = Some(result.fold(identity, identity))
            pending.set(false)
          }
      }
  def close(): Unit =
    if closed.compareAndSet(false, true) then
      threads.foreach(_.interrupt())
      threads.foreach(_.join(1000L))
      ledger.close()
