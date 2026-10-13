package desk

import java.nio.file.Path
import scala.util.Try

final class ExecutionRouter(
    initialConfig: TradingConfig,
    initialMode: String,
    initialCoin: String,
    initialLive: TradingWorker,
    initialFactory: String => TradingWorker,
    rest: Rest,
    directory: Path,
    wall: () => Long,
    monotonic: () => Long
) extends AutoCloseable:
  // A reload replaces the config and the live factory. A worker already made keeps its own.
  private var config = initialConfig
  private var createLive = initialFactory
  private var selected = initialMode
  private var coin = initialCoin
  private var live = initialLive
  private var liveEnabled = initialMode == "LIVE"
  private var prop = Option.empty[PropWorker]
  private var connected = false
  private var retirement = Option.empty[Thread]
  private var retirementFailed = false
  // The live orders resting when the last live worker retired. They keep resting at the venue,
  // so prop and view show the count.
  private var liveResting = 0
  private var closed = false
  // While settings run a child, no live worker exists, so none holds the journal lock or a key.
  private var suspended = false
  private def transitionReason: String =
    if retirementFailed then "Trading did not stop. Restart to retry"
    else "Changing mode"
  private def startLive(): Unit =
    live = createLive(coin)
    liveEnabled = true
    if connected then live.observe(Event.SocketUp)
    live.start()
  private def retireLive(): Unit =
    if retirement.isEmpty && !live.isRetired && !retirementFailed then
      val previous = live
      previous.disable()
      liveEnabled = false
      val thread = new Thread(
        () => {
          val result = Try {
            previous.close()
            if !previous.isRetired then previous.awaitRetirement()
          }
          val count = previous.restingCount
          synchronized {
            liveResting = count
            retirement = None
            retirementFailed = result.isFailure || !previous.isRetired
            if !closed && !suspended && selected == "LIVE" && !retirementFailed then startLive()
          }
        },
        "execution-retirement"
      )
      thread.setDaemon(true)
      retirement = Some(thread)
      thread.start()
  private def propWorker(): PropWorker =
    prop.getOrElse {
      val ledger = PropLedger.open(
        directory.resolve("prop"),
        config.network,
        config.propBalance,
        config.propFee
      )
      val worker = new PropWorker(
        config,
        coin,
        ledger,
        rest,
        wall,
        monotonic,
        live.acceptedMetadata
      )
      prop = Some(worker)
      if connected then worker.observe(Event.SocketUp)
      worker.start()
      worker
    }
  def mode: String = synchronized(selected)
  def start(): Unit = synchronized {
    if selected == "PROP" then
      val _ = propWorker()
    else live.start()
  }
  def observe(event: Event): Unit = synchronized {
    event match
      case Event.SocketUp   => connected = true
      case Event.SocketDown => connected = false
      case _                => ()
    live.observe(event)
    prop.foreach(_.observe(event))
  }
  def select(next: String): Unit = synchronized {
    coin = next
    live.select(next)
    prop.foreach(_.select(next))
  }
  def view: TradingView = synchronized {
    if selected == "PROP" then propWorker().view.copy(liveResting = liveResting)
    else if selected == "LIVE" && !liveEnabled then
      TradingView(
        coin,
        message = transitionReason,
        block = Some(transitionReason)
      )
    else if selected == "LIVE" then live.view
    else live.view.copy(ready = false, block = Some("View only"), liveResting = liveResting)
  }
  def switchMode(next: String): Either[String, Unit] = synchronized {
    if closed then Left("Trading stopped")
    else if live.view.pending || prop.exists(_.view.pending) then
      Left("Wait for the pending order outcome before changing mode")
    else if next == "PROP" then
      Try {
        val _ = propWorker()
        selected = "PROP"
        retireLive()
        ()
      }.toEither.left.map(_ => "Prop storage cannot be opened. Its saved balance was preserved")
    else if next == "LIVE" && suspended then Left("Settings are open")
    else if next == "LIVE" && !config.configured then Left(TradingWorker.setupMessage)
    else if next == "LIVE" && retirementFailed then Left(transitionReason)
    else if next == "LIVE" then
      selected = "LIVE"
      if !liveEnabled && retirement.isEmpty then
        if live.isRetired then startLive() else retireLive()
      Right(())
    else Left("Unknown trading mode")
  }
  def preview(intent: OrderIntent, terms: TradeTerms): Either[String, OrderPlan] = synchronized {
    if selected == "PROP" then propWorker().preview(intent, terms)
    else if selected == "LIVE" && !liveEnabled then Left(transitionReason)
    else live.preview(intent, terms)
  }
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if selected == "PROP" then propWorker().submitReviewed(review)
    else selected == "LIVE" && liveEnabled && live.submitReviewed(review)
  }
  // A cancel goes to the worker of the selected mode. View holds no orders, so it answers false.
  // A live worker that is retiring is disabled and refuses the cancel itself.
  def cancel(ids: Vector[String]): Boolean = synchronized {
    if selected == "PROP" then propWorker().cancel(ids)
    else selected == "LIVE" && live.cancel(ids)
  }
  // Only live runs a row cancel long enough for CLOSE to wait on it.
  def closeFirst(): Boolean = synchronized(selected == "LIVE" && liveEnabled && live.closeFirst())
  def withdrawClose(): Unit = synchronized {
    if selected == "LIVE" && liveEnabled then live.withdrawClose()
  }
  def livePending: Boolean = synchronized(live.view.pending)
  // Settings show the prop balance of the file they read and hand it here, so the reset starts
  // over at the figure the owner confirmed. Every other setting waits for a reload.
  def followPropBalance(balance: BigDecimal): Unit = synchronized {
    config = config.copy(propBalance = balance)
  }
  // Only prop resets its wallet, so live and view never start the prop worker for it.
  def resetProp(): Either[String, String] = synchronized {
    if selected != "PROP" then Left("Switch to prop first")
    else propWorker().reset(config.propBalance)
  }
  // Stops the live worker for a child and waits for it, as close does. A worker that has not
  // retired in time may still hold the journal lock, so no child may start.
  def suspendLive(waitMs: Long = 12000L): Either[String, Unit] =
    val stopping = synchronized {
      suspended = true
      retireLive()
      retirement
    }
    stopping.foreach(_.join(waitMs))
    synchronized {
      Either.cond(
        retirement.isEmpty && !retirementFailed && live.isRetired,
        (),
        "Trading did not stop. Restart Desk"
      )
    }
  // Ends a suspension whatever the child did. A retirement still running or failed keeps the old
  // worker's journal lock, so live starts only through the next switch to it.
  def resumeLive(): Unit = synchronized {
    suspended = false
    if !closed && selected == "LIVE" && retirement.isEmpty && !retirementFailed && live.isRetired
    then startLive()
  }
  // Takes the reloaded settings and factory, retires the current live worker and waits for it.
  // The retirement starts a new worker only when live is selected and settings run no child.
  def replaceLive(
      next: TradingConfig,
      factory: String => TradingWorker,
      waitMs: Long = 12000L
  ): Unit =
    val stopping = synchronized {
      config = next
      createLive = factory
      retireLive()
      retirement
    }
    stopping.foreach(_.join(waitMs))
  def close(): Unit =
    val (current, simulation, stopping) = synchronized {
      closed = true
      live.disable()
      (live, prop, retirement)
    }
    try
      stopping match
        case Some(thread) => thread.join(12000L)
        case None         => current.close()
    finally simulation.foreach(_.close())
