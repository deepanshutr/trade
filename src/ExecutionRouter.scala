package desk

import java.nio.file.Path
import scala.util.Try

final class ExecutionRouter(
    config: TradingConfig,
    initialMode: String,
    initialCoin: String,
    initialLive: TradingWorker,
    createLive: String => TradingWorker,
    rest: Rest,
    directory: Path,
    wall: () => Long,
    monotonic: () => Long
) extends AutoCloseable:
  private var selected = initialMode
  private var coin = initialCoin
  private var live = initialLive
  private var liveEnabled = initialMode == "LIVE"
  private var prop = Option.empty[PropWorker]
  private var connected = false
  private var retirement = Option.empty[Thread]
  private var retirementFailed = false
  private var closed = false
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
          synchronized {
            retirement = None
            retirementFailed = result.isFailure || !previous.isRetired
            if !closed && selected == "LIVE" && !retirementFailed then startLive()
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
    if selected == "PROP" then propWorker().view
    else if selected == "LIVE" && !liveEnabled then
      TradingView(
        coin,
        message = transitionReason,
        block = Some(transitionReason)
      )
    else if selected == "LIVE" then live.view
    else live.view.copy(ready = false, block = Some("View only"))
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
    else if next == "LIVE" && !config.configured then Left(TradingWorker.setupMessage)
    else if next == "LIVE" && retirementFailed then Left(transitionReason)
    else if next == "LIVE" then
      selected = "LIVE"
      if !liveEnabled && retirement.isEmpty then
        if live.isRetired then startLive() else retireLive()
      Right(())
    else Left("Unknown trading mode")
  }
  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized {
    if selected == "PROP" then propWorker().preview(intent)
    else if selected == "LIVE" && !liveEnabled then Left(transitionReason)
    else live.preview(intent)
  }
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if selected == "PROP" then propWorker().submitReviewed(review)
    else selected == "LIVE" && liveEnabled && live.submitReviewed(review)
  }
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
