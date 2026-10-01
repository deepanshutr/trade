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
  private var paper = Option.empty[PaperWorker]
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
  private def paperWorker(): PaperWorker =
    paper.getOrElse {
      val ledger = PaperLedger.open(
        directory.resolve("paper"),
        config.network,
        config.paperBalance,
        config.paperFee
      )
      val worker = new PaperWorker(
        config,
        coin,
        ledger,
        rest,
        wall,
        monotonic,
        live.acceptedMetadata
      )
      paper = Some(worker)
      if connected then worker.observe(Event.SocketUp)
      worker.start()
      worker
    }
  def mode: String = synchronized(selected)
  def start(): Unit = synchronized {
    if selected == "PAPER" then
      val _ = paperWorker()
    else live.start()
  }
  def observe(event: Event): Unit = synchronized {
    event match
      case Event.SocketUp   => connected = true
      case Event.SocketDown => connected = false
      case _                => ()
    live.observe(event)
    paper.foreach(_.observe(event))
  }
  def select(next: String): Unit = synchronized {
    coin = next
    live.select(next)
    paper.foreach(_.select(next))
  }
  def view: TradingView = synchronized {
    if selected == "PAPER" then paperWorker().view
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
    else if live.view.pending || paper.exists(_.view.pending) then
      Left("Wait for the pending order outcome before changing mode")
    else if next == "PAPER" then
      Try {
        val _ = paperWorker()
        selected = "PAPER"
        retireLive()
        ()
      }.toEither.left.map(_ => "Paper storage cannot be opened. Its saved balance was preserved")
    else if next == "LIVE" && !config.configured then
      Left(
        "Live setup required: owner in settings and an active key (desk --keys). See docs/trading.md"
      )
    else if next == "LIVE" && retirementFailed then Left(transitionReason)
    else if next == "LIVE" then
      selected = "LIVE"
      if !liveEnabled && retirement.isEmpty then
        if live.isRetired then startLive() else retireLive()
      Right(())
    else Left("Unknown trading mode")
  }
  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized {
    if selected == "PAPER" then paperWorker().preview(intent)
    else if selected == "LIVE" && !liveEnabled then Left(transitionReason)
    else live.preview(intent)
  }
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if selected == "PAPER" then paperWorker().submitReviewed(review)
    else selected == "LIVE" && liveEnabled && live.submitReviewed(review)
  }
  def close(): Unit =
    val (current, simulation, stopping) = synchronized {
      closed = true
      live.disable()
      (live, paper, retirement)
    }
    try
      stopping match
        case Some(thread) => thread.join(12000L)
        case None         => current.close()
    finally simulation.foreach(_.close())
