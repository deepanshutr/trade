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
  private def paperWorker(): PaperWorker =
    paper.getOrElse {
      val ledger = PaperLedger.open(
        directory.resolve("paper"),
        config.network,
        config.paperBalance,
        config.paperFee
      )
      val worker = new PaperWorker(config, coin, ledger, rest, wall, monotonic)
      paper = Some(worker)
      if connected then worker.observe(Event.SocketUp)
      worker.start()
      worker
    }
  def mode: String = synchronized(selected)
  def start(): Unit = synchronized {
    live.start()
    if selected == "PAPER" then
      val _ = paperWorker()
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
    else if selected == "LIVE" then live.view
    else live.view.copy(ready = false, block = Some("View only"))
  }
  def accountLabel: String = synchronized {
    if selected == "PAPER" then paperWorker().accountLabel
    else if selected == "LIVE" then
      config.network + " " + config.owner.fold("setup required")(address =>
        address.take(8) + "..." + address.takeRight(6)
      )
    else "Public market view"
  }
  def switchMode(next: String): Either[String, String] = synchronized {
    if live.view.pending || paper.exists(_.view.pending) then
      Left("Wait for the pending order outcome before changing mode")
    else if next == "PAPER" then
      Try {
        val _ = paperWorker()
        selected = "PAPER"
        accountLabel
      }.toEither.left.map(_ => "Paper storage cannot be opened. Its saved balance was preserved")
    else if next == "LIVE" && !config.configured then
      Left(
        "Live setup required: public owner, delegated signer and protected key. See docs/trading.md"
      )
    else if next == "LIVE" then
      if !liveEnabled then
        live.close()
        live = createLive(coin)
        liveEnabled = true
        if connected then live.observe(Event.SocketUp)
        live.start()
      selected = "LIVE"
      Right(accountLabel)
    else Left("Unknown trading mode")
  }
  def preview(intent: OrderIntent): Either[String, OrderPlan] = synchronized {
    if selected == "PAPER" then paperWorker().preview(intent) else live.preview(intent)
  }
  def submitReviewed(review: OrderReview): Boolean = synchronized {
    if selected == "PAPER" then paperWorker().submitReviewed(review)
    else selected == "LIVE" && live.submitReviewed(review)
  }
  def close(): Unit = synchronized {
    try live.close()
    finally paper.foreach(_.close())
  }
