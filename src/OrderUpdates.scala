package desk

object OrderUpdates:
  private val sequence = new java.util.concurrent.atomic.AtomicLong(0L)
  def nextId(): Long = sequence.incrementAndGet()

/** Delivers completed orders observed by the workspace frame loop. */
final class OrderUpdates:
  private var delivered = 0L
  private var previous = Option.empty[String]
  // A worker's view lists its last outcomes, so every one above the last delivered arrives, in
  // order, however many landed between two frames. A view without the list has one outcome.
  def collect(view: TradingView)(receive: String => Unit): Unit =
    if view.outcomes.nonEmpty then
      view.outcomes.filter(_._1 > delivered).foreach { (id, text) =>
        receive(text)
        delivered = id
      }
      previous = view.outcome
    else if view.outcomeId > delivered || (view.outcomeId == 0L && view.outcome != previous) then
      view.outcome.foreach(receive)
      delivered = delivered.max(view.outcomeId)
      previous = view.outcome
