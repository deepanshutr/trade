package desk

object OrderUpdates:
  private val sequence = new java.util.concurrent.atomic.AtomicLong(0L)
  def nextId(): Long = sequence.incrementAndGet()

/** Delivers completed orders observed by the workspace frame loop. */
final class OrderUpdates:
  private var delivered = 0L
  private var previous = Option.empty[String]
  def collect(view: TradingView)(receive: String => Unit): Unit =
    if view.outcomeId > delivered || (view.outcomeId == 0L && view.outcome != previous) then
      view.outcome.foreach(receive)
      delivered = delivered.max(view.outcomeId)
      previous = view.outcome
