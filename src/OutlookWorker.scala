package desk

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

final class OutlookWorker(
    snapshot: () => Snapshot,
    wall: () => Long,
    publish: OutlookView => Unit,
    calculate: (Snapshot, Long) => OutlookView = Outlook.compute
) extends AutoCloseable:
  private val closed = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private var thread = Option.empty[Thread]
  private var previous = Option.empty[(String, Vector[(Long, Vector[Candle])])]
  private var raw = Outlook.empty("")
  private type Target = (String, String, Option[Long], Option[Double])
  private val reached = new AtomicReference(Set.empty[Target])
  private def key(coin: String, column: HorizonOutlook): Target =
    (coin, column.horizon.label, column.at, column.target)

  private def observe(coin: String, columns: Vector[HorizonOutlook]): Unit =
    val touched = columns.map(key(coin, _)).toSet
    if touched.nonEmpty then
      val _ = reached.updateAndGet(_ ++ touched)

  def forDisplay(value: OutlookView, market: Snapshot, now: Long): OutlookView =
    val visible = if value.coin.isEmpty then value else Outlook.visible(value, market, now)
    observe(visible.coin, visible.columns.filter(_.status == "reached"))
    val latched = reached.get()
    val columns = visible.columns.map { column =>
      if latched.contains(key(visible.coin, column)) && Set(
          "ready",
          "limited history",
          "gapped history",
          "reached"
        ).contains(column.status)
      then
        column.copy(
          direction = OutlookDirection.Mixed,
          status = "reached",
          base = column.base.copy(available = false),
          matched = column.matched.copy(available = false)
        )
      else column
    }
    visible.copy(columns = columns)

  private[desk] def refresh(): Unit =
    val market = snapshot()
    val now = wall()
    val evidence = Outlook.horizons.map { horizon =>
      val step = State.intervalMs(horizon.interval)
      (
        now / step,
        market.candles.getOrElse(horizon.interval, Vector.empty).filter(_.t <= now - step)
      )
    }
    val revision = (market.coin, evidence)
    if !previous.contains(revision) then
      val next = calculate(market, now)
      raw = next
      val current = raw.columns.map(key(raw.coin, _)).toSet
      val _ = reached.updateAndGet(_.intersect(current))
      previous = Some(revision)
    observe(raw.coin, raw.columns.filter(Outlook.targetTouched(_, market, now)))
    if !closed.get() then publish(raw.copy(calculatedAt = now))

  def start(): Unit = synchronized {
    if !closed.get() && started.compareAndSet(false, true) then
      val worker = Threads.daemon("market-outlook") {
        while !closed.get() do
          try refresh()
          catch
            case _: InterruptedException => throw new InterruptedException()
            case _: Exception            =>
              val failed = Outlook.empty(snapshot().coin)
              publish(
                failed.copy(
                  calculatedAt = wall(),
                  columns = failed.columns.map(_.copy(status = "failed"))
                )
              )
          Thread.sleep(1000L)
      }
      thread = Some(worker)
      worker.start()
  }

  def close(): Unit =
    closed.set(true)
    thread.foreach(_.interrupt())
    thread.foreach(_.join(2000L))
