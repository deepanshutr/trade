package desk

import java.lang.management.ManagementFactory

object DisplayCost:
  @volatile private var sink: Any = null
  private val bean =
    ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  val lenses: Vector[Long] = Gauges.tapeLenses.map(_._2)

  def allocated(): Long =
    require(bean.isThreadAllocatedMemorySupported, "Allocation measurement is unavailable")
    if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
    bean.getThreadAllocatedBytes(Thread.currentThread().threadId())

  def tape(hours: Int, count: Int): Vector[Trade] =
    val seed = Golden.snapshot.tape
    val span = math.min(hours, 4) * 3600000L
    Vector.tabulate(count)(i =>
      seed(i % seed.size).copy(time = Golden.now - i.toLong * span / count)
    )

  def warmedBytes(body: => Any): Long =
    for _ <- 0 until 30 do sink = body
    val before = allocated()
    for _ <- 0 until 8 do sink = body
    (allocated() - before) / 8
