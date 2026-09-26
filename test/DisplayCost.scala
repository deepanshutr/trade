package desk

import java.lang.management.ManagementFactory
import java.security.MessageDigest
import java.time.ZoneOffset

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

  def retained(hours: Int, count: Int): Snapshot =
    val s = Golden.snapshot
    val seconds = hours * 3600
    def samples(seed: Vector[(Long, Double)]): Vector[(Long, Double)] =
      Vector.tabulate(seconds + 1)(i => (Golden.now / 1000 - seconds + i, seed(i % seed.size)._2))
    def candles(seed: Map[String, Vector[Candle]]): Map[String, Vector[Candle]] =
      State.intervals.map { interval =>
        val raw = seed(interval)
        val step = State.intervalMs(interval)
        val end = Golden.now / step * step
        interval -> Vector.tabulate(State.seriesCap)(i =>
          raw(i % raw.size).copy(t = end - (State.seriesCap - 1L - i) * step)
        )
      }.toMap
    s.copy(
      samples = samples(s.samples),
      tape = tape(hours, count),
      books =
        Vector.tabulate(901)(i => (Golden.now / 1000 - 900 + i, s.books(i % s.books.size)._2)),
      candles = candles(s.candles),
      refs = s.refs.map((coin, ref) => coin -> Ref(samples(ref.samples), candles(ref.candles))),
      since = Golden.now - seconds * 1000L
    )

  def frameHashes: Vector[String] =
    Vector(Golden.snapshot, retained(1, 20000), retained(6, 200000)).zipWithIndex.flatMap {
      (snapshot, i) =>
        Vector((100, 30), (120, 40)).flatMap { (cols, rows) =>
          Vector(0, 5, 9, 15).map { zoom =>
            val frame = Screen.render(
              snapshot.copy(zoom = zoom),
              cols,
              rows,
              Golden.now,
              ZoneOffset.UTC,
              Style.colour
            )
            val hash = MessageDigest
              .getInstance("SHA-256")
              .digest(Term.bytes(frame))
              .map(b => f"${b & 0xff}%02x")
              .mkString
            s"$i,$cols,$rows,$zoom,$hash"
          }
        }
    }

  def warmedBytes(body: => Any): Long =
    for _ <- 0 until 30 do sink = body
    val before = allocated()
    for _ <- 0 until 8 do sink = body
    (allocated() - before) / 8
