package desk

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.time.ZoneOffset
import java.util.concurrent.TimeUnit

object DisplayCost:
  @volatile private var sink: Any = null
  @volatile private var kept = Vector.empty[Trade]
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

  private def once(body: => Any): ujson.Value =
    val bytes = allocated()
    val start = System.nanoTime()
    sink = body
    val elapsed = System.nanoTime() - start
    val used = allocated() - bytes
    ujson.Obj("nanoseconds" -> elapsed.toDouble, "bytes" -> used.toDouble)

  private def bench(name: String)(body: => Any): ujson.Value =
    for _ <- 0 until 200 do sink = body
    val rows = Vector.fill(500)(once(body))
    val sorted = rows.map(_("nanoseconds").num).sorted
    ujson.Obj(
      "name" -> name,
      "warmup" -> 200,
      "samples" -> 500,
      "median_ms" -> sorted(249) / 1e6,
      "p99_ms" -> sorted(494) / 1e6,
      "bytes_per_call" -> rows.map(_("bytes").num).sum / rows.size,
      "raw" -> ujson.Arr.from(rows)
    )

  private def heap(path: Path): ujson.Value =
    sink = null
    val process = new ProcessBuilder(
      Path.of(System.getProperty("java.home"), "bin", "jcmd").toString,
      ProcessHandle.current().pid().toString,
      "GC.class_histogram"
    ).redirectErrorStream(true).redirectOutput(path.toFile).start()
    if !process.waitFor(30, TimeUnit.SECONDS) then
      process.destroyForcibly()
      throw new IllegalStateException("Memory measurement did not finish")
    require(process.exitValue() == 0, "Memory measurement failed")
    val entries = Files
      .readString(path)
      .linesIterator
      .flatMap { line =>
        val fields = line.trim.split("\\s+")
        if fields.length >= 4 && Set("desk.Trade", "java.lang.Double").contains(fields(3)) then
          Some(fields(3) -> ujson.Obj("count" -> fields(1).toDouble, "bytes" -> fields(2).toDouble))
        else None
      }
      .toVector
    ujson.Obj.from(entries)

  private def memory(prefix: String): ujson.Value =
    val empty = heap(Path.of(prefix + ".empty-heap.txt"))
    val creation = once { kept = tape(6, 200000) }
    val cold = heap(Path.of(prefix + ".cold-heap.txt"))
    val first = once(Gauges.tapeStats(kept, Golden.now, lenses))
    val warm = heap(Path.of(prefix + ".warm-heap.txt"))
    kept = Vector.empty
    val released = heap(Path.of(prefix + ".released-heap.txt"))
    ujson.Obj(
      "empty" -> empty,
      "cold" -> cold,
      "warm" -> warm,
      "released" -> released,
      "creation" -> creation,
      "first_tape" -> first
    )

  def main(args: Array[String]): Unit =
    require(args.length == 1, "Give the output file prefix")
    val prefix = args(0)
    val seed = Golden.snapshot
    for _ <- 0 until 200 do
      sink = Gauges.tapeStats(seed.tape, Golden.now, lenses)
      sink = Screen.render(seed, 100, 30, Golden.now, ZoneOffset.UTC, Style.colour)
    val memoryResult = memory(prefix)
    val createAndDraw = bench("create_tape_and_first_draw") {
      val s = seed.copy(tape = tape(6, 200000))
      Screen.render(s, 100, 30, Golden.now, ZoneOffset.UTC, Style.colour)
    }
    val cap = retained(6, 200000)
    val firstFrame = once(Screen.render(cap, 100, 30, Golden.now, ZoneOffset.UTC, Style.colour))
    val rows = Vector(
      createAndDraw,
      bench("tape_stats")(Gauges.tapeStats(cap.tape, Golden.now, lenses)),
      bench("render_100x30")(
        Screen.render(cap, 100, 30, Golden.now, ZoneOffset.UTC, Style.colour)
      ),
      bench("render_120x40")(
        Screen.render(cap, 120, 40, Golden.now, ZoneOffset.UTC, Style.mono)
      )
    )
    val result = ujson.Obj(
      "runtime" -> System.getProperty("java.runtime.version"),
      "memory" -> memoryResult,
      "first_full_frame" -> firstFrame,
      "measurements" -> ujson.Arr.from(rows)
    )
    Files.writeString(Path.of(prefix + ".json"), ujson.write(result, indent = 2))
    Files.writeString(Path.of(prefix + ".frames"), frameHashes.mkString("\n") + "\n")
    println(ujson.write(result.obj.filter(_._1 != "measurements").toMap))
    rows.foreach(row => println(ujson.write(row.obj.filter(_._1 != "raw").toMap)))
