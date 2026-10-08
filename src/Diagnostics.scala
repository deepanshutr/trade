package desk

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scala.util.Try

enum DiagnosticPart:
  case Input, Build, FrameLock, Terminal, Render, Fold, Feed, Account, Background, OrderResults

object Diagnostics:
  private[desk] trait Sink extends AutoCloseable:
    def append(line: String): Unit

  private[desk] final class FilesSink(directory: Path, limit: Long = 2L * 1024 * 1024) extends Sink:
    private val lease = LocalFiles.lock(directory.resolve("diagnostics.lock"))
    private def path(i: Int): Path = directory.resolve(s"events-$i.jsonl")
    private def checked(i: Int): Unit =
      val file = path(i)
      require(
        !Files.exists(file, LinkOption.NOFOLLOW_LINKS) ||
          (TradingConfig.privateFile(file) && Files.size(file) <= limit),
        "invalid diagnostic file"
      )
    private var channel: FileChannel = null
    try
      (0 until 4).foreach(checked)
      channel = open()
    catch
      case error: Exception =>
        lease.close()
        throw error
    private def open(): FileChannel = FileChannel.open(
      path(0),
      java.util.Set.of[java.nio.file.OpenOption](
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        StandardOpenOption.APPEND,
        LinkOption.NOFOLLOW_LINKS
      ),
      LocalFiles.privateFile*
    )
    def append(line: String): Unit =
      val bytes = (line + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
      require(bytes.length <= limit && bytes.length <= 4096, "diagnostic record too large")
      if channel.size() + bytes.length > limit then
        (0 until 4).foreach(checked)
        channel.close()
        Files.deleteIfExists(path(3))
        (2 to 0 by -1).foreach { i =>
          if Files.exists(path(i), LinkOption.NOFOLLOW_LINKS) then
            Files.move(path(i), path(i + 1), StandardCopyOption.REPLACE_EXISTING)
        }
        channel = open()
      val buffer = ByteBuffer.wrap(bytes)
      while buffer.hasRemaining do channel.write(buffer)
    def close(): Unit =
      try if channel != null then channel.close()
      finally lease.close()

  def open(directory: Path, now: () => Long, quoteAgeMs: Long): Diagnostics =
    new Diagnostics(() => new FilesSink(directory.resolve("diagnostics")), now, quoteAgeMs)

  def component(thread: String): DiagnosticPart = thread match
    case "keys"                                                   => DiagnosticPart.Input
    case "fold" | "market"                                        => DiagnosticPart.Fold
    case "orders" | "prop-orders" | "account" | "balance-display" => DiagnosticPart.Account
    case "feed" | "poll" | "coarse" | "heartbeat" | "onchain" | "history-repair" =>
      DiagnosticPart.Feed
    case _ => DiagnosticPart.Background

  private def build: String = Try {
    val source = Path.of(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    if Files.isRegularFile(source) && source.getFileName.toString.endsWith(".jar") then
      val digest = java.security.MessageDigest.getInstance("SHA-256")
      val stream = Files.newInputStream(source)
      try
        val buffer = new Array[Byte](65536)
        var n = stream.read(buffer)
        while n >= 0 do
          if n > 0 then digest.update(buffer, 0, n)
          n = stream.read(buffer)
        java.util.HexFormat.of().formatHex(digest.digest())
      finally stream.close()
    else "development"
  }.getOrElse("unknown")

/** No prompt, account, order, exception message or object is retained by this writer. */
final class Diagnostics private[desk] (
    openSink: () => Diagnostics.Sink,
    now: () => Long,
    quoteAgeMs: Long,
    capacity: Int = 1024,
    monotonic: () => Long = () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime())
) extends AutoCloseable:
  private val queue = new ArrayBlockingQueue[ujson.Obj](capacity)
  private val started = new AtomicBoolean(false)
  private val closed = new AtomicBoolean(false)
  private val failed = new AtomicBoolean(false)
  private val dropped = new AtomicLong(0L)
  private val reportedDrops = new AtomicLong(0L)
  private val writerAt = new AtomicLong(0L)
  private val frameAt = new AtomicLong(0L)
  private var selected = ""
  private var verified = false
  private var quoteAt = 0L
  private var quoteTime = Long.MinValue
  private var sourceAge = 0L
  private var request = Option.empty[(Long, Long)]
  private var firstFrame = true
  private var health = ""
  private var healthAt = 0L
  private var frameStalled = false
  private var switchStalled = false
  private var executionSeen = Option.empty[((String, String), Long, Long)]
  // The live worker's ignored order updates and orders not found, as the last live frame saw
  // them. A new worker starts from zero, so each rise is written once.
  private var ordersSeen = (0L, 0L)
  private val writer = Threads.daemon("desk-diagnostics")(run())

  private def emit(kind: String, fields: (String, ujson.Value)*): Unit =
    if !closed.get() then
      val record = ujson.Obj("at" -> now(), "event" -> kind)
      fields.foreach((key, value) => record(key) = value)
      if !queue.offer(record) then dropped.incrementAndGet()

  def droppedCount: Long = dropped.get()
  def unavailable: Boolean = failed.get() || dropped.get() != reportedDrops.get() ||
    (started.get() && !closed.get() && monotonic() - writerAt.get() > 3000L)
  def status: String = if unavailable then "Logs unavailable" else ""

  def start(): Unit =
    if !closed.get() && started.compareAndSet(false, true) then
      writerAt.set(monotonic())
      frameAt.set(monotonic())
      writer.start()

  def slow(part: DiagnosticPart, nanos: Long): Unit =
    if nanos >= 50000000L then
      emit("slow", "part" -> part.toString, "ms" -> (nanos / 1000000L), "queued" -> queue.size())

  def failure(part: DiagnosticPart, error: Throwable): Unit =
    val name = error.getClass.getSimpleName.filter(c => c.isLetterOrDigit || c == '_').take(80)
    emit("exception", "part" -> part.toString, "class" -> name)

  def execution(mode: String, view: TradingView): Unit = synchronized {
    val kind = mode match
      case "PROP" => "prop"
      case "LIVE" => "live"
      case "VIEW" => "view"
      case _      => "unknown"
    val state = if kind == "view" then "view"
    else if view.pending then "pending"
    else if view.ready then "ready"
    else
      view.blockCode match
        case BlockCode.Market                        => "market"
        case BlockCode.Account                       => "account"
        case BlockCode.Leverage                      => "leverage"
        case BlockCode.Storage                       => "storage"
        case BlockCode.Prices | BlockCode.Connection => "prices"
        case BlockCode.Blocked                       => "blocked"
    if kind == "live" then
      val (ignored, missing) = ordersSeen
      if view.ignoredOrderUpdates > ignored then
        emit(
          "order_updates_ignored",
          "ignoredOrderUpdates" -> ujson.Num(view.ignoredOrderUpdates.toDouble)
        )
      if view.notFoundOrders > missing then
        emit("order_not_found", "count" -> ujson.Num((view.notFoundOrders - missing).toDouble))
      ordersSeen = (view.ignoredOrderUpdates, view.notFoundOrders)
    val key = (kind, state)
    val elapsed = monotonic()
    val changed = executionSeen.forall(_._1 != key)
    val began = if changed then elapsed else executionSeen.get._2
    if changed || executionSeen.exists(s => elapsed - s._3 >= 30000L) then
      emit(
        "execution",
        "mode" -> kind,
        "state" -> state,
        "state_ms" -> (elapsed - began).max(0L),
        "previous_ms" -> (if changed then executionSeen.fold(0L)(s => (elapsed - s._2).max(0L))
                          else 0L)
      )
      executionSeen = Some((key, began, elapsed))
  }

  def folded(before: Snapshot, event: Event, after: Snapshot): Unit = synchronized {
    if selected.isEmpty then selected = before.coin
    val time = now()
    val elapsed = monotonic()
    if before.marketPending != after.marketPending then
      after.marketPending match
        case Some((id, _)) =>
          request = Some(id -> elapsed)
          emit("switch_requested", "request" -> id)
        case None =>
          request.foreach { (id, at) =>
            val result = event match
              case Event.MarketChecked(_, Right(_)) => "accepted"
              case Event.MarketChecked(_, Left(_))  => "rejected"
              case _                                => "cancelled"
            emit(
              "switch_checked",
              "request" -> id,
              "result" -> result,
              "ms" -> (elapsed - at).max(0L)
            )
          }
          request = None
    if before.coin != after.coin then
      emit(
        "switch_committed",
        "from" -> (if verified then ujson.Str(selected) else ujson.Null),
        "to" -> after.coin,
        "samples" -> after.samples.size,
        "candles" -> ujson.Obj.from(
          State.intervals.map(i => i -> ujson.Num(after.candles.getOrElse(i, Vector.empty).size))
        ),
        "tape" -> after.tape.size,
        "chart" -> after.chartMode.toString,
        "offset" -> after.offsetSec
      )
      selected = after.coin
      verified = true
      quoteAt = 0L
      quoteTime = Long.MinValue
      health = ""
      firstFrame = true
    def quote(coin: String, bidText: String, askText: String, at: Long): Unit =
      val bid = bidText.toDoubleOption.getOrElse(Double.NaN)
      val ask = askText.toDoubleOption.getOrElse(Double.NaN)
      if coin == selected && bid.isFinite && ask.isFinite && bid > 0 && ask > bid &&
        at > quoteTime && BigInt(time) >= BigInt(at) - 1000L && BigInt(time) - at <= quoteAgeMs
      then
        verified = true
        quoteAt = elapsed
        quoteTime = at
        sourceAge = (time - at).max(0L)
    event match
      case Event.BookUpdate(c, book, _) if book.bids.nonEmpty && book.asks.nonEmpty =>
        quote(c, book.bids.head.px, book.asks.head.px, book.time)
      case Event.BboUpdate(c, bid, ask, at) => quote(c, bid.px, ask.px, at)
      case Event.CandleHistory(c, interval, candles, requestedAt)
          if State.intervals.contains(
            interval
          ) && ((c == selected && verified) || State.refCoins.contains(c)) =>
        val retained =
          if c == selected then after.candles else after.refs.getOrElse(c, Ref.empty).candles
        emit(
          "history",
          "market" -> c,
          "interval" -> interval,
          "received" -> candles.size,
          "retained" -> retained.getOrElse(interval, Vector.empty).size,
          "ms" -> (time - requestedAt).max(0L)
        )
      case Event.CandleHistoryFailed => emit("history_failed")
      case Event.SocketDown          => emit("connection", "state" -> "disconnected")
      case Event.SocketUp            => emit("connection", "state" -> "connected")
      case Event.Bad                 => emit("invalid_public_event")
      case _                         => ()
  }

  def presented(market: Snapshot, cols: Int, rows: Int): Unit = synchronized {
    frameAt.set(monotonic())
    if firstFrame && market.coin == selected && verified then
      emit(
        "frame",
        "market" -> selected,
        "cols" -> cols,
        "rows" -> rows,
        "samples" -> market.samples.size,
        "candles" -> market.candles.valuesIterator.map(_.size).sum
      )
      firstFrame = false
  }

  private[desk] def tick(): Unit = synchronized {
    val time = monotonic()
    val age = (time - quoteAt).max(0L) + sourceAge
    val current = if quoteAt == 0L then "missing"
    else if time >= quoteAt && age <= quoteAgeMs then "fresh"
    else "stale"
    if current != health || time - healthAt >= 30000L then
      emit(
        "quote_health",
        "market" -> (if verified then ujson.Str(selected) else ujson.Null),
        "state" -> current,
        "age_ms" -> (if quoteAt == 0L then ujson.Null else ujson.Num(age.toDouble))
      )
      health = current
      healthAt = time
    val stalled = time - frameAt.get() > 2000L
    if stalled != frameStalled then
      emit("frame_health", "state" -> (if stalled then "stalled" else "recovered"))
      frameStalled = stalled
    val pending = request.exists((_, at) => time - at > 10000L)
    if pending != switchStalled then
      emit("switch_health", "state" -> (if pending then "stalled" else "recovered"))
      switchStalled = pending
  }

  private def run(): Unit =
    var sink = Option.empty[Diagnostics.Sink]
    try
      sink = Some(openSink())
      emit("started", "build" -> Diagnostics.build)
      var sampled = 0L
      while !closed.get() || !queue.isEmpty do
        writerAt.set(monotonic())
        if !closed.get() && monotonic() - sampled >= 1000L then
          tick()
          sampled = monotonic()
        val record = queue.poll(100L, TimeUnit.MILLISECONDS)
        if record != null then sink.get.append(ujson.write(record))
        val lost = dropped.get()
        if lost != reportedDrops.get() then
          sink.get.append(
            ujson.write(ujson.Obj("at" -> now(), "event" -> "dropped", "count" -> lost))
          )
          reportedDrops.set(lost)
    catch
      case _: InterruptedException => ()
      case _: Exception            => failed.set(true)
    finally sink.foreach(s => Try(s.close()))

  def close(): Unit =
    if closed.compareAndSet(false, true) && started.get() then
      writer.join(500L)
      if writer.isAlive then
        failed.set(true)
        writer.interrupt()
