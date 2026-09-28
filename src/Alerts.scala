package desk

import java.nio.charset.StandardCharsets
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import scala.collection.mutable
import scala.util.Try

final case class Announcement(text: String)

final class AlertPolicy:
  private final case class Point(at: Long, price: BigDecimal)
  private val hour = 3600000L
  private val freshness = 5000L
  private val points = mutable.ArrayDeque.empty[Point]
  private var coin = ""
  private var observedAt = Option.empty[Long]
  private var lastQuote = Option.empty[Long]
  private var up = false
  private var down = false
  private var lastUp = Option.empty[Long]
  private var lastDown = Option.empty[Long]

  private def reset(): Unit =
    points.clear()
    lastQuote = None
    up = false
    down = false
    lastUp = None
    lastDown = None

  private def live(snapshot: Snapshot, now: Long): Option[Point] =
    def fresh(at: Long): Boolean = at >= 0 && at <= now && now - at <= freshness
    val connected = snapshot.feed match
      case Health.Live(at) => fresh(at)
      case _               => false
    def price(text: String): Option[BigDecimal] =
      Option.when(text.length <= 80)(text).flatMap { value =>
        Try(value.toDouble).toOption
          .filter(n => n.isFinite && n > 0)
          .flatMap(_ => Try(BigDecimal(value)).toOption)
      }
    for
      book <- snapshot.book if connected && fresh(book.time)
      bid <- book.bids.headOption.flatMap(level => price(level.px))
      ask <- book.asks.headOption.flatMap(level => price(level.px))
      if bid <= ask
    yield Point(book.time, (bid + ask) / 2)

  def market(snapshot: Snapshot, now: Long): Vector[Announcement] =
    synchronized {
      if coin != snapshot.coin || observedAt.exists(t => now < t || now - t > freshness)
      then
        reset()
        coin = snapshot.coin
      observedAt = Some(now)
      live(snapshot, now) match
        case None =>
          reset()
          Vector.empty
        case Some(current) if lastQuote.exists(_ >= current.at) => Vector.empty
        case Some(current)                                      =>
          if lastQuote.exists(t => current.at - t > freshness) then reset()
          while points.headOption.exists(p => current.at - p.at >= hour) do points.removeHead()
          val previous = points.lastOption
          val low = points.minByOption(_.price)
          val high = points.maxByOption(_.price)
          val gain = low.map(p => (current.price - p.price) / p.price)
          val loss = high.map(p => (p.price - current.price) / p.price)
          if gain.forall(_ < BigDecimal("0.04")) then up = false
          if loss.forall(_ < BigDecimal("0.04")) then down = false
          def cooled(last: Option[Long]): Boolean =
            last.forall(t => now >= t && now - t >= 60000L)
          val signal =
            if previous.exists(_.price < current.price) && gain.exists(
                _ > BigDecimal("0.05")
              ) && !up && cooled(lastUp)
            then
              up = true
              lastUp = Some(now)
              Some(("up", low.get, gain.get))
            else if previous.exists(_.price > current.price) && loss.exists(
                _ > BigDecimal("0.05")
              ) && !down && cooled(lastDown)
            then
              down = true
              lastDown = Some(now)
              Some(("down", high.get, loss.get))
            else None
          points.append(current)
          while points.size > 16000 do points.removeHead()
          lastQuote = Some(current.at)
          signal.toVector.map { (direction, origin, fraction) =>
            val elapsed = (current.at - origin.at) / 1000L
            val period = if elapsed == 0 then "under 1s"
            else if elapsed < 60 then s"${elapsed}s"
            else s"${elapsed / 60}m ${elapsed % 60}s"
            val change = f"${(fraction * 100).toDouble}%.2f"
            Announcement(
              s"${Workspace.safeText(snapshot.coin).take(40)} $direction $change% in $period"
            )
          }
    }

trait AnnouncementOutput extends AutoCloseable:
  def play(message: String, voice: Boolean, canceled: () => Boolean): Unit

object NativeAnnouncementOutput:
  def create(): AnnouncementOutput = new NativeOutput
  private[desk] def speechText(text: String): String =
    Workspace
      .safeText(text)
      .replaceAll("(?s)\\[\\[.*?\\]\\]", " ")
      .replace('[', ' ')
      .replace(']', ' ')
      .replaceAll("\\s+", " ")
      .trim
      .take(240)

  private class NativeOutput extends AnnouncementOutput:
    private val closed = new AtomicBoolean(false)
    private val active = new AtomicReference[Process]()
    private def run(
        command: Vector[String],
        input: Option[String],
        canceled: () => Boolean,
        deadline: Long
    ): Unit =
      if !closed.get() && !canceled() then
        require(System.nanoTime() < deadline, "Announcement timed out")
        val process = new ProcessBuilder(command*)
          .redirectOutput(ProcessBuilder.Redirect.DISCARD)
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        var interrupted = false
        try
          require(
            active.compareAndSet(null, process),
            "Another announcement is active"
          )
          val stdin = process.getOutputStream
          try
            input.foreach(text => stdin.write(text.getBytes(StandardCharsets.UTF_8)))
          finally stdin.close()
          while process.isAlive && !closed.get() && !canceled() && System
              .nanoTime() < deadline
          do
            val _ = process.waitFor(50L, TimeUnit.MILLISECONDS)
          if !closed.get() && !canceled() then
            require(
              !process.isAlive && process.exitValue() == 0,
              "Announcement audio unavailable"
            )
        catch case _: InterruptedException => interrupted = true
        finally
          if process.isAlive then process.destroy()
          try
            if !process.waitFor(500L, TimeUnit.MILLISECONDS) then
              val _ = process.destroyForcibly()
              val _ = process.waitFor(500L, TimeUnit.MILLISECONDS)
          catch
            case _: InterruptedException =>
              interrupted = true
              val _ = process.destroyForcibly()
          val _ = active.compareAndSet(process, null)
          if interrupted then Thread.currentThread().interrupt()

    def play(message: String, voice: Boolean, canceled: () => Boolean): Unit =
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(15L)
      run(
        Vector("/usr/bin/afplay", "/System/Library/Sounds/Glass.aiff"),
        None,
        canceled,
        deadline
      )
      if voice then
        run(
          Vector("/usr/bin/say", "-f", "-"),
          Some(speechText(message)),
          canceled,
          deadline
        )

    def close(): Unit =
      closed.set(true)
      Option(active.get()).foreach(_.destroy())

final class Announcements(
    output: AnnouncementOutput,
    voice: Boolean = true,
    mutedInitially: Boolean = false
) extends AutoCloseable:
  private final case class Pending(value: Announcement, generation: Long)
  private val queue = new ArrayBlockingQueue[Pending](4)
  private val closed = new AtomicBoolean(false)
  private val started = new AtomicBoolean(false)
  private val silence = new AtomicBoolean(mutedInitially)
  private val generation = new AtomicLong(0L)
  private var thread = Option.empty[Thread]
  def muted: Boolean = silence.get()

  def offer(value: Announcement): Boolean = synchronized {
    if closed.get() || muted || value.text.isEmpty then false
    else
      queue.offer(
        Pending(value.copy(text = value.text.take(500)), generation.get())
      )
  }

  def setMuted(value: Boolean): Unit = synchronized {
    silence.set(value)
    if value then
      generation.incrementAndGet()
      queue.clear()
  }

  def start(): Unit = synchronized {
    if !closed.get() && started.compareAndSet(false, true) then
      val worker = new Thread(
        () => {
          try
            while !closed.get() do
              val job = queue.poll(100L, TimeUnit.MILLISECONDS)
              if job != null then
                val canceled = () => closed.get() || muted || generation.get() != job.generation
                if !canceled() then
                  try output.play(job.value.text, voice, canceled)
                  catch case _: Exception => ()
          catch case _: InterruptedException => ()
        },
        "desk-announcements"
      )
      worker.setDaemon(true)
      thread = Some(worker)
      worker.start()
  }

  def close(): Unit =
    val current = synchronized {
      closed.set(true)
      generation.incrementAndGet()
      queue.clear()
      thread
    }
    current.foreach(_.interrupt())
    output.close()
    current.foreach(_.join(2000L))
