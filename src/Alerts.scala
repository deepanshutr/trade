package desk

import java.net.URI
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import scala.collection.mutable
import scala.util.Try

final case class Announcement(
    id: String,
    at: Long,
    kind: String,
    text: String,
    url: Option[String] = None,
    publishedAt: Option[Long] = None,
    retrievedAt: Option[Long] = None
)

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
              s"move:${snapshot.coin}:$direction:${current.at}",
              now,
              "market",
              s"${Workspace.safeText(snapshot.coin).take(40)} $direction $change% in $period"
            )
          }
    }

  private val newsSources = Map(
    News.crypto -> ("Crypto", "www.coindesk.com"),
    News.gold -> ("Gold", "www.fxstreet.com"),
    News.war -> ("World", "news.un.org"),
    News.zcash -> ("Zcash", "zfnd.org"),
    News.monero -> ("Monero", "www.getmonero.org")
  )
  private val seeded = mutable.Set.empty[String]
  private val seen = mutable.LinkedHashMap.empty[String, Long]
  private var newsAt = Option.empty[Long]
  private val goldWords = "(?i)\\b(?:gold|bullion|xau(?:usd)?)\\b".r

  private def identity(item: NewsItem, host: String): Option[String] = Try {
    require(item.url.length <= 2048)
    val uri = URI.create(item.url)
    require(
      uri.getScheme == "https" && uri.getHost == host && uri.getUserInfo == null && Set(
        -1,
        443
      ).contains(uri.getPort)
    )
    val query = Option(uri.getRawQuery).toVector
      .flatMap(_.split('&'))
      .filterNot { part =>
        val key = part.takeWhile(_ != '=').toLowerCase(java.util.Locale.ROOT)
        key.startsWith("utm_") || Set(
          "at_medium",
          "at_campaign",
          "fbclid",
          "gclid"
        ).contains(key)
      }
      .mkString("&")
    val canonical = "https://" + host + Option(uri.getRawPath)
      .filter(_.nonEmpty)
      .getOrElse("/") +
      (if query.isEmpty then "" else "?" + query)
    val digest = MessageDigest
      .getInstance("SHA-256")
      .digest(canonical.getBytes(StandardCharsets.UTF_8))
    java.util.HexFormat.of().formatHex(digest)
  }.toOption

  def news(
      batches: Map[String, Vector[NewsItem]],
      now: Long
  ): Vector[Announcement] = synchronized {
    if newsAt.exists(_ > now) then
      seeded.clear()
      seen.clear()
    newsAt = Some(now)
    seen.filterInPlace((_, at) => at <= now && now - at <= 86400000L)
    val result = Vector.newBuilder[Announcement]
    newsSources.toVector.sortBy(_._1).foreach { (source, descriptor) =>
      batches.get(source).foreach { items =>
        val first = seeded.add(source)
        items.take(100).foreach { item =>
          identity(item, descriptor._2).foreach { id =>
            val unseen = !seen.contains(id)
            seen.update(id, now)
            val recent = item.publishedAt
              .exists(t => t >= 0 && t <= now && now - t <= hour)
            val retrieved =
              item.retrievedAt >= 0 && item.retrievedAt <= now && now - item.retrievedAt <= 1800000L
            val relevant = source != News.gold || goldWords
              .findFirstIn(item.title.take(500))
              .nonEmpty
            val title = Workspace
              .safeText(item.title)
              .replaceAll("\\s+", " ")
              .trim
              .take(180)
            if !first && unseen && recent && retrieved && relevant && title.nonEmpty
            then
              result += Announcement(
                "news:" + id,
                now,
                "news",
                descriptor._1 + ": " + title,
                Some(item.url),
                item.publishedAt,
                Some(item.retrievedAt)
              )
          }
        }
      }
    }
    while seen.size > 10000 do seen.remove(seen.head._1)
    result.result()
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
  private val dropped = new AtomicLong(0L)
  @volatile private var unavailable = false
  private var thread = Option.empty[Thread]
  def muted: Boolean = silence.get()
  def status: String =
    val state = if closed.get() then "Sound stopped"
    else if muted then "Muted"
    else if unavailable then "Sound unavailable"
    else "Sound on"
    state + (if queue.isEmpty then "" else s". ${queue.size()} queued") +
      (if dropped.get() == 0 then "" else s". ${dropped.get()} dropped")

  def offer(value: Announcement): Boolean = synchronized {
    if closed.get() || muted || value.text.isEmpty then false
    else
      val accepted = queue.offer(
        Pending(value.copy(text = value.text.take(500)), generation.get())
      )
      if !accepted then dropped.incrementAndGet()
      accepted
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
                  try
                    output.play(job.value.text, voice, canceled)
                    if !canceled() then unavailable = false
                  catch
                    case _: InterruptedException =>
                      if !closed.get() then unavailable = true
                    case _: Exception => if !canceled() then unavailable = true
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
