package desk

import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

final class PublicRecorder(archive: MarketArchive, wall: () => Long, capacity: Int = 8192)
    extends AutoCloseable:
  private val queue = new ArrayBlockingQueue[(Event, Long)](capacity)
  private val running = new AtomicBoolean(false)
  private val stopping = new AtomicBoolean(false)
  private val dropped = new AtomicLong(0L)
  private val hadGap = new AtomicBoolean(false)
  @volatile private var text = "Public capture starting"
  private var thread = Option.empty[Thread]
  def status: String =
    val current = text
    if hadGap.get() && !current.contains("gap") then current + ". History has a gap" else current
  def observe(event: Event): Unit = synchronized {
    if running.get() && !stopping.get() && !queue.offer((event, wall())) then
      hadGap.set(true)
      val _ = dropped.incrementAndGet()
  }
  def start(): Unit = synchronized {
    if !stopping.get() && running.compareAndSet(false, true) then
      val worker = new Thread(
        () => {
          var checkpointAt = wall()
          try
            while !stopping.get() || !queue.isEmpty do
              val item = queue.poll(250, TimeUnit.MILLISECONDS)
              if item != null then
                if dropped.getAndSet(0L) > 0 then
                  archive.record(Event.Bad("recorder queue overflow"), item._2)
                archive.record(item._1, item._2)
              if wall() - checkpointAt >= 10000L then
                archive.checkpoint()
                checkpointAt = wall()
                text = "Public capture saved " + archive.bytes / 1000000L + " MB"
            if dropped.getAndSet(0L) > 0 then
              archive.record(Event.Bad("recorder queue overflow"), wall())
            archive.close()
            text = "Public capture saved"
          catch
            case _: Exception =>
              text = "Public capture failed. History has a gap"
              try archive.close()
              catch case _: Exception => ()
          finally
            stopping.set(true)
            running.set(false)
        },
        "public-recorder"
      )
      worker.setDaemon(true)
      thread = Some(worker)
      worker.start()
  }
  def close(): Unit =
    val current = synchronized {
      stopping.set(true)
      thread
    }
    current match
      case Some(worker) =>
        worker.join(5000L)
        if worker.isAlive then
          text = "Public capture stopping with unsaved events"
          worker.interrupt()
          worker.join(1000L)
      case None => archive.close()
