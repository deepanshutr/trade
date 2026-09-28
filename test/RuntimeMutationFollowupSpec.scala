package desk

import java.nio.file.Files
import java.util.concurrent.{ArrayBlockingQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

class RuntimeMutationFollowupSpec extends TempDirectorySuite:
  private def await(latch: CountDownLatch, label: String): Unit =
    val ready = latch.await(5, TimeUnit.SECONDS)
    assert(ready, label)

  test("close saves a dropped marker published after the consumer drained its last queued event") {
    val directory = tempDirectory("desk-recorder-final-gap-mutation")
    val startupEntered = new CountDownLatch(1)
    val releaseStartup = new CountDownLatch(1)
    val failedOfferEntered = new CountDownLatch(1)
    val releaseFailedOffer = new CountDownLatch(1)
    val drained = new CountDownLatch(1)
    val releaseDrained = new CountDownLatch(1)
    val ticks = new AtomicInteger()
    val queue = new ArrayBlockingQueue[(Event, Long)](1):
      override def offer(value: (Event, Long)): Boolean =
        val accepted = super.offer(value)
        if !accepted then
          failedOfferEntered.countDown()
          releaseFailedOffer.await()
        accepted
    val recorder = new PublicRecorder(
      MarketArchive.open(directory),
      () => {
        if Thread.currentThread().getName == "public-recorder" then
          ticks.incrementAndGet() match
            case 1 =>
              startupEntered.countDown()
              releaseStartup.await()
            case 2 =>
              drained.countDown()
              releaseDrained.await()
            case _ => ()
        1800000000000L
      },
      capacity = 1
    )
    val queueField = classOf[PublicRecorder].getDeclaredField("queue")
    queueField.setAccessible(true)
    queueField.set(recorder, queue)
    val producer = new Thread(() => recorder.observe(Event.SocketDown), "late-drop-producer")
    val closer = new Thread(() => recorder.close(), "late-drop-closer")
    try
      recorder.start()
      await(startupEntered, "worker did not enter startup")
      recorder.observe(Event.SocketUp)
      producer.start()
      await(failedOfferEntered, "offer did not fail on the full queue")
      releaseStartup.countDown()
      await(drained, "worker did not drain the queued item")
      releaseFailedOffer.countDown()
      producer.join(3000L)
      assert(!producer.isAlive)
      closer.start()
      val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while closer.getState != Thread.State.TIMED_WAITING && System.nanoTime() < until do
        Thread.sleep(5L)
      assertEquals(closer.getState, Thread.State.TIMED_WAITING)
      releaseDrained.countDown()
      closer.join(3000L)
      assert(!closer.isAlive)
      val paths = Files.list(directory)
      val contents = try
        paths
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".jsonl"))
          .map(Files.readString(_))
          .mkString
      finally paths.close()
      assert(contents.contains("invalid-or-dropped-event"), contents)
      assert(recorder.status.toLowerCase.contains("gap"))
    finally
      releaseStartup.countDown()
      releaseFailedOffer.countDown()
      releaseDrained.countDown()
      producer.join(3000L)
      closer.join(3000L)
      recorder.close()
  }
