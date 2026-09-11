package desk

import java.nio.file.Files
import java.util.concurrent.{ArrayBlockingQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

class RuntimeMutationFollowupSpec extends munit.FunSuite:
  private def await(latch: CountDownLatch, label: String): Unit =
    val ready = latch.await(5, TimeUnit.SECONDS)
    assert(ready, label)

  test("opening invalid saved messages rejects every invariant and releases the failed lease") {
    val directory = Files.createTempDirectory("desk-message-open-mutation")
    val saved = Vector(DeskMessage(1, 1800000000000L, "analysis", "known good", Some(true)))
    val initial = MessageHistory.open(directory)
    try initial.save(saved)
    finally initial.close()
    val file = directory.resolve("messages.json")
    val valid = Files.readString(file)
    def changed(key: String, value: ujson.Value): ujson.Value =
      val next = ujson.read(valid)
      next(0)(key) = value
      next
    val invalid = Vector(
      changed("id", ujson.Num(-1)),
      changed("at", ujson.Num(-1)),
      changed("kind", ujson.Str("unknown")),
      changed("text", ujson.Str("x" * 30001)),
      ujson.Arr(ujson.read(valid)(0), ujson.read(valid)(0)),
      ujson.Arr.from(Vector.tabulate(301) { id =>
        val value = ujson.read(valid)(0)
        value("id") = ujson.Num(id)
        value
      })
    )
    invalid.foreach { encoded =>
      LocalFiles.write(file, ujson.write(encoded))
      intercept[IllegalArgumentException] {
        val unexpected = MessageHistory.open(directory)
        unexpected.close()
      }
      LocalFiles.write(file, valid)
      val repaired = MessageHistory.open(directory)
      try assertEquals(repaired.messages, saved)
      finally repaired.close()
    }
  }

  test("message count and text limits accept their boundary and reject one beyond it") {
    val directory = Files.createTempDirectory("desk-message-boundary-mutation")
    val valid = Vector.tabulate(300) { id =>
      DeskMessage(id, 1800000000000L, "analysis", if id == 0 then "x" * 30000 else "saved")
    }
    val history = MessageHistory.open(directory)
    try
      history.save(valid)
      intercept[IllegalArgumentException](history.save(valid :+ valid.last.copy(id = 300)))
      assertEquals(history.messages, valid)
      intercept[IllegalArgumentException](history.save(Vector(valid.head.copy(text = "x" * 30001))))
      assertEquals(history.messages, valid)
    finally history.close()
    val reopened = MessageHistory.open(directory)
    try assertEquals(reopened.messages, valid)
    finally reopened.close()
  }

  test("close saves a dropped marker published after the consumer drained its last queued event") {
    val directory = Files.createTempDirectory("desk-recorder-final-gap-mutation")
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

  test("close between retrieval and parsing stops later sources and the final refresh sleep") {
    def body(source: String): String =
      if source == News.bls then
        "BEGIN:VCALENDAR\r\nBEGIN:VEVENT\r\nUID:jobs\r\nDTSTART;TZID=America/New_York:20260912T083000\r\nSUMMARY:Public release\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
      else if source == News.fedCalendar then
        "<a>2026 FOMC Meetings</a><div class=\"fomc-meeting__month\"><strong>September</strong></div><div class=\"fomc-meeting__date\">15-16</div>"
      else
        s"<rss><channel><item><title>Public headline</title><link>$source</link><pubDate>Fri, 11 Sep 2026 12:00:00 GMT</pubDate></item></channel></rss>"
    Vector(1, 4).foreach { stopAfter =>
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val interrupted = new CountDownLatch(1)
      val ticks = new AtomicInteger()
      val calls = new AtomicInteger()
      val active = new java.util.concurrent.CopyOnWriteArrayList[Thread]()
      val worker = new News.Worker(
        () => {
          if ticks.incrementAndGet() == stopAfter then
            entered.countDown()
            try release.await()
            catch case _: InterruptedException => interrupted.countDown()
          1800000000000L
        },
        Some(source => {
          active.add(Thread.currentThread())
          calls.incrementAndGet()
          body(source)
        })
      )
      try
        worker.start()
        await(entered, "worker did not reach the selected retrieval boundary")
        worker.close()
        await(interrupted, "close did not interrupt the worker")
        assertEquals(calls.get(), stopAfter)
        assert(
          !active.asScala.exists(_.isAlive),
          "closed worker remained alive after close returned"
        )
      finally
        release.countDown()
        worker.close()
        active.asScala.foreach(_.interrupt())
        active.asScala.foreach(_.join(1000L))
    }
  }
