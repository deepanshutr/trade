package desk

import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

class NewsWorkerSpec extends munit.FunSuite:
  private val now = Instant.parse("2026-09-11T13:00:00Z").toEpochMilli
  private val sources = News.sources.toSet
  private def waitFor(label: String)(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < until do Thread.sleep(5)
    assert(condition, label)
  private def body(source: String): String =
    assert(sources(source), source)
    if source == News.bls then
      "BEGIN:VCALENDAR\r\nBEGIN:VEVENT\r\nUID:jobs\r\nDTSTART;TZID=America/New_York:20260912T083000\r\nSUMMARY:Public release\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
    else if source == News.fedCalendar then
      "<a>2026 FOMC Meetings</a><div class=\"fomc-meeting__month\"><strong>September</strong></div><div class=\"fomc-meeting__date\">15-16</div>"
    else
      s"<rss><channel><item><title>${
          if source == News.monero then "Cached Monero headline" else "Current Fed headline"
        }</title><link>$source</link><pubDate>Fri, 11 Sep 2026 12:00:00 GMT</pubDate></item></channel></rss>"

  test("news status ages with successful retrieval and detects a backward clock") {
    val clock = new AtomicLong(now)
    val worker = new News.Worker(() => clock.get(), Some(body), refreshEveryMs = 600000L)
    try
      worker.start()
      waitFor("news did not load")(worker.status == "News sources updated")
      clock.set(now + 1800001L)
      assert(worker.status.toLowerCase.contains("stale"), worker.status)
      assert(worker.context.toLowerCase.contains("stale"))
      clock.set(now - 1)
      assert(worker.status.toLowerCase.contains("stale"), worker.status)
      assert(worker.context.toLowerCase.contains("stale"))
    finally worker.close()
  }

  test(
    "a failed source retains its dated cache with a visible failure and original retrieval time"
  ) {
    val clock = new AtomicLong(now)
    val fail = new AtomicBoolean(false)
    val worker = new News.Worker(
      () => clock.get(),
      Some(source => {
        if source == News.monero && fail.get() then
          throw new java.io.IOException("synthetic unavailable source")
        body(source)
      }),
      refreshEveryMs = 50L
    )
    try
      worker.start()
      waitFor("initial news missing")(worker.status == "News sources updated")
      fail.set(true)
      clock.set(now + 600000L)
      waitFor("source failure missing")(worker.status.contains("unavailable"))
      val text = worker.context
      assert(text.contains("Cached Monero headline"), text)
      assert(text.contains("Source unavailable: " + News.monero), text)
      assert(text.contains("retrieved " + Instant.ofEpochMilli(now)), text)
      assert(!text.contains("synthetic unavailable source"))
    finally worker.close()
  }

  test("news start is idempotent and close interrupts its current public fetch") {
    val calls = new AtomicInteger()
    val active = new java.util.concurrent.CopyOnWriteArrayList[Thread]()
    val entered = new CountDownLatch(1)
    val interrupted = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val worker = new News.Worker(
      () => now,
      Some(source => {
        assert(sources(source))
        active.add(Thread.currentThread())
        calls.incrementAndGet()
        entered.countDown()
        try release.await()
        catch
          case error: InterruptedException =>
            interrupted.countDown()
            throw error
        body(source)
      })
    )
    try
      worker.start()
      val began = entered.await(5, TimeUnit.SECONDS)
      assert(began, worker.status + "; calls=" + calls.get())
      worker.start()
      Thread.sleep(100L)
      assertEquals(calls.get(), 1)
      worker.close()
      assert(interrupted.await(1, TimeUnit.SECONDS))
      assertEquals(calls.get(), 1)
      worker.start()
      assertEquals(calls.get(), 1)
    finally
      release.countDown()
      worker.close()
      active.asScala.foreach(_.interrupt())
      active.asScala.foreach(_.join(1000L))
  }

  test("closing does not schedule more sources when a fetch consumes its interruption") {
    val calls = new AtomicInteger()
    val entered = new CountDownLatch(1)
    val interrupted = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val active = new java.util.concurrent.CopyOnWriteArrayList[Thread]()
    val worker = new News.Worker(
      () => now,
      Some(source => {
        active.add(Thread.currentThread())
        if calls.incrementAndGet() == 1 then
          entered.countDown()
          try release.await()
          catch case _: InterruptedException => interrupted.countDown()
        body(source)
      })
    )
    try
      worker.start()
      val began = entered.await(5, TimeUnit.SECONDS)
      assert(began)
      worker.close()
      assert(interrupted.await(1, TimeUnit.SECONDS))
      assertEquals(calls.get(), 1)
    finally
      release.countDown()
      worker.close()
      active.asScala.foreach(_.interrupt())
      active.asScala.foreach(_.join(1000L))
  }
