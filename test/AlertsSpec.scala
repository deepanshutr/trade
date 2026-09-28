package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class AlertsSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private def quote(price: String, at: Long, coin: String = "XMR"): Snapshot =
    Snapshot
      .initial(coin, at)
      .copy(
        feed = Health.Live(at),
        book = Some(
          Book(Vector(Level(price, "1", 1)), Vector(Level(price, "1", 1)), at)
        )
      )
  private def checkSoon(message: String)(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while !condition && System.nanoTime() < until do Thread.sleep(5)
    assert(condition, message)

  test("first snapshot is silent even with a large restored sample history") {
    val policy = new AlertPolicy
    val initial =
      quote("110", now).copy(samples = Vector((now / 1000 - 20, 50d)))
    assertEquals(policy.market(initial, now), Vector.empty)
    assertEquals(
      policy.market(quote("110", now + 1000), now + 1000),
      Vector.empty
    )
  }

  test(
    "a subsecond move states its measured period without claiming zero seconds"
  ) {
    val policy = new AlertPolicy
    policy.market(quote("100", now), now)
    val result = policy.market(quote("106", now + 250), now + 250)
    assertEquals(result.size, 1)
    assert(result.head.text.contains("under 1s"), result.head.text)
  }

  test(
    "strict positive and negative five percent thresholds do not round into alerts"
  ) {
    for (boundary, over, direction) <- Vector(
        ("105", "105.0001", "up"),
        ("95", "94.9999", "down")
      )
    do
      val policy = new AlertPolicy
      assertEquals(policy.market(quote("100", now), now), Vector.empty)
      assertEquals(
        policy.market(quote(boundary, now + 1000), now + 1000),
        Vector.empty
      )
      val alerts = policy.market(quote(over, now + 2000), now + 2000)
      assertEquals(alerts.size, 1)
      assert(alerts.head.text.contains(direction), alerts.head.text)
      assertEquals(
        policy.market(quote(over, now + 2000), now + 2000),
        Vector.empty
      )
    end for
  }

  test("exactly one hour old endpoints are excluded, younger endpoints work") {
    for (age, expected) <- Vector((3600000L, 0), (3599999L, 1)) do
      val policy = new AlertPolicy
      policy.market(quote("100", now), now)
      // Observe unchanged intermediate books so there is no stale-data gap.
      (1000L until age by 1000L).foreach { elapsed =>
        val p = if elapsed >= 1000L then "104.9" else "100"
        policy.market(quote(p, now + elapsed), now + elapsed)
      }
      assertEquals(
        policy.market(quote("105.1", now + age), now + age).size,
        expected
      )
  }

  test(
    "repeated threshold crossing waits for reset and cooldown, opposite direction works"
  ) {
    val policy = new AlertPolicy
    policy.market(quote("100", now), now)
    assertEquals(policy.market(quote("106", now + 1000), now + 1000).size, 1)
    assertEquals(
      policy.market(quote("107", now + 2000), now + 2000),
      Vector.empty
    )
    policy.market(quote("103", now + 3000), now + 3000)
    assertEquals(
      policy.market(quote("106", now + 4000), now + 4000),
      Vector.empty
    )
    val opposite = policy.market(quote("100", now + 5000), now + 5000)
    assertEquals(opposite.size, 1)
    assert(opposite.head.text.contains("down"))
    (6000L to 62000L by 1000L).foreach(t => policy.market(quote("103", now + t), now + t))
    assertEquals(policy.market(quote("106", now + 63000), now + 63000).size, 1)
  }

  test("switching market starts a new silent baseline") {
    val policy = new AlertPolicy
    policy.market(quote("100", now), now)
    assertEquals(
      policy.market(quote("500", now + 1000, "ZEC"), now + 1000),
      Vector.empty
    )
    assertEquals(
      policy.market(quote("110", now + 2000), now + 2000),
      Vector.empty
    )
  }

  test(
    "stale, future, disconnected and inconsistent books break the live baseline"
  ) {
    val broken = Vector(
      quote("200", now + 2000).copy(feed = Health.Live(now + 1000)),
      quote("200", now + 1000).copy(feed = Health.Live(now - 5001)),
      quote("200", now + 1000).copy(feed = Health.Live(now + 2000)),
      quote("200", now + 1000).copy(feed = Health.Down(now + 1000, now + 1000)),
      quote("200", now + 1000).copy(book =
        Some(
          Book(
            Vector(Level("201", "1", 1)),
            Vector(Level("200", "1", 1)),
            now + 1000
          )
        )
      ),
      quote("NaN", now + 1000),
      quote("Infinity", now + 1000),
      quote("0", now + 1000),
      quote("-1", now + 1000)
    )
    broken.foreach { snapshot =>
      val policy = new AlertPolicy
      policy.market(quote("100", now), now)
      assertEquals(policy.market(snapshot, now + 1000), Vector.empty)
      assertEquals(
        policy.market(quote("200", now + 2000), now + 2000),
        Vector.empty
      )
    }
  }

  test("newer exchange timestamps still fail when the actual book is stale") {
    val policy = new AlertPolicy
    policy.market(quote("100", now - 4000).copy(feed = Health.Live(now)), now)
    val oldBook = quote("200", now - 3000).copy(feed = Health.Live(now + 3000))
    assertEquals(policy.market(oldBook, now + 3000), Vector.empty)
    assertEquals(
      policy.market(quote("200", now + 4000), now + 4000),
      Vector.empty
    )
  }

  test("a gap between actual book timestamps restarts the baseline") {
    val policy = new AlertPolicy
    policy.market(quote("100", now - 4000).copy(feed = Health.Live(now)), now)
    assertEquals(
      policy.market(quote("200", now + 2000), now + 2000),
      Vector.empty
    )
  }

  test(
    "a long pause without an observed stale frame and a backwards clock reset safely"
  ) {
    val policy = new AlertPolicy
    policy.market(quote("100", now), now)
    assertEquals(
      policy.market(quote("200", now + 6000), now + 6000),
      Vector.empty
    )
    assertEquals(policy.market(quote("100", now - 1), now - 1), Vector.empty)
    assertEquals(policy.market(quote("100", now + 1), now + 1), Vector.empty)
    assertEquals(policy.market(quote("106", now + 2), now + 2).size, 1)
  }

  test("duplicate and older book timestamps cannot replace the endpoint") {
    val policy = new AlertPolicy
    policy.market(quote("100", now), now)
    assertEquals(policy.market(quote("200", now), now + 1), Vector.empty)
    assertEquals(policy.market(quote("200", now - 1), now + 2), Vector.empty)
    assertEquals(
      policy.market(quote("101", now + 1000), now + 1000),
      Vector.empty
    )
  }

  test(
    "speech text strips terminal bytes and speech directives and has a fixed bound"
  ) {
    val text = NativeAnnouncementOutput.speechText(
      "Gold [[volm 0]] rises\u001b[2J\u0007 " + "x" * 400
    )
    assert(!text.contains('[') && !text.contains(']'))
    assert(!text.contains(27.toChar) && !text.contains(7.toChar))
    assert(text.length <= 240)
    assert(text.startsWith("Gold"))
  }

  private class BlockingOutput extends AnnouncementOutput:
    val entered = new CountDownLatch(1)
    val stopped = new CountDownLatch(1)
    val calls = new AtomicInteger()
    val closed = new AtomicBoolean()
    def play(message: String, voice: Boolean, canceled: () => Boolean): Unit =
      calls.incrementAndGet()
      entered.countDown()
      try while !canceled() do Thread.sleep(5)
      finally stopped.countDown()
    def close(): Unit = closed.set(true)

  private def announcement(id: Int): Announcement =
    Announcement(s"Market moved $id")

  test(
    "offer never waits for sound, queue is bounded and repeated starts are harmless"
  ) {
    val output = new BlockingOutput
    val alerts = new Announcements(output)
    try
      alerts.start()
      alerts.start()
      assert(alerts.offer(announcement(1)))
      assert(output.entered.await(3, TimeUnit.SECONDS))
      val began = System.nanoTime()
      (2 to 5).foreach(i => assert(alerts.offer(announcement(i))))
      assert(!alerts.offer(announcement(6)))
      assert(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began) < 1000)
      assertEquals(output.calls.get(), 1)
    finally alerts.close()
    assert(output.closed.get())
  }

  test("muting cancels active and queued sounds without replay after unmute") {
    val output = new BlockingOutput
    val alerts = new Announcements(output)
    try
      alerts.start()
      assert(alerts.offer(announcement(1)))
      assert(output.entered.await(3, TimeUnit.SECONDS))
      assert(alerts.offer(announcement(2)))
      alerts.setMuted(true)
      assert(alerts.muted)
      assert(output.stopped.await(3, TimeUnit.SECONDS))
      assert(!alerts.offer(announcement(3)))
      alerts.setMuted(false)
      Thread.sleep(100)
      assertEquals(output.calls.get(), 1)
      assert(alerts.offer(announcement(4)))
      checkSoon("new announcement did not play")(output.calls.get() == 2)
    finally alerts.close()
  }

  test("initial mute, output failures and close cannot strand the queue") {
    val calls = new AtomicInteger()
    val output = new AnnouncementOutput:
      def play(message: String, voice: Boolean, canceled: () => Boolean): Unit =
        calls.incrementAndGet()
        throw new java.io.IOException("synthetic unavailable audio")
      def close(): Unit = ()
    val alerts = new Announcements(output, mutedInitially = true)
    alerts.start()
    assert(!alerts.offer(announcement(1)))
    alerts.setMuted(false)
    assert(alerts.offer(announcement(2)))
    checkSoon("failed output was not called")(calls.get() == 1)
    assert(alerts.offer(announcement(3)))
    checkSoon("output failure stranded the queue")(calls.get() == 2)
    alerts.close()
    alerts.close()
    alerts.start()
    assert(!alerts.offer(announcement(3)))
    assertEquals(calls.get(), 2)
  }

  test(
    "chime-only selection is preserved and canceled output stays canceled after unmute"
  ) {
    val heardVoice = new AtomicBoolean(true)
    val entered = new CountDownLatch(1)
    val checkCancellation = new CountDownLatch(1)
    val canceledAfterUnmute = new AtomicBoolean(false)
    val finished = new CountDownLatch(1)
    val output = new AnnouncementOutput:
      def play(message: String, voice: Boolean, canceled: () => Boolean): Unit =
        heardVoice.set(voice)
        entered.countDown()
        checkCancellation.await()
        canceledAfterUnmute.set(canceled())
        finished.countDown()
      def close(): Unit = checkCancellation.countDown()
    val alerts = new Announcements(output, voice = false)
    try
      alerts.start()
      assert(alerts.offer(announcement(1)))
      assert(entered.await(3, TimeUnit.SECONDS))
      alerts.setMuted(true)
      alerts.setMuted(false)
      checkCancellation.countDown()
      assert(finished.await(3, TimeUnit.SECONDS))
      assert(!heardVoice.get())
      assert(canceledAfterUnmute.get())
    finally alerts.close()
  }
