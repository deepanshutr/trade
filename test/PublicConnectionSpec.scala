package desk

import java.io.IOException
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

class PublicConnectionSpec extends munit.FunSuite:
  private def await(label: String)(ready: => Boolean): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while !ready && System.nanoTime() < deadline do Thread.sleep(1)
    assert(ready, label)

  private final class Probe extends Connector:
    final class Link extends Socket:
      val sent = new ConcurrentLinkedQueue[String]()
      val failSend = new AtomicBoolean(false)
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      @volatile var hold = false
      var closes = 0
      def send(text: String): Unit =
        if hold then
          entered.countDown()
          release.await()
        if failSend.get() then throw new IOException("public send failed")
        val _ = sent.add(text)
      def close(): Unit = synchronized { closes += 1 }
    val links = new ConcurrentLinkedQueue[Link]()
    val messages = new ConcurrentLinkedQueue[String => Unit]()
    val closures = new ConcurrentLinkedQueue[() => Unit]()
    @volatile var failOpen = false
    @volatile var failInitialSend = false
    @volatile var holdOpen = false
    val openEntered = new CountDownLatch(1)
    val openRelease = new CountDownLatch(1)
    def connect(text: String => Unit, closed: () => Unit): Socket =
      if failOpen then throw new IOException("public open failed")
      val link = new Link
      link.failSend.set(failInitialSend)
      val _ = links.add(link)
      val _ = messages.add(text)
      val _ = closures.add(closed)
      if holdOpen then
        openEntered.countDown()
        openRelease.await()
      link

  private final class Running(feed: Feed):
    val failure = new AtomicReference[Throwable]()
    val result = new AtomicReference[Boolean]()
    val thread = new Thread(() =>
      try result.set(feed.connectOnce())
      catch case error: Throwable => failure.set(error)
    )
    thread.setDaemon(true)
    thread.start()
    def finish(): Unit =
      thread.join(3000)
      assert(
        !thread.isAlive,
        "the owned connection did not finish: " + thread.getStackTrace.mkString(" | ")
      )
      assertEquals(Option(failure.get()), None)

  private val pong = """{"channel":"pong"}"""
  private val quote =
    """{"channel":"bbo","data":{"coin":"ETH","time":1000001,"bbo":[{"px":"20","sz":"1","n":1},{"px":"22","sz":"1","n":1}]}}"""

  private def make(probe: Probe) =
    val queue = new LinkedBlockingQueue[Event]()
    val history = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        val request = ujson.read(body)
        if request("type").str == "l2Book" then
          Some(MarketSelectionSpec.bookResponse(request("coin").str))
        else
          val _ = history.add(body)
          Some("[]")
    (new Feed(probe, rest, queue, () => 1_000_000L, "XMR"), queue, history)

  test("a refused public connection publishes down state and enters backoff") {
    val probe = new Probe
    probe.failOpen = true
    val (feed, queue, _) = make(probe)
    final class Stop extends RuntimeException
    val waits = new ConcurrentLinkedQueue[Long]()
    intercept[Stop] {
      feed.runForever { delay =>
        val _ = waits.add(delay)
        if waits.size() == 3 then throw new Stop
      }
    }
    assertEquals(waits.asScala.toVector, Vector(1000L, 2000L, 4000L))
    assertEquals(queue.asScala.toVector, Vector.fill(3)(Event.SocketDown))
  }

  test("a failed initial subscription closes its socket and resets the open backoff") {
    val probe = new Probe
    probe.failInitialSend = true
    val (feed, queue, history) = make(probe)
    final class Stop extends RuntimeException
    var waited = 0L
    intercept[Stop] {
      feed.runForever { delay =>
        waited = delay
        throw new Stop
      }
    }
    assertEquals(waited, 1000L)
    assertEquals(probe.links.peek().closes, 1)
    assertEquals(queue.asScala.toVector, Vector(Event.SocketUp, Event.SocketDown))
    assert(history.isEmpty)
  }

  for operation <- Vector("heartbeat", "switch") do
    test(s"a failed $operation send closes once and leaves later work usable") {
      val probe = new Probe
      val (feed, queue, history) = make(probe)
      val first = new Running(feed)
      feed.awaitHistoryRepair()
      await("the initial history did not finish")(history.size() == 18)
      val old = probe.links.peek()
      old.failSend.set(true)
      try
        if operation == "heartbeat" then feed.heartbeat() else feed.setMarket("ETH")
        if operation == "switch" then feed.awaitHistoryRepair()
        first.finish()
        assertEquals(first.result.get(), true)
        assertEquals(old.closes, 1)
        assertEquals(queue.asScala.count(_ == Event.SocketDown), 1)
        if operation == "switch" then
          assertEquals(history.size(), 24)
          assert(history.asScala.toVector.takeRight(6).forall(_.contains("\"coin\":\"ETH\"")))
        val second = new Running(feed)
        await("the replacement did not subscribe")(
          probe.links.size() == 2 && probe.links.asScala.last.sent.size() == 24
        )
        val replacement = probe.links.asScala.last
        assertEquals(replacement.sent.asScala.toVector, Feed.subscriptions(feed.coin))
        assertEquals(history.size(), if operation == "switch" then 24 else 18)
        probe.closures.asScala.last()
        second.finish()
      finally
        probe.closures.asScala.foreach(_())
        first.thread.join(3000)
    }

  test("late messages and closure cannot alter the replacement connection") {
    val probe = new Probe
    val (feed, queue, history) = make(probe)
    val first = new Running(feed)
    feed.awaitHistoryRepair()
    await("the first connection did not finish startup")(history.size() == 18)
    val oldText = probe.messages.peek()
    val oldClose = probe.closures.peek()
    oldClose()
    first.finish()
    val second = new Running(feed)
    try
      await("the replacement did not open")(
        probe.links.size() == 2 && queue.asScala.count(_ == Event.SocketUp) == 2
      )
      queue.clear()
      oldText(pong)
      oldText(quote)
      oldClose()
      assert(queue.isEmpty)
      assertEquals(probe.links.asScala.last.closes, 0)
      probe.messages.asScala.last(pong)
      assertEquals(queue.asScala.toVector, Vector(Event.Pong))
      feed.heartbeat()
      assertEquals(probe.links.asScala.last.sent.asScala.last, Feed.ping)
    finally
      probe.closures.asScala.last()
      second.finish()
  }

  test("a reconnect during a held switch keeps the new subscription set and the fold moving") {
    val probe = new Probe
    val (feed, queue, history) = make(probe)
    val state = new AtomicReference(Snapshot.initial("XMR", 1_000_000L))
    val first = new Running(feed)
    feed.awaitHistoryRepair()
    await("the first connection did not finish startup")(history.size() == 18)
    val old = probe.links.peek()
    old.hold = true
    val workers = Main.startFold(queue, state, feed, () => 1_000_000L)
    var second: Option[Running] = None
    try
      queue.put(Event.Key(Command.StartMarketPrompt))
      "ETH".foreach(c => queue.put(Event.Key(Command.PromptChar(c))))
      queue.put(Event.Key(Command.PromptSubmit))
      assert(old.entered.await(3, TimeUnit.SECONDS))
      probe.messages.peek()(quote)
      queue.put(Event.Resize(120, 40))
      queue.put(Event.Key(Command.ZoomOut))
      queue.put(Event.Key(Command.Quit))
      await("the fold stalled behind a public send")(state.get().quit)
      assertEquals(state.get().book.get.bids.head.px, "20")
      assertEquals((state.get().cols, state.get().rows, state.get().zoom), (120, 40, 1))
      assertEquals(old.release.getCount, 1L)
      val disconnect = probe.closures.peek()
      disconnect()
      first.finish()
      second = Some(new Running(feed))
      await("the replacement dial did not start")(probe.links.size() == 2)
      await("the replacement did not wait or start sending")(
        second.exists(_.thread.getState == Thread.State.BLOCKED) ||
          !probe.links.asScala.last.sent.isEmpty
      )
      assert(probe.links.asScala.last.sent.isEmpty)
      old.release.countDown()
      await("the replacement did not subscribe")(probe.links.asScala.last.sent.size() == 24)
      assertEquals(probe.links.asScala.last.sent.asScala.toVector, Feed.subscriptions("ETH"))
      feed.awaitHistoryRepair()
      await("the explicit switch did not fetch all six histories")(history.size() == 24)
      assert(history.asScala.toVector.takeRight(6).forall(_.contains("\"coin\":\"ETH\"")))
      assertEquals(old.closes, 1)
    finally
      old.release.countDown()
      probe.closures.asScala.foreach(_())
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
      first.thread.join(3000)
      second.foreach(_.finish())
  }

  test("public input delivery does not swallow an observer failure") {
    val probe = new Probe
    val queue = new ConcurrentLinkedQueue[Event]()
    val rest = new Rest:
      def post(body: String): Option[String] = Some("[]")
    final class Stop extends RuntimeException
    val feed = new Feed(
      probe,
      rest,
      queue,
      () => 1_000_000L,
      "XMR",
      State.refCoins,
      e => if e == Event.Pong then throw new Stop
    )
    val running = new Running(feed)
    try
      await("the connection did not become ready")(queue.contains(Event.SocketUp))
      intercept[Stop](probe.messages.peek()(pong))
      assert(!queue.contains(Event.SocketDown))
    finally
      val disconnect = probe.closures.peek()
      disconnect()
      running.finish()
  }

  test("a switch before a held reconnect finishes gives the replacement the full current set") {
    val probe = new Probe
    val (feed, queue, history) = make(probe)
    val first = new Running(feed)
    feed.awaitHistoryRepair()
    await("the first history did not finish")(history.size() == 18)
    val closeFirst = probe.closures.peek()
    closeFirst()
    first.finish()
    probe.holdOpen = true
    val second = new Running(feed)
    try
      assert(probe.openEntered.await(3, TimeUnit.SECONDS))
      feed.setMarket("ETH")
      feed.awaitHistoryRepair()
      assertEquals(history.size(), 24)
      assert(probe.links.asScala.last.sent.isEmpty)
      probe.openRelease.countDown()
      await("the replacement did not finish subscription")(
        queue.asScala.count(_ == Event.SocketUp) == 2 && probe.links.asScala.last.sent.size() == 24
      )
      assertEquals(probe.links.asScala.last.sent.asScala.toVector, Feed.subscriptions("ETH"))
      assertEquals(history.size(), 24)
    finally
      probe.openRelease.countDown()
      val closeSecond = probe.closures.asScala.last
      closeSecond()
      second.finish()
  }
