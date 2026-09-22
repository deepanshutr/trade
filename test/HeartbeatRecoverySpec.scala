package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

class HeartbeatRecoverySpec extends munit.FunSuite:
  private val pong = """{"channel":"pong"}"""
  private final class Fixture:
    val time = new AtomicLong(0L)
    val wall = new AtomicLong(1800000000000L)
    val events = new ConcurrentLinkedQueue[Event]()
    val replies = new ConcurrentLinkedQueue[String => Unit]()
    val closures = new ConcurrentLinkedQueue[() => Unit]()
    val pings = new AtomicInteger(0)
    val closes = new AtomicInteger(0)
    val subscribed = new CountDownLatch(24)
    @volatile var replyInsideSend = false
    val connector = new Connector:
      def connect(receive: String => Unit, close: () => Unit): Socket =
        val _ = replies.add(receive)
        val _ = closures.add(close)
        new Socket:
          def send(text: String): Unit =
            subscribed.countDown()
            if text == Feed.ping then
              val _ = pings.incrementAndGet()
              if replyInsideSend then receive(pong)
          def close(): Unit = { val _ = closes.incrementAndGet() }
    val rest = new Rest:
      def post(body: String): Option[String] = None
    val feed =
      new Feed(connector, rest, events, () => wall.get(), "XMR", monotonic = () => time.get())
    private var threads = Vector.empty[Thread]
    def open(): Unit =
      val expected = replies.size() + 1
      val thread = new Thread(() => { val _ = feed.connectOnce() })
      thread.setDaemon(true)
      threads :+= thread
      thread.start()
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
      while (replies.size() < expected || events.asScala.count(
          _ == Event.SocketUp
        ) < expected) && System.nanoTime() < deadline
      do Thread.sleep(1)
      assert(subscribed.await(3, TimeUnit.SECONDS))
      assertEquals(replies.size(), expected)
    def finish(): Unit =
      closures.asScala.foreach(_())
      threads.foreach(_.join(3000))
      assert(threads.forall(!_.isAlive))

  test("reply deadline closes once despite wall clock rollback and duplicate callbacks") {
    val f = new Fixture
    try
      f.open()
      f.feed.heartbeat()
      f.wall.set(0L)
      f.time.set(4999L)
      f.feed.heartbeat()
      assertEquals(f.closes.get(), 0)
      f.time.set(5000L)
      f.feed.heartbeat()
      f.closures.peek()()
      f.feed.heartbeat()
      assertEquals(f.closes.get(), 1)
      assertEquals(f.events.asScala.count(_ == Event.SocketDown), 1)
      assert(f.feed.status.contains("heartbeat"))
    finally f.finish()
  }

  test("synchronous and delayed pong replies leave the current connection usable") {
    val f = new Fixture
    try
      f.open()
      f.replyInsideSend = true
      f.feed.heartbeat()
      f.time.set(5000L)
      f.replyInsideSend = false
      f.feed.heartbeat()
      f.time.set(9999L)
      f.replies.peek()(pong)
      f.feed.heartbeat()
      assertEquals(f.closes.get(), 0)
      f.time.set(10000L)
      f.feed.heartbeat()
      assertEquals(f.pings.get(), 3)
    finally f.finish()
  }

  test("obsolete pong cannot rescue a replacement and selection cannot reset its deadline") {
    val f = new Fixture
    try
      f.open()
      f.feed.heartbeat()
      f.time.set(5000L)
      f.feed.heartbeat()
      f.open()
      f.feed.heartbeat()
      f.time.set(9000L)
      f.replies.peek()(pong)
      f.closures.peek()()
      f.feed.setMarket("ETH")
      assertEquals(f.closes.get(), 1)
      f.time.set(10000L)
      f.feed.heartbeat()
      assertEquals(f.closes.get(), 2)
    finally f.finish()
  }

  test("connection status explains a timeout without copying exception text") {
    val connector = new Connector:
      def connect(text: String => Unit, close: () => Unit): Socket =
        throw new java.util.concurrent.TimeoutException("private details must not be displayed")
    val rest = new Rest:
      def post(body: String): Option[String] = None
    val feed = new Feed(connector, rest, new ConcurrentLinkedQueue[Event](), () => 0L, "XMR")
    final class Done extends RuntimeException
    intercept[Done](feed.runForever(_ => throw new Done))
    assertEquals(feed.status, "Connection timed out. Retrying in 1s")
  }
