package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import scala.jdk.CollectionConverters.*

class StartupRaceFinderSpec extends munit.FunSuite:
  private val at = 1800000000000L

  test("a delayed rejection for old XMR cannot close a later XMR selection") {
    val release = new CountDownLatch(1)
    val returned = new CountDownLatch(1)
    val opened = new CountDownLatch(1)
    val closed = new CountDownLatch(1)
    val events = new ConcurrentLinkedQueue[Event]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        try
          require(release.await(4L, TimeUnit.SECONDS))
          Some("null")
        finally returned.countDown()
    val connector = new Connector:
      def connect(text: String => Unit, ended: () => Unit): Socket =
        opened.countDown()
        new Socket:
          def send(body: String): Unit = ()
          def close(): Unit = closed.countDown()
    val feed = new Feed(connector, rest, events, () => at, "XMR")
    val thread = new Thread(() =>
      try feed.runForever(_ => throw new InterruptedException())
      catch case _: InterruptedException => ()
    )
    thread.setDaemon(true)
    thread.start()
    try
      assert(opened.await(1500L, TimeUnit.MILLISECONDS))
      feed.setMarket("BTC")
      feed.setMarket("XMR")
      release.countDown()
      assert(returned.await(1L, TimeUnit.SECONDS))
      assert(!closed.await(100L, TimeUnit.MILLISECONDS), "Old XMR validation closed reselected XMR")
      assert(!events.asScala.exists(_.isInstanceOf[Event.MarketUnavailable]))
    finally
      release.countDown()
      thread.interrupt()
      thread.join(3000L)
      assert(!thread.isAlive)
  }

  test("a late rejection during socket opening cannot publish connection up") {
    val releaseLookup = new CountDownLatch(1)
    val lookupReturned = new CountDownLatch(1)
    val socketEntered = new CountDownLatch(1)
    val releaseSocket = new CountDownLatch(1)
    val socketClosed = new CountDownLatch(1)
    val events = new ConcurrentLinkedQueue[Event]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        try
          require(releaseLookup.await(4L, TimeUnit.SECONDS))
          Some("null")
        finally lookupReturned.countDown()
    val connector = new Connector:
      def connect(text: String => Unit, ended: () => Unit): Socket =
        socketEntered.countDown()
        require(releaseSocket.await(4L, TimeUnit.SECONDS))
        new Socket:
          def send(body: String): Unit = ()
          def close(): Unit = socketClosed.countDown()
    val feed = new Feed(connector, rest, events, () => at, "XMR")
    val thread = new Thread(() =>
      try feed.runForever(_ => throw new InterruptedException())
      catch case _: InterruptedException => ()
    )
    thread.setDaemon(true)
    thread.start()
    try
      assert(socketEntered.await(1500L, TimeUnit.MILLISECONDS))
      releaseLookup.countDown()
      assert(lookupReturned.await(1L, TimeUnit.SECONDS))
      val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(1L)
      while !events.asScala
          .exists(_.isInstanceOf[Event.MarketUnavailable]) && System.nanoTime() < end
      do Thread.sleep(2L)
      assert(events.asScala.exists(_.isInstanceOf[Event.MarketUnavailable]))
      releaseSocket.countDown()
      assert(socketClosed.await(1L, TimeUnit.SECONDS))
      thread.join(3000L)
      assert(!thread.isAlive)
      assert(!events.asScala.exists(_ == Event.SocketUp))
    finally
      releaseLookup.countDown()
      releaseSocket.countDown()
      thread.interrupt()
      thread.join(3000L)
      assert(!thread.isAlive)
  }
