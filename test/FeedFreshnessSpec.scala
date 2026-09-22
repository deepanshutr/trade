package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import scala.jdk.CollectionConverters.*

class FeedFreshnessSpec extends munit.FunSuite:
  private val at = 1800000000000L
  private val update =
    s"""{"channel":"l2Book","data":{"coin":"XMR","time":$at,"levels":[[{"px":"99","sz":"10","n":1}],[{"px":"101","sz":"10","n":1}]]}}"""

  test("connection-up precedes a price delivered during subscription") {
    val events = new ConcurrentLinkedQueue[Event]()
    val received = new CountDownLatch(1)
    var close: () => Unit = () => ()
    val connector = new Connector:
      def connect(text: String => Unit, ended: () => Unit): Socket =
        close = ended
        new Socket:
          def send(body: String): Unit =
            if body.contains("l2Book") then
              text(update)
              received.countDown()
          def close(): Unit = ()
    val rest = new Rest:
      def post(body: String): Option[String] = None
    val feed = new Feed(connector, rest, events, () => at, "XMR", Vector.empty)
    val thread = new Thread(() => { val _ = feed.connectOnce() })
    thread.setDaemon(true)
    thread.start()
    try
      assert(received.await(3, TimeUnit.SECONDS))
      val captured = events.asScala.toVector
      val price = captured.indexWhere(_.isInstanceOf[Event.BookUpdate])
      assert(price >= 0, captured)
      assertEquals(captured.head, Event.SocketUp)
    finally
      close()
      thread.join(3000)
      assert(!thread.isAlive)
  }

  test("selected market changes finish without waiting for candle history") {
    val release = new CountDownLatch(1)
    val returned = new CountDownLatch(1)
    val rest = new Rest:
      def post(body: String): Option[String] =
        assert(release.await(5, TimeUnit.SECONDS))
        None
    val connector = new Connector:
      def connect(text: String => Unit, ended: () => Unit): Socket =
        throw new UnsupportedOperationException("no connection needed")
    val feed =
      new Feed(connector, rest, new ConcurrentLinkedQueue[Event](), () => at, "XMR", Vector.empty)
    val thread = new Thread(() =>
      try
        feed.setMarket("BTC")
        feed.setMarket("PAXG")
      finally returned.countDown()
    )
    thread.setDaemon(true)
    thread.start()
    try
      assert(returned.await(300, TimeUnit.MILLISECONDS), "history delayed market selection")
      assertEquals(feed.coin, "PAXG")
    finally
      release.countDown()
      thread.join(3000)
      assert(!thread.isAlive)

  }

  test("selected depth requests the fast stream and unsubscribes its exact shape") {
    val subscriptions = Feed.subscriptions("XMR").map(ujson.read(_))
    val books = subscriptions.map(_("subscription")).filter(_("type").str == "l2Book")
    assertEquals(books.size, 1)
    assertEquals(books.head.obj.get("fast").map(_.bool), Some(true))
    val stop = Feed.unsubscriptions("XMR").map(ujson.read(_)).map(_("subscription"))
    assert(stop.contains(books.head))
  }
