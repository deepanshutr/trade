package desk

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class FeedRepairSpec extends munit.FunSuite:
  test("reconnect repairs every interval incrementally while respecting the minute floor") {
    val now = new AtomicLong(1800000000000L)
    val requests = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        Some("""[{"t":1799999940000,"o":"100","h":"101","l":"99","c":"100","v":"2"}]""")
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket = new Socket:
        def send(text: String): Unit = ()
        def close(): Unit = ()
    val feed = new Feed(connector, rest, new ConcurrentLinkedQueue[Event](), () => now.get(), "XMR")
    def reconnectAndRepair(): Unit =
      val thread = new Thread(() =>
        try { val _ = feed.connectOnce() }
        catch case _: InterruptedException => ()
      )
      thread.start()
      try feed.awaitHistoryRepair()
      finally
        thread.interrupt()
        thread.join(5000L)
      assert(!thread.isAlive)

    feed.repairHistory()
    assertEquals(requests.size(), 18)
    val firstSuccess = now.get()
    now.addAndGet(10L * Feed.historyEveryMs)
    reconnectAndRepair()
    assertEquals(requests.size(), 36)
    val repaired = requests.asScala.toVector.drop(18).map(body => ujson.read(body)("req"))
    assertEquals(repaired.map(_("interval").str).toSet, State.intervals.toSet)
    repaired.foreach { request =>
      assertEquals(
        request("startTime").num.toLong,
        firstSuccess - 2L * State.intervalMs(request("interval").str)
      )
    }
    now.addAndGet(8000L)
    reconnectAndRepair()
    assertEquals(requests.size(), 36)
    now.addAndGet(Feed.historyEveryMs - 8000L)
    reconnectAndRepair()
    assertEquals(requests.size(), 54)
  }

  test(
    "periodic history repair repeats after a minute without reconnecting or duplicating a cycle"
  ) {
    val now = new AtomicLong(1800000000000L)
    val requests = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        val request = ujson.read(body)("req")
        val step = State.intervalMs(request("interval").str)
        Some(
          ujson.write(
            ujson.Arr(
              ujson.Obj(
                "t" -> ujson.Num((now.get() - step).toDouble),
                "o" -> "100",
                "h" -> "101",
                "l" -> "99",
                "c" -> "100",
                "v" -> "2"
              )
            )
          )
        )
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new UnsupportedOperationException("no socket required for repair")
    val events = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(connector, rest, events, () => now.get(), "XMR")
    val method = classOf[Feed].getMethod("repairHistory")
    method.invoke(feed)
    assertEquals(requests.size(), 18)
    method.invoke(feed)
    assertEquals(requests.size(), 18)
    now.addAndGet(Feed.historyEveryMs)
    method.invoke(feed)
    assertEquals(requests.size(), 21)
    now.addAndGet(4L * Feed.historyEveryMs)
    method.invoke(feed)
    assertEquals(requests.size(), 27)
    assertEquals(events.asScala.count(_.isInstanceOf[Event.CandleHistory]), 27)
  }
