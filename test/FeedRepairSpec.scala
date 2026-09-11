package desk

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class FeedRepairSpec extends munit.FunSuite:
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
