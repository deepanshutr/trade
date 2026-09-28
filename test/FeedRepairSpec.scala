package desk

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class FeedRepairSpec extends munit.FunSuite:
  test("a crossed UTC boundary stays due after reconnect until the minute floor expires") {
    val midnight = java.time.Instant.parse("2026-09-28T00:00:00Z").toEpochMilli
    val now = new AtomicLong(midnight - 20000L)
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
    now.set(midnight + 10000L)
    reconnectAndRepair()
    assertEquals(requests.size(), 18) // The last attempt was only thirty seconds ago.
    now.set(midnight + 40000L)
    feed.repairHistory()
    assertEquals(requests.size(), 36)
    val repaired = requests.asScala.toVector.drop(18).map(body => ujson.read(body)("req"))
    assertEquals(repaired.map(_("interval").str).toSet, State.intervals.toSet)
    repaired.foreach { request =>
      assertEquals(
        request("startTime").num.toLong,
        firstSuccess - 2L * State.intervalMs(request("interval").str)
      )
    }
    now.set(midnight + 50000L)
    reconnectAndRepair()
    assertEquals(requests.size(), 36)
    now.set(midnight + 100000L)
    reconnectAndRepair()
    val later = requests.asScala.toVector.drop(36).map(body => ujson.read(body)("req"))
    assertEquals(later.map(_("interval").str), Vector.fill(3)("1m"))
  }

  test(
    "periodic history repair repeats after a minute without reconnecting or duplicating a cycle"
  ) {
    val now = new AtomicLong(1800000005000L)
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

  test("recorded venue four-hour and daily candles align to UTC boundaries") {
    val saved =
      ujson.read(java.nio.file.Files.readString(java.nio.file.Path.of("fixtures/r2_candles.json")))
    for (coin, intervals) <- saved.obj; interval <- Vector("4h", "1d") do
      val bars = intervals(interval).arr
      assert(bars.nonEmpty, coin)
      assert(bars.forall(_("t").num.toLong % State.intervalMs(interval) == 0L), coin + interval)
  }

  test("a fetch during the boundary grace window does not satisfy the closed bar repair") {
    val midnight = java.time.Instant.parse("2026-09-28T00:00:00Z").toEpochMilli
    val now = new AtomicLong(midnight + 1000L)
    val requests = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        Some("""[{"t":1799999940000,"o":"100","h":"101","l":"99","c":"100","v":"2"}]""")
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket = fail("no socket needed")
    val feed = new Feed(
      connector,
      rest,
      new ConcurrentLinkedQueue[Event](),
      () => now.get(),
      "XMR"
    )
    feed.repairHistory()
    now.set(midnight + 61000L)
    feed.repairHistory()
    assertEquals(requests.size(), 36)
  }

  test("failed and empty repairs retry after a minute without a new long-bar boundary") {
    for answer <- Vector(None, Some("[]")) do
      val now = new AtomicLong(1800000005000L)
      val requests = new ConcurrentLinkedQueue[String]()
      val rest = new Rest:
        def post(body: String): Option[String] =
          requests.add(body)
          answer
      val connector = new Connector:
        def connect(text: String => Unit, closed: () => Unit): Socket = fail("no socket needed")
      val feed = new Feed(
        connector,
        rest,
        new ConcurrentLinkedQueue[Event](),
        () => now.get(),
        "XMR"
      )
      feed.repairHistory()
      val first = requests.size()
      now.addAndGet(59999L)
      feed.repairHistory()
      assertEquals(requests.size(), first)
      now.incrementAndGet()
      feed.repairHistory()
      assertEquals(requests.size(), first * 2)
  }
