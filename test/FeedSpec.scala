package desk

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class FeedSpec extends munit.FunSuite:
  final class FakeSocket extends Socket:
    val sent = new ConcurrentLinkedQueue[String]()
    @volatile var closed = false
    @volatile var sending = false
    @volatile var overlapped = false
    def send(text: String): Unit =
      if sending then overlapped = true
      sending = true
      Thread.sleep(5)
      val _ = sent.add(text)
      sending = false
    def close(): Unit = closed = true

  final class FakeConnector(failFirst: Int = 0) extends Connector:
    @volatile var attempts = 0
    @volatile var onText: String => Unit = _ => ()
    @volatile var onClose: () => Unit = () => ()
    val sockets = new ConcurrentLinkedQueue[FakeSocket]()
    def connect(t: String => Unit, c: () => Unit): Socket =
      attempts += 1
      if attempts <= failFirst then throw new RuntimeException("refused")
      onText = t
      onClose = c
      val s = new FakeSocket
      val _ = sockets.add(s)
      s

  final class FakeRest(answers: PartialFunction[String, String]) extends Rest:
    val bodies = new ConcurrentLinkedQueue[String]()
    def post(body: String): Option[String] =
      val _ = bodies.add(body)
      answers.lift(body)

  def fixture(name: String): String =
    java.nio.file.Files.readString(java.nio.file.Paths.get("fixtures", name))
  val candlesBody = fixture("rest_candles.json")
  def restAnswering(book: Boolean = true, candles: Boolean = true): FakeRest = FakeRest {
    case b if b.contains("l2Book") && book            => fixture("rest_l2Book.json")
    case b if b.contains("candleSnapshot") && candles => candlesBody
  }
  def newFeed(connector: Connector, rest: Rest, clock: () => Long = () => 1_000_000L) =
    val queue = new ConcurrentLinkedQueue[Event]()
    (new Feed(connector, rest, queue, clock, "XMR"), queue)
  def startConnected(feed: Feed): Thread =
    val t = new Thread(() => { val _ = feed.connectOnce() })
    t.setDaemon(true)
    t.start()
    Thread.sleep(300)
    t

  test("a message split in pieces is joined and delivered once") {
    val j = new Joiner
    assertEquals(j.append("{\"chan", last = false), None)
    assertEquals(j.append("nel\":\"po", last = false), None)
    assertEquals(j.append("ng\"}", last = true), Some("""{"channel":"pong"}"""))
    assertEquals(j.append("x", last = true), Some("x"))
  }
  test(
    "on open subscriptions and connection-up finish before the separate history repair"
  ) {
    val c = new FakeConnector
    val rest = restAnswering()
    val (feed, queue) = newFeed(c, rest)
    val t = startConnected(feed)
    assert(rest.bodies.isEmpty)
    feed.awaitHistoryRepair()
    val deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(5)
    while queue.asScala.count(_.isInstanceOf[Event.CandleHistory]) < 18 && System
        .nanoTime() < deadline
    do Thread.sleep(10L)
    val sent = c.sockets.peek().sent.asScala.toVector
    assertEquals(sent, Feed.subscriptions("XMR"))
    assert(
      sent(0).contains("\"l2Book\"") && sent(1).contains("\"bbo\"") && sent(2).contains(
        "\"trades\""
      )
    )
    assert(sent(3).contains("\"activeAssetCtx\""))
    assert(sent(4).contains("\"candle\"") && sent(4).contains("\"interval\":\"1m\""))
    assertEquals(rest.bodies.asScala.count(_.contains("candleSnapshot")), 18)
    val events = queue.asScala.toVector
    assert(events.contains(Event.SocketUp))
    assertEquals(events.count(_.isInstanceOf[Event.CandleHistory]), 18)
    c.onClose()
    t.join(1000)
    assertEquals(queue.asScala.toVector.last, Event.SocketDown)
  }
  test("subscriptions cover the main market, six series and both references without duplicates") {
    val subs = Feed.subscriptions("XMR")
    assertEquals(subs.size, 24)
    assertEquals(subs.distinct.size, 24)
    assert(subs.exists(_.contains("\"type\":\"l2Book\"")))
    assertEquals(subs.count(_.contains("\"type\":\"candle\"")), 18)
    assertEquals(subs.count(_.contains("\"coin\":\"PAXG\"")), 7)
    assert(subs.exists(s => s.contains("\"interval\":\"1d\"") && s.contains("\"coin\":\"BTC\"")))
    assertEquals(Feed.subscriptions("BTC").size, 17)
    val unsubs = Feed.unsubscriptions("XMR")
    assertEquals(unsubs.size, 24)
    assert(unsubs.forall(_.contains("\"method\":\"unsubscribe\"")), unsubs)
    assertEquals(unsubs.map(_.replace("unsubscribe", "subscribe")), subs)
  }
  test("socket text is parsed into the queue and ignored messages produce nothing") {
    val c = new FakeConnector
    val (feed, queue) = newFeed(c, restAnswering())
    val t = startConnected(feed)
    queue.clear()
    c.onText(fixture("pong.json"))
    c.onText(fixture("subscriptionResponse.json"))
    c.onText("garbage")
    assertEquals(queue.asScala.toVector, Vector(Event.Pong, Event.Bad("garbage")))
    c.onClose()
    t.join(1000)
  }
  test("concurrent heartbeat ticks share one outstanding venue ping and sends never overlap") {
    val c = new FakeConnector
    val (feed, _) = newFeed(c, restAnswering())
    val t = startConnected(feed)
    val threads = (1 to 8).map(_ => new Thread(() => feed.heartbeat()))
    threads.foreach(_.start())
    threads.foreach(_.join())
    val s = c.sockets.peek()
    assertEquals(s.sent.asScala.count(_ == """{"method":"ping"}"""), 1)
    assert(!s.overlapped, "sends overlapped")
    c.onClose()
    t.join(1000)
  }
  test("a poll puts a REST book on the queue marked as REST, and a failed poll puts nothing") {
    val (feed, queue) = newFeed(new FakeConnector, restAnswering())
    feed.poll()
    queue.asScala.toVector match
      case Vector(Event.BookUpdate("XMR", b, false)) => assertEquals(b.time, 1788345727341L)
      case other                                     => fail(s"unexpected $other")
    val (feed2, queue2) = newFeed(new FakeConnector, restAnswering(book = false))
    feed2.poll()
    assert(queue2.isEmpty)
    val (feed3, queue3) = newFeed(new FakeConnector, FakeRest { case _ => "not json" })
    feed3.poll()
    assert(queue3.isEmpty)
  }
  test("history bodies ask for five thousand candles ending now") {
    val body = Feed.historyBody("BTC", "5m", 1_000_000_000L)
    assert(body.contains("\"coin\":\"BTC\"") && body.contains("\"interval\":\"5m\""), body)
    assert(body.contains("\"startTime\":" + (1_000_000_000L - 5000L * 300_000L)), body)
    assert(body.contains("\"endTime\":1000000000"), body)
    assertEquals(Feed.coarseBody("XMR", 3), """{"type":"l2Book","coin":"XMR","nSigFigs":3}""")
  }
  test("a failed history request is retried once before it counts as bad") {
    var failed = false
    val rest = FakeRest {
      case b if b.contains("candleSnapshot") =>
        if !failed then
          failed = true
          "not json"
        else candlesBody
    }
    val (feed, queue) = newFeed(new FakeConnector, rest)
    feed.fetchHistory()
    assertEquals(rest.bodies.size, 19)
    val events = queue.asScala.toVector
    assertEquals(events.count { case Event.CandleHistory(_, _, _, _) => true; case _ => false }, 18)
    assertEquals(events.count(_ == Event.CandleHistoryFailed), 0)
  }

  test("fetching history sends eighteen requests and tags every series") {
    val rest = restAnswering()
    val (feed, queue) = newFeed(new FakeConnector, rest, () => 10_000_000L)
    feed.fetchHistory()
    val bodies = rest.bodies.asScala.toVector
    assertEquals(bodies.size, 18)
    assert(bodies.forall(_.contains("candleSnapshot")), bodies)
    for c <- Vector("XMR", "BTC", "PAXG") do
      assertEquals(bodies.count(_.contains(s""""coin":"$c"""")), 6, c)
    assertEquals(bodies.head, Feed.historyBody("XMR", "1m", 10_000_000L))
    val tags = queue.asScala.toVector.collect { case Event.CandleHistory(c, i, _, _) => (c, i) }
    assertEquals(tags.size, 18)
    assertEquals(tags.distinct.size, 18)
    assertEquals(tags.map(_._1).distinct, Vector("XMR", "BTC", "PAXG"))
    assertEquals(tags.filter(_._1 == "PAXG").map(_._2), State.intervals)
    val (feed2, queue2) = newFeed(new FakeConnector, restAnswering(candles = false))
    feed2.fetchHistory()
    assertEquals(queue2.asScala.toVector, Vector.fill(18)(Event.CandleHistoryFailed))
  }
  test("history for a market already followed as a reference is fetched once") {
    val rest = restAnswering()
    val queue = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(new FakeConnector, rest, queue, () => 10_000_000L, "BTC")
    feed.fetchHistory()
    assertEquals(rest.bodies.asScala.count(_.contains("\"coin\":\"BTC\"")), 6)
    assertEquals(rest.bodies.asScala.size, 12)
    val tags = queue.asScala.toVector.collect { case Event.CandleHistory(c, _, _, _) => c }
    assertEquals(tags.distinct, Vector("BTC", "PAXG"))
  }
  test("polling the coarse books sends three requests and tags the resolution") {
    val rest = restAnswering()
    val (feed, queue) = newFeed(new FakeConnector, rest)
    feed.pollCoarse()
    assertEquals(
      rest.bodies.asScala.toVector,
      Vector(
        """{"type":"l2Book","coin":"XMR","nSigFigs":4}""",
        """{"type":"l2Book","coin":"XMR","nSigFigs":3}""",
        """{"type":"l2Book","coin":"XMR","nSigFigs":2}"""
      )
    )
    queue.asScala.toVector match
      case Vector(
            Event.CoarseBook("XMR", 4, b4),
            Event.CoarseBook("XMR", 3, b3),
            Event.CoarseBook("XMR", 2, b2)
          ) =>
        assertEquals(b4.time, 1788345727341L)
        assertEquals(b3, b4)
        assertEquals(b2, b4)
      case other => fail(s"unexpected $other")
    val (feed2, queue2) = newFeed(new FakeConnector, restAnswering(book = false))
    feed2.pollCoarse()
    assert(queue2.isEmpty)
  }
  test("connection wakes repair for every interval after the minute floor") {
    val c = new FakeConnector
    val rest = restAnswering()
    var t = 1_000_000L
    val queue = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(c, rest, queue, () => t, "XMR")
    def cycle(): Int =
      val thread = startConnected(feed)
      feed.awaitHistoryRepair()
      c.onClose()
      val _ = thread.join(1000)
      rest.bodies.asScala.count(_.contains("candleSnapshot"))
    assertEquals(cycle(), 18)
    t += 1_000L
    assertEquals(cycle(), 18)
    t += 59_000L
    assertEquals(cycle(), 36)
  }
  test("backoff is 1, 2, 4 then 8 seconds forever") {
    assertEquals((1 to 6).map(Feed.backoffMs), Seq(1000L, 2000L, 4000L, 8000L, 8000L, 8000L))
  }
  test("the reconnect loop resets after a connection has received data for ten seconds") {
    val c = new FakeConnector(failFirst = 2)
    val clock = new java.util.concurrent.atomic.AtomicLong(0)
    val feed = new Feed(
      c,
      restAnswering(),
      new ConcurrentLinkedQueue[Event](),
      () => 1_000_000L,
      "XMR",
      monotonic = () => clock.get()
    )
    val waits = new ConcurrentLinkedQueue[Long]()
    val t = new Thread(() =>
      try
        feed.runForever { ms =>
          val _ = waits.add(ms)
          Thread.sleep(20)
        }
      catch case _: InterruptedException => ()
    )
    t.setDaemon(true)
    t.start()
    try
      val deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(3)
      while (c.sockets.isEmpty || c.sockets.peek().sent.size() < 24) && System.nanoTime() < deadline
      do Thread.sleep(1)
      assert(!c.sockets.isEmpty && c.sockets.peek().sent.size() == 24)
      c.onText(fixture("pong.json"))
      clock.set(10000L)
      c.onClose()
      while waits.size() < 3 && System.nanoTime() < deadline do Thread.sleep(1)
      assertEquals(waits.asScala.toVector.take(3), Vector(1000L, 2000L, 1000L))
    finally
      t.interrupt()
      t.join(3000)
      assert(!t.isAlive)
  }
  test("a market switch resubscribes immediately and wakes the latest market history repair") {
    val c = new FakeConnector
    val rest = restAnswering()
    val (feed, _) = newFeed(c, rest)
    val t = startConnected(feed)
    feed.awaitHistoryRepair()
    val s = c.sockets.peek()
    s.sent.clear()
    rest.bodies.clear()
    feed.setMarket("ETH")
    assert(rest.bodies.isEmpty)
    feed.awaitHistoryRepair()
    assertEquals(feed.coin, "ETH")
    val sent = s.sent.asScala.toVector
    assertEquals(sent.size, 20)
    assertEquals(sent.take(10), Feed.unsubscriptions("XMR", Vector.empty))
    assertEquals(sent.drop(10), Feed.subscriptions("ETH", Vector.empty))
    assert(sent.take(10).forall(m => m.contains("\"method\":\"unsubscribe\"")), sent.take(10))
    val bodies = rest.bodies.asScala.toVector
    assertEquals(bodies.size, 6)
    assert(
      bodies.forall(b => b.contains("candleSnapshot") && b.contains("\"coin\":\"ETH\"")),
      bodies
    )
    s.sent.clear()
    rest.bodies.clear()
    feed.setMarket("ETH")
    assert(s.sent.isEmpty)
    assert(rest.bodies.isEmpty)
    c.onClose()
    t.join(1000)
  }

  test("the order observer receives an event before the fold queue") {
    val queue = new ConcurrentLinkedQueue[Event]()
    val seen = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(
      new FakeConnector,
      restAnswering(),
      queue,
      () => 1_000_000L,
      "XMR",
      State.refCoins,
      event => {
        assert(queue.isEmpty)
        val _ = seen.add(event)
      }
    )
    feed.poll()
    assertEquals(seen.asScala.toVector, queue.asScala.toVector)
    assertEquals(seen.size(), 1)
  }

  test("configured references control subscriptions and history without duplicates") {
    val refs = Vector("BTC", "PAXG", "ETH", "ETH")
    for (main, markets, count) <- Vector(
        ("ETH", Set("ETH", "BTC", "PAXG"), 18),
        ("XMR", Set("XMR", "ETH", "BTC", "PAXG"), 24)
      )
    do
      val subscriptions = Feed.subscriptions(main, refs)
      val decoded = subscriptions.map(ujson.read(_)("subscription"))
      assertEquals(decoded.map(_("coin").str).toSet, markets)
      assertEquals(subscriptions.distinct.size, subscriptions.size)
      assertEquals(decoded.count(v => v("type").str == "candle"), count)
      val rest = restAnswering()
      val queue = new ConcurrentLinkedQueue[Event]()
      val feed = new Feed(new FakeConnector, rest, queue, () => 10_000_000L, main, refs)
      feed.fetchHistory()
      val history = queue.asScala.toVector.collect { case Event.CandleHistory(c, i, _, _) =>
        (c, i)
      }
      assertEquals(history.toSet, markets.flatMap(c => State.intervals.map(c -> _)))
      assertEquals(history.size, count)
      assertEquals(rest.bodies.size, count)
  }

  test("a market switch retains the configured reference subscription set") {
    val refs = Vector("BTC", "PAXG", "ETH")
    val connector = new FakeConnector
    val rest = restAnswering()
    val queue = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(connector, rest, queue, () => 10_000_000L, "XMR", refs)
    val thread = startConnected(feed)
    try
      feed.awaitHistoryRepair()
      val socket = connector.sockets.peek()
      socket.sent.clear()
      rest.bodies.clear()
      feed.setMarket("SOL")
      assert(rest.bodies.isEmpty)
      feed.awaitHistoryRepair()
      assertEquals(
        socket.sent.asScala.toVector,
        Feed.unsubscriptions("XMR", Vector.empty) ++ Feed.subscriptions("SOL", Vector.empty)
      )
      assertEquals(rest.bodies.size, 6)
      socket.sent.clear()
      feed.setMarket("BTC")
      val switched = socket.sent.asScala.toVector.map(ujson.read(_))
      val subscribed = switched.filter(_("method").str == "subscribe")
      assertEquals(
        subscribed.map(_("subscription")("type").str).toSet,
        Set("l2Book", "trades", "activeAssetCtx")
      )
      assert(subscribed.forall(_("subscription")("coin").str == "BTC"))
      socket.sent.clear()
      feed.setMarket("SOL")
      val removed = socket.sent.asScala.toVector
        .map(ujson.read(_))
        .filter(_("method").str == "unsubscribe")
      assertEquals(
        removed.map(_("subscription")("type").str).toSet,
        Set("l2Book", "trades", "activeAssetCtx")
      )
      assert(removed.forall(_("subscription")("coin").str == "BTC"))
    finally
      connector.onClose()
      thread.join(1000)
  }
