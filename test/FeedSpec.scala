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
    Thread.sleep(150)
    t

  test("a message split in pieces is joined and delivered once") {
    val j = new Joiner
    assertEquals(j.append("{\"chan", last = false), None)
    assertEquals(j.append("nel\":\"po", last = false), None)
    assertEquals(j.append("ng\"}", last = true), Some("""{"channel":"pong"}"""))
    assertEquals(j.append("x", last = true), Some("x"))
  }
  test(
    "on open the five subscriptions go out in order, history is fetched, the queue sees SocketUp"
  ) {
    val c = new FakeConnector
    val rest = restAnswering()
    val (feed, queue) = newFeed(c, rest)
    val t = startConnected(feed)
    val sent = c.sockets.peek().sent.asScala.toVector
    assertEquals(sent, feed.subscriptions("XMR"))
    assert(
      sent(0).contains("\"l2Book\"") && sent(1).contains("\"bbo\"") && sent(2).contains(
        "\"trades\""
      )
    )
    assert(sent(3).contains("\"candle\"") && sent(3).contains("\"interval\":\"1m\""))
    assert(sent(4).contains("\"activeAssetCtx\""))
    assert(rest.bodies.asScala.exists(_.contains("candleSnapshot")))
    val events = queue.asScala.toVector
    assert(events.contains(Event.SocketUp))
    assert(events.exists(_.isInstanceOf[Event.CandleHistory]))
    c.onClose()
    t.join(1000)
    assertEquals(queue.asScala.toVector.last, Event.SocketDown)
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
  test("the heartbeat is the venue's text ping and sends never overlap") {
    val c = new FakeConnector
    val (feed, _) = newFeed(c, restAnswering())
    val t = startConnected(feed)
    val threads = (1 to 8).map(_ => new Thread(() => feed.heartbeat()))
    threads.foreach(_.start())
    threads.foreach(_.join())
    val s = c.sockets.peek()
    assertEquals(s.sent.asScala.count(_ == """{"method":"ping"}"""), 8)
    assert(!s.overlapped, "sends overlapped")
    c.onClose()
    t.join(1000)
  }
  test("a poll puts a REST book on the queue marked as REST, and a failed poll puts nothing") {
    val (feed, queue) = newFeed(new FakeConnector, restAnswering())
    feed.poll()
    queue.asScala.toVector match
      case Vector(Event.BookUpdate(b, false)) => assertEquals(b.time, 1788345727341L)
      case other                              => fail(s"unexpected $other")
    val (feed2, queue2) = newFeed(new FakeConnector, restAnswering(book = false))
    feed2.poll()
    assert(queue2.isEmpty)
    val (feed3, queue3) = newFeed(new FakeConnector, FakeRest { case _ => "not json" })
    feed3.poll()
    assert(queue3.isEmpty)
  }
  test("history asks for a thousand minutes ending now and a failure is reported") {
    val rest = restAnswering()
    val (feed, queue) = newFeed(new FakeConnector, rest, () => 10_000_000L)
    feed.fetchHistory()
    val body = rest.bodies.asScala.last
    assert(body.contains("\"startTime\":" + (10_000_000L - 60_000_000L)), body)
    assert(body.contains("\"endTime\":10000000"), body)
    assert(body.contains("\"interval\":\"1m\""), body)
    assert(queue.asScala.toVector.head.isInstanceOf[Event.CandleHistory])
    val (feed2, queue2) = newFeed(new FakeConnector, restAnswering(candles = false))
    feed2.fetchHistory()
    assertEquals(queue2.asScala.toVector, Vector(Event.CandleHistoryFailed))
  }
  test("backoff is 1, 2, 4 then 8 seconds forever") {
    assertEquals((1 to 6).map(Feed.backoffMs), Seq(1000L, 2000L, 4000L, 8000L, 8000L, 8000L))
  }
  test("the reconnect loop waits by attempt and resets after a connection that opened") {
    val c = new FakeConnector(failFirst = 2)
    val (feed, _) = newFeed(c, restAnswering())
    val waits = new ConcurrentLinkedQueue[Long]()
    val t = new Thread(() =>
      feed.runForever { ms =>
        val _ = waits.add(ms)
        Thread.sleep(20)
      }
    )
    t.setDaemon(true)
    t.start()
    Thread.sleep(300)
    c.onClose()
    Thread.sleep(300)
    assertEquals(waits.asScala.toVector.take(3), Vector(1000L, 2000L, 1000L))
  }
  test(
    "setMarket on a new coin unsubscribes the old five, subscribes the new five and refetches history; the same coin sends nothing"
  ) {
    val c = new FakeConnector
    val rest = restAnswering()
    val (feed, _) = newFeed(c, rest)
    val t = startConnected(feed)
    val s = c.sockets.peek()
    val bodiesBefore = rest.bodies.asScala.size
    s.sent.clear()
    feed.setMarket("BTC")
    val sent = s.sent.asScala.toVector
    assertEquals(sent.size, 10)
    assert(sent.take(5).forall(m => m.contains("\"unsubscribe\"") && m.contains("\"XMR\"")))
    assert(sent.drop(5).forall(m => m.contains("\"subscribe\"") && m.contains("\"BTC\"")))
    assert(sent(0).contains("\"l2Book\""))
    assert(sent(1).contains("\"bbo\""))
    assert(sent(2).contains("\"trades\""))
    assert(sent(3).contains("\"candle\""))
    assert(sent(4).contains("\"activeAssetCtx\""))
    assert(sent(5).contains("\"l2Book\""))
    assert(sent(6).contains("\"bbo\""))
    assert(sent(7).contains("\"trades\""))
    assert(sent(8).contains("\"candle\"") && sent(8).contains("\"interval\":\"1m\""))
    assert(sent(9).contains("\"activeAssetCtx\""))
    assert(
      rest.bodies.asScala
        .drop(bodiesBefore)
        .exists(b => b.contains("candleSnapshot") && b.contains("\"coin\":\"BTC\""))
    )
    val bodiesAfterCoin = rest.bodies.asScala.size
    s.sent.clear()
    feed.setMarket("BTC")
    assert(s.sent.isEmpty)
    assertEquals(rest.bodies.asScala.size, bodiesAfterCoin)
    c.onClose()
    t.join(1000)
  }
  test("the candle stream is one minute and history is a thousand minutes") {
    assertEquals(Feed.interval, "1m")
    assertEquals(Feed.historyMs, 60_000_000L)
  }
