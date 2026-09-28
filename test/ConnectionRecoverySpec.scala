package desk

import java.io.IOException
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

class ConnectionRecoverySpec extends munit.FunSuite:
  private val rest = new Rest:
    def post(body: String): Option[String] = None

  test("a silent open socket is closed after an unanswered heartbeat") {
    val subscribed = new CountDownLatch(24)
    val closed = new AtomicInteger(0)
    var end: () => Unit = () => ()
    val connector = new Connector:
      def connect(text: String => Unit, disconnected: () => Unit): Socket =
        end = disconnected
        new Socket:
          def send(text: String): Unit = subscribed.countDown()
          def close(): Unit = { val _ = closed.incrementAndGet() }
    val events = new ConcurrentLinkedQueue[Event]()
    val clock = new AtomicLong(1000L)
    val feed =
      new Feed(connector, rest, events, () => 1_000_000L, "XMR", monotonic = () => clock.get())
    val thread = new Thread(() => { val _ = feed.connectOnce() })
    thread.setDaemon(true)
    thread.start()
    try
      assert(subscribed.await(3, TimeUnit.SECONDS))
      feed.heartbeat()
      clock.addAndGet(Feed.replyWaitMs - 1)
      feed.heartbeat()
      assertEquals(closed.get(), 0, "the socket must stay open until the reply deadline")
      clock.incrementAndGet()
      feed.heartbeat()
      assertEquals(closed.get(), 1, "successful writes must not keep an unresponsive socket open")
      assertEquals(events.asScala.count(_ == Event.SocketDown), 1)
    finally
      end()
      thread.join(3000)
      assert(!thread.isAlive)
  }

  test("repeated failed first subscriptions increase the reconnect delay") {
    val connector = new Connector:
      def connect(text: String => Unit, disconnected: () => Unit): Socket = new Socket:
        def send(text: String): Unit = throw new IOException("send refused")
        def close(): Unit = ()
    val events = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(connector, rest, events, () => 1_000_000L, "XMR")
    val waits = new ConcurrentLinkedQueue[Long]()
    final class Done extends RuntimeException
    intercept[Done] {
      feed.runForever { delay =>
        val _ = waits.add(delay)
        if waits.size() == 4 then throw new Done
      }
    }
    assertEquals(waits.asScala.toVector, Vector(1000L, 2000L, 4000L, 8000L))
  }

  test("interactive market selection rejects values that cannot be identifiers") {
    val initial = Snapshot.initial("XMR", 1000L)
    for invalid <- Vector("XMR\"", "XMR BTC", "XMR\\", ":BTC", "X" * 41) do
      val changed =
        State.reduce(initial.copy(prompt = Some(invalid)), Event.Key(Command.PromptSubmit), 1001L)
      assertEquals(changed.coin, "XMR", invalid)
  }
