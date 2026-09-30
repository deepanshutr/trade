package desk

import java.net.{Authenticator, CookieHandler, ProxySelector, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse, WebSocket}
import java.time.Duration
import java.util.Optional
import java.util.concurrent.Executor
import javax.net.ssl.{SSLContext, SSLParameters}
import java.nio.ByteBuffer
import java.util.concurrent.{
  CompletableFuture,
  ConcurrentLinkedQueue,
  CountDownLatch,
  TimeUnit,
  TimeoutException
}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class SocketWaitSpec extends munit.FunSuite:

  private final class Wire extends WebSocket:
    val aborted = new AtomicInteger(0)
    val requests = new AtomicInteger(0)
    val sent = new ConcurrentLinkedQueue[String]()
    val entered = new CountDownLatch(1)
    val held = new CompletableFuture[WebSocket]()
    @volatile var hold: String => Boolean = _ => false
    def sendText(text: CharSequence, last: Boolean): CompletableFuture[WebSocket] =
      val _ = sent.add(text.toString)
      if hold(text.toString) then
        entered.countDown()
        held
      else CompletableFuture.completedFuture(this)
    def sendBinary(data: ByteBuffer, last: Boolean): CompletableFuture[WebSocket] = fail(
      "unexpected binary send"
    )
    def sendPing(data: ByteBuffer): CompletableFuture[WebSocket] = fail("unexpected protocol ping")
    def sendPong(data: ByteBuffer): CompletableFuture[WebSocket] = fail("unexpected protocol pong")
    def sendClose(code: Int, reason: String): CompletableFuture[WebSocket] = fail(
      "closure must abort without waiting"
    )
    def request(n: Long): Unit = { val _ = requests.addAndGet(n.toInt) }
    def getSubprotocol(): String = ""
    def isInputClosed(): Boolean = aborted.get() > 0
    def isOutputClosed(): Boolean = aborted.get() > 0
    def abort(): Unit = { val _ = aborted.incrementAndGet() }

  private final class Opener extends HttpClient:
    val wire = new Wire
    var listener: WebSocket.Listener = null
    override def newWebSocketBuilder(): WebSocket.Builder = new WebSocket.Builder:
      def header(name: String, value: String): WebSocket.Builder = this
      def subprotocols(first: String, rest: String*): WebSocket.Builder = this
      def connectTimeout(timeout: Duration): WebSocket.Builder = this
      def buildAsync(uri: URI, target: WebSocket.Listener): CompletableFuture[WebSocket] =
        assertEquals(uri, URI.create("wss://example.invalid/ws"))
        listener = target
        CompletableFuture.completedFuture(wire)
    def cookieHandler(): Optional[CookieHandler] = Optional.empty()
    def connectTimeout(): Optional[Duration] = Optional.empty()
    def followRedirects(): HttpClient.Redirect = HttpClient.Redirect.NEVER
    def proxy(): Optional[ProxySelector] = Optional.empty()
    def sslContext(): SSLContext = SSLContext.getDefault()
    def sslParameters(): SSLParameters = new SSLParameters
    def authenticator(): Optional[Authenticator] = Optional.empty()
    def version(): HttpClient.Version = HttpClient.Version.HTTP_1_1
    def executor(): Optional[Executor] = Optional.empty()
    def send[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T]): HttpResponse[T] =
      fail("unexpected HTTP request")
    def sendAsync[T](
        request: HttpRequest,
        handler: HttpResponse.BodyHandler[T]
    ): CompletableFuture[HttpResponse[T]] = fail("unexpected HTTP request")
    def sendAsync[T](
        request: HttpRequest,
        handler: HttpResponse.BodyHandler[T],
        push: HttpResponse.PushPromiseHandler[T]
    ): CompletableFuture[HttpResponse[T]] = fail("unexpected HTTP request")

  for reporting <- Vector(false, true) do
    test(s"public connect reports each close, error and overflow once, reporting=$reporting") {
      val opener = new Opener
      val connector = new SocketConnector("wss://example.invalid/ws")
      // Replace only the network opener so both public methods and listener wiring run.
      val client = classOf[SocketConnector].getDeclaredField("client")
      client.setAccessible(true)
      client.set(connector, opener)
      val received = new ConcurrentLinkedQueue[String]()
      val reports = new ConcurrentLinkedQueue[String]()
      val receive: String => Unit = text => { val _ = received.add(text) }
      val socket =
        if reporting then
          connector.connectReporting(receive, reason => { val _ = reports.add(reason) })
        else connector.connect(receive, () => { val _ = reports.add("closed") })
      val listener = opener.listener
      val wire = opener.wire
      val expected = if reporting then
        Vector(
          "Server closed connection (1000)",
          "Network error",
          "Socket message too large"
        )
      else Vector.fill(3)("closed")
      try
        assert(reports.isEmpty)
        val _ = listener.onText(wire, Feed.ping, true)
        assertEquals(received.asScala.toVector, Vector(Feed.ping))
        val _ = listener.onClose(wire, 1000, "done")
        assertEquals(reports.asScala.toVector, expected.take(1))
        listener.onError(wire, new java.io.IOException("broken"))
        assertEquals(reports.asScala.toVector, expected.take(2))
        val _ = listener.onText(wire, "x" * 2_000_001, false)
        assertEquals(reports.asScala.toVector, expected)
        assertEquals(wire.aborted.get(), 1)
        val _ = listener.onText(wire, "tail", true)
        assertEquals(reports.asScala.toVector, expected)
        assertEquals(wire.aborted.get(), 1)
        assertEquals(received.asScala.toVector, Vector(Feed.ping))
      finally socket.close()
    }

  private final class Boundary(waitMs: Long = 40L) extends Connector:
    val replies = new ConcurrentLinkedQueue[CompletableFuture[WebSocket]]()
    val listeners = new ConcurrentLinkedQueue[WebSocket.Listener]()
    private val connector = new SocketConnector("unused", waitMs)
    def connect(text: String => Unit, closed: () => Unit): Socket =
      connector.connectWith(
        text,
        listener =>
          val _ = listeners.add(listener)
          Option(replies.poll()).getOrElse(fail("unexpected dial"))
        ,
        _ => closed()
      )

  private final class Running(feed: Feed):
    val failure = new AtomicReference[Throwable]()
    val thread = new Thread(() =>
      try { val _ = feed.connectOnce() }
      catch case error: Throwable => failure.set(error)
    )
    thread.setDaemon(true)
    thread.start()
    def finish(): Unit =
      thread.join(3000)
      assert(!thread.isAlive)
      assertEquals(Option(failure.get()), None)

  private def started(queue: ConcurrentLinkedQueue[Event], wire: Wire): Boolean =
    queue.contains(Event.SocketUp) && wire.sent.size() == 24

  private def make(connector: Connector) =
    val queue = new ConcurrentLinkedQueue[Event]()
    val rest = new Rest:
      def post(body: String): Option[String] = Some("[]")
    (new Feed(connector, rest, queue, () => 1_000_000L, "XMR"), queue)

  test("a delayed open is aborted on completion and its callbacks cannot refresh a replacement") {
    val boundary = new Boundary
    val pending = new CompletableFuture[WebSocket]()
    val _ = boundary.replies.add(pending)
    val (feed, queue) = make(boundary)
    assertEquals(feed.connectOnce(), false)
    assertEquals(queue.asScala.toVector, Vector(Event.SocketDown))
    val replacement = new Wire
    val _ = boundary.replies.add(CompletableFuture.completedFuture(replacement))
    val running = new Running(feed)
    try
      Wait.until("the replacement startup did not finish")(started(queue, replacement))
      queue.clear()
      val obsolete = new Wire
      assert(pending.complete(obsolete))
      Wait.until("the late socket was not aborted")(obsolete.aborted.get() == 1)
      val old = boundary.listeners.peek()
      val _ = old.onText(obsolete, """{"channel":"pong"}""", true)
      val _ = old.onClose(obsolete, 1000, "late")
      old.onError(obsolete, new java.io.IOException("late"))
      assert(queue.isEmpty)
      assertEquals(replacement.aborted.get(), 0)
      val _ = boundary.listeners.asScala.last.onText(replacement, """{"channel":"pong"}""", true)
      assertEquals(queue.asScala.toVector, Vector(Event.Pong))
      feed.heartbeat()
      assertEquals(replacement.sent.asScala.last, Feed.ping)
    finally
      val _ = boundary.listeners.asScala.last.onClose(replacement, 1000, "done")
      running.finish()
      assertEquals(replacement.aborted.get(), 1)
  }

  for operation <- Vector("subscription", "heartbeat", "switch") do
    test(s"a held $operation future times out through the production wait and releases its owner") {
      val boundary = new Boundary
      val wire = new Wire
      if operation == "subscription" then wire.hold = _ => true
      val _ = boundary.replies.add(CompletableFuture.completedFuture(wire))
      val (feed, queue) = make(boundary)
      val running = new Running(feed)
      try
        if operation != "subscription" then
          Wait.until("the connection startup did not finish")(started(queue, wire))
          wire.hold = _ => true
          if operation == "heartbeat" then feed.heartbeat() else feed.setMarket("ETH")
        running.finish()
        assertEquals(wire.entered.getCount, 0L)
        assertEquals(wire.aborted.get(), 1)
        assert(!wire.held.isDone)
        assertEquals(queue.asScala.count(_ == Event.SocketDown), 1)
        assertEquals(
          queue.asScala.count(_ == Event.SocketUp),
          1
        )
        assertEquals(
          queue.asScala.last,
          Event.SocketDown
        )
      finally
        val _ = wire.held.complete(wire)
        boundary.listeners.asScala.foreach(listener => {
          val _ = listener.onClose(wire, 1000, "done")
        })
        running.thread.join(3000)
    }

  test("the shared future wait still delivers a timely complete and fragmented response") {
    val boundary = new Boundary
    val wire = new Wire
    val _ = boundary.replies.add(CompletableFuture.completedFuture(wire))
    val received = new ConcurrentLinkedQueue[String]()
    var closes = 0
    val socket = boundary.connect(text => { val _ = received.add(text) }, () => closes += 1)
    val listener = boundary.listeners.peek()
    val _ = listener.onText(wire, "{\"channel\":", false)
    assert(received.isEmpty)
    val _ = listener.onText(wire, "\"pong\"}", true)
    assertEquals(received.asScala.toVector, Vector("""{"channel":"pong"}"""))
    socket.send(Feed.ping)
    assertEquals(wire.sent.asScala.toVector, Vector(Feed.ping))
    assertEquals(wire.requests.get(), 2)
    val _ = listener.onClose(wire, 1000, "done")
    assertEquals(closes, 1)
    socket.close()
    assertEquals(wire.aborted.get(), 1)
  }

  test("a bounded direct send retains its timeout instead of pretending delivery") {
    val boundary = new Boundary
    val wire = new Wire
    val _ = boundary.replies.add(CompletableFuture.completedFuture(wire))
    val socket = boundary.connect(_ => (), () => ())
    wire.hold = _ => true
    intercept[TimeoutException](socket.send(Feed.ping))
    socket.close()
    assertEquals(wire.aborted.get(), 1)
  }

  for last <- Vector(false, true) do
    test(s"oversized market fragments abort and allow a fresh connection, final=$last") {
      val boundary = new Boundary
      val wire = new Wire
      val _ = boundary.replies.add(CompletableFuture.completedFuture(wire))
      val received = new ConcurrentLinkedQueue[String]()
      var closes = 0
      val socket = boundary.connect(text => { val _ = received.add(text) }, () => closes += 1)
      val listener = boundary.listeners.peek()
      val fragment = "x" * 1_000_000
      val _ = listener.onText(wire, fragment, false)
      val _ = listener.onText(wire, fragment, false)
      assertEquals(wire.aborted.get(), 0)
      val _ = listener.onText(wire, "x", last)
      assert(received.isEmpty)
      assertEquals(wire.aborted.get(), 1)
      assertEquals(closes, 1)
      val _ = listener.onText(wire, "tail", true)
      assertEquals(closes, 1)
      assertEquals(wire.aborted.get(), 1)
      assert(received.isEmpty)
      val replacement = new Wire
      val _ = boundary.replies.add(CompletableFuture.completedFuture(replacement))
      val fresh = boundary.connect(text => { val _ = received.add(text) }, () => ())
      try
        val _ = boundary.listeners.asScala.last.onText(replacement, Feed.ping, true)
        assertEquals(received.asScala.toVector, Vector(Feed.ping))
      finally
        fresh.close()
        socket.close()
    }
