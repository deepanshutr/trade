package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class MainSpec extends munit.FunSuite:
  test("conflicting or repeated audio flags explain the choice") {
    for flags <- Seq(
        Seq("--chime", "--mute"),
        Seq("--mute", "--chime"),
        Seq("--chime", "--chime"),
        Seq("--mute", "--mute")
      )
    do assertEquals(Main.parseArgs(flags), Left("choose one of chime or mute"))
  }

  private final class SwitchFixture(blockSocket: Boolean):
    val time = 1_000_000L
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", time))
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val sent = new ConcurrentLinkedQueue[String]()
    val bodies = new ConcurrentLinkedQueue[String]()
    val callers = new ConcurrentLinkedQueue[Thread]()
    private val startup = new CountDownLatch(18)
    private val armed = new AtomicBoolean(false)
    @volatile private var receive: String => Unit = _ => ()
    @volatile private var disconnect: () => Unit = () => ()
    var workers = Vector.empty[Thread]
    val repairs = new AtomicInteger()

    private def hold(): Unit =
      if armed.compareAndSet(true, false) then
        entered.countDown()
        release.await()

    private val connector = new Connector:
      def connect(onText: String => Unit, onClose: () => Unit): Socket =
        receive = onText
        disconnect = onClose
        new Socket:
          def send(text: String): Unit =
            val _ = callers.add(Thread.currentThread())
            if blockSocket then hold()
            val _ = sent.add(text)
          def close(): Unit = disconnect()

    private val rest = new Rest:
      def post(body: String): Option[String] =
        val _ = callers.add(Thread.currentThread())
        val request = ujson.read(body)
        if request("type").str == "l2Book" then
          Some(MarketSelectionSpec.bookResponse(request("coin").str))
        else
          val _ = bodies.add(body)
          if !blockSocket then hold()
          startup.countDown()
          Some("[]")

    val feed = new Feed(connector, rest, queue, () => time, "XMR")
    private val connection = new Thread(() =>
      try
        val _ = feed.connectOnce()
      catch case _: InterruptedException => Thread.currentThread().interrupt()
    )
    val history = new Thread(() =>
      try
        while true do
          feed.awaitHistoryRepair()
          val _ = repairs.incrementAndGet()
      catch case _: InterruptedException => ()
    )

    def start(): Unit =
      connection.setDaemon(true)
      history.setDaemon(true)
      history.start()
      connection.start()
      assert(startup.await(3, TimeUnit.SECONDS), "initial histories did not finish")
      workers = Main.startFold(queue, state, feed, () => time)
      Wait.until("initial histories did not reach the fold") {
        val s = state.get()
        s.candles.keySet == State.intervals.toSet &&
        s.refs.values.forall(_.candles.keySet == State.intervals.toSet)
      }
      assertEquals(state.get().feed, Health.Live(time))
      Wait.until("initial repair did not finish")(repairs.get() == 1)
      assertEquals(sent.size(), 24)
      assertEquals(bodies.size(), 18)
      sent.clear()
      bodies.clear()
      callers.clear()
      armed.set(true)

    def market(coin: String): Unit =
      queue.put(Event.Key(Command.StartMarketPrompt))
      coin.foreach(c => queue.put(Event.Key(Command.PromptChar(c))))
      queue.put(Event.Key(Command.PromptSubmit))

    def book(): Unit = receive(
      """{"channel":"bbo","data":{"coin":"ETH","time":1000001,"bbo":[{"px":"20","sz":"1","n":1},{"px":"22","sz":"1","n":1}]}}"""
    )

    def close(): Unit =
      release.countDown()
      workers.foreach(_.interrupt())
      disconnect()
      connection.interrupt()
      history.interrupt()
      (workers ++ Vector(connection, history)).foreach(_.join(3000))
      assert(
        (workers ++ Vector(connection, history)).forall(!_.isAlive),
        "a test thread did not stop"
      )

  private def withSwitch(blockSocket: Boolean)(body: SwitchFixture => Unit): Unit =
    val f = new SwitchFixture(blockSocket)
    try
      f.start()
      body(f)
    finally f.close()

  for blockSocket <- Vector(true, false) do
    val held = if blockSocket then "socket send" else "history request"
    test(s"the production fold keeps processing while a switch waits in a $held") {
      withSwitch(blockSocket) { f =>
        f.market("ETH")
        assert(f.entered.await(3, TimeUnit.SECONDS), s"switch never reached the $held")
        assertEquals(f.state.get().coin, "ETH")
        f.book()
        f.queue.put(Event.Key(Command.ZoomOut))
        f.queue.put(Event.Resize(120, 40))
        f.queue.put(Event.Key(Command.Quit))
        Wait.until(s"fold stopped behind the held $held")(f.state.get().quit)
        val s = f.state.get()
        assertEquals(s.book.map(_.bids.head.px), Some("20"))
        assertEquals(s.book.map(_.asks.head.px), Some("22"))
        assertEquals(s.book.map(_.time), Some(1_000_001L))
        assertEquals(s.zoom, 1)
        assertEquals((s.cols, s.rows), (120, 40))
        assertEquals(f.release.getCount, 1L)
        assertEquals(f.workers.size, 2)
        assert(f.workers.forall(_.isDaemon), "fold workers must be daemons")
      }
    }

  test("queued switches keep their order and a repeated market does no network work") {
    withSwitch(blockSocket = false) { f =>
      f.market("ETH")
      assert(f.entered.await(3, TimeUnit.SECONDS), "first history request did not start")
      f.market("SOL")
      Wait.until("verified SOL selection did not arrive")(f.state.get().coin == "SOL")
      f.market("SOL")
      f.market("XMR")
      f.queue.put(Event.Resize(121, 41))
      Wait.until("queued markets held up the fold")(f.state.get().cols == 121)
      Wait.until("verified return selection did not arrive")(f.state.get().coin == "XMR")
      Wait.until("history held up subscription changes")(
        f.feed.coin == "XMR" && f.sent.size() == 60
      )
      assertEquals(f.bodies.size(), 1)
      f.release.countDown()
      Wait.until("latest repair wake was lost")(f.repairs.get() >= 3)
      val expectedSends = Vector("XMR", "ETH", "SOL", "XMR")
        .sliding(2)
        .flatMap(pair =>
          Feed
            .unsubscriptions(pair.head)
            .filter(m => ujson.read(m)("subscription")("coin").str == pair.head) ++ Feed
            .subscriptions(pair.last)
            .filter(m => ujson.read(m)("subscription")("coin").str == pair.last)
        )
        .toVector
      assertEquals(f.sent.asScala.toVector, expectedSends)
      val requests = f.bodies.asScala.toVector.map { body =>
        val req = ujson.read(body)("req")
        (req("coin").str, req("interval").str)
      }
      val expectedRequests = Vector("ETH" -> "1m") ++ State.intervals.map("XMR" -> _)
      assertEquals(requests, expectedRequests)
      assertEquals(f.feed.coin, "XMR")
      assertEquals(f.callers.asScala.toSet, Set(f.workers.head, f.history))
    }
  }

  test("parseArgs takes an optional coin and the mono flag") {
    assertEquals(Main.parseArgs(Nil), Right(Main.Args("XMR", false)))
    assertEquals(Main.parseArgs(Seq("BTC")), Right(Main.Args("BTC", false)))
    assertEquals(Main.parseArgs(Seq("--mono")), Right(Main.Args("XMR", true)))
    assertEquals(Main.parseArgs(Seq("BTC", "--mono")), Right(Main.Args("BTC", true)))
    assert(Main.parseArgs(Seq("--colour")).isLeft)
    assert(Main.parseArgs(Seq("BTC", "ETH")).isLeft)
    assertEquals(Main.style(Main.Args("XMR", false), Map.empty), Style.colour)
    assertEquals(Main.style(Main.Args("XMR", true), Map.empty), Style.mono)
    assertEquals(Main.style(Main.Args("XMR", false), Map("NO_COLOR" -> "1")), Style.mono)
  }
  test("a size event fires only when the size changes") {
    assertEquals(Main.sizeEvent((100, 30), (100, 30)), None)
    assertEquals(Main.sizeEvent((100, 30), (120, 40)), Some(Event.Resize(120, 40)))
  }
  test("the key thread's prompt mode follows the commands it emits") {
    assertEquals(Main.nextPromptOpen(false, Command.StartMarketPrompt), true)
    assertEquals(Main.nextPromptOpen(true, Command.PromptSubmit), false)
    assertEquals(Main.nextPromptOpen(true, Command.PromptCancel), false)
    assertEquals(Main.nextPromptOpen(true, Command.PromptChar('x')), true)
    assertEquals(Main.nextPromptOpen(true, Command.PromptBackspace), true)
    assertEquals(Main.nextPromptOpen(false, Command.Quit), false)
    assertEquals(Main.nextPromptOpen(false, Command.ZoomIn), false)
  }

  test("trading is explicit and the settings path is optional") {
    assertEquals(
      Main.parseArgs(Seq("XMR", "--trade", "--config", "/tmp/desk.json")),
      Right(Main.Args("XMR", false, true, Some("/tmp/desk.json")))
    )
    assertEquals(
      Main.parseArgs(Seq("--config", "/tmp/desk.json", "--mono", "BTC")),
      Right(Main.Args("BTC", true, false, Some("/tmp/desk.json")))
    )
    assertEquals(Main.parseArgs(Nil), Right(Main.Args("XMR", false, false, None)))
    assert(Main.parseArgs(Seq("--config")).isLeft)
    assert(Main.parseArgs(Seq("--config", "--trade")).isLeft)
    assert(Main.parseArgs(Seq("--config", "one", "--config", "two")).isLeft)
  }

  test("a missing default settings file uses defaults but an explicit path stays explicit") {
    val home = java.nio.file.Files.createTempDirectory("desk-home")
    assertEquals(Main.configPath(Main.Args("XMR", false), home), None)
    val directory = home.resolve(".config/desk")
    java.nio.file.Files.createDirectories(directory)
    val file = directory.resolve("config.json")
    java.nio.file.Files.writeString(file, "{}")
    assertEquals(Main.configPath(Main.Args("XMR", false), home), Some(file))
    assertEquals(
      Main.configPath(Main.Args("XMR", false, config = Some("missing.json")), home),
      Some(java.nio.file.Path.of("missing.json"))
    )
  }

  test("public quote sources use the selected account network") {
    val (mainSocket, mainRest) = Main.publicSources(TradingConfig())
    assertEquals(mainSocket.url, "wss://api.hyperliquid.xyz/ws")
    assertEquals(mainRest.url, "https://api.hyperliquid.xyz/info")
    val (testSocket, testRest) = Main.publicSources(TradingConfig(network = "testnet"))
    assertEquals(testSocket.url, "wss://api.hyperliquid-testnet.xyz/ws")
    assertEquals(testRest.url, "https://api.hyperliquid-testnet.xyz/info")
  }

  test("the fold selects the order market before publishing it and before subscription work") {
    val time = 1_000_000L
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", time).copy(prompt = Some("BTC")))
    val events = new ConcurrentLinkedQueue[String]()
    val called = new CountDownLatch(1)
    val rest = new Rest:
      def post(body: String): Option[String] =
        val request = ujson.read(body)
        if request("type").str == "l2Book" then
          Some(MarketSelectionSpec.bookResponse(request("coin").str))
        else
          val _ = events.add("history " + state.get().coin)
          called.countDown()
          Some("[]")
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new IllegalStateException("not connected in this test")
    val feed = new Feed(connector, rest, queue, () => time, "XMR")
    val workers = Main.startFold(
      queue,
      state,
      feed,
      () => time,
      coin => {
        assertEquals(state.get().coin, "XMR")
        val _ = events.add("selected " + coin)
      }
    )
    val history = new Thread(() =>
      try feed.awaitHistoryRepair()
      catch case _: InterruptedException => ()
    )
    history.setDaemon(true)
    history.start()
    try
      queue.put(Event.Key(Command.PromptSubmit))
      assert(called.await(3, TimeUnit.SECONDS))
      assertEquals(events.asScala.toVector.take(2), Vector("selected BTC", "history BTC"))
    finally
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
      history.interrupt()
      history.join(3000)
  }
