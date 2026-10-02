package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class StartupReadinessSpec extends TempDirectorySuite:
  private val at = 1800000000000L
  private def temporary(prefix: String): java.nio.file.Path =
    tempDirectory("desk-startup-" + prefix)

  test("a transient first metadata miss retries promptly while usable prices are present") {
    val calls = new AtomicInteger(0)
    val first = new CountDownLatch(1)
    val rest = new Rest:
      def post(body: String): Option[String] =
        val n = calls.incrementAndGet()
        first.countDown()
        if n == 1 then None
        else Some("""{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5}]}""")
    val directory = temporary("ledger-")
    val config = TradingConfig()
    val ledger = PropLedger.open(directory, "mainnet", config.propBalance, config.propFee)
    val worker = new PropWorker(config, "XMR", ledger, rest, () => at, () => at)
    worker.observe(Event.SocketUp)
    worker.observe(
      Event.BookUpdate(
        "XMR",
        Book(Vector(Level("99", "20", 1)), Vector(Level("101", "20", 1)), at),
        true
      )
    )
    worker.start()
    try
      assert(first.await(2L, TimeUnit.SECONDS), "first metadata request never started")
      val until = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(2500L)
      while !worker.view.ready && System.nanoTime() < until do Thread.sleep(5L)
      assert(
        worker.view.ready,
        s"Still blocked after 2500 ms, calls=${calls.get()}, reason=${worker.view.block}"
      )
    finally worker.close()
  }

  test("opening the independent socket does not wait for the slow public book check") {
    val releaseLookup = new CountDownLatch(1)
    val openedSocket = new CountDownLatch(1)
    val requests = new AtomicInteger(0)
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.incrementAndGet()
        releaseLookup.await(3L, TimeUnit.SECONDS)
        None
    val connector = new Connector:
      def connect(text: String => Unit, ended: () => Unit): Socket =
        openedSocket.countDown()
        new Socket:
          def send(body: String): Unit = ()
          def close(): Unit = ()
    val feed =
      new Feed(connector, rest, new ConcurrentLinkedQueue[Event](), () => at, "XMR")
    val thread = new Thread(() =>
      try feed.runForever(_ => throw new InterruptedException())
      catch case _: InterruptedException => ()
    )
    thread.setDaemon(true)
    thread.start()
    try
      assert(
        openedSocket.await(1250L, TimeUnit.MILLISECONDS),
        s"Socket not attempted after bounded lookup; blocking book lookups=${requests.get()}"
      )
    finally
      releaseLookup.countDown()
      thread.interrupt()
      thread.join(3000L)
      assert(!thread.isAlive, "test-owned feed thread did not stop")
  }

  test("a temporary unknown startup reply is retried after backoff") {
    val requests = new AtomicInteger(0)
    val connections = new AtomicInteger(0)
    val sleeps = new AtomicInteger(0)
    val good = s"""{"coin":"XMR","time":$at,"levels":[[],[]]}"""
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some(if requests.incrementAndGet() == 1 then "null" else good)
    val connector = new Connector:
      def connect(text: String => Unit, ended: () => Unit): Socket =
        connections.incrementAndGet()
        throw new java.io.IOException("test closes each connection")
    val feed =
      new Feed(connector, rest, new ConcurrentLinkedQueue[Event](), () => at, "XMR")
    final class Stop extends RuntimeException
    intercept[Stop] {
      feed.runForever(_ =>
        if sleeps.incrementAndGet() >= 2 then throw new Stop
        else Thread.sleep(20L)
      )
    }
    assertEquals(requests.get(), 2)
    assertEquals(connections.get(), 1)
  }

  private class HeldLookup:
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val exited = new CountDownLatch(1)
    val opened = new CountDownLatch(1)
    val closed = new CountDownLatch(1)
    val events = new ConcurrentLinkedQueue[Event]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        entered.countDown()
        try
          release.await(5L, TimeUnit.SECONDS)
          Some("null")
        finally exited.countDown()
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
    def stop(): Unit =
      release.countDown()
      thread.interrupt()
      thread.join(3000L)
      assert(!thread.isAlive, "owned feed did not stop")
      assert(exited.await(1L, TimeUnit.SECONDS), "owned lookup did not stop")

  test("a late unknown startup result closes its matching fallback connection") {
    import scala.jdk.CollectionConverters.*
    val held = new HeldLookup
    try
      assert(held.opened.await(1500L, TimeUnit.MILLISECONDS), "socket fallback did not open")
      held.release.countDown()
      assert(held.closed.await(1L, TimeUnit.SECONDS), "unknown market kept its connection")
      Wait.until("StartupReadinessSpec condition")(held.events.asScala.exists {
        case Event.MarketUnavailable("XMR", reason) => reason == Feed.unknownMarket
        case _                                      => false
      })
    finally held.stop()
  }

  test("a late startup rejection cannot close a replacement market") {
    import scala.jdk.CollectionConverters.*
    val held = new HeldLookup
    try
      assert(held.opened.await(1500L, TimeUnit.MILLISECONDS), "socket fallback did not open")
      held.feed.setMarket("BTC")
      held.release.countDown()
      assert(held.exited.await(1L, TimeUnit.SECONDS), "lookup did not finish")
      assert(!held.closed.await(100L, TimeUnit.MILLISECONDS), "old rejection closed replacement")
      assert(!held.events.asScala.exists(_.isInstanceOf[Event.MarketUnavailable]))
      assertEquals(held.feed.coin, "BTC")
    finally held.stop()
  }

  test("stopping startup interrupts and joins its owned market lookup") {
    val held = new HeldLookup
    try
      assert(held.entered.await(1L, TimeUnit.SECONDS), "lookup did not start")
      held.thread.interrupt()
      assert(held.exited.await(1L, TimeUnit.SECONDS), "lookup was left blocked after feed stop")
    finally held.stop()
  }

  test("history reload retains the pending startup check for the same market") {
    import scala.jdk.CollectionConverters.*
    val held = new HeldLookup
    try
      assert(held.opened.await(1500L, TimeUnit.MILLISECONDS))
      held.feed.reloadHistory()
      held.release.countDown()
      assert(held.closed.await(1L, TimeUnit.SECONDS), "reload hid the unknown-market result")
      Wait.until("StartupReadinessSpec condition")(
        held.events.asScala.exists(_.isInstanceOf[Event.MarketUnavailable])
      )
    finally held.stop()
  }

  test("switching to simulated trading reuses already accepted public metadata") {
    val metadata = ujson.read("""{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5}]}""")
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = Some(metadata)
      def post(body: ujson.Value): Option[ujson.Value] =
        throw new AssertionError("No order request permitted")
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit = ()
    val lookupHeld = new CountDownLatch(1)
    val rest = new Rest:
      def post(body: String): Option[String] =
        lookupHeld.await(3L, TimeUnit.SECONDS)
        Some(ujson.write(metadata))
    val directory = temporary("router-")
    val config = TradingConfig()
    val live = new TradingWorker(
      config,
      false,
      "XMR",
      exchange,
      () => throw new AssertionError("No key access permitted"),
      () => throw new AssertionError("No order journal permitted"),
      () => at,
      () => at
    )
    val router = new ExecutionRouter(
      config,
      "VIEW",
      "XMR",
      live,
      _ => throw new AssertionError("No real worker permitted"),
      rest,
      directory,
      () => at,
      () => at
    )
    router.start()
    try
      val acceptedBy = System.nanoTime() + TimeUnit.SECONDS.toNanos(3L)
      while router.view.rules.isEmpty && System.nanoTime() < acceptedBy do Thread.sleep(5L)
      assert(router.view.rules.nonEmpty, "initial read-only worker did not accept metadata")
      router.observe(Event.SocketUp)
      assert(router.switchMode("PROP").isRight)
      router.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99", "20", 1)), Vector(Level("101", "20", 1)), at),
          true
        )
      )
      val readyBy = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(300L)
      while !router.view.ready && System.nanoTime() < readyBy do Thread.sleep(5L)
      assert(
        router.view.ready,
        s"Known market rules were discarded during mode switch: ${router.view.block}"
      )
    finally
      lookupHeld.countDown()
      router.close()
  }
