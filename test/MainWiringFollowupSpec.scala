package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class MainWiringFollowupSpec extends munit.FunSuite:
  private def awaitCondition(label: String)(ready: => Boolean): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while !ready && System.nanoTime() < deadline do Thread.sleep(1)
    assert(ready, label)

  test("the application fold helper acknowledges each prompt after its state is published") {
    val now = 1000L
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", now).copy(prompt = Some("BTC")))
    val pending = new AtomicInteger(0)
    val selected = new AtomicReference(Option.empty[(Side, BigDecimal)])
    val targetLock = new Object
    val callbacks = new ConcurrentLinkedQueue[(String, String, Int, Boolean)]()
    val enteredSecond = new CountDownLatch(1)
    val releaseSecond = new CountDownLatch(1)
    val rest = new Rest:
      def post(body: String): Option[String] = Some("[]")
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new IllegalStateException("no connection in this test")
    val feed = new Feed(connector, rest, queue, () => now, "XMR")
    Main.queueInput(Command.PromptSubmit, queue, pending)
    Main.queueInput(Command.StartMarketPrompt, queue, pending)
    "ETH".foreach(c => Main.queueInput(Command.PromptChar(c), queue, pending))
    Main.queueInput(Command.PromptSubmit, queue, pending)
    assertEquals(pending.get(), 2)
    val workers = Main.startTradingFold(
      queue,
      state,
      feed,
      () => now,
      pending,
      targetLock,
      selected,
      coin =>
        val _ = callbacks.add((coin, state.get().coin, pending.get(), Thread.holdsLock(targetLock)))
        if coin == "ETH" then
          enteredSecond.countDown()
          val _ = releaseSecond.await(3, TimeUnit.SECONDS)
    )
    try
      assert(enteredSecond.await(3, TimeUnit.SECONDS))
      assertEquals(state.get().coin, "BTC")
      assertEquals(pending.get(), 1)
      assertEquals(selected.get(), None)
      assertEquals(
        callbacks.asScala.toVector,
        Vector(("BTC", "XMR", 2, true), ("ETH", "BTC", 1, true))
      )
      releaseSecond.countDown()
      awaitCondition("the second published prompt was not acknowledged")(pending.get() == 0)
      assertEquals(state.get().coin, "ETH")
      assertEquals(state.get().prompt, None)
      Main.queueInput(Command.StartMarketPrompt, queue, pending)
      Main.queueInput(Command.PromptCancel, queue, pending)
      awaitCondition("the cancel was not acknowledged")(pending.get() == 0)
      assertEquals(state.get().prompt, None)
      assertEquals(callbacks.size(), 2)
    finally
      releaseSecond.countDown()
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
      assert(workers.forall(!_.isAlive), "a fold test thread did not stop")
  }

  test("the application frame helper withholds a market for either undersized dimension") {
    for size <- Vector((99, 30), (100, 29), (0, 0), (100, 30), (120, 40)) do
      val shown = new AtomicReference(Option("XMR"))
      var writes = 0
      Main.writeMarketFrame(
        "BTC",
        size,
        shown,
        () => {
          assertEquals(shown.get(), None)
          writes += 1
        }
      )
      assertEquals(writes, 1)
      val expected = if size._1 >= 100 && size._2 >= 30 then Some("BTC") else None
      assertEquals(shown.get(), expected)
    val shown = new AtomicReference(Option("XMR"))
    intercept[java.io.IOException] {
      Main.writeMarketFrame(
        "BTC",
        (100, 30),
        shown,
        () => throw new java.io.IOException("failed frame")
      )
    }
    assertEquals(shown.get(), None)
  }

  test("application startup passes loaded test network settings to its public source factory") {
    val config = java.nio.file.Files.createTempFile("desk-screen-network", ".json")
    java.nio.file.Files.setPosixFilePermissions(
      config,
      java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    )
    java.nio.file.Files.writeString(config, """{"network":"testnet"}""")
    final class Stop extends RuntimeException
    var calls = 0
    try
      intercept[Stop] {
        Main.run(
          Main.Args("XMR", true, config = Some(config.toString)),
          settings => {
            calls += 1
            assertEquals(settings.network, "testnet")
            assertEquals(settings.socketUrl, "wss://api.hyperliquid-testnet.xyz/ws")
            assertEquals(settings.baseUrl + "/info", "https://api.hyperliquid-testnet.xyz/info")
            throw new Stop
          }
        )
      }
      assertEquals(calls, 1)
    finally java.nio.file.Files.deleteIfExists(config)
  }
