package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class MainWiringFollowupSpec extends munit.FunSuite:

  test("the application fold helper acknowledges each prompt after its state is published") {
    val now = 1000L
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", now).copy(prompt = Some("BTC")))
    val pending = new AtomicInteger(0)
    val callbacks = new ConcurrentLinkedQueue[(String, String, Int)]()
    val enteredSecond = new CountDownLatch(1)
    val releaseSecond = new CountDownLatch(1)
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some(MarketSelectionSpec.bookResponse(ujson.read(body)("coin").str))
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new IllegalStateException("no connection in this test")
    val feed = new Feed(connector, rest, queue, () => now, "XMR")
    Main.queueInput(Command.PromptSubmit, queue, pending)
    Main.queueInput(Command.StartMarketPrompt, queue, pending)
    "ETH".foreach(c => Main.queueInput(Command.PromptChar(c), queue, pending))
    Main.queueInput(Command.PromptSubmit, queue, pending)
    assertEquals(pending.get(), 2)
    val workers = Main.startFold(
      queue,
      state,
      feed,
      () => now,
      onMarket = coin =>
        val _ = callbacks.add((coin, state.get().coin, pending.get()))
        if coin == "ETH" then
          enteredSecond.countDown()
          val _ = releaseSecond.await(3, TimeUnit.SECONDS)
      ,
      onPromptDone = () => { val _ = pending.decrementAndGet() }
    )
    try
      assert(enteredSecond.await(3, TimeUnit.SECONDS))
      assertEquals(state.get().coin, "XMR")
      assertEquals(pending.get(), 0)
      assert(state.get().marketPending.nonEmpty)
      assertEquals(
        callbacks.asScala.toVector,
        Vector(("ETH", "XMR", 0))
      )
      releaseSecond.countDown()
      Wait.until("the verified selection was not published")(state.get().coin == "ETH")
      assertEquals(pending.get(), 0)
      assertEquals(state.get().coin, "ETH")
      assertEquals(state.get().prompt, None)
      Main.queueInput(Command.StartMarketPrompt, queue, pending)
      Main.queueInput(Command.PromptCancel, queue, pending)
      Wait.until("the cancel was not acknowledged")(pending.get() == 0)
      assertEquals(state.get().prompt, None)
      assertEquals(callbacks.size(), 1)
    finally
      releaseSecond.countDown()
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
      assert(workers.forall(!_.isAlive), "a fold test thread did not stop")
  }

  test("application startup passes loaded test network settings to its public source factory") {
    val config = java.nio.file.Files.createTempFile("desk-screen-network", ".json")
    Platform.setMode(config, "rw-------")
    java.nio.file.Files.writeString(config, """{"network":"testnet"}""")
    final class Stop extends RuntimeException
    var calls = 0
    val home = java.nio.file.Files.createTempDirectory("desk-wiring-home")
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
          },
          keys = KeyStartup.Hooks(home = home, stop = (code, _) => fail(s"Desk stopped ($code)")),
          polymarket = OfflinePolymarket.client
        )
      }
      assertEquals(calls, 1)
    finally
      java.nio.file.Files.deleteIfExists(config)
      java.nio.file.Files.deleteIfExists(home)
  }
