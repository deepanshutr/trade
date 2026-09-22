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
        },
        complete = true
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
        () => throw new java.io.IOException("failed frame"),
        complete = true
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

  test("an incomplete footer cannot acknowledge a tradable frame at either supported width") {
    for cols <- Vector(100, 120) do
      val snapshot = Snapshot.initial("XMR", 1000)
      val view = TradingView(
        "XMR",
        Some(BigDecimal(0)),
        "Ready",
        ready = true,
        rules = Some(MarketRules(0, 3, 5)),
        bid = Some(BigDecimal("99.99")),
        ask = Some(BigDecimal(100)),
        outcome = Some("XMR " + "x" * 500)
      )
      val target = Some(Side.Buy -> BigDecimal(100))
      val plan = OrderPlan(
        "XMR",
        0,
        Side.Buy,
        OrderKind.Target,
        BigDecimal(1),
        BigDecimal(100),
        BigDecimal(100),
        BigDecimal(1),
        BigDecimal(100),
        Some(BigDecimal(100)),
        BigDecimal(1)
      )
      val footer = TradingScreen.rows(view, target, _ => Right(plan), cols = cols)
      assert(footer.size < Layout.footerRows)
      val shown = new AtomicReference(Option("XMR"))
      var writes = 0
      Main.writeMarketFrame(
        "XMR",
        (cols, 30),
        shown,
        () => { writes += 1 },
        complete = footer.size == Layout.footerRows
      )
      assertEquals(writes, 1)
      assertEquals(shown.get(), None)
      val commands = (49 to 56).map(n => Keys.step(Vector.empty, n, false)._2.get).toVector ++
        Vector(
          Command.SubmitTarget,
          Command.SelectTarget(Side.Buy),
          Command.SelectTarget(Side.Sell)
        )
      for selected <- Vector(None, target) do
        for command <- commands do
          val _ = Main.handleTrade(
            command,
            snapshot,
            false,
            (cols, 30),
            view,
            shown.get(),
            0,
            selected,
            (_, _) => fail("an incomplete display sent an order")
          )
      Main.writeMarketFrame("XMR", (cols, 30), shown, () => { writes += 1 }, complete = true)
      assertEquals(writes, 2)
      assertEquals(shown.get(), Some("XMR"))
      var sends = 0
      val accepted = Main.handleTrade(
        Command.SubmitTarget,
        snapshot,
        false,
        (cols, 30),
        view,
        shown.get(),
        0,
        target,
        (_, _) => { sends += 1; true }
      )
      assertEquals(sends, 1)
      assertEquals(accepted, (true, None))
  }

  test(
    "oversized prompt markets are rejected and synthetic long identities still require a full display"
  ) {
    val now = 1000L
    val rules = MarketRules(0, 3, 5)
    val book = Book(Vector(Level("99.99", "10", 1)), Vector(Level("100", "10", 1)), now)
    for cols <- Vector(100, 120) do
      for length <- Vector(80, 200) do
        val name = "X" * length
        val editing = name.foldLeft(
          State.reduce(
            Snapshot.initial("XMR", now),
            Event.Key(Command.StartMarketPrompt),
            now
          )
        )((s, c) => State.reduce(s, Event.Key(Command.PromptChar(c)), now))
        val submitted = State.reduce(editing, Event.Key(Command.PromptSubmit), now)
        assertEquals(submitted.coin, "XMR")
        val snapshot = submitted.copy(coin = name)
        val view = TradingView(
          name,
          Some(BigDecimal(0)),
          "Ready",
          ready = true,
          rules = Some(rules),
          bid = Some(BigDecimal("99.99")),
          ask = Some(BigDecimal(100))
        )
        val preview: OrderIntent => Either[String, OrderPlan] = intent =>
          Trading.plan(
            name,
            rules,
            book,
            view.reference(intent.side).get,
            BigDecimal(0),
            intent,
            TradeSizes()
          )
        for target <- Vector(None, Some(Side.Buy -> BigDecimal(100))) do
          val footer = TradingScreen.rows(view, target, preview, cols = cols)
          val complete = footer.size == Layout.footerRows
          assertEquals(complete, length == 80, footer.mkString("\n"))
          if complete then assert(footer.exists(_.contains(name)), footer.mkString("\n"))
          else assert(footer.head.contains("Order keys disabled"), footer.head)
          val shown = new AtomicReference(Option("XMR"))
          var writes = 0
          Main.writeMarketFrame(name, (cols, 30), shown, () => { writes += 1 }, complete)
          assertEquals(writes, 1)
          assertEquals(shown.get(), Option.when(complete)(name))
          val command = if target.nonEmpty then Command.SubmitTarget
          else Keys.step(Vector.empty, 49, false)._2.get
          var sends = 0
          val submit: (OrderIntent, String) => Boolean = (_, coin) => {
            assertEquals(coin, name)
            sends += 1
            true
          }
          val _ = Main.handleTrade(
            command,
            snapshot,
            false,
            (cols, 30),
            view,
            shown.get(),
            0,
            target,
            submit
          )
          assertEquals(sends, if complete then 1 else 0)
          val _ = Main.handleTrade(
            command,
            snapshot,
            false,
            (cols, 30),
            view,
            Some(name.take(37) + "..."),
            0,
            target,
            (_, _) => fail("shortened identity authorized an order")
          )
  }
