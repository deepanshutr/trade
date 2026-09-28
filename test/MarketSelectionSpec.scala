package desk

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

object MarketSelectionSpec:
  def bookResponse(coin: String): String = ujson.write(
    ujson.Obj(
      "coin" -> coin,
      "time" -> ujson.Num(1000),
      "levels" -> ujson.Arr(ujson.Arr(), ujson.Arr())
    )
  )

class MarketSelectionSpec extends munit.FunSuite:

  private final class Fixture(reply: String => Option[String]):
    val returned = new CountDownLatch(1)
    val queue = new LinkedBlockingQueue[Event]():
      override def put(event: Event): Unit =
        super.put(event)
        event match
          case Event.MarketChecked(_, _) => returned.countDown()
          case _                         => ()
    val book = Book(Vector(Level("100", "1", 1)), Vector(Level("102", "1", 1)), 1000L)
    val initial =
      State.reduce(Snapshot.initial("XMR", 1000L), Event.BookUpdate("XMR", book, true), 1000L)
    val state = new AtomicReference(initial)
    val requested = new ConcurrentLinkedQueue[String]()
    val selected = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        val request = ujson.read(body)
        assertEquals(request("type").str, "l2Book")
        val coin = request("coin").str
        requested.add(coin)
        reply(coin)
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new AssertionError("this test must not open a connection")
    val feed = new Feed(connector, rest, queue, () => 1000L, "XMR")
    val workers =
      Main.startFold(queue, state, feed, () => 1000L, coin => { selected.add(coin); () })
    def choose(name: String): Unit =
      queue.put(Event.Key(Command.StartMarketPrompt))
      name.foreach(c => queue.put(Event.Key(Command.PromptChar(c))))
      queue.put(Event.Key(Command.PromptSubmit))
    private var columns = 134
    def drain(): Unit =
      columns += 1
      queue.put(Event.Resize(columns, 45))
      Wait.until("input stopped advancing")(state.get().cols == columns)
    def close(): Unit =
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
      assert(workers.forall(!_.isAlive), "selection test left a worker running")

  test("colon market names are refused visibly before any lookup or market switch") {
    val f = new Fixture(coin => Some(MarketSelectionSpec.bookResponse(coin)))
    try
      f.choose("xyz:TSLA")
      f.drain()
      assertEquals(
        f.state.get().marketNotice,
        Some("Colon markets are not supported. Press m to retry")
      )
      for flags <- Vector(Seq.empty, Seq("--paper"), Seq("--trade"), Seq("--record")) do
        assertEquals(
          Main.parseArgs(Seq("xyz:TSLA") ++ flags).swap.toOption,
          Some("Colon markets are not supported.")
        )
      assert(Screen.stats(f.state.get(), 1000L, 100).contains("Colon markets are not supported"))
      assertEquals(f.state.get().coin, "XMR")
      assertEquals(f.state.get().book, f.initial.book)
      assertEquals(f.state.get().marketPending, None)
      assertEquals(f.requested.size(), 0)
      assertEquals(f.selected.size(), 0)
    finally f.close()
  }

  test("an unknown market never replaces the current chart or execution market") {
    val f = new Fixture(_ => Some("null"))
    try
      f.choose("mcchhhhaa1pp")
      f.drain()
      assertEquals(f.state.get().coin, "XMR")
      assertEquals(f.state.get().book, f.initial.book)
      assertEquals(f.state.get().samples, f.initial.samples)
      Wait.until("unknown market was not checked")(f.requested.size() == 1)
      Wait.until("missing visible rejection") {
        Screen.stats(f.state.get(), 1000L, 100).contains("Unknown market")
      }
      assertEquals(f.selected.asScala.toVector, Vector.empty)
      assertEquals(f.feed.coin, "XMR")
    finally f.close()
  }

  test("a held market check keeps prices and input moving") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(_ => {
      entered.countDown()
      release.await()
      None
    })
    try
      f.choose("BTC")
      assert(entered.await(3, TimeUnit.SECONDS), "selection never checked the market")
      f.drain()
      assertEquals(f.state.get().coin, "XMR")
      assertEquals(f.state.get().book, f.initial.book)
      assert(Screen.stats(f.state.get(), 1000L, 100).contains("Checking BTC"))
      val fresh = f.book.copy(time = 1001L)
      f.queue.put(Event.BookUpdate("XMR", fresh, true))
      f.queue.put(Event.Key(Command.Quit))
      Wait.until("quit blocked behind a public request")(f.state.get().quit)
      assertEquals(f.state.get().book, Some(fresh))
      assertEquals(release.getCount, 1L)
    finally
      release.countDown()
      f.close()
  }

  test("a verified market switches only after checking and clears the old market data") {
    val f = new Fixture(coin => Some(MarketSelectionSpec.bookResponse(coin)))
    try
      f.choose("BTC")
      Wait.until("verified selection did not arrive")(f.state.get().coin == "BTC")
      Wait.until("verified selection did not reach feed")(f.feed.coin == "BTC")
      assertEquals(f.selected.asScala.toVector, Vector("BTC"))
      assertEquals(f.state.get().book, None)
      assertEquals(f.state.get().samples, Vector.empty)
      assertEquals(f.state.get().marketPending, None)
      assertEquals(f.state.get().marketNotice, None)
    finally f.close()
  }

  for response <- Vector(
      None,
      Some("bad json"),
      Some("{}"),
      Some(MarketSelectionSpec.bookResponse("ETH"))
    )
  do
    test(s"unavailable or mismatched validation preserves the selected market: $response") {
      val f = new Fixture(_ => response)
      try
        f.choose("BTC")
        assert(f.returned.await(3, TimeUnit.SECONDS))
        f.drain()
        assertEquals(f.state.get().coin, "XMR")
        assertEquals(f.state.get().book, f.initial.book)
        assert(Screen.stats(f.state.get(), 1000L, 53).contains("Cannot check market"))
        assertEquals(f.selected.size(), 0)
      finally f.close()
    }

  for cancel <- Vector(
      Command.Cancel,
      Command.PromptCancel,
      Command.StartMarketPrompt,
      Command.Quit
    )
  do
    test(s"a late successful check cannot switch markets after $cancel") {
      val entered = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val f = new Fixture(coin => {
        entered.countDown()
        release.await()
        Some(MarketSelectionSpec.bookResponse(coin))
      })
      try
        f.choose("BTC")
        assert(entered.await(3, TimeUnit.SECONDS))
        f.queue.put(Event.Key(cancel))
        f.drain()
        release.countDown()
        assert(f.returned.await(3, TimeUnit.SECONDS))
        f.drain()
        assertEquals(f.state.get().coin, "XMR")
        assertEquals(f.state.get().book, f.initial.book)
        assertEquals(f.selected.size(), 0)
        assertEquals(f.state.get().marketPending, None)
      finally
        release.countDown()
        f.close()
    }

  test("reselecting the same requested name cannot accept an earlier canceled check") {
    val entered = Vector.fill(2)(new CountDownLatch(1))
    val release = Vector.fill(2)(new CountDownLatch(1))
    val count = new java.util.concurrent.atomic.AtomicInteger()
    val f = new Fixture(coin => {
      val index = count.getAndIncrement()
      entered(index).countDown()
      release(index).await()
      Some(MarketSelectionSpec.bookResponse(coin))
    })
    try
      f.choose("BTC")
      assert(entered.head.await(3, TimeUnit.SECONDS))
      f.queue.put(Event.Key(Command.Cancel))
      f.choose("BTC")
      f.drain()
      release.head.countDown()
      assert(entered.last.await(3, TimeUnit.SECONDS))
      f.drain()
      assertEquals(f.state.get().coin, "XMR")
      assertEquals(f.selected.size(), 0)
      assert(f.state.get().marketPending.nonEmpty)
      release.last.countDown()
      Wait.until("latest selection was not accepted")(f.state.get().coin == "BTC")
      assertEquals(f.selected.asScala.toVector, Vector("BTC"))
    finally
      release.foreach(_.countDown())
      f.close()
  }

  test("syntax errors make no public request and keep a short visible correction") {
    val f = new Fixture(_ => throw new AssertionError("invalid identifier reached the endpoint"))
    try
      f.choose("bad\"name")
      f.drain()
      assertEquals(f.state.get().coin, "XMR")
      assertEquals(f.requested.size(), 0)
      assert(Screen.stats(f.state.get(), 1000L, 53).contains("Invalid market name"))
    finally f.close()
  }

  test("pending selection blocks displayed and keyboard trade actions") {
    val normal = Snapshot.initial("XMR", 1000L)
    val pending = normal.copy(marketPending = Some((1L, "BTC")))
    val view = TradingView(
      "XMR",
      Some(BigDecimal(0)),
      "ready",
      true,
      false,
      Some(MarketRules(0, 3, 5)),
      Some(BigDecimal(100)),
      Some(BigDecimal(102))
    )
    var previews = 0
    var sends = 0
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => 1000L,
      _ => { previews += 1; Left("must not preview") },
      _ => { sends += 1; true },
      _ => Left("disabled")
    )
    def frame(s: Snapshot) = Workspace.draw(
      s,
      view,
      controller.panel,
      100,
      30,
      1000L,
      java.time.ZoneOffset.UTC,
      Style.mono
    )
    val before = frame(normal)
    controller.publish(before)
    val action = before.buttons.find(_.action.isInstanceOf[DeskAction.Prepare]).get
    controller.handle(Command.PlaceOrder(Trading.action(0)), pending, view, (100, 30), false)
    controller.handle(
      Command.PointerPress(action.x, action.y, 0),
      pending,
      view,
      (100, 30),
      false
    )
    controller.handle(Command.Pointer(action.x, action.y, 0), pending, view, (100, 30), false)
    assertEquals((previews, sends), (0, 0))
    assert(!frame(pending).buttons.exists(_.action.isInstanceOf[DeskAction.Prepare]))
    assertEquals(sends, 0)
    assert(!controller.handle(Command.Cancel, pending, view, (100, 30), false))
  }

  test("Escape after Enter reaches the fold before pending selection has been published") {
    val controller = new DeskController(
      PanelView(),
      () => 1000L,
      _ => Left("disabled"),
      _ => false,
      _ => Left("disabled")
    )
    for prompt <- Vector(None, Some("BTC")) do
      val snapshot = Snapshot.initial("XMR", 1000L).copy(prompt = prompt)
      assert(
        !controller.handle(
          Command.Cancel,
          snapshot,
          TradingView("XMR", None, "view", false, false),
          (100, 30),
          true
        )
      )
  }

  test("pending Target and mode controls are visibly disabled") {
    val snapshot = Snapshot.initial("XMR", 1000L).copy(marketPending = Some((1L, "BTC")))
    val view = TradingView(
      "XMR",
      Some(BigDecimal(0)),
      "ready",
      true,
      false,
      Some(MarketRules(0, 3, 5)),
      Some(BigDecimal(100)),
      Some(BigDecimal(102))
    )
    val frame = Workspace.draw(
      snapshot,
      view,
      PanelView(mode = "PAPER"),
      100,
      30,
      1000L,
      java.time.ZoneOffset.UTC,
      Style.mono
    )
    assert(
      !frame.buttons.exists(b => b.action.isInstanceOf[DeskAction.Mode])
    )
    assertEquals(frame.buttons, Vector.empty)
    assert(frame.controls.exists(_.action.isInstanceOf[DeskAction.Mode]))
  }

  test("unknown startup markets never open a subscription connection") {
    val queue = new ConcurrentLinkedQueue[Event]()
    var connections = 0
    val connector = new Connector:
      def connect(text: String => Unit, close: () => Unit): Socket =
        connections += 1
        throw new java.io.IOException("unexpected subscription")
    val rest = new Rest:
      def post(body: String): Option[String] = Some("null")
    val feed = new Feed(connector, rest, queue, () => 1000L, "mcchhhhaa1pp")
    final class Stop extends RuntimeException
    intercept[Stop](feed.runForever(_ => throw new Stop))
    assertEquals(connections, 0)
    assert(feed.status.contains("Unknown market"))
    val unknown = Snapshot.initial("mcchhhhaa1pp", 1000L)
    val shown = queue.asScala.foldLeft(unknown)((s, e) => State.reduce(s, e, 1000L))
    assert(Screen.stats(shown, 1000L, 100).contains("Unknown market"))
    val other =
      queue.asScala.foldLeft(Snapshot.initial("XMR", 1000L))((s, e) => State.reduce(s, e, 1000L))
    assertEquals(other.marketNotice, None)
  }

  test("a delayed startup rejection keeps the replacement-market prompt visible") {
    val editing = Snapshot.initial("mcchhhhaa1pp", 1000L).copy(prompt = Some("XM"))
    val rejected =
      State.reduce(editing, Event.MarketUnavailable(editing.coin, Feed.unknownMarket), 1000L)
    for width <- Vector(53, 100) do
      assert(Screen.stats(rejected, 1000L, width).contains("market: XM_"))
      assert(Screen.stats(rejected.copy(prompt = None), 1000L, width).contains("Unknown market"))
  }

  for response <- Vector(None, Some("malformed"), Some(MarketSelectionSpec.bookResponse("XMR"))) do
    test(
      s"startup can use its socket when lookup confirms the market or is unavailable: $response"
    ) {
      var connections = 0
      val connector = new Connector:
        def connect(text: String => Unit, close: () => Unit): Socket =
          connections += 1
          throw new java.io.IOException("bounded test connection")
      val rest = new Rest:
        def post(body: String): Option[String] = response
      val feed = new Feed(connector, rest, new ConcurrentLinkedQueue[Event](), () => 1000L, "XMR")
      final class Stop extends RuntimeException
      intercept[Stop](feed.runForever(_ => throw new Stop))
      assertEquals(connections, 1)
    }
