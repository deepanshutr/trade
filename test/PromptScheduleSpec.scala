package desk

import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

class PromptScheduleSpec extends munit.FunSuite:
  private def awaitMarket(state: AtomicReference[Snapshot], coin: String): Unit =
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while state.get().coin != coin && System.nanoTime() < deadline do Thread.sleep(1)
    assertEquals(state.get().coin, coin)
  private val now = 10000L
  private val view = TradingView(
    "XMR",
    Some(BigDecimal(0)),
    "ready",
    true,
    false,
    Some(MarketRules(0, 3, 5)),
    Some(BigDecimal("99.99")),
    Some(BigDecimal(100)),
    None
  )

  private def feed(queue: LinkedBlockingQueue[Event]): Feed =
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new IllegalStateException("not connected in this test")
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some(MarketSelectionSpec.bookResponse(ujson.read(body)("coin").str))
    new Feed(connector, rest, queue, () => now, "XMR")

  private final class Input(
      state: AtomicReference[Snapshot],
      queue: LinkedBlockingQueue[Event],
      pendingPrompts: AtomicInteger
  ):
    var pending = Vector.empty[Int]
    var promptOpen = false
    var target = Option.empty[(Side, BigDecimal)]
    val sent = scala.collection.mutable.ArrayBuffer.empty[String]

    def byte(value: Int): Unit =
      val (next, command) = Keys.step(pending, value, promptOpen)
      pending = next
      command.foreach { cmd =>
        val snapshot = state.get()
        val (handled, selected) = Main.handleTrade(
          cmd,
          snapshot,
          promptOpen,
          (100, 30),
          view.copy(coin = snapshot.coin),
          Some(snapshot.coin),
          pendingPrompts.get(),
          target,
          (_, coin) => {
            sent += coin
            true
          }
        )
        target = selected
        promptOpen = Main.nextPromptOpen(promptOpen, cmd)
        if !handled then Main.queueInput(cmd, queue, pendingPrompts)
      }

  test("a submitted prompt blocks orders while the fold still shows no prompt") {
    val state = new AtomicReference(Snapshot.initial("XMR", now))
    val queue = new LinkedBlockingQueue[Event]()
    val pending = new AtomicInteger(0)
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val acknowledged = new CountDownLatch(1)
    val published = new AtomicReference(Option.empty[String])
    val workers = Main.startFold(
      queue,
      state,
      feed(queue),
      () => {
        entered.countDown()
        val _ = release.await(3, TimeUnit.SECONDS)
        now
      },
      onPromptDone = () => {
        published.set(Some(state.get().coin))
        val _ = pending.decrementAndGet()
        acknowledged.countDown()
      }
    )
    val input = new Input(state, queue, pending)
    try
      input.byte(109)
      assert(entered.await(3, TimeUnit.SECONDS))
      Vector(66, 84, 67, 13, 49, 57).foreach(input.byte)
      assert(!input.promptOpen)
      assertEquals(state.get().prompt, None)
      assertEquals(state.get().coin, "XMR")
      assertEquals(pending.get(), 1)
      assertEquals(input.sent.toVector, Vector.empty)
      assertEquals(input.target, None)
      release.countDown()
      assert(acknowledged.await(3, TimeUnit.SECONDS))
      assertEquals(published.get(), Some("XMR"))
      assertEquals(pending.get(), 0)
      awaitMarket(state, "BTC")
      input.byte(49)
      assertEquals(input.sent.toVector, Vector("BTC"))
    finally
      release.countDown()
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
  }

  test("one published prompt cannot acknowledge a second queued submission") {
    val state = new AtomicReference(Snapshot.initial("XMR", now))
    val queue = new LinkedBlockingQueue[Event]()
    val pending = new AtomicInteger(0)
    val firstEntered = new CountDownLatch(1)
    val releaseFirst = new CountDownLatch(1)
    val firstDone = new CountDownLatch(1)
    val releaseSecond = new CountDownLatch(1)
    val secondDone = new CountDownLatch(1)
    val acknowledgements = new AtomicInteger(0)
    val workers = Main.startFold(
      queue,
      state,
      feed(queue),
      () => {
        firstEntered.countDown()
        val _ = releaseFirst.await(3, TimeUnit.SECONDS)
        now
      },
      onPromptDone = () => {
        val _ = pending.decrementAndGet()
        if acknowledgements.incrementAndGet() == 1 then
          firstDone.countDown()
          val _ = releaseSecond.await(3, TimeUnit.SECONDS)
        else secondDone.countDown()
      }
    )
    val input = new Input(state, queue, pending)
    try
      input.byte(109)
      assert(firstEntered.await(3, TimeUnit.SECONDS))
      Vector(66, 84, 67, 13, 109, 69, 84, 72, 13, 49).foreach(input.byte)
      assertEquals(pending.get(), 2)
      assertEquals(input.sent.toVector, Vector.empty)
      releaseFirst.countDown()
      assert(firstDone.await(3, TimeUnit.SECONDS))
      assertEquals(state.get().coin, "XMR")
      assertEquals(state.get().prompt, None)
      assertEquals(pending.get(), 1)
      input.byte(49)
      assertEquals(input.sent.toVector, Vector.empty)
      releaseSecond.countDown()
      assert(secondDone.await(3, TimeUnit.SECONDS))
      awaitMarket(state, "ETH")
      assertEquals(pending.get(), 0)
      input.byte(49)
      assertEquals(input.sent.toVector, Vector("ETH"))
    finally
      releaseFirst.countDown()
      releaseSecond.countDown()
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
  }

  test("submit and cancel counts are visible before their queue writes") {
    val pending = new AtomicInteger(0)
    val observed = scala.collection.mutable.ArrayBuffer.empty[Int]
    val queue = new LinkedBlockingQueue[Event]():
      override def put(event: Event): Unit =
        val _ = observed.addOne(pending.get())
        super.put(event)
    Main.queueInput(Command.StartMarketPrompt, queue, pending)
    Main.queueInput(Command.PromptSubmit, queue, pending)
    Main.queueInput(Command.StartMarketPrompt, queue, pending)
    Main.queueInput(Command.PromptCancel, queue, pending)
    assertEquals(observed.toVector, Vector(0, 1, 1, 2))
    val state = new AtomicReference(Snapshot.initial("XMR", now))
    val done = new CountDownLatch(2)
    val publishedPrompts = new java.util.concurrent.ConcurrentLinkedQueue[Boolean]()
    val workers = Main.startFold(
      queue,
      state,
      feed(queue),
      () => now,
      onPromptDone = () => {
        val _ = publishedPrompts.add(state.get().prompt.isEmpty)
        val _ = pending.decrementAndGet()
        done.countDown()
      }
    )
    try
      assert(done.await(3, TimeUnit.SECONDS))
      assertEquals(pending.get(), 0)
      assertEquals(publishedPrompts.size(), 2)
      assert(publishedPrompts.stream().allMatch(value => value))
    finally
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
  }
