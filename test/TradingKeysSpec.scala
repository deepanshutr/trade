package desk

class TradingKeysSpec extends munit.FunSuite:
  private val now = 1_700_000_000_000L
  private val snapshot = Snapshot.initial("XMR", now)
  private val rules = MarketRules(0, 3, 5)
  private val view = TradingView(
    "XMR",
    Some(BigDecimal(0)),
    "ready",
    true,
    false,
    Some(rules),
    Some(BigDecimal("546.59")),
    Some(BigDecimal("546.71")),
    None
  )
  private val buy = OrderIntent(Side.Buy, OrderKind.Best, big = true)

  private def invoke(
      command: Command,
      s: Snapshot = snapshot,
      localPrompt: Boolean = false,
      size: (Int, Int) = (100, 30),
      v: TradingView = view,
      target: Option[(Side, BigDecimal)] = None,
      accept: Boolean = true,
      shownCoin: Option[String] = Some("XMR"),
      pendingPrompts: Int = 0
  ): ((Boolean, Option[(Side, BigDecimal)]), Vector[(OrderIntent, String)]) =
    val calls = scala.collection.mutable.ArrayBuffer.empty[(OrderIntent, String)]
    val result = Main.handleTrade(
      command,
      s,
      localPrompt,
      size,
      v,
      shownCoin,
      pendingPrompts,
      target,
      (intent, coin) => {
        calls += ((intent, coin))
        accept
      }
    )
    (result, calls.toVector)

  test("every immediate key dispatches its exact intent and captured market without a fold") {
    val decoded = (49 to 56).map(n => Keys.step(Vector.empty, n, false)._2.get)
    decoded.foreach { command =>
      val (result, calls) = invoke(command)
      val intent = command match
        case Command.PlaceOrder(value) => value
        case other                     => fail(s"unexpected $other")
      assertEquals(result, (true, None))
      assertEquals(calls, Vector(intent -> "XMR"))
    }
  }

  test("both prompt states and a stopped screen block immediate orders") {
    assert(invoke(Command.PlaceOrder(buy), localPrompt = true)._2.isEmpty)
    assert(invoke(Command.PlaceOrder(buy), s = snapshot.copy(prompt = Some("BTC")))._2.isEmpty)
    assert(invoke(Command.PlaceOrder(buy), s = snapshot.copy(quit = true))._2.isEmpty)
  }

  test("unacknowledged prompts block each trading entry even when the old frame still agrees") {
    for pending <- Vector(1, 2) do
      assert(invoke(Command.PlaceOrder(buy), pendingPrompts = pending)._2.isEmpty)
      assert(
        invoke(
          Command.SubmitTarget,
          target = Some(Side.Buy -> BigDecimal("546.71")),
          pendingPrompts = pending
        )._2.isEmpty
      )
      assertEquals(invoke(Command.SelectTarget(Side.Buy), pendingPrompts = pending)._1._2, None)
  }

  test("a small screen and unavailable or mismatched worker block immediate orders") {
    for size <- Vector((99, 30), (100, 29), (0, 0)) do
      assert(invoke(Command.PlaceOrder(buy), size = size)._2.isEmpty)
    for unavailable <- Vector(
        view.copy(ready = false),
        view.copy(pending = true),
        view.copy(coin = "BTC")
      )
    do assert(invoke(Command.PlaceOrder(buy), v = unavailable)._2.isEmpty)
    assertEquals(invoke(Command.PlaceOrder(buy), size = (100, 30))._2.size, 1)
  }

  test("target selection captures the correct quote and its arrows never submit") {
    val buyTarget = invoke(Command.SelectTarget(Side.Buy))
    assertEquals(buyTarget._1, (true, Some(Side.Buy -> BigDecimal("546.71"))))
    assert(buyTarget._2.isEmpty)
    val sellTarget = invoke(Command.SelectTarget(Side.Sell))
    assertEquals(sellTarget._1, (true, Some(Side.Sell -> BigDecimal("546.59"))))
    val moved = invoke(Command.ZoomIn, target = buyTarget._1._2)
    assertEquals(moved._1._2, Some(Side.Buy -> BigDecimal("546.72")))
    assert(moved._2.isEmpty)
    assertEquals(invoke(Command.ZoomOut, target = moved._1._2)._1._2, buyTarget._1._2)
    assertEquals(invoke(Command.ZoomIn)._1, (false, None))
  }

  test("the last completed frame must match the state and worker before an order or selection") {
    for shown <- Vector(None, Some("BTC")) do
      assert(invoke(Command.PlaceOrder(buy), shownCoin = shown)._2.isEmpty)
      assert(
        invoke(
          Command.SubmitTarget,
          target = Some(Side.Buy -> BigDecimal("546.71")),
          shownCoin = shown
        )._2.isEmpty
      )
      assertEquals(invoke(Command.SelectTarget(Side.Buy), shownCoin = shown)._1._2, None)
    val btc = snapshot.copy(coin = "BTC")
    assert(invoke(Command.PlaceOrder(buy), s = btc, v = view.copy(coin = "BTC"))._2.isEmpty)
    assertEquals(
      invoke(
        Command.PlaceOrder(buy),
        s = btc,
        v = view.copy(coin = "BTC"),
        shownCoin = Some("BTC")
      )._2,
      Vector(buy -> "BTC")
    )
  }

  test("a held frame write blocks new-market orders without holding the fold") {
    val shown = new java.util.concurrent.atomic.AtomicReference[Option[String]](Some("XMR"))
    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val switched = new java.util.concurrent.CountDownLatch(1)
    val state = new java.util.concurrent.atomic.AtomicReference(snapshot.copy(prompt = Some("BTC")))
    val queue = new java.util.concurrent.LinkedBlockingQueue[Event]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        switched.countDown()
        Some("[]")
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        throw new IllegalStateException("not connected in this test")
    val feed = new Feed(connector, rest, queue, () => now, "XMR")
    val workers = Main.startFold(queue, state, feed, () => now)
    val writer = new Thread(() =>
      Main.writeMarketFrame(
        "BTC",
        (100, 30),
        shown,
        () => {
          entered.countDown()
          val _ = release.await(3, java.util.concurrent.TimeUnit.SECONDS)
        },
        complete = true
      )
    )
    writer.start()
    try
      assert(entered.await(3, java.util.concurrent.TimeUnit.SECONDS))
      queue.put(Event.Key(Command.PromptSubmit))
      assert(switched.await(3, java.util.concurrent.TimeUnit.SECONDS))
      val btc = state.get()
      assertEquals(btc.coin, "BTC")
      assertEquals(shown.get(), None)
      assert(
        invoke(
          Command.PlaceOrder(buy),
          s = btc,
          v = view.copy(coin = "BTC"),
          shownCoin = shown.get()
        )._2.isEmpty
      )
      release.countDown()
      writer.join(3000)
      assert(!writer.isAlive)
      assertEquals(shown.get(), Some("BTC"))
      assertEquals(
        invoke(
          Command.PlaceOrder(buy),
          s = btc,
          v = view.copy(coin = "BTC"),
          shownCoin = shown.get()
        )._2,
        Vector(buy -> "BTC")
      )
    finally
      release.countDown()
      writer.join(3000)
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
  }

  test("a failed write or small-terminal message cannot publish a trading market") {
    val shown = new java.util.concurrent.atomic.AtomicReference[Option[String]](Some("XMR"))
    intercept[java.io.IOException] {
      Main.writeMarketFrame(
        "BTC",
        (100, 30),
        shown,
        () => throw new java.io.IOException("held"),
        complete = true
      )
    }
    assertEquals(shown.get(), None)
    Main.writeMarketFrame("BTC", (99, 30), shown, () => (), complete = true)
    assertEquals(shown.get(), None)
    assert(invoke(Command.PlaceOrder(buy), shownCoin = shown.get())._2.isEmpty)
  }

  test("target selection respects missing metadata and valid price steps") {
    assertEquals(invoke(Command.SelectTarget(Side.Buy), v = view.copy(rules = None))._1._2, None)
    assertEquals(invoke(Command.SelectTarget(Side.Buy), v = view.copy(ask = None))._1._2, None)
    val original = Some(Side.Sell -> BigDecimal("1"))
    assertEquals(
      invoke(Command.ZoomOut, v = view.copy(rules = None), target = original)._1._2,
      original
    )
  }

  test("hard and best keys are ignored during target selection") {
    val target = Some(Side.Buy -> BigDecimal("546.75"))
    for key <- 49 to 56 do
      val command = Keys.step(Vector.empty, key, false)._2.get
      val (result, calls) = invoke(command, target = target)
      assertEquals(result, (true, target))
      assert(calls.isEmpty)
    assertEquals(invoke(Command.ScrubBack, target = target)._1, (true, target))
  }

  test("enter submits the selected target and clears it only when accepted") {
    val target = Some(Side.Sell -> BigDecimal("546.05"))
    val (accepted, calls) = invoke(Command.SubmitTarget, target = target)
    assertEquals(accepted, (true, None))
    assertEquals(
      calls,
      Vector(OrderIntent(Side.Sell, OrderKind.Target, target = Some(BigDecimal("546.05"))) -> "XMR")
    )
    assertEquals(invoke(Command.SubmitTarget, target = target, accept = false)._1, (true, target))
    assert(invoke(Command.SubmitTarget)._2.isEmpty)
    assert(invoke(Command.SubmitTarget, target = target, v = view.copy(pending = true))._2.isEmpty)
    assert(invoke(Command.SubmitTarget, target = target, localPrompt = true)._2.isEmpty)
    assert(invoke(Command.SubmitTarget, target = target, size = (99, 30))._2.isEmpty)
  }

  test("escape and opening the market prompt cancel the target without orders") {
    val target = Some(Side.Buy -> BigDecimal(100))
    assertEquals(invoke(Command.CancelTarget, target = target), ((true, None), Vector.empty))
    assertEquals(invoke(Command.StartMarketPrompt, target = target), ((false, None), Vector.empty))
  }

  test("trading commands leave the reducer pure and unchanged") {
    val commands = Vector(
      Command.PlaceOrder(buy),
      Command.SelectTarget(Side.Buy),
      Command.SubmitTarget,
      Command.CancelTarget
    )
    commands.foreach(command =>
      assertEquals(State.reduce(snapshot, Event.Key(command), now), snapshot)
    )
  }

  test("terminal intermediate sequences and mouse fields cannot reach a ready submission") {
    val sequences = Vector(
      Vector(27, 35, 56),
      Vector(27, 40, 48),
      Vector(27, 91, 77, 32, 49, 50),
      Vector(27, 91, 77, 32, 194, 160, 49),
      Vector(27, 91, 116, 49, 50),
      Vector(27, 91, 84, 49, 50, 51, 52, 53, 54)
    )
    for bytes <- sequences do
      var pending = Vector.empty[Int]
      var target = Option.empty[(Side, BigDecimal)]
      val sent = scala.collection.mutable.ArrayBuffer.empty[(OrderIntent, String)]
      bytes.foreach { byte =>
        val (next, command) = Keys.step(pending, byte, false)
        pending = next
        command.foreach { value =>
          val (result, calls) = invoke(value, target = target)
          target = result._2
          sent ++= calls
        }
      }
      assertEquals(sent.toVector, Vector.empty)
      assertEquals(target, None)
  }

  test("the candle toggle reaches the chart without submitting or moving a selected target") {
    val command = Keys.step(Vector.empty, 99, false)._2.get
    for target <- Vector(None, Some(Side.Buy -> BigDecimal("546.71"))) do
      assertEquals(invoke(command, target = target), ((false, target), Vector.empty))
      val changed = State.reduce(snapshot, Event.Key(command), now)
      assertEquals(changed.chartMode, ChartMode.Candles)
  }
