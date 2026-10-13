package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

object FreshnessProcessProbe:
  private val at = 1800000000000L
  def main(args: Array[String]): Unit =
    val root = Path.of(args(0))
    val role = args(1)
    val now = new AtomicLong(at)
    Files.writeString(root.resolve(role + ".started"), ProcessHandle.current().pid().toString)
    val budget = new PublicRequestBudget(root.resolve("budget"), () => now.get())
    val weight = if role == "price" then 2 else 104
    budget.execute(weight, delay => { val _ = now.addAndGet(delay) }) {
      Files.writeString(root.resolve(role + ".entered"), now.get().toString)
      if role == "first" then
        val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(15)
        while !Files.exists(root.resolve("release")) && System.nanoTime() < until do Thread.sleep(2)
        require(Files.exists(root.resolve("release")), "held request was not released")
    }

class IndependentFreshnessSpec extends TempDirectorySuite:
  override val munitTimeout = 60.seconds
  private val at = 1800000000000L
  private val metadata =
    """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""
  private def launch(errors: ConcurrentLinkedQueue[Throwable])(body: => Unit): Thread =
    val thread = new Thread(() =>
      try body
      catch
        case _: InterruptedException => ()
        case error: Throwable        => { val _ = errors.add(error) }
    )
    thread.setDaemon(true)
    thread.start()
    thread
  private def stop(thread: Thread): Unit =
    if thread.isAlive then thread.interrupt()
    thread.join(3000L)
    assert(!thread.isAlive, "owned thread did not stop")
  private def bar: String =
    s"""[{"t":${at - 60000L},"o":"100","h":"101","l":"99","c":"100","v":"1"}]"""
  private val stoppedConnector = new Connector:
    def connect(text: String => Unit, closed: () => Unit): Socket =
      throw new UnsupportedOperationException("no connection in this fixture")

  test("held history permits selection and close, then only the latest selection drains") {
    val errors = new ConcurrentLinkedQueue[Throwable]()
    val events = new ConcurrentLinkedQueue[Event]()
    val requests = new ConcurrentLinkedQueue[(String, String)]()
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val selected = new CountDownLatch(1)
    val ended = new CountDownLatch(1)
    val subscribed = new CountDownLatch(1)
    val close = new AtomicReference[() => Unit](() => ())
    val rest = new Rest:
      def post(body: String): Option[String] =
        val req = ujson.read(body)("req")
        val key = (req("coin").str, req("interval").str)
        requests.add(key)
        if key == ("XMR", "1m") then
          entered.countDown()
          require(release.await(5, TimeUnit.SECONDS))
          None
        else Some(bar)
    val connector = new Connector:
      def connect(text: String => Unit, closed: () => Unit): Socket =
        close.set(closed)
        new Socket:
          def send(body: String): Unit = subscribed.countDown()
          def close(): Unit = ()
    val feed = new Feed(connector, rest, events, () => at, "XMR")
    val repair = launch(errors) { feed.awaitHistoryRepair(); feed.awaitHistoryRepair() }
    val connection = launch(errors) {
      try { val _ = feed.connectOnce() }
      finally ended.countDown()
    }
    var selection = Option.empty[Thread]
    try
      assert(subscribed.await(3, TimeUnit.SECONDS))
      assert(entered.await(3, TimeUnit.SECONDS), "repair never actually entered held transport")
      selection = Some(launch(errors) {
        feed.setMarket("BTC")
        feed.setMarket("PAXG")
        selected.countDown()
      })
      assert(selected.await(500, TimeUnit.MILLISECONDS), "held history blocked market selection")
      close.get()()
      assert(ended.await(500, TimeUnit.MILLISECONDS), "held history blocked socket close")
      assertEquals(requests.asScala.toVector, Vector(("XMR", "1m")))
      release.countDown()
      repair.join(3000L)
      assert(!repair.isAlive)
      val expected = Vector(("XMR", "1m")) ++
        Vector("PAXG", "BTC").flatMap(c => State.intervals.map(i => (c, i)))
      assertEquals(requests.asScala.toVector, expected)
      assert(!events.asScala.exists(_ == Event.CandleHistoryFailed))
      assertEquals(feed.coin, "PAXG")
    finally
      release.countDown()
      close.get()()
      selection.foreach(stop)
      stop(connection)
      stop(repair)
    assertEquals(errors.asScala.toVector, Vector.empty)
  }

  test("many market changes coalesce into one queued history wake") {
    val errors = new ConcurrentLinkedQueue[Throwable]()
    val requests = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(ujson.read(body)("req")("coin").str)
        Some(bar)
    val feed = new Feed(
      stoppedConnector,
      rest,
      new ConcurrentLinkedQueue[Event](),
      () => at,
      "XMR"
    )
    (1 to 100).foreach(n => feed.setMarket(if n % 2 == 0 then "PAXG" else "BTC"))
    val completed = new AtomicInteger(0)
    val repair = launch(errors) {
      feed.awaitHistoryRepair()
      completed.incrementAndGet()
      feed.awaitHistoryRepair()
      completed.incrementAndGet()
    }
    try
      Wait.until("first history wake did not run")(completed.get() >= 1)
      Thread.sleep(100L)
      assertEquals(completed.get(), 1)
      assertEquals(
        requests.asScala.toVector,
        Vector("PAXG", "BTC").flatMap(c => Vector.fill(State.intervals.size)(c))
      )
    finally stop(repair)
    assertEquals(errors.asScala.toVector, Vector.empty)
  }

  test("returning to a recently loaded market restores every selected candle series") {
    val events = new ConcurrentLinkedQueue[Event]()
    val requests = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        Some(bar)
    val feed = new Feed(stoppedConnector, rest, events, () => at, "XMR")
    var state = Snapshot.initial("XMR", at)
    def drain(): Unit =
      var event = events.poll()
      while event != null do
        state = State.reduce(state, event, at)
        event = events.poll()
    def loaded: Boolean = State.intervals.forall(i => state.candles.get(i).exists(_.nonEmpty))
    def select(coin: String): Unit =
      state = State.reduce(state.copy(prompt = Some(coin)), Event.Key(Command.PromptSubmit), at)
      assertEquals(state.coin, coin)
      if coin == "XMR" then assert(loaded, "return did not restore cached candles")
      feed.setMarket(coin)
      feed.awaitHistoryRepair()
      drain()
    feed.repairHistory()
    drain()
    assert(loaded, "initial history did not load")
    select("BTC")
    assert(loaded, "first switch did not load")
    select("XMR")
    assert(loaded, s"returning selection stayed empty after requests ${requests.asScala.toVector}")
    requests.asScala.toVector.takeRight(State.intervals.size).foreach { body =>
      val request = ujson.read(body)("req")
      assertEquals(request("coin").str, "XMR")
      val interval = request("interval").str
      assertEquals(request("startTime").num.toLong, at - 5000L * State.intervalMs(interval))
    }
  }

  test("an obsolete held response cannot restore a reset selected-history cursor") {
    val events = new ConcurrentLinkedQueue[Event]()
    val requests = new ConcurrentLinkedQueue[String]()
    val errors = new ConcurrentLinkedQueue[Throwable]()
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val first = new AtomicBoolean(true)
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        if first.compareAndSet(true, false) then
          entered.countDown()
          require(release.await(5, TimeUnit.SECONDS))
        Some(bar)
    val feed = new Feed(stoppedConnector, rest, events, () => at, "XMR")
    val repair = launch(errors) { feed.repairHistory(); feed.awaitHistoryRepair() }
    try
      assert(entered.await(3, TimeUnit.SECONDS))
      feed.setMarket("BTC")
      feed.setMarket("XMR")
      release.countDown()
      repair.join(3000L)
      assert(!repair.isAlive)
      val oneMinute = requests.asScala.toVector
        .map(ujson.read(_)("req"))
        .filter(r => r("coin").str == "XMR" && r("interval").str == "1m")
      assertEquals(oneMinute.size, 2)
      assertEquals(oneMinute.last("startTime").num.toLong, at - 5000L * State.intervalMs("1m"))
    finally
      release.countDown()
      stop(repair)
    assertEquals(errors.asScala.toVector, Vector.empty)
  }

  private class PropFixture(held: BigDecimal = BigDecimal(0)):
    val now = new AtomicLong(at)
    val config = TradingConfig()
    val terms = TradeTerms(config.sizes, config.slippage)
    val root = tempDirectory("review-prop-")
    if held > 0 then
      LocalFiles.write(
        root.resolve(PropLedger.file("mainnet")),
        ujson.write(
          ujson.Obj(
            "cash" -> config.propBalance.toString,
            "deposit" -> (config.propBalance + held * 100).toString,
            "realized" -> "0",
            "fees" -> "0",
            "orders" -> ujson.Arr(),
            "count" -> "0",
            "positions" -> ujson.Obj(
              "XMR" -> ujson.Obj(
                "quantity" -> held.toString,
                "entry" -> "100"
              )
            )
          )
        )
      )
    val ledger = PropLedger.open(root, "mainnet", config.propBalance, config.propFee)
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some(metadata)
    val worker = new PropWorker(config, "XMR", ledger, rest, () => now.get(), () => now.get())
    val bids = (0 until 5)
      .map(i => Level((BigDecimal("99.99") - BigDecimal(i) / 100).toString, "0.20", 1))
      .toVector
    val asks = (0 until 5)
      .map(i => Level((BigDecimal(100) + BigDecimal(i) / 100).toString, "0.20", 1))
      .toVector
    val book = Book(bids, asks, at)
    def event(b: Book = book): String =
      def levels(raw: Vector[Level]): ujson.Arr =
        ujson.Arr.from(raw.map(l => ujson.Obj("px" -> l.px, "sz" -> l.sz, "n" -> l.n)))
      ujson.write(
        ujson.Obj(
          "channel" -> "l2Book",
          "data" -> ujson.Obj(
            "coin" -> "XMR",
            "time" -> ujson.Num(b.time.toDouble),
            "levels" -> ujson.Arr(levels(b.bids), levels(b.asks))
          )
        )
      )
    def start(): Unit =
      worker.start()
      Wait.until("metadata fixture did not load")(worker.view.rules.nonEmpty)
    def receive(text: String): Unit = Events.parse(text).foreach(worker.observe)
    def close(): Unit = worker.close()

  for side <- Vector(Side.Buy, Side.Sell) do
    test(s"$side subscription-time five-level prices only fill visible liquidity") {
      val f = new PropFixture(if side == Side.Buy then BigDecimal(0) else BigDecimal(10))
      val errors = new ConcurrentLinkedQueue[Throwable]()
      val delivered = new CountDownLatch(1)
      val close = new AtomicReference[() => Unit](() => ())
      val connector = new Connector:
        def connect(text: String => Unit, ended: () => Unit): Socket =
          close.set(ended)
          new Socket:
            def send(body: String): Unit =
              if body.contains("l2Book") then
                assertEquals(ujson.read(body)("subscription")("fast").bool, true)
                text(f.event())
                delivered.countDown()
            def close(): Unit = ()
      val feed = new Feed(
        connector,
        f.rest,
        new ConcurrentLinkedQueue[Event](),
        () => f.now.get(),
        "XMR",
        f.worker.observe
      )
      f.start()
      val connection = launch(errors) { val _ = feed.connectOnce() }
      try
        assert(delivered.await(3, TimeUnit.SECONDS))
        assert(f.worker.view.ready, f.worker.view.message)
        val intent = OrderIntent(side, OrderKind.Hard)
        val plan = f.worker.preview(intent, f.terms).toOption.get
        // $500 at 5x is 25, and a sell of it closes the holding of 10.
        assertEquals(plan.quantity, BigDecimal(if side == Side.Buy then 25 else 10))
        assertEquals(plan.estimatedQuantity, BigDecimal(1))
        val value = BigDecimal(if side == Side.Buy then "100.02" else "99.97")
        assertEquals(plan.estimatedValue, value)
        val review = OrderReview(intent, plan, f.terms, at, at + 30000L)
        assert(f.worker.submitReviewed(review))
        Wait.until("simulated order did not settle")(
          !f.worker.view.pending && f.worker.view.outcome.nonEmpty
        )
        assertEquals(f.ledger.account.count, 1L)
        assertEquals(
          f.ledger.account.positions("XMR").quantity,
          BigDecimal(if side == Side.Buy then 1 else 9)
        )
        assertEquals(
          f.ledger.account.positions("XMR").entry,
          if side == Side.Buy then value else BigDecimal(100)
        )
        assert(f.worker.view.outcome.exists(_.contains("partial")))
        val unknown = BigDecimal(if side == Side.Buy then "100.05" else "99.94")
        assert(
          f.worker
            .preview(OrderIntent(side, OrderKind.Target, target = Some(unknown)), f.terms)
            .isLeft
        )
        assertEquals(f.ledger.account.count, 1L)
      finally
        close.get()()
        stop(connection)
        f.close()
      assertEquals(errors.asScala.toVector, Vector.empty)
    }

  test("old duplicate and malformed fast frames cannot renew expired prop depth") {
    val f = new PropFixture
    try
      f.start()
      f.worker.observe(Event.SocketUp)
      f.receive(f.event())
      assert(f.worker.view.ready)
      f.now.set(at + 1000L)
      assert(f.worker.view.ready)
      f.now.incrementAndGet()
      assert(!f.worker.view.ready)
      Vector(
        f.event(),
        f.event(f.book.copy(time = at - 1L)),
        "{bad-json",
        f.event().replace("0.20", "oops")
      ).foreach { frame =>
        f.receive(frame)
        assert(!f.worker.view.ready, frame)
        assert(f.worker.preview(Trading.action(0), f.terms).isLeft)
      }
      assertEquals(f.ledger.account.count, 0L)
      f.receive(f.event(f.book.copy(time = f.now.get())))
      assert(f.worker.view.ready)
      assert(f.worker.preview(Trading.action(0), f.terms).isRight)
    finally f.close()
  }

  for side <- Vector(Side.Buy, Side.Sell) do
    test(s"$side a fast five-level book reduces the actual fill after a deeper REST preview") {
      val f = new PropFixture(if side == Side.Buy then BigDecimal(0) else BigDecimal(10))
      try
        f.start()
        f.worker.observe(Event.SocketUp)
        val bids = (0 until 20)
          .map(i => Level((BigDecimal("99.99") - BigDecimal(i) / 100).toString, "1", 1))
          .toVector
        val asks = (0 until 20)
          .map(i => Level((BigDecimal(100) + BigDecimal(i) / 100).toString, "1", 1))
          .toVector
        f.worker.observe(Event.BookUpdate("XMR", Book(bids, asks, at), fromSocket = false))
        assert(f.worker.view.ready)
        val intent = OrderIntent(side, OrderKind.Hard)
        val plan = f.worker.preview(intent, f.terms).toOption.get
        // The buy of 25 meets eleven levels inside its limit. The sell closes the holding of 10.
        assertEquals(plan.estimatedQuantity, BigDecimal(if side == Side.Buy then 11 else 10))
        val review = OrderReview(intent, plan, f.terms, at, at + 30000L)
        f.now.incrementAndGet()
        f.receive(f.event(f.book.copy(time = f.now.get())))
        assert(f.worker.view.ready)
        assert(f.worker.submitReviewed(review))
        Wait.until("simulated order did not settle after depth shrank")(
          !f.worker.view.pending && f.worker.view.outcome.nonEmpty
        )
        assertEquals(f.ledger.account.count, 1L)
        assertEquals(
          f.ledger.account.positions("XMR").quantity,
          BigDecimal(if side == Side.Buy then 1 else 9)
        )
        assertEquals(
          f.ledger.account.positions("XMR").entry,
          BigDecimal(if side == Side.Buy then "100.02" else "100")
        )
        assert(f.worker.view.outcome.exists(_.contains("partial")))
      finally f.close()
    }

  test(
    "boundary request weights charge every chunk and mixed dispatch stays below its finite burst bound"
  ) {
    assertEquals(PublicRequestBudget.cost("""{"type":"l2Book","coin":"XMR"}"""), 2)
    assertEquals(PublicRequestBudget.cost("""{"type":"meta"}"""), 20)
    for interval <- State.intervals do
      for (count, expected) <- Vector((1L, 21), (60L, 21), (61L, 22), (5000L, 104), (10000L, 104))
      do
        val body = Feed.historyBody(
          "XMR",
          interval,
          at,
          Some(at - (count - 1L) * State.intervalMs(interval))
        )
        assertEquals(PublicRequestBudget.cost(body), expected)
    val root = tempDirectory("review-dispatch-")
    val now = new AtomicLong(at)
    val budget = new PublicRequestBudget(root, () => now.get())
    for weight <- Vector(1, 2, 3, 20, 21, 103, 104) do
      val before = now.get()
      budget.execute(weight, delay => { val _ = now.addAndGet(delay) }) { () }
      val expected = (weight / 2L) * 172L + (weight % 2L) * 86L
      assertEquals(now.get() - before, expected, s"weight=$weight")
    val sent = scala.collection.mutable.ArrayBuffer.empty[(Long, Int)]
    val prices = new PublicRequestBudget(root, () => now.get())
    def sleep(delay: Long): Unit =
      val due = now.get() + delay
      val asked = now.get()
      prices.execute(2, ms => { val _ = now.addAndGet(ms) }) { sent += ((now.get(), 2)) }
      assert(now.get() - asked <= 344L)
      now.set(now.get().max(due))
    for _ <- 1 to 5 do budget.execute(104, sleep) { sent += ((now.get(), 104)) }
    assertEquals(sent.count(_._2 == 104), 5)
    assertEquals(sent.count(_._2 == 2), 5 * 52)
    val windows = sent.map { (end, _) =>
      sent.filter { (time, _) => end - 60000L < time && time <= end }.map(_._2).sum
    }
    assert(windows.max <= 804, s"largest rolling dispatch weight was ${windows.max}")
  }

  test(
    "interrupting an expensive gate waiter cannot reserve credit or release another owner's gate"
  ) {
    val root = tempDirectory("review-gate-interrupt-")
    val now = new AtomicLong(at)
    val budget = new PublicRequestBudget(root, () => now.get())
    val errors = new ConcurrentLinkedQueue[Throwable]()
    val held = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val waiting = new CountDownLatch(1)
    val interrupted = new CountDownLatch(1)
    val sent = new AtomicBoolean(false)
    def sleep(ms: Long): Unit = { val _ = now.addAndGet(ms) }
    val first = launch(errors) {
      budget.execute(104, sleep) { held.countDown(); require(release.await(5, TimeUnit.SECONDS)) }
    }
    var waiter = Option.empty[Thread]
    try
      assert(held.await(3, TimeUnit.SECONDS))
      val saved = Files.readString(root.resolve("schedule.txt"))
      waiter = Some(launch(errors) {
        waiting.countDown()
        try budget.execute(20, sleep) { sent.set(true) }
        catch case _: InterruptedException => interrupted.countDown()
      })
      assert(waiting.await(3, TimeUnit.SECONDS))
      Wait.until("waiter did not park at the expensive gate")(
        waiter.get.getState == Thread.State.WAITING
      )
      waiter.get.interrupt()
      assert(
        interrupted.await(500, TimeUnit.MILLISECONDS),
        "expensive gate wait ignored interruption"
      )
      assert(!sent.get())
      assertEquals(Files.readString(root.resolve("schedule.txt")), saved)
      budget.execute(2, sleep) { () }
    finally
      release.countDown()
      waiter.foreach(stop)
      stop(first)
    budget.execute(20, sleep) { sent.set(true) }
    assert(sent.get())
    assertEquals(errors.asScala.toVector, Vector.empty)
  }

  test("separate processes hold expensive transport exclusively while selected depth passes") {
    val root = tempDirectory("review-expensive-processes-", keepOnFailure = true)
    val children = scala.collection.mutable.ArrayBuffer.empty[(String, Process)]
    def child(role: String): Process =
      val process = new ProcessBuilder(
        Path.of(System.getProperty("java.home"), "bin/java").toString,
        "-cp",
        System.getProperty("java.class.path"),
        "desk.FreshnessProcessProbe",
        root.toString,
        role
      ).redirectErrorStream(true).redirectOutput(root.resolve(role + ".log").toFile).start()
      children += ((role, process))
      process
    try
      child("first")
      Wait.until("first process did not enter transport")(
        Files.exists(root.resolve("first.entered"))
      )
      val heldDeadline = Files.readString(root.resolve("budget/schedule.txt")).toLong
      child("second")
      Wait.until("second process did not start")(Files.exists(root.resolve("second.started")))
      child("price")
      Wait.until("selected depth was blocked behind expensive transport")(
        Files.exists(root.resolve("price.entered"))
      )
      Thread.sleep(100L)
      assertEquals(
        Files.readString(root.resolve("budget/schedule.txt")).toLong,
        heldDeadline + 172L
      )
      assert(!Files.exists(root.resolve("second.entered")), "two expensive transports overlapped")
      Files.writeString(root.resolve("release"), "go")
      children.foreach { (role, process) =>
        assert(process.waitFor(5, TimeUnit.SECONDS), role)
        assertEquals(process.exitValue(), 0, Files.readString(root.resolve(role + ".log")))
      }
      assert(Files.exists(root.resolve("second.entered")))
      assertEquals(Files.readString(root.resolve("budget/schedule.txt")).toLong, at + 18060L)
    finally
      Files.writeString(root.resolve("release"), "go")
      children.foreach { (_, process) =>
        if process.isAlive then
          process.destroyForcibly()
          process.waitFor(5, TimeUnit.SECONDS)
      }
  }
