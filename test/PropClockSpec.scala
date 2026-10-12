package desk

import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong

// The prop clock: hourly funding, liquidation at maintenance, and reset.
class PropClockSpec extends TempDirectorySuite:
  private val start = 1800000000000L
  private val hour = 3600000L
  // The first wall-clock hour boundary after a fixture starts at start + 500.
  private val boundary = 1800003600000L
  private def d(text: String): BigDecimal = BigDecimal(text)
  private def held(cash: String, quantity: String, entry: String, coin: String = "XMR") =
    PropAccount(
      d(cash),
      positions = Map(coin -> PropPosition(d(quantity), d(entry))),
      deposit = d(cash)
    )
  private def terms(dollars: String): TradeTerms =
    TradeTerms(TradeSizes(d(dollars), d(dollars), d(dollars)), Trading.defaultSlippage)
  private val market = ujson.read(
    """{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5},{"name":"BTC","szDecimals":5,"maxLeverage":40}]}"""
  )
  private def context(oracle: String, rate: String, mark: String = "500"): Ctx =
    Ctx(mark, oracle, rate, "0", "0", "500", None)
  private val fresh = Ctx("500", "500", "0.0000125", "0", "0", "500", None)
  private def liquidated(money: String, mark: String): String =
    s"XMR liquidated $$$money at $mark"
  private def resting(digit: Char, side: Side, price: String, quantity: String, reduce: Boolean) =
    PropOrder("0x" + digit.toString * 32, "XMR", side, d(price), d(quantity), reduce, start)
  private def background(name: String)(body: => Unit): Thread =
    val thread = new Thread(() => body, name)
    thread.setDaemon(true)
    thread.start()
    thread

  // A memory ledger whose saves the test can hold or fail. Every save runs under the ledger's lock.
  private final class Store(initial: PropAccount):
    @volatile var saves = Vector.empty[PropAccount]
    @volatile var holding = false
    @volatile var failing = false
    // Errors the next saves throw, one each, before any hold. The ledger catches only exceptions.
    @volatile var thrown = List.empty[Throwable]
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val ledger = new PropLedger(
      initial,
      d("0.00045"),
      next => {
        if failing then throw new java.io.IOException("simulated storage failure")
        thrown match
          case error :: rest =>
            thrown = rest
            throw error
          case Nil => ()
        if holding then
          entered.countDown()
          if !release.await(5, TimeUnit.SECONDS) then
            throw new IllegalStateException("test did not release simulated storage")
        saves :+= next
      },
      () => ()
    )

  // A wallet file as the ledger writes it, for a ledger opened from a folder.
  private def write(directory: Path, account: PropAccount): Unit =
    def plain(value: BigDecimal): ujson.Value = ujson.Str(value.bigDecimal.toPlainString)
    val body = ujson.Obj(
      "cash" -> plain(account.cash),
      "realized" -> plain(account.realized),
      "fees" -> plain(account.fees),
      "positions" -> ujson.Obj.from(
        account.positions.map((coin, p) =>
          coin -> ujson.Obj("quantity" -> plain(p.quantity), "entry" -> plain(p.entry))
        )
      ),
      "orders" -> ujson.Arr.from(account.orders.map(ujson.Str(_))),
      "count" -> ujson.Str(account.count.toString),
      "deposit" -> plain(account.deposit),
      "resting" -> ujson.Arr.from(
        account.resting.map(o =>
          ujson.Obj(
            "id" -> o.id,
            "coin" -> o.coin,
            "side" -> o.side.toString,
            "price" -> plain(o.price),
            "quantity" -> plain(o.quantity),
            "reduceOnly" -> o.reduceOnly,
            "createdAt" -> ujson.Str(o.createdAt.toString)
          )
        )
      ),
      "funding" -> plain(account.funding)
    )
    LocalFiles.write(directory.resolve(PropLedger.file("mainnet")), ujson.write(body))
  private def open(directory: Path): PropLedger =
    PropLedger.open(directory, "mainnet", d("10000"), d("0.00045"))

  private final class Fixture(
      val ledger: PropLedger,
      bid: String,
      ask: String,
      run: Boolean,
      coin: String
  ):
    val wall = new AtomicLong(start + 500)
    val mono = new AtomicLong(start)
    private val bookTime = new AtomicLong(start)
    private val rest = new Rest:
      def post(body: String): Option[String] = Some(ujson.write(market))
    val worker = new PropWorker(
      TradingConfig(),
      coin,
      ledger,
      rest,
      () => wall.get(),
      () => mono.get(),
      Some((market, start))
    )
    if run then worker.start()
    worker.observe(Event.SocketUp)
    book(bid, ask)
    assert(worker.view.ready, worker.view.message)

    def book(bid: String, ask: String): Unit =
      worker.observe(
        Event.BookUpdate(
          coin,
          Book(
            Vector(Level(bid, "50", 1)),
            Vector(Level(ask, "50", 1)),
            bookTime.incrementAndGet()
          ),
          true
        )
      )
    def context(ctx: Ctx, market: String = coin): Unit =
      worker.observe(Event.CtxUpdate(market, ctx))
    def account: PropAccount = ledger.account
    def preview(
        side: Side,
        dollars: String,
        kind: OrderKind = OrderKind.Hard
    ): Either[String, OrderPlan] =
      worker.preview(OrderIntent(side, kind), terms(dollars))
    def review(side: Side, dollars: String): OrderReview =
      val plan = preview(side, dollars).fold(reason => fail(reason), identity)
      OrderReview(
        OrderIntent(side, OrderKind.Hard),
        plan,
        terms(dollars),
        wall.get(),
        wall.get() + 30000,
        None,
        None
      )
    def submit(review: OrderReview): Unit =
      assert(worker.submitReviewed(review))
      Wait.until("the prop fill did not finish")(!worker.view.pending)
    // The running clock may have liquidated first, and announces just after its save.
    def announced(text: String): Unit =
      Wait.until(s"no outcome $text, saw ${worker.view.outcome}")(
        worker.view.outcome.contains(text)
      )

  private def withFixture[A](
      ledger: PropLedger,
      bid: String,
      ask: String,
      run: Boolean = true,
      coin: String = "XMR"
  )(body: Fixture => A): A =
    val f = new Fixture(ledger, bid, ask, run, coin)
    try body(f)
    finally f.worker.close()

  // Desk's own failure handler and restoration, wired as the workspace wires them, with the
  // exit, the report, the terminal and the other closes recorded. Each thread started in the
  // group reports to the handler, as each thread in Desk reports to the default handler.
  private final class Shutdown(worker: PropWorker):
    @volatile private var seen = Vector.empty[String]
    private val done = new CountDownLatch(1)
    private def add(event: String): Unit = synchronized { seen :+= event }
    private def closing(name: String): AutoCloseable = () => add(name)
    private val restoration = new WorkspaceApp.Restoration(
      () =>
        WorkspaceApp.closeAll(
          closing("outlook"),
          closing("announcements"),
          Some(closing("recorder")),
          () => { worker.close(); add("prop closed") },
          closing("diagnostics"),
          Some(closing("key wiped"))
        ),
      () => add("terminal")
    )
    private val handler = WorkspaceApp.failureHandler(
      restoration,
      Thread.currentThread(),
      (thread, _) => add("record " + thread.getName),
      add,
      status => add("exit " + status)
    )
    private val group = new ThreadGroup("prop-clock-spec"):
      override def uncaughtException(thread: Thread, error: Throwable): Unit =
        try handler.uncaughtException(thread, error)
        catch case thrown: Throwable => add("handler threw " + thrown.getClass.getSimpleName)
        finally done.countDown()
    def run(name: String)(body: => Unit): Thread =
      val thread = new Thread(group, () => body, name)
      thread.setDaemon(true)
      thread.start()
      thread
    def events: Vector[String] =
      assert(done.await(5, TimeUnit.SECONDS), s"the failure handler never finished, saw $seen")
      seen
  private def stopped(thread: String, error: String): Vector[String] = Vector(
    "record " + thread,
    "outlook",
    "announcements",
    "recorder",
    "prop closed",
    "diagnostics",
    "key wiped",
    "terminal",
    "Desk stopped: " + error,
    "exit 1"
  )
  private val clockFailed = "Prop clock failed. Restart to recover"

  test("funding charges the position once per hour from a fresh context") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01") { f =>
      f.context(fresh)
      val id = f.worker.view.outcomeId
      f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      f.wall.set(boundary)
      // The running clock may take the hour first, so the read waits for its charge.
      f.worker.tick()
      Wait.until(s"no funding, saw ${f.account.funding}")(f.account.funding != 0)
      val after = f.account
      assertEquals(after.cash, d("999.9875"))
      assertEquals(after.funding, d("0.0125"))
      assertEquals(after.count, 0L)
      assertEquals(after.orders, Vector.empty)
      assertEquals(store.saves, Vector(after))
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.worker.view.outcomeId, id)
      f.worker.tick()
      assertEquals(f.account, after)
      assertEquals(store.saves.size, 1)
    }
  }

  test("a short receives at a positive rate, a long receives and a short pays at a negative rate") {
    for (quantity, rate, cash, funding) <- Vector(
        ("-2", "0.0000125", "1000.0125", "-0.0125"),
        ("2", "-0.0000125", "1000.0125", "-0.0125"),
        ("-2", "-0.0000125", "999.9875", "0.0125")
      )
    do
      val store = new Store(held("1000", quantity, "500"))
      // No clock thread runs here, so the read after the test's own tick sees its charge.
      val after = withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
        f.context(context("500", rate))
        f.wall.set(boundary)
        f.worker.tick()
        f.account
      }
      assertEquals((after.cash, after.funding), (d(cash), d(funding)), s"$quantity at $rate")
  }

  test("the hour the worker starts in is settled, so hours before it are never charged") {
    val store = new Store(held("1000", "2", "500"))
    // No clock thread runs here, so no tick settles the first hour before the context arrives.
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      f.wall.set(boundary)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
    }
  }

  test("an hour without a fresh context is skipped and never charged later") {
    val store = new Store(held("1000", "2", "500"))
    // No clock thread runs here, so each read after the test's own ticks sees their charge.
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.mono.addAndGet(hour + 1)
      f.wall.set(boundary)
      f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      // A context that arrives later in the skipped hour does not charge it.
      f.context(fresh)
      f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      f.wall.set(boundary + hour)
      f.worker.tick()
      f.worker.tick()
      assertEquals(f.account.cash, d("999.9875"))
      assertEquals(f.account.funding, d("0.0125"))
      // A context exactly an hour old still counts.
      f.context(fresh)
      f.mono.addAndGet(hour)
      f.wall.set(boundary + 2 * hour)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.025"))
    }
  }

  // A sleep can pause the monotonic clock while the wall clock moves on, so a context must also
  // be an hour old at most on the wall clock.
  test(
    "a context older than an hour on the wall clock charges nothing, though the monotonic clock stood still"
  ) {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.wall.set(boundary + 4 * hour)
      for _ <- 1 to 5 do f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      assertEquals(store.saves, Vector.empty)
      // A context exactly an hour old on the wall clock still counts.
      f.context(fresh)
      f.wall.set(boundary + 5 * hour)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
      assertEquals(f.account.cash, d("999.9875"))
    }
  }

  // Nothing but a context for the selected market renews its ages.
  test("only a context for the selected market renews the context's receipt times") {
    // The wall clock passes the hour while the monotonic clock stands still.
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.wall.set(boundary + 4 * hour)
      f.book("499.99", "500.01")
      f.context(Ctx("60000", "60000", "0.0001", "0", "0", "60000", None), "BTC")
      // A book for another market renews nothing either.
      f.worker.observe(
        Event.BookUpdate(
          "BTC",
          Book(Vector(Level("59999", "1", 1)), Vector(Level("60001", "1", 1)), start + 10),
          true
        )
      )
      f.worker.tick()
      assertEquals(f.ledger.account, held("1000", "2", "500"))
      assertEquals(store.saves, Vector.empty)
    }
    // The monotonic clock passes the hour while the wall clock stays inside it.
    val second = new Store(held("1000", "2", "500"))
    withFixture(second.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.mono.addAndGet(hour + 1)
      f.wall.set(boundary)
      f.book("499.99", "500.01")
      f.context(Ctx("60000", "60000", "0.0001", "0", "0", "60000", None), "BTC")
      // A book for another market renews nothing either.
      f.worker.observe(
        Event.BookUpdate(
          "BTC",
          Book(Vector(Level("59999", "1", 1)), Vector(Level("60001", "1", 1)), start + 10),
          true
        )
      )
      f.worker.tick()
      assertEquals(f.ledger.account, held("1000", "2", "500"))
      assertEquals(second.saves, Vector.empty)
    }
  }

  // The wall age is measured when the tick runs, not at the boundary or by the clock hour the
  // context arrived in.
  test("a context over an hour old on the wall clock when the tick runs charges nothing") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      // Under an hour old at the boundary, an hour and 100 ms old at the tick.
      f.wall.set(boundary + 600)
      f.worker.tick()
      assertEquals(f.ledger.account, held("1000", "2", "500"))
      f.context(fresh)
      f.wall.set(boundary + hour)
      f.worker.tick()
      assertEquals(f.ledger.account.funding, d("0.0125"))
    }
  }

  // After the wall clock steps back, the monotonic age alone decides.
  test("a wall clock step back after a context still charges from it while it is fresh") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.wall.set(boundary + 2 * hour)
      f.context(fresh)
      f.wall.set(boundary + 1)
      f.worker.tick()
      assertEquals(f.ledger.account.funding, d("0.0125"))
      assertEquals(f.ledger.account.cash, d("999.9875"))
    }
  }

  test("funding never takes cash below zero") {
    val store = new Store(held("0.01", "2", "500"))
    assertEquals(store.ledger.fund("XMR", d("500"), d("0.0000125")), Right(d("0.01")))
    assertEquals(store.ledger.account.cash, d("0"))
    assertEquals(store.ledger.account.funding, d("0.01"))
    assertEquals(store.ledger.account.count, 0L)
    assertEquals(store.ledger.fund("XMR", d("500"), d("0.0000125")), Right(d("0")))
    assertEquals(store.ledger.account.funding, d("0.01"))
    // Nothing held on the market asked for charges nothing and saves nothing.
    val saves = store.saves.size
    assertEquals(store.ledger.fund("BTC", d("500"), d("0.0000125")), Right(d("0")))
    assertEquals(store.saves.size, saves)
  }

  test("funding and liquidation leave another market alone") {
    val store = new Store(held("140", "1", "500", "BTC"))
    withFixture(store.ledger, "399.99", "400.01") { f =>
      f.context(fresh)
      f.context(context("500", "0.0000125"), "BTC")
      f.wall.set(boundary)
      f.worker.tick()
      assertEquals(f.account, held("140", "1", "500", "BTC"))
      assertEquals(store.saves, Vector.empty)
      assertEquals(f.worker.view.outcome, None)
    }
  }

  test("funding takes the oracle and rate of the latest context for the selected market") {
    val store = new Store(held("1000", "2", "500"))
    // No clock thread runs here, so each read after the test's own tick sees its charge.
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(context("400", "0.0000125", mark = "520"))
      f.context(context("500", "0.0000125", mark = "520"))
      f.context(context("60000", "0.001"), "BTC")
      f.wall.set(boundary)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
      assertEquals(f.account.cash, d("999.9875"))
      // A market change drops the context, so the next hour waits for a new one.
      f.context(fresh)
      f.worker.select("BTC")
      f.worker.select("XMR")
      f.wall.set(boundary + hour)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
    }
  }

  test("a context without a valid oracle and rate charges nothing") {
    val store = new Store(held("1000", "2", "500"))
    // No clock thread runs here, so the read after the test's own tick sees its charge.
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      for (ctx, n) <- Vector(
          context("0", "0.0000125"),
          context("-500", "0.0000125"),
          context("bad", "0.0000125"),
          context("500", "bad")
        ).zipWithIndex
      do
        f.context(ctx)
        f.wall.set(boundary + n * hour)
        f.worker.tick()
        assertEquals(store.saves, Vector.empty, ctx.toString)
      f.context(fresh)
      f.wall.set(boundary + 4 * hour)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
    }
  }

  test("liquidation closes at the mark when equity meets maintenance") {
    val store = new Store(held("140", "1", "500"))
    withFixture(store.ledger, "399.99", "400.01") { f =>
      f.worker.tick()
      val after = f.account
      assertEquals(after.cash, d("40"))
      assertEquals(after.realized, d("-100"))
      assertEquals(after.positions, Map.empty)
      assertEquals(after.count, 1L)
      assertEquals(
        after.orders,
        Vector(liquidated("80.00", "400") + " | " + Instant.ofEpochMilli(start + 500))
      )
      f.announced(liquidated("80.00", "400"))
      f.worker.tick()
      assertEquals(f.account, after)
    }
    // Mark 400.01: equity 40.01 against maintenance 40.001.
    val safe = new Store(held("140", "1", "500"))
    withFixture(safe.ledger, "400.00", "400.02") { f =>
      f.worker.tick()
      assertEquals(f.account, held("140", "1", "500"))
      assertEquals(safe.saves, Vector.empty)
      assertEquals(f.worker.view.outcome, None)
    }
  }

  test("a gap past the liquidation price leaves cash at zero and the file reopens") {
    val directory = tempDirectory("prop-clock-gap-")
    write(directory, held("140", "1", "500"))
    val after = withFixture(open(directory), "299.99", "300.01") { f =>
      f.worker.tick()
      f.announced(liquidated("60.00", "300"))
      f.account
    }
    assertEquals(after.cash, d("0"))
    assertEquals(after.realized, d("-140"))
    assertEquals(after.positions, Map.empty)
    assertEquals(after.cash, after.deposit + after.realized - after.fees - after.funding)
    val reopened = open(directory)
    try assertEquals(reopened.account, after)
    finally reopened.close()
  }

  test("liquidation removes resting orders and says so") {
    val sell = resting('1', Side.Sell, "450.00", "1", reduce = true)
    val store = new Store(held("140", "1", "500").copy(resting = Vector(sell)))
    withFixture(store.ledger, "399.99", "400.01") { f =>
      f.worker.tick()
      val text = liquidated("80.00", "400") + "\nResting orders removed: 1"
      assertEquals(f.account.resting, Vector.empty)
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.orders.last, text + " | " + Instant.ofEpochMilli(start + 500))
      f.announced(text)
    }
    // An opening order goes as well.
    val buy = resting('2', Side.Buy, "350.00", "0.1", reduce = false)
    val both = new Store(held("140", "1", "500").copy(resting = Vector(sell, buy)))
    withFixture(both.ledger, "399.99", "400.01") { f =>
      f.worker.tick()
      assertEquals(f.account.resting, Vector.empty)
      f.announced(liquidated("80.00", "400") + "\nResting orders removed: 2")
    }
  }

  test("a review made before a liquidation is refused") {
    val store = new Store(held("140", "1", "500"))
    withFixture(store.ledger, "400.00", "400.02") { f =>
      val review = f.review(Side.Sell, "20")
      assertEquals(review.plan.quantity, d("0.25"))
      assert(review.plan.reduceOnly)
      f.book("399.99", "400.01")
      f.worker.tick()
      assertEquals(f.account.count, 1L)
      assert(!f.worker.submitReviewed(review))
      assert(!f.worker.view.pending)
      assertEquals(f.account.count, 1L)
      assertEquals(f.account.positions, Map.empty)
      assertEquals(store.saves.size, 1)
    }
  }

  test("the clock waits while an order is pending") {
    val store = new Store(held("140", "1", "500"))
    withFixture(store.ledger, "400.00", "400.02") { f =>
      val review = f.review(Side.Sell, "20")
      try
        store.holding = true
        assert(f.worker.submitReviewed(review))
        assert(store.entered.await(3, TimeUnit.SECONDS), "the fill never reached its save")
        f.book("299.99", "300.01")
        val ticking = background("prop-clock-spec-tick")(f.worker.tick())
        ticking.join(1000L)
        assert(!ticking.isAlive, "the tick waited on the pending fill")
        assertEquals(f.account, held("140", "1", "500"))
      finally store.release.countDown()
      Wait.until("the prop fill did not finish")(!f.worker.view.pending)
      // The fill was planned at 400.00, before the book moved.
      assertEquals(
        store.saves.headOption.map(a => (a.cash, a.positions)),
        Some((d("114.955"), Map("XMR" -> PropPosition(d("0.75"), d("500")))))
      )
      f.worker.tick()
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.cash, d("0"))
      assertEquals(f.account.realized, d("-139.955"))
      assertEquals(f.account.count, 2L)
      f.announced(liquidated("45.00", "300"))
    }
  }

  test("an underwater CLOSE is refused until the clock liquidates") {
    val store = new Store(held("140", "1", "500"))
    // No clock thread runs here, so the CLOSE meets the account before any tick.
    withFixture(store.ledger, "299.99", "300.01", run = false) { f =>
      val close = f.preview(Side.Sell, "20", OrderKind.Close)
      assertEquals(close, Left(PropLedger.noStables))
      f.worker.tick()
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.cash, d("0"))
      assertEquals(f.worker.view.outcome, Some(liquidated("60.00", "300")))
      assertEquals(f.preview(Side.Sell, "20", OrderKind.Close), Left("No position to close"))
    }
  }

  test("the view answers while the clock saves funding or a liquidation") {
    val funding = new Store(held("1000", "2", "500"))
    withFixture(funding.ledger, "499.99", "500.01") { f =>
      f.context(fresh)
      try
        funding.holding = true
        f.wall.set(boundary)
        val _ = background("prop-clock-spec-tick")(f.worker.tick())
        assert(funding.entered.await(3, TimeUnit.SECONDS), "funding never reached its save")
        val viewing = background("prop-clock-spec-view")(f.worker.view)
        viewing.join(1000L)
        assert(!viewing.isAlive, "the view waited behind the funding save")
      finally funding.release.countDown()
      Wait.until("funding did not save")(f.account.funding == d("0.0125"))
    }
    val liquidation = new Store(held("140", "1", "500"))
    withFixture(liquidation.ledger, "400.00", "400.02") { f =>
      try
        liquidation.holding = true
        f.book("399.99", "400.01")
        val _ = background("prop-clock-spec-tick")(f.worker.tick())
        assert(liquidation.entered.await(3, TimeUnit.SECONDS), "liquidation never reached its save")
        val viewing = background("prop-clock-spec-view")(f.worker.view)
        viewing.join(1000L)
        assert(!viewing.isAlive, "the view waited behind the liquidation save")
      finally liquidation.release.countDown()
      f.announced(liquidated("80.00", "400"))
    }
  }

  test("the running clock funds and liquidates without a call") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01") { f =>
      f.context(fresh)
      f.wall.set(boundary)
      Wait.until("the clock did not fund")(f.account.funding == d("0.0125"))
    }
    val due = new Store(held("140", "1", "500"))
    withFixture(due.ledger, "400.00", "400.02") { f =>
      f.book("399.99", "400.01")
      f.announced(liquidated("80.00", "400"))
    }
  }

  test("a failed clock save leaves the account and marks the ledger unavailable") {
    val store = new Store(held("1000", "2", "500"))
    store.failing = true
    assertEquals(
      store.ledger.fund("XMR", d("500"), d("0.0000125")),
      Left("Prop storage failed. Restart to recover")
    )
    assertEquals(store.ledger.account, held("1000", "2", "500"))
    assert(!store.ledger.available)
    store.failing = false
    assertEquals(store.ledger.fund("XMR", d("500"), d("0.0000125")), Left(PropLedger.unavailable))
    assertEquals(
      store.ledger.liquidate("XMR", d("100"), d("0.1"), start),
      Left(PropLedger.unavailable)
    )
    assertEquals(store.ledger.reset(d("1000"), start), Left(PropLedger.unavailable))
    assertEquals(store.saves, Vector.empty)
  }

  test("reset starts over at the balance and keeps the history") {
    val directory = tempDirectory("prop-clock-reset-")
    val before = PropAccount(
      d("40"),
      d("-100"),
      d("0.25"),
      Map("XMR" -> PropPosition(d("0.1"), d("500"))),
      Vector("one", "two", "three"),
      3L,
      d("140"),
      Vector(resting('1', Side.Sell, "450.00", "0.1", reduce = true)),
      d("-0.25")
    )
    write(directory, before)
    val ledger = open(directory)
    val after =
      try
        assertEquals(ledger.account, before)
        for balance <- Vector("0", "-1", "1000000000.01") do
          assertEquals(
            ledger.reset(d(balance), start),
            Left("Prop rejected: invalid balance"),
            balance
          )
        assertEquals(ledger.account, before)
        assertEquals(ledger.reset(d("1000"), start + 1), Right("PROP reset to $1000"))
        ledger.account
      finally ledger.close()
    assertEquals(
      after,
      PropAccount(
        d("1000"),
        orders =
          Vector("one", "two", "three", "PROP reset to $1000 | " + Instant.ofEpochMilli(start + 1)),
        count = 4L,
        deposit = d("1000")
      )
    )
    assertEquals((after.realized, after.fees, after.funding), (d("0"), d("0"), d("0")))
    val reopened = open(directory)
    try assertEquals(reopened.account, after)
    finally reopened.close()
  }

  test("the worker resets once nothing is pending, and its outcome reaches the result log") {
    val store = new Store(PropAccount(d("10000"), deposit = d("10000")))
    withFixture(store.ledger, "546.09", "546.10") { f =>
      val first = f.review(Side.Sell, "20")
      try
        store.holding = true
        assert(f.worker.submitReviewed(first))
        assert(store.entered.await(3, TimeUnit.SECONDS), "the fill never reached its save")
        assertEquals(f.worker.reset(d("10000")), Left("Wait for the pending prop order"))
        assertEquals(f.account.count, 0L)
      finally store.release.countDown()
      Wait.until("the prop fill did not finish")(!f.worker.view.pending)
      assertEquals(f.account.positions.get("XMR").map(_.quantity), Some(d("-0.183")))
      val prepared = f.review(Side.Sell, "20")
      val log = new OrderUpdates
      var received = Vector.empty[String]
      log.collect(f.worker.view)(_ => ())
      val id = f.worker.view.outcomeId
      assertEquals(f.worker.reset(d("10000")), Right("PROP reset to $10000"))
      val view = f.worker.view
      assertEquals(view.outcome, Some("PROP reset to $10000"))
      assert(view.outcomeId > id)
      log.collect(view)(text => received :+= text)
      assertEquals(received, Vector("PROP reset to $10000"))
      assertEquals(view.lastPlan, None)
      assertEquals(f.account.cash, d("10000"))
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.count, 2L)
      assert(!f.worker.submitReviewed(prepared))
      assert(!f.worker.view.pending)
      assertEquals(f.account.count, 2L)
      assertEquals(f.account.positions, Map.empty)
      f.worker.close()
      assertEquals(f.worker.reset(d("10000")), Left("Prop worker is closed"))
    }
  }

  test("a reset lifts the opening pause of the old account") {
    val store = new Store(PropAccount(d("10000"), deposit = d("10000")))
    withFixture(store.ledger, "546.09", "546.10") { f =>
      f.submit(f.review(Side.Sell, "20"))
      f.submit(f.review(Side.Buy, "20"))
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.preview(Side.Sell, "20"), Left(TradingWorker.openingPausedMessage))
      assertEquals(f.worker.reset(d("10000")), Right("PROP reset to $10000"))
      assert(f.preview(Side.Sell, "20").isRight)
    }
  }

  // The settled hour never follows the wall clock backwards.
  test("a clock step back and forward again charges the hour once") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.wall.set(boundary)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
      f.wall.set(boundary - 1)
      f.worker.tick()
      f.wall.set(boundary + 1)
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
      assertEquals(f.account.cash, d("999.9875"))
      assertEquals(store.saves.size, 1)
    }
  }

  // A jump of several hours settles to the current hour, so the skipped hours are never charged
  // once a fresh context arrives.
  test("a clock jump of several hours never charges the skipped hours later") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(fresh)
      f.mono.addAndGet(5 * hour)
      f.wall.set(boundary + 4 * hour)
      f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      f.context(fresh)
      for _ <- 1 to 5 do f.worker.tick()
      assertEquals(f.account, held("1000", "2", "500"))
      f.wall.set(boundary + 5 * hour)
      f.worker.tick()
      f.worker.tick()
      assertEquals(f.account.funding, d("0.0125"))
    }
  }

  // The other fixtures have entry, midpoint and oracle at 500.
  test("funding values the position at the context's oracle, not the entry, mark or midpoint") {
    val store = new Store(held("1000", "2", "500"))
    withFixture(store.ledger, "499.99", "500.01", run = false) { f =>
      f.context(context("520", "0.0000125", mark = "530"))
      f.wall.set(boundary)
      f.worker.tick()
      // 2 * 520 * 0.0000125
      assertEquals(f.account.funding, d("0.013"))
      assertEquals(f.account.cash, d("999.987"))
    }
  }

  // The other liquidation fixtures hold only longs.
  test("a short liquidates at the mark with its own sign, at and past maintenance") {
    // Liquidation price (500 + 140) / 1.1 = 581.818...
    val safe = new Store(held("140", "-1", "500"))
    withFixture(safe.ledger, "581.80", "581.82", run = false) { f =>
      f.worker.tick()
      assertEquals(f.account, held("140", "-1", "500"))
    }
    val due = new Store(held("140", "-1", "500"))
    withFixture(due.ledger, "599.99", "600.01", run = false) { f =>
      f.worker.tick()
      assertEquals(f.account.cash, d("40"))
      assertEquals(f.account.realized, d("-100"))
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.count, 1L)
      assertEquals(f.worker.view.outcome, Some("XMR liquidated $120.00 at 600"))
    }
    val gap = new Store(held("140", "-1", "500"))
    withFixture(gap.ledger, "699.99", "700.01", run = false) { f =>
      f.worker.tick()
      val after = f.account
      assertEquals(after.cash, d("0"))
      assertEquals(after.realized, d("-140"))
      assertEquals(after.cash, after.deposit + after.realized - after.fees - after.funding)
    }
  }

  // The other liquidation fixtures are XMR, whose maintenance 0.1 a constant would match.
  test("liquidation takes the selected market's maintenance") {
    // BTC at 40x: maintenance 1/80. Liquidation price (100000 - 2000) / 0.9875 = 99240.506...
    val safe = new Store(held("2000", "1", "100000", "BTC"))
    withFixture(safe.ledger, "99499", "99501", run = false, coin = "BTC") { f =>
      f.worker.tick()
      assertEquals(f.account, held("2000", "1", "100000", "BTC"))
    }
    val due = new Store(held("2000", "1", "100000", "BTC"))
    withFixture(due.ledger, "99239", "99241", run = false, coin = "BTC") { f =>
      f.worker.tick()
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.cash, d("1240"))
      assertEquals(f.worker.view.outcome, Some("BTC liquidated $19848.00 at 99240"))
    }
  }

  // The clock liquidates only while the worker is ready.
  test("a stale book does not liquidate, and a fresh one does") {
    val store = new Store(held("140", "1", "500"))
    withFixture(store.ledger, "400.00", "400.02", run = false) { f =>
      f.book("399.99", "400.01")
      f.mono.addAndGet(TradingConfig().quoteAgeMs + 1)
      assert(!f.worker.view.ready)
      f.worker.tick()
      assertEquals(f.account, held("140", "1", "500"))
      assertEquals(store.saves, Vector.empty)
      f.book("399.99", "400.01")
      f.worker.tick()
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.cash, d("40"))
    }
  }

  // A clock change that meets a save in progress waits for it and then decides on the account it
  // left, so neither change is lost.
  test("fund, liquidate and reset wait for a save in progress and lose neither change") {
    val order = PropOrder("0x" + "a" * 32, "XMR", Side.Sell, d("450"), d("1"), true, start)
    def race(account: PropAccount)(change: PropLedger => Unit): PropAccount =
      val store = new Store(account)
      store.holding = true
      val resting = background("prop-clock-spec-rest") { val _ = store.ledger.rest(order, 0L) }
      assert(store.entered.await(3, TimeUnit.SECONDS), "the resting save never started")
      val clock = background("prop-clock-spec-change")(change(store.ledger))
      Wait.until("the clock call never reached the lock or the save")(
        clock.getState != Thread.State.RUNNABLE && clock.getState != Thread.State.NEW
      )
      store.release.countDown()
      resting.join(5000L)
      clock.join(5000L)
      store.ledger.account
    val funded = race(held("1000", "2", "500"))(_.fund("XMR", d("500"), d("0.0000125")))
    assertEquals(
      (funded.resting.size, funded.funding, funded.cash),
      (1, d("0.0125"), d("999.9875"))
    )
    val liquidated = race(held("140", "1", "500"))(_.liquidate("XMR", d("400"), d("0.1"), start))
    assertEquals(
      (liquidated.count, liquidated.resting, liquidated.positions),
      (2L, Vector.empty, Map.empty)
    )
    assert(
      liquidated.orders.last.startsWith("XMR liquidated $80.00 at 400\nResting orders removed: 1")
    )
    val reset = race(held("140", "1", "500"))(_.reset(d("1000"), start))
    assertEquals((reset.count, reset.resting, reset.orders.size), (2L, Vector.empty, 2))
  }

  // Funding runs before the liquidation check in a tick.
  test("a funding charge that meets maintenance liquidates in the same tick") {
    val store = new Store(held("140.005", "1", "500"))
    withFixture(store.ledger, "399.99", "400.01", run = false) { f =>
      f.worker.tick()
      assertEquals(f.account.positions.keySet, Set("XMR"))
      f.context(context("400", "0.0000125"))
      f.wall.set(boundary)
      f.worker.tick()
      val after = f.account
      assertEquals(after.funding, d("0.005"))
      assertEquals(after.positions, Map.empty)
      assertEquals(after.cash, d("40"))
      assertEquals(after.cash, after.deposit + after.realized - after.fees - after.funding)
    }
  }

  // The reset's top bound is open's, inclusive.
  test("reset accepts the largest balance open accepts") {
    val store = new Store(held("40", "1", "500"))
    assertEquals(store.ledger.reset(d("1000000000"), start), Right("PROP reset to $1000000000"))
    assertEquals(store.ledger.account.cash, d("1000000000"))
  }

  // A reset the ledger refuses changes nothing in the worker.
  test("a refused reset keeps the review and announces nothing") {
    val store = new Store(PropAccount(d("10000"), deposit = d("10000")))
    withFixture(store.ledger, "546.09", "546.10", run = false) { f =>
      val review = f.review(Side.Sell, "20")
      val id = f.worker.view.outcomeId
      assertEquals(f.worker.reset(d("0")), Left("Prop rejected: invalid balance"))
      assertEquals((f.worker.view.outcome, f.worker.view.outcomeId), (None, id))
      assert(f.worker.submitReviewed(review))
    }
  }

  // The plan's limit (D3.15). The clock tests the midpoint and a CLOSE fills at the bid, so on a
  // book wider than about twice the maintenance gap an underwater CLOSE is refused and never
  // liquidated. Long 1 at 500 with cash 140, maintenance 0.1, bid 360.00, ask 440.02, mid 400.01.
  // CLOSE at 360: 140 - 140 - 0.162 < 0, refused. Mid: equity 40.01 > 40.001, no liquidation.
  test(
    "an underwater CLOSE on a book wider than the maintenance gap stays refused, as the plan's limit states"
  ) {
    val store = new Store(held("140", "1", "500"))
    withFixture(store.ledger, "360.00", "440.02", run = false) { f =>
      assertEquals(f.preview(Side.Sell, "20", OrderKind.Close), Left(PropLedger.noStables))
      f.worker.tick()
      assertEquals(f.account, held("140", "1", "500"))
      assertEquals(f.preview(Side.Sell, "20", OrderKind.Close), Left(PropLedger.noStables))
      // A smaller REDUCE still passes: 0.277 at 360 realizes -38.78.
      val reduce = f.preview(Side.Sell, "20")
      assert(reduce.exists(_.reduceOnly), reduce.toString)
    }
    // Bid 360.20 leaves the CLOSE above zero, so it passes.
    val narrower = new Store(held("140", "1", "500"))
    withFixture(narrower.ledger, "360.20", "440.02", run = false) { f =>
      assert(f.preview(Side.Sell, "20", OrderKind.Close).isRight)
    }
  }

  // Desk's failure handler runs the shutdown on the failing thread, so a fatal throw in a tick
  // closes the prop worker from its own clock thread. The control throws on a plain thread while
  // the clock runs.
  test("a fatal throw in a tick ends in Desk's full shutdown, as a throw on a plain thread does") {
    val safe = new Store(held("1000", "2", "500"))
    withFixture(safe.ledger, "499.99", "500.01", run = false) { f =>
      val shutdown = new Shutdown(f.worker)
      shutdown.run("prop-clock-spec-start")(f.worker.start()).join(1000L)
      shutdown.run("plain")(throw new ArithmeticException("long overflow"))
      assertEquals(shutdown.events, stopped("plain", "ArithmeticException"))
      assert(!safe.ledger.available)
    }
    val store = new Store(held("140", "1", "500"))
    store.thrown = List(new OutOfMemoryError("simulated"))
    withFixture(store.ledger, "399.99", "400.01", run = false) { f =>
      val shutdown = new Shutdown(f.worker)
      shutdown.run("prop-clock-spec-start")(f.worker.start()).join(1000L)
      assertEquals(shutdown.events, stopped("prop-clock", "OutOfMemoryError"))
      assert(!store.ledger.available)
      assertEquals(f.account, held("140", "1", "500"))
    }
  }

  test("a non fatal throw in one tick keeps the money, shows once and the next tick runs") {
    val store = new Store(held("140", "1", "500"))
    store.thrown = List(new Error("simulated clock failure"))
    store.holding = true
    withFixture(store.ledger, "399.99", "400.01") { f =>
      try
        assert(store.entered.await(5, TimeUnit.SECONDS), "the next tick never reached its save")
        assertEquals(f.account, held("140", "1", "500"))
        assertEquals(f.worker.clockFailures, 1L)
        assertEquals(f.worker.view.outcomes.map(_._2), Vector(clockFailed))
      finally store.release.countDown()
      f.announced(liquidated("80.00", "400"))
      assertEquals(f.account.cash, d("40"))
      assertEquals(f.account.count, 1L)
      assertEquals(store.saves.size, 1)
      assertEquals(f.worker.clockFailures, 1L)
      assertEquals(
        f.worker.view.outcomes.map(_._2),
        Vector(clockFailed, liquidated("80.00", "400"))
      )
    }
  }

  // A trade through a resting order while the liquidation saves waits on the ledger and finds
  // the order gone with the position. The control runs the same steps in order.
  test("a trade through a resting order a liquidation removes announces only the liquidation") {
    val buy = resting('3', Side.Buy, "390.00", "0.1", reduce = false)
    val text = liquidated("80.00", "400") + "\nResting orders removed: 1"
    def through(f: Fixture): Unit = f.worker.observe(
      Event.Trades("XMR", Vector(Trade(start + 100, Side.Sell, "389.00", "0.01", Some(1L))))
    )
    def check(f: Fixture, store: Store): Unit =
      assertEquals(f.worker.view.outcomes.map(_._2), Vector(text))
      assertEquals(f.account.positions, Map.empty)
      assertEquals(f.account.resting, Vector.empty)
      assertEquals((f.account.cash, f.account.count), (d("40"), 1L))
      assertEquals(store.saves.size, 1)
    val control = new Store(held("140", "1", "500").copy(resting = Vector(buy)))
    withFixture(control.ledger, "399.99", "400.01", run = false) { f =>
      f.worker.tick()
      through(f)
      check(f, control)
    }
    val store = new Store(held("140", "1", "500").copy(resting = Vector(buy)))
    withFixture(store.ledger, "399.99", "400.01", run = false) { f =>
      store.holding = true
      val clock = background("prop-clock-spec-tick")(f.worker.tick())
      val trading =
        try
          assert(store.entered.await(3, TimeUnit.SECONDS), "the liquidation never reached its save")
          val thread = background("prop-clock-spec-trade")(through(f))
          Wait.until("the trade did not wait on the ledger")(
            thread.getState == Thread.State.BLOCKED
          )
          thread
        finally store.release.countDown()
      clock.join(5000L)
      trading.join(5000L)
      check(f, store)
    }
  }

  // The release review's input: a wallet at the largest count, so each liquidation save
  // overflows the count. The load refuses such a wallet, so this ledger is made directly.
  test("a tick that throws every time is shown once and leaves the money as it was") {
    val account = held("140", "1", "500").copy(count = Long.MaxValue)
    val store = new Store(account)
    withFixture(store.ledger, "399.99", "400.01") { f =>
      Wait.until("the clock did not fail twice")(f.worker.clockFailures >= 2)
      assertEquals(f.worker.view.outcomes.map(_._2), Vector(clockFailed))
      assertEquals(f.account, account)
      assertEquals(store.saves, Vector.empty)
      assert(f.worker.view.ready, f.worker.view.message)
    }
  }
