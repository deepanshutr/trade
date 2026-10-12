package desk

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

// Release review: the money paths of lanes 3A, 3E and 3F as one. Each case drives the real worker
// on a margin wallet in a folder, and after each step checks the cash identity, the view's Usable
// and liquidation price against figures derived by hand, and the file against the ledger.
class ReleaseMoneySpec extends TempDirectorySuite:
  private val start = 1800000000000L
  private val boundary = 1800003600000L
  private def d(text: String): BigDecimal = BigDecimal(text)
  private def terms(dollars: String): TradeTerms =
    TradeTerms(TradeSizes(d(dollars), d(dollars), d(dollars)), Trading.defaultSlippage)
  private val market = ujson.read(
    """{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5},{"name":"BTC","szDecimals":5,"maxLeverage":40}]}"""
  )
  private val context = Ctx("500", "500", "0.0000125", "0", "0", "500", None)

  private final class Fixture(balance: String, val directory: Path):
    val wall = new AtomicLong(start + 500)
    val mono = new AtomicLong(start)
    private val bookTime = new AtomicLong(start)
    private val trades = new AtomicLong(0)
    val ledger = PropLedger.open(directory, "mainnet", d(balance), d("0.00045"))
    private val rest = new Rest:
      def post(body: String): Option[String] = Some(ujson.write(market))
    val worker = new PropWorker(
      TradingConfig(),
      "XMR",
      ledger,
      rest,
      () => wall.get(),
      () => mono.get(),
      Some((market, start))
    )
    worker.start()
    worker.observe(Event.SocketUp)
    book("499.9", "500")
    assert(worker.view.ready, worker.view.message)

    def book(bid: String, ask: String): Unit =
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level(bid, "50", 1)),
            Vector(Level(ask, "50", 1)),
            bookTime.incrementAndGet()
          ),
          true
        )
      )
    def account: PropAccount = ledger.account
    def review(side: Side, price: Option[String], dollars: String): OrderReview =
      val intent = OrderIntent(side, OrderKind.Hard, price = price.map(d))
      val plan = worker.preview(intent, terms(dollars)).fold(reason => fail(reason), identity)
      OrderReview(
        intent,
        plan,
        terms(dollars),
        wall.get(),
        wall.get() + 30000,
        Some(5),
        Some("cross")
      )
    def fill(side: Side, dollars: String): Unit = submit(review(side, None, dollars))
    def submit(review: OrderReview): Unit =
      assert(worker.submitReviewed(review))
      Wait.until("the prop fill did not finish")(!worker.view.pending)
    def place(side: Side, price: String, dollars: String): Unit =
      assert(worker.submitReviewed(review(side, Some(price), dollars)))
    def trade(price: String): Unit =
      worker.observe(
        Event.Trades(
          "XMR",
          Vector(Trade(wall.get() + 1, Side.Sell, price, "0.01", Some(trades.incrementAndGet())))
        )
      )
    def funded(amount: String): Unit =
      worker.tick()
      Wait.until(s"no funding $amount, saw ${account.funding}")(account.funding == d(amount))
    def liquidated(text: String): Unit =
      worker.tick()
      Wait.until(s"no liquidation, saw ${worker.view.outcome}")(
        worker.view.outcome.contains(text)
      )
    // The saved file, read as the wallet's own reader reads it, equals the ledger's account.
    def saved(): Unit =
      val text = LocalFiles.read(directory.resolve(PropLedger.file("mainnet")), 2_000_000L)
      val data = ujson.read(text.getOrElse(fail("no margin wallet saved")))
      def figure(name: String): BigDecimal = BigDecimal(data(name).str)
      assertEquals(figure("cash"), account.cash)
      assertEquals(figure("realized"), account.realized)
      assertEquals(figure("fees"), account.fees)
      assertEquals(figure("funding"), account.funding)
      assertEquals(figure("deposit"), account.deposit)
      val count = data("count") match
        case ujson.Str(text) => text.toLong
        case other           => other.num.toLong
      assertEquals(count, account.count)
      assertEquals(data("resting").arr.size, account.resting.size)
      val positions = data("positions").obj.map((coin, p) =>
        coin -> (BigDecimal(p("quantity").str), BigDecimal(p("entry").str))
      )
      assertEquals(positions.toMap, account.positions.map((c, p) => c -> (p.quantity, p.entry)))

  private def withFixture(balance: String)(body: Fixture => Unit): Path =
    val directory = tempDirectory("release-money-")
    val f = new Fixture(balance, directory)
    try body(f)
    finally f.worker.close()
    directory

  // cash = deposit + realized - fees - funding, the identity every save keeps.
  private def balanced(account: PropAccount): Unit =
    assertEquals(
      account.cash,
      account.deposit + account.realized - account.fees - account.funding,
      account.toString
    )
  private def near(actual: Option[BigDecimal], expected: String): Unit =
    val value = actual.getOrElse(fail(s"no figure where $expected was due"))
    assert((value - d(expected)).abs < d("1e-25"), s"$value is not $expected")

  test("a fill, an hourly charge and a liquidation in one hour keep every figure on one account") {
    val directory = withFixture("120") { f =>
      f.fill(Side.Buy, "100")
      assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("1"), d("500"))))
      assertEquals((f.account.cash, f.account.fees), (d("119.775"), d("0.225")))
      balanced(f.account)
      f.saved()
      assertEquals(f.worker.view.usableMargin, Some(d("19.735")))
      near(f.worker.view.portfolio.liquidation, "422.4722222222222222222222222222222")
      f.worker.observe(Event.CtxUpdate("XMR", context))
      f.wall.set(boundary)
      f.funded("0.00625")
      assertEquals(f.account.cash, d("119.76875"))
      assertEquals(f.account.count, 1L)
      balanced(f.account)
      f.saved()
      // The charged cash moves Usable and the liquidation price at once.
      assertEquals(f.worker.view.usableMargin, Some(d("19.72875")))
      near(f.worker.view.portfolio.liquidation, "422.4791666666666666666666666666667")
      // Mark 422.475 is past the charged account's liquidation price and short of the uncharged
      // one's, so only the charge liquidates here.
      f.book("422.45", "422.5")
      f.liquidated("XMR liquidated $84.50 at 422.475")
      val after = f.account
      assertEquals(after.positions, Map.empty[String, PropPosition])
      assertEquals(
        (after.cash, after.realized, after.fees, after.funding, after.count),
        (d("42.24375"), d("-77.525"), d("0.225"), d("0.00625"), 2L)
      )
      balanced(after)
      f.saved()
      assertEquals(f.worker.view.usableMargin, Some(d("42.24375")))
      assertEquals(f.worker.view.portfolio.liquidation, None)
      assertEquals(f.worker.view.portfolio.net, Some(d("-77.75625")))
      // A venue liquidation starts no opening pause in live, so none starts here.
      for side <- Vector(Side.Buy, Side.Sell) do
        assert(f.worker.preview(OrderIntent(side, OrderKind.Hard), terms("20")).isRight, side)
    }
    // The order path and the clock wrote one wallet that the ledger reads back whole.
    val reopened = PropLedger.open(directory, "mainnet", d("10000"), d("0.00045"))
    try
      assertEquals(reopened.account.cash, d("42.24375"))
      assertEquals(reopened.account.funding, d("0.00625"))
      assertEquals(reopened.account.orders.size, 2)
      assert(reopened.account.orders(1).startsWith("XMR liquidated $84.50 at 422.475 | "))
    finally reopened.close()
  }

  test("the same book does not liquidate the account the hour left uncharged") {
    withFixture("120") { f =>
      f.fill(Side.Buy, "100")
      f.book("422.45", "422.5")
      f.worker.tick()
      f.worker.tick()
      assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("1"), d("500"))))
      assertEquals(f.account.count, 1L)
      near(f.worker.view.portfolio.liquidation, "422.4722222222222222222222222222222")
    }
  }

  test("a review made before an hourly charge fills against the charged cash") {
    for (charged, filled) <- Vector((true, false), (false, true)) do
      withFixture("120.27") { f =>
        f.fill(Side.Buy, "100")
        f.wall.set(boundary - 1000)
        f.worker.observe(Event.CtxUpdate("XMR", context))
        // Free collateral at 500 is 20.045, exactly a $20 press's margin and fee.
        val grow = f.review(Side.Buy, None, "20")
        if charged then
          f.wall.set(boundary)
          f.funded("0.00625")
        f.submit(grow)
        balanced(f.account)
        f.saved()
        if filled then
          assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("1.2"), d("500"))))
        else
          assertEquals(f.worker.view.outcome, Some(PropLedger.noStables))
          assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("1"), d("500"))))
          assertEquals(f.account.cash, d("120.03875"))
      }
  }

  test("a resting order flips the position, and the clock liquidates the short it opened") {
    val directory = withFixture("100") { f =>
      f.place(Side.Sell, "510", "95")
      assertEquals(
        f.account.resting.map(o => (o.quantity, o.reduceOnly)),
        Vector((d("0.931"), false))
      )
      assertEquals(f.worker.view.usableMargin, Some(d("4.9667785")))
      f.fill(Side.Buy, "2")
      assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("0.02"), d("500"))))
      assertEquals(f.worker.view.usableMargin, Some(d("2.9614785")))
      assertEquals(f.worker.view.portfolio.liquidation, None)
      balanced(f.account)
      f.book("511", "511.1")
      f.trade("511")
      assertEquals(f.worker.view.outcome, Some("XMR filled $94.96 at 510.00"))
      assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("-0.911"), d("510"))))
      assertEquals(f.account.resting, Vector.empty)
      assertEquals((f.account.cash, f.account.realized), (d("100.1242785"), d("0.2")))
      balanced(f.account)
      f.saved()
      assertEquals(f.worker.view.usableMargin, Some(d("6.0544185")))
      near(f.worker.view.portfolio.liquidation, "563.5508217742740245484482586568207")
      // Mark 563.55 sits just under the short's liquidation price, 563.65 just over it.
      f.book("563.5", "563.6")
      f.worker.tick()
      assertEquals(f.account.positions.keySet, Set("XMR"))
      f.book("563.6", "563.7")
      f.liquidated("XMR liquidated $102.70 at 563.65")
      assertEquals(
        (f.account.cash, f.account.realized, f.account.fees, f.account.count),
        (d("51.2491285"), d("-48.67515"), d("0.0757215"), 4L)
      )
      assertEquals(f.account.positions, Map.empty[String, PropPosition])
      balanced(f.account)
      f.saved()
      assertEquals(f.worker.view.usableMargin, Some(d("51.2491285")))
    }
    val reopened = PropLedger.open(directory, "mainnet", d("10000"), d("0.00045"))
    try assertEquals(reopened.account.cash, d("51.2491285"))
    finally reopened.close()
  }

  test("a reset with orders resting leaves nothing for a later trade to fill") {
    val directory = withFixture("1000") { f =>
      f.fill(Side.Buy, "20")
      f.place(Side.Sell, "510", "20")
      f.place(Side.Buy, "490", "20")
      assertEquals(
        f.account.resting.map(o => (o.side, o.quantity, o.reduceOnly)),
        Vector((Side.Sell, d("0.2"), true), (Side.Buy, d("0.204"), false))
      )
      val stale = f.review(Side.Buy, None, "20")
      val history = f.account.orders
      assertEquals(f.worker.reset(d("1000")), Right("PROP reset to $1000"))
      val fresh = f.account
      assertEquals(
        fresh.copy(orders = Vector.empty),
        PropAccount(d("1000"), count = 4L, deposit = d("1000"))
      )
      assertEquals(fresh.orders.dropRight(1), history)
      balanced(fresh)
      f.saved()
      val id = f.worker.view.outcomeId
      f.trade("489")
      f.trade("511")
      assertEquals(f.account, fresh)
      assertEquals(f.worker.view.outcomeId, id)
      assert(!f.worker.submitReviewed(stale))
      assertEquals(f.account, fresh)
      assertEquals(f.worker.view.usableMargin, Some(d("1000")))
      assertEquals(f.worker.view.portfolio.liquidation, None)
      assertEquals(f.worker.view.resting, Vector.empty)
      // The new account trades at once, with no pause left from the old one.
      f.fill(Side.Sell, "20")
      assertEquals(f.account.positions.get("XMR").map(_.quantity), Some(d("-0.2")))
      balanced(f.account)
      f.saved()
    }
    val reopened = PropLedger.open(directory, "mainnet", d("10000"), d("0.00045"))
    try
      assertEquals(reopened.account.deposit, d("1000"))
      assertEquals(reopened.account.positions.get("XMR").map(_.quantity), Some(d("-0.2")))
    finally reopened.close()
  }

  test("a stale book still pays the hour from a fresh context and keeps the liquidation price") {
    withFixture("120") { f =>
      f.fill(Side.Buy, "100")
      f.worker.observe(Event.CtxUpdate("XMR", context))
      f.mono.addAndGet(2000)
      assert(!f.worker.view.ready, f.worker.view.message)
      assertEquals(f.worker.view.usableMargin, None)
      near(f.worker.view.portfolio.liquidation, "422.4722222222222222222222222222222")
      f.wall.set(boundary)
      f.funded("0.00625")
      near(f.worker.view.portfolio.liquidation, "422.4791666666666666666666666666667")
      balanced(f.account)
      f.saved()
    }
  }

  test("a liquidation removes a resting order on another market too") {
    withFixture("140") { f =>
      f.worker.select("BTC")
      f.worker.observe(
        Event.BookUpdate(
          "BTC",
          Book(Vector(Level("99999", "5", 1)), Vector(Level("100000", "5", 1)), start + 100),
          true
        )
      )
      f.place(Side.Buy, "99000", "20")
      assertEquals(f.account.resting.map(_.coin), Vector("BTC"))
      f.worker.select("XMR")
      f.book("499.9", "500")
      f.fill(Side.Buy, "100")
      assertEquals(f.account.positions, Map("XMR" -> PropPosition(d("1"), d("500"))))
      f.book("399.9", "400")
      f.liquidated("XMR liquidated $79.99 at 399.95\nResting orders removed: 1")
      assertEquals(f.account.resting, Vector.empty)
      balanced(f.account)
      f.saved()
    }
  }
