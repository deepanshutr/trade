package desk

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

// Prop trades long and short at 5x on one cross account, as live does.
class PropShortSpec extends TempDirectorySuite:
  private val start = 1800000000000L
  private val paused = TradingWorker.openingPausedMessage
  private def d(text: String): BigDecimal = BigDecimal(text)
  private def terms(dollars: String): TradeTerms =
    TradeTerms(TradeSizes(d(dollars), d(dollars), d(dollars)), Trading.defaultSlippage)
  private def meta(maxLeverage: Int): ujson.Value = ujson.read(
    s"""{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":$maxLeverage},{"name":"BTC","szDecimals":5,"maxLeverage":40}]}"""
  )

  private final class Fixture(maxLeverage: Int):
    val wall = new AtomicLong(start)
    val mono = new AtomicLong(start)
    private val bookTime = new AtomicLong(start)
    private val market = meta(maxLeverage)
    val directory: Path = tempDirectory("prop-short-")
    val ledger = PropLedger.open(directory, "mainnet", d("10000"), d("0.00045"))
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
    book()
    assert(worker.view.ready, worker.view.message)

    def book(): Unit =
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("546.09", "50", 1)),
            Vector(Level("546.10", "50", 1)),
            bookTime.incrementAndGet()
          ),
          true
        )
      )
    def intent(side: Side, price: Option[String], kind: OrderKind): OrderIntent =
      OrderIntent(side, kind, price = price.map(d))
    def preview(
        side: Side,
        price: Option[String],
        dollars: String,
        kind: OrderKind = OrderKind.Hard
    ): Either[String, OrderPlan] =
      worker.preview(intent(side, price, kind), terms(dollars))
    def review(
        side: Side,
        price: Option[String],
        dollars: String,
        kind: OrderKind = OrderKind.Hard,
        leverage: Option[Int] = None,
        marginMode: Option[String] = None
    ): OrderReview =
      val plan = preview(side, price, dollars, kind).fold(reason => fail(reason), identity)
      OrderReview(
        intent(side, price, kind),
        plan,
        terms(dollars),
        wall.get(),
        wall.get() + 30000,
        leverage,
        marginMode
      )
    def place(side: Side, price: String, dollars: String): PropOrder =
      assert(worker.submitReviewed(review(side, Some(price), dollars)))
      assert(!worker.view.pending, "a posted order left the worker pending")
      ledger.account.resting.last
    def fill(side: Side, dollars: String, kind: OrderKind = OrderKind.Hard): Unit =
      submit(review(side, None, dollars, kind))
    def submit(review: OrderReview): Unit =
      assert(worker.submitReviewed(review))
      Wait.until("the prop fill did not finish")(!worker.view.pending)
    def trade(price: String, number: Long): Unit =
      worker.observe(
        Event.Trades("XMR", Vector(Trade(wall.get() + 1, Side.Sell, price, "0.01", Some(number))))
      )
    def position: Option[PropPosition] = ledger.account.positions.get("XMR")
    def firstLine: Option[String] = worker.view.outcome.map(_.takeWhile(_ != '\n'))

  private def withFixture(maxLeverage: Int = 5)(run: Fixture => Unit): Unit =
    val f = new Fixture(maxLeverage)
    try run(f)
    finally f.worker.close()

  // The owner's report: in prop a SHORT with no holding said "No XMR to sell".
  private def short(f: Fixture): Unit =
    val plan = f.preview(Side.Sell, None, "20")
    assertNotEquals(plan, Left("No XMR to sell"))
    f.fill(Side.Sell, "20")

  test("SHORT with no holding opens a short at 5x") {
    withFixture() { f =>
      val plan = f.preview(Side.Sell, None, "20").fold(reason => fail(reason), identity)
      assertEquals(plan.quantity, d("0.183"))
      assertEquals(plan.estimatedValue, d("99.93447"))
      assert(!plan.reduceOnly)
      assertEquals(plan.positionAfter, d("-0.183"))
      short(f)
      assertEquals(f.position, Some(PropPosition(d("-0.183"), d("546.09"))))
      assertEquals(f.ledger.account.cash, d("9999.9550294885"))
      assertEquals(f.firstLine, Some("PROP XMR sell filled $19.99 at 546.09"))
      // An opening fill does not pause the next opening press.
      assert(f.preview(Side.Sell, None, "20").isRight)
    }
  }

  test("the view reports 5x cross and the short's figures") {
    withFixture() { f =>
      short(f)
      val view = f.worker.view
      assertEquals(view.leverage, Some(5))
      assertEquals(view.marginMode, Some("cross"))
      assertEquals(view.position, Some(d("-0.183")))
      assertEquals(view.portfolio.tokenValue, Some(d("-99.935385")))
      assertEquals(view.portfolio.openProfit, Some(d("-0.000915")))
      assertEquals(view.portfolio.net, Some(d("-0.0458855115")))
      assertEquals(view.portfolio.stables, Some(d("9999.9550294885")))
      assertEquals(view.portfolio.entry, Some(d("546.09")))
      assertEquals(view.usableMargin, Some(d("9979.9670374885")))
      // (546.09 + 9999.9550294885 / 0.183) / 1.1, each division to 34 digits half even.
      assertEquals(view.portfolio.liquidation, Some(d("50173.32091151763537009438648782911")))
    }
  }

  test("REDUCE on a short closes it and pauses opening") {
    withFixture() { f =>
      short(f)
      val plan = f.preview(Side.Buy, None, "20").fold(reason => fail(reason), identity)
      assertEquals(plan.kind, OrderKind.Close)
      assertEquals(plan.quantity, d("0.183"))
      assert(plan.reduceOnly)
      f.fill(Side.Buy, "20")
      assertEquals(f.position, None)
      assertEquals(f.ledger.account.cash, d("9999.9082281535"))
      assertEquals(f.preview(Side.Sell, None, "20"), Left(paused))
      f.mono.addAndGet(TradingWorker.openingPauseMs)
      f.book()
      assert(f.preview(Side.Sell, None, "20").isRight)
    }
  }

  test("CLOSE on a short buys the whole position") {
    withFixture() { f =>
      short(f)
      val plan = f.preview(Side.Buy, None, "20", OrderKind.Close)
      assertEquals(plan.map(_.quantity), Right(d("0.183")))
      assertEquals(plan.map(_.kind), Right(OrderKind.Close))
      f.fill(Side.Buy, "20", OrderKind.Close)
      assertEquals(f.position, None)
      assertEquals(f.preview(Side.Buy, None, "20", OrderKind.Close), Left("No position to close"))
      // A sell CLOSE that meets a flat account answers as live does.
      assertEquals(f.preview(Side.Sell, None, "20", OrderKind.Close), Left("No position to close"))
    }
  }

  test("the review accepts 5x cross and refuses other terms") {
    withFixture() { f =>
      for (leverage, mode) <- Vector(
          (Some(1), Some("cross")),
          (Some(5), Some("isolated")),
          (Some(1), None),
          (None, Some("isolated"))
        )
      do
        val review = f.review(Side.Sell, None, "20", leverage = leverage, marginMode = mode)
        assert(!f.worker.submitReviewed(review), s"$leverage $mode")
        assertEquals(f.ledger.account.count, 0L)
        assertEquals(f.position, None)
      f.submit(f.review(Side.Sell, None, "20", leverage = Some(5), marginMode = Some("cross")))
      assertEquals(f.position.map(_.quantity), Some(d("-0.183")))
      f.submit(f.review(Side.Sell, None, "20"))
      assertEquals(f.position.map(_.quantity), Some(d("-0.366")))
    }
  }

  test("a market whose maximum is under 5x is refused at the press") {
    withFixture(maxLeverage = 3) { f =>
      val refused = Left("Leverage is outside the market limit")
      assertEquals(f.preview(Side.Buy, None, "20"), refused)
      assertEquals(f.preview(Side.Sell, None, "20"), refused)
      assertEquals(f.preview(Side.Sell, Some("546.20"), "20"), refused)
    }
  }

  test("a posted sell with no holding rests with its reserve and fills as a short") {
    withFixture() { f =>
      val plan = f.preview(Side.Sell, Some("546.20"), "20").fold(reason => fail(reason), identity)
      assert(plan.post)
      assertEquals(plan.quantity, d("0.183"))
      assert(!plan.reduceOnly)
      f.place(Side.Sell, "546.20", "20")
      assertEquals(f.worker.view.usableMargin, Some(d("9979.99408681")))
      f.trade("546.30", 1)
      assertEquals(f.position, Some(PropPosition(d("-0.183"), d("546.20"))))
      assertEquals(f.ledger.account.cash, d("9999.98500681"))
      assertEquals(f.worker.view.outcome, Some("XMR filled $19.99 at 546.20"))
      assert(f.preview(Side.Sell, None, "20").isRight)
    }
  }

  test("a resting reduce-only buy that faced the short pauses opening after its fill") {
    withFixture() { f =>
      short(f)
      val order = f.place(Side.Buy, "546.00", "20")
      assert(order.reduceOnly)
      assertEquals(order.quantity, d("0.183"))
      f.trade("545.90", 1)
      assertEquals(f.position, None)
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.preview(Side.Sell, None, "20"), Left(paused))
    }
  }

  test("a reduce-only buy whose short was closed is cancelled whole at its trade") {
    withFixture() { f =>
      short(f)
      f.place(Side.Buy, "546.00", "20")
      f.fill(Side.Buy, "20", OrderKind.Close)
      assertEquals(f.position, None)
      val fills = f.worker.view.fills
      f.trade("545.90", 1)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR canceled $19.98 at 546.00 (nothing left to reduce)")
      )
      assertEquals(f.position, None)
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.worker.view.fills, fills)
    }
  }

  test("usable margin needs a mark for a held position on the selected market") {
    withFixture() { f =>
      // Flat, free collateral needs no mark.
      f.mono.addAndGet(1001)
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.usableMargin, Some(d("10000")))
      f.book()
      short(f)
      f.mono.addAndGet(1001)
      assertEquals(f.worker.view.usableMargin, None)
      f.book()
      assertEquals(f.worker.view.usableMargin, Some(d("9979.9670374885")))
      // On another market the mark there cannot value the XMR short.
      f.worker.select("BTC")
      f.worker.observe(
        Event.BookUpdate(
          "BTC",
          Book(Vector(Level("60000", "1", 1)), Vector(Level("60001", "1", 1)), start + 50),
          true
        )
      )
      assert(f.worker.view.ready, f.worker.view.message)
      assertEquals(f.worker.view.usableMargin, None)
      // The BTC axis draws no liquidation price for the XMR short.
      assertEquals(f.worker.view.portfolio.liquidation, None)
    }
  }

  test("the view's liquidation uses the selected market's maintenance") {
    withFixture(maxLeverage = 10) { f =>
      short(f)
      // A market of 10x liquidates at 1 / 20 of the position's value:
      // (546.09 + 9999.9550294885 / 0.183) / 1.05, each division to 34 digits half even.
      assertEquals(
        f.worker.view.portfolio.liquidation,
        Some(d("52562.52666920895134009888108248764"))
      )
    }
  }

  test("usable margin is unknown while the ledger is unavailable") {
    withFixture() { f =>
      assertEquals(f.worker.view.usableMargin, Some(d("10000")))
      f.ledger.close()
      assertEquals(f.worker.view.usableMargin, None)
      assertEquals(f.worker.view.portfolio, PortfolioView())
    }
  }

  test("a resting opening sell that flips a long pauses opening after its fill") {
    withFixture() { f =>
      val order = f.place(Side.Sell, "546.20", "40")
      assert(!order.reduceOnly)
      assertEquals(order.quantity, d("0.366"))
      f.fill(Side.Buy, "20")
      assertEquals(f.position.map(_.quantity), Some(d("0.183")))
      f.trade("546.30", 1)
      assertEquals(f.position, Some(PropPosition(d("-0.183"), d("546.20"))))
      assertEquals(f.preview(Side.Sell, None, "20"), Left(paused))
    }
  }

  test("a buy CLOSE on a long is refused as live refuses it, and nothing changes") {
    withFixture() { f =>
      f.fill(Side.Buy, "20")
      val long = f.ledger.account
      assertEquals(f.position.map(_.quantity), Some(d("0.183")))
      assertEquals(f.preview(Side.Buy, None, "20", OrderKind.Close), Left("No position to close"))
      assertEquals(f.ledger.account, long)
    }
  }
