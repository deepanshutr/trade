package desk

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

class PropRestingSpec extends TempDirectorySuite:
  private val start = 1800000000000L
  private val meta = ujson.read(
    """{"universe":[{"name":"XMR","szDecimals":3,"maxLeverage":5},{"name":"BTC","szDecimals":5,"maxLeverage":40}]}"""
  )
  private val noStables =
    "Prop limit: not enough stables including fees. Lower order sizes in settings. " +
      "The change applies to the next order."
  private val capText = "Cancel a resting order first. Desk holds at most 20"
  private def d(text: String): BigDecimal = BigDecimal(text)
  private def terms(dollars: String): TradeTerms =
    TradeTerms(TradeSizes(d(dollars), d(dollars), d(dollars)), Trading.defaultSlippage)
  private def id(n: Int): String = "0x%032x".format(n)

  private final class Fixture(cash: String = "10000", memory: Boolean = false):
    val wall = new AtomicLong(start)
    val mono = new AtomicLong(start)
    private val bookTime = new AtomicLong(start)
    val directory: Path = tempDirectory("prop-resting-")
    // A memory ledger skips the disk where a test saves hundreds of times.
    val ledger =
      if memory then
        new PropLedger(
          PropAccount(d(cash), deposit = d(cash)),
          d("0.0005"),
          _ => (),
          new AutoCloseable { def close(): Unit = () }
        )
      else PropLedger.open(directory, "mainnet", d(cash), d("0.0005"))
    private val rest = new Rest:
      def post(body: String): Option[String] = Some(ujson.write(meta))
    val worker = new PropWorker(
      TradingConfig(),
      "XMR",
      ledger,
      rest,
      () => wall.get(),
      () => mono.get(),
      Some((meta, start))
    )
    worker.start()
    worker.observe(Event.SocketUp)
    book()
    assert(worker.view.ready, worker.view.message)

    def book(socket: Boolean = true): Unit =
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("546.09", "50", 1)),
            Vector(Level("546.10", "50", 1)),
            bookTime.incrementAndGet()
          ),
          socket
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
        kind: OrderKind = OrderKind.Hard
    ): OrderReview =
      val plan = preview(side, price, dollars, kind).fold(reason => fail(reason), identity)
      OrderReview(intent(side, price, kind), plan, terms(dollars), wall.get(), wall.get() + 30000)
    // A posted order rests at once, so nothing is waited for.
    def place(side: Side, price: String, dollars: String): PropOrder =
      assert(worker.submitReviewed(review(side, Some(price), dollars)))
      assert(!worker.view.pending, "a posted order left the worker pending")
      ledger.account.resting.last
    def fill(side: Side, dollars: String, kind: OrderKind = OrderKind.Hard): Unit =
      assert(worker.submitReviewed(review(side, None, dollars, kind)))
      Wait.until("the prop fill did not finish")(!worker.view.pending)
    def trades(values: Trade*): Unit = worker.observe(Event.Trades("XMR", values.toVector))
    def trade(price: String, number: Long, at: Long = wall.get() + 1): Trade =
      Trade(at, Side.Sell, price, "0.01", Some(number))
    def held: BigDecimal =
      ledger.account.positions.get("XMR").fold(BigDecimal(0))(_.quantity)
    def seen: Set[(Long, Long)] =
      val field = classOf[PropWorker].getDeclaredFields.find(_.getName.contains("seenTrades")).get
      field.setAccessible(true)
      field.get(worker).asInstanceOf[scala.collection.Set[(Long, Long)]].toSet
    def close(): Unit = worker.close()

  private def withFixture(cash: String = "10000")(run: Fixture => Unit): Unit =
    val f = new Fixture(cash)
    try run(f)
    finally f.close()

  test("a posted buy rests at once without a fill, and the wallet keeps it across a restart") {
    val f = new Fixture
    val plan = f.preview(Side.Buy, Some("546.00"), "109.20").toOption.get
    assert(plan.post)
    assertEquals(plan.estimatedQuantity, d("0"))
    assertEquals(plan.quantity, d("0.2"))
    val order =
      try
        val order = f.place(Side.Buy, "546.00", "109.20")
        assertEquals(f.worker.view.outcome, Some("XMR resting $109.20 at 546.00"))
        assert(order.id.matches("0x[0-9a-f]{32}"))
        assertEquals(
          order,
          PropOrder(order.id, "XMR", Side.Buy, d("546"), d("0.2"), false, start)
        )
        assertEquals(f.ledger.account.cash, d("10000"))
        assertEquals(f.ledger.account.positions, Map.empty[String, PropPosition])
        assertEquals(f.ledger.account.count, 1L)
        assertEquals(f.ledger.usable, d("9890.78362"))
        val view = f.worker.view
        assertEquals(view.leverage, Some(1))
        assertEquals(view.usableMargin, Some(d("9890.78362")))
        assertEquals(
          view.resting,
          Vector(
            RestingOrder(order.id, "XMR", 0, Side.Buy, d("546"), d("0.2"), d("0.2"), false, start)
          )
        )
        assert(view.ready && !view.pending)
        order
      finally f.close()
    val reopened = PropLedger.open(f.directory, "mainnet", d("500"), d("0"))
    try
      assertEquals(reopened.account.resting, Vector(order))
      assertEquals(reopened.account.resting.head.price.bigDecimal.toPlainString, "546.00")
    finally reopened.close()
  }

  test("a crossing price is refused at preview on both sides") {
    withFixture() { f =>
      assertEquals(
        f.preview(Side.Buy, Some("546.10"), "109.20"),
        Left("Price would take liquidity. Move it below the ask")
      )
      f.fill(Side.Buy, "109.22")
      assertEquals(
        f.preview(Side.Sell, Some("546.09"), "55.20"),
        Left("Price would take liquidity. Move it above the bid")
      )
      assert(f.preview(Side.Sell, Some("546.10"), "55.20").isRight)
    }
  }

  test("preview and rest refuse a buy beyond the cash left after other resting buys and fees") {
    withFixture("300") { f =>
      f.place(Side.Buy, "546.00", "109.20")
      f.place(Side.Buy, "546.00", "163.80")
      assertEquals(f.ledger.usable, d("26.95905"))
      assertEquals(f.preview(Side.Buy, Some("546.00"), "109.20"), Left(noStables))
      val order = PropOrder(id(1), "XMR", Side.Buy, d("546.00"), d("0.2"), false, start)
      assertEquals(f.ledger.checkResting(order, f.ledger.account), Left(noStables))
      assertEquals(f.ledger.rest(order, f.ledger.account.count), Left(noStables))
      assertEquals(f.ledger.account.resting.size, 2)
      val small = order.copy(quantity = d("0.049"))
      assertEquals(f.ledger.checkResting(small, f.ledger.account), Right(()))
      val sell = order.copy(side = Side.Sell, quantity = d("1000"))
      assertEquals(f.ledger.checkResting(sell, f.ledger.account), Right(()))
    }
  }

  test("nineteen resting buys reserve their fees, so a twentieth that fits by value is refused") {
    withFixture("1998.36") { f =>
      for _ <- 1 to 19 do f.place(Side.Buy, "546.00", "100")
      assertEquals(f.ledger.account.resting.map(_.quantity).distinct, Vector(d("0.183")))
      assertEquals(f.ledger.usable, d("99.6332337"))
      assertEquals(f.preview(Side.Buy, Some("546.00"), "100"), Left(noStables))
      val twentieth = PropOrder(id(20), "XMR", Side.Buy, d("546.00"), d("0.183"), false, start)
      assertEquals(f.ledger.rest(twentieth, f.ledger.account.count), Left(noStables))
      assertEquals(f.ledger.account.resting.size, 19)
      assertEquals(f.ledger.account.cash, d("1998.36"))
    }
  }

  test("the cap of twenty holds and an order at a price already resting gets its own id") {
    withFixture() { f =>
      for _ <- 1 to 20 do f.place(Side.Buy, "546.00", "20")
      val resting = f.ledger.account.resting
      assertEquals(resting.size, 20)
      assertEquals(resting.map(_.id).distinct.size, 20)
      assertEquals(resting.map(_.price).distinct, Vector(d("546")))
      assertEquals(f.preview(Side.Buy, Some("545.00"), "20"), Left(capText))
      val extra = PropOrder(id(21), "XMR", Side.Buy, d("545.00"), d("0.036"), false, start)
      assertEquals(f.ledger.rest(extra, f.ledger.account.count), Left(capText))
      assertEquals(f.ledger.account.resting.size, 20)
    }
  }

  test("a resting buy whose cash an immediate buy spent is cancelled when its trade comes") {
    withFixture("1000") { f =>
      val order = f.place(Side.Buy, "546.00", "109.20")
      f.fill(Side.Buy, "950")
      assertEquals(f.held, d("1.739"))
      val cash = f.ledger.account.cash
      assertEquals(cash, d("49.85726605"))
      f.trades(f.trade("545.90", 1))
      assertEquals(
        f.worker.view.outcome,
        Some("XMR canceled $109.20 at 546.00 (not enough stables)")
      )
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.ledger.account.cash, cash)
      assertEquals(f.held, d("1.739"))
      assertEquals(
        f.worker.view.fills,
        Vector(Fill("XMR", Side.Buy, start, d("546.1"), d("1.739")))
      )
      assertEquals(f.ledger.fillResting(order.id, start), Left("Prop order not found"))
    }
  }

  test("a trade below a resting buy fills it whole at its own price with the maker fee") {
    withFixture() { f =>
      f.place(Side.Buy, "546.00", "109.20")
      val at = start + 5
      f.trades(Trade(at, Side.Sell, "545.90", "0.01", Some(7)))
      assertEquals(f.worker.view.outcome, Some("XMR filled $109.20 at 546.00"))
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.ledger.account.positions, Map("XMR" -> PropPosition(d("0.2"), d("546"))))
      assertEquals(f.ledger.account.fees, d("0.01638"))
      assertEquals(f.ledger.account.cash, d("9890.78362"))
      assertEquals(f.worker.view.fills, Vector(Fill("XMR", Side.Buy, at, d("546"), d("0.2"))))
      assertEquals(f.worker.view.position, Some(d("0.2")))
    }
  }

  test("a trade at or above a resting buy's price fills nothing") {
    withFixture() { f =>
      val order = f.place(Side.Buy, "546.00", "109.20")
      f.trades(f.trade("546.00", 1), f.trade("546.05", 2))
      assertEquals(f.ledger.account.resting, Vector(order))
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.worker.view.fills, Vector.empty)
    }
  }

  test(
    "a trade above a resting sell fills it, a trade at its price does not, and the pause starts"
  ) {
    withFixture() { f =>
      f.fill(Side.Buy, "109.22")
      val order = f.place(Side.Sell, "552.00", "55.20")
      assertEquals(order.quantity, d("0.1"))
      assert(order.reduceOnly)
      f.trades(f.trade("552.00", 1))
      assertEquals(f.ledger.account.resting, Vector(order))
      val at = start + 9
      f.trades(Trade(at, Side.Buy, "552.10", "0.01", Some(2)))
      assertEquals(f.worker.view.outcome, Some("XMR filled $55.20 at 552.00"))
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.held, d("0.1"))
      assertEquals(f.ledger.account.fees, d("0.06289"))
      assertEquals(f.worker.view.fills.last, Fill("XMR", Side.Sell, at, d("552"), d("0.1")))
      f.mono.addAndGet(TradingWorker.openingPauseMs - 1)
      f.book()
      assertEquals(f.preview(Side.Buy, None, "20"), Left(TradingWorker.openingPausedMessage))
      assertEquals(
        f.preview(Side.Buy, Some("546.00"), "20"),
        Left(TradingWorker.openingPausedMessage)
      )
      assert(f.preview(Side.Sell, None, "20").isRight)
      f.mono.incrementAndGet()
      f.book()
      assert(f.preview(Side.Buy, None, "20").isRight)
      assert(f.preview(Side.Buy, Some("546.00"), "20").isRight)
    }
  }

  test("a trade through two resting buys fills both, the one nearest the trade first") {
    withFixture() { f =>
      f.place(Side.Buy, "546.00", "109.20")
      f.place(Side.Buy, "545.95", "109.19")
      f.trades(f.trade("545.90", 1))
      assertEquals(
        f.worker.view.outcome,
        Some("XMR filled $109.19 at 545.95\nXMR filled $109.20 at 546.00")
      )
      assertEquals(f.worker.view.fills.map(_.price), Vector(d("545.95"), d("546")))
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.held, d("0.4"))
    }
  }

  test("a replayed trade fills nothing and a new trade fills") {
    withFixture() { f =>
      f.place(Side.Buy, "546.00", "109.20")
      val batch = Trade(start + 1000, Side.Sell, "545.90", "0.01", Some(1))
      f.trades(batch)
      assertEquals(f.held, d("0.2"))
      f.wall.incrementAndGet()
      val second = f.place(Side.Buy, "546.00", "109.20")
      assertEquals(second.createdAt, start + 1)
      f.trades(batch)
      assertEquals(f.ledger.account.resting, Vector(second))
      assertEquals(f.held, d("0.2"))
      f.trades(Trade(start + 1001, Side.Sell, "545.90", "0.01", Some(2)))
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.held, d("0.4"))
    }
  }

  test("a seen trade older than the tape window leaves the set and an older trade never enters") {
    withFixture() { f =>
      val old = start - State.tapeMs + 10
      f.trades(Trade(old, Side.Sell, "546.00", "0.01", Some(1)))
      assertEquals(f.seen, Set((old, 1L)))
      f.wall.addAndGet(20)
      f.trades(Trade(f.wall.get(), Side.Sell, "546.00", "0.01", Some(2)))
      assertEquals(f.seen, Set((f.wall.get(), 2L)))
      f.trades(Trade(f.wall.get() - State.tapeMs - 1, Side.Sell, "546.00", "0.01", Some(3)))
      assertEquals(f.seen, Set((f.wall.get(), 2L)))
    }
  }

  test("a trade without an id fills nothing") {
    withFixture() { f =>
      val order = f.place(Side.Buy, "546.00", "109.20")
      f.trades(Trade(start + 1, Side.Sell, "545.90", "0.01", None))
      assertEquals(f.ledger.account.resting, Vector(order))
      f.trades(f.trade("545.90", 1))
      assertEquals(f.ledger.account.resting, Vector.empty)
    }
  }

  test("a trade at or before the order's creation fills nothing") {
    withFixture() { f =>
      f.wall.addAndGet(10)
      val order = f.place(Side.Buy, "546.00", "109.20")
      assertEquals(order.createdAt, start + 10)
      f.trades(
        Trade(start + 9, Side.Sell, "545.90", "0.01", Some(1)),
        Trade(start + 10, Side.Sell, "545.90", "0.01", Some(2))
      )
      assertEquals(f.ledger.account.resting, Vector(order))
      f.trades(Trade(start + 11, Side.Sell, "545.90", "0.01", Some(3)))
      assertEquals(f.ledger.account.resting, Vector.empty)
    }
  }

  test("a batch while backup prices are in use fills nothing") {
    withFixture() { f =>
      val order = f.place(Side.Buy, "546.00", "109.20")
      f.book(socket = false)
      assert(f.worker.view.message.contains("backup"), f.worker.view.message)
      f.trades(f.trade("545.90", 1))
      assertEquals(f.ledger.account.resting, Vector(order))
      f.book()
      assert(!f.worker.view.message.contains("backup"), f.worker.view.message)
      f.trades(f.trade("545.90", 2))
      assertEquals(f.ledger.account.resting, Vector.empty)
    }
  }

  test("a resting sell larger than the position fills the position and cancels the rest") {
    withFixture() { f =>
      f.fill(Side.Buy, "109.22")
      val order = f.place(Side.Sell, "552.00", "82.80")
      assertEquals(order.quantity, d("0.15"))
      f.fill(Side.Sell, "20", OrderKind.Close)
      assertEquals(f.held, d("0"))
      f.mono.addAndGet(TradingWorker.openingPauseMs)
      f.book()
      f.fill(Side.Buy, "54.61")
      assertEquals(f.held, d("0.1"))
      val at = start + 3
      f.trades(Trade(at, Side.Buy, "552.10", "0.01", Some(1)))
      assertEquals(
        f.worker.view.outcome,
        Some(
          "XMR part filled $55.20 of $82.80 at 552.00\n" +
            "XMR canceled $27.60 at 552.00 (nothing left to sell)"
        )
      )
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.ledger.account.positions, Map.empty[String, PropPosition])
      assertEquals(f.worker.view.fills.last, Fill("XMR", Side.Sell, at, d("552"), d("0.1")))
      f.book()
      assertEquals(f.preview(Side.Buy, None, "20"), Left(TradingWorker.openingPausedMessage))
    }
  }

  test("cancel removes the orders at once with the live text") {
    withFixture() { f =>
      val first = f.place(Side.Buy, "546.00", "109.20")
      val second = f.place(Side.Buy, "545.95", "109.19")
      assert(f.worker.cancel(Vector(first.id, second.id)))
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR canceled $109.20 at 546.00\nXMR canceled $109.19 at 545.95")
      )
      assertEquals(f.ledger.account.cash, d("10000"))
      assertEquals(f.ledger.account.count, 4L)
      assertEquals(f.ledger.usable, d("10000"))
      assert(!f.worker.cancel(Vector(first.id)))
    }
  }

  test("reserved caps a prop reduce and refuses one when resting sells cover the position") {
    withFixture() { f =>
      f.fill(Side.Buy, "109.22")
      f.place(Side.Sell, "552.00", "82.80")
      assertEquals(f.preview(Side.Sell, None, "109.22").map(_.quantity), Right(d("0.05")))
      val last = f.place(Side.Sell, "553.00", "27.65")
      assertEquals(last.quantity, d("0.05"))
      assertEquals(
        f.preview(Side.Sell, None, "109.22"),
        Left("Resting orders already reduce the whole position")
      )
      assertEquals(
        f.preview(Side.Sell, None, "20", OrderKind.Close).map(_.quantity),
        Right(d("0.2"))
      )
    }
  }

  test("the count guard refuses a reviewed order after the account changed") {
    withFixture() { f =>
      val first = f.place(Side.Buy, "546.00", "109.20")
      val review = f.review(Side.Buy, Some("545.95"), "109.19")
      assert(f.worker.cancel(Vector(first.id)))
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.ledger.account.resting, Vector.empty)
      val order = PropOrder(id(3), "XMR", Side.Buy, d("545.95"), d("0.2"), false, start)
      assertEquals(
        f.ledger.rest(order, f.ledger.account.count - 1),
        Left("Prop canceled: account changed after review")
      )
      assertEquals(f.ledger.account.resting, Vector.empty)
    }
  }

  test("Close sells the whole long at the touch and leaves a resting sell for its trade") {
    withFixture() { f =>
      f.fill(Side.Buy, "109.22")
      assertEquals(f.held, d("0.2"))
      val order = f.place(Side.Sell, "552.00", "55.20")
      val plan = f.preview(Side.Sell, None, "20", OrderKind.Close).toOption.get
      assertEquals(plan.quantity, d("0.2"))
      assert(!plan.post)
      f.fill(Side.Sell, "20", OrderKind.Close)
      assertEquals(f.ledger.account.positions, Map.empty[String, PropPosition])
      assertEquals(f.ledger.account.fees, d("0.109219"))
      assertEquals(f.ledger.account.cash, d("9999.888781"))
      assertEquals(f.ledger.account.resting, Vector(order))
      assertEquals(f.worker.view.fills.last, Fill("XMR", Side.Sell, start, d("546.09"), d("0.2")))
      assertEquals(f.preview(Side.Buy, None, "20"), Left(TradingWorker.openingPausedMessage))
      f.trades(f.trade("552.10", 1))
      assertEquals(
        f.worker.view.outcome,
        Some("XMR canceled $55.20 at 552.00 (nothing left to sell)")
      )
      assertEquals(f.ledger.account.resting, Vector.empty)
      assertEquals(f.ledger.account.cash, d("9999.888781"))
      assertEquals(f.worker.view.fills.size, 2)
    }
  }

  test("Close with no position is refused") {
    withFixture() { f =>
      assertEquals(f.preview(Side.Sell, None, "20", OrderKind.Close), Left("No XMR to sell"))
    }
  }

  test("view.stale follows the quote age") {
    withFixture() { f =>
      assert(!f.worker.view.stale)
      f.mono.addAndGet(1001)
      assert(f.worker.view.stale)
      f.book()
      assert(!f.worker.view.stale)
    }
  }

  test("the worker keeps the last two hundred fills of a market") {
    val f = new Fixture("1000000", memory = true)
    try
      for round <- 1 to 11 do
        for _ <- 1 to 20 do f.place(Side.Buy, "546.00", "20")
        f.trades(Trade(start + round, Side.Sell, "545.90", "0.01", Some(round.toLong)))
        assertEquals(f.ledger.account.resting, Vector.empty)
      val fills = f.worker.view.fills
      assertEquals(fills.size, 200)
      assertEquals(fills.map(_.time).distinct, (2 to 11).map(start + _).toVector)
    finally f.close()
  }
