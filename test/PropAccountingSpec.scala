package desk

import java.nio.file.{Files, Path}
import java.time.ZoneOffset
import java.util.concurrent.atomic.AtomicBoolean

class PropAccountingSpec extends TempDirectorySuite:
  private val terms = TradeTerms(TradeSizes(), Trading.defaultSlippage)
  private val now = 1800000000000L
  private val fee = BigDecimal("0.001")

  private def plan(side: Side, quantity: BigDecimal, price: BigDecimal): OrderPlan =
    OrderPlan(
      "XMR",
      0,
      side,
      OrderKind.Hard,
      quantity,
      price,
      price,
      quantity,
      quantity * price,
      Some(price),
      BigDecimal(0),
      false
    )

  private def temporary[A](body: Path => A): A =
    val directory = tempDirectory("desk-accounting-check-")
    body(directory)

  private def workerFor(ledger: PropLedger, bid: String = "99", ask: String = "100"): PropWorker =
    val rest = new Rest:
      def post(body: String): Option[String] = None
    val metadata = ujson.read(
      """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""
    )
    val worker = new PropWorker(
      TradingConfig(),
      "XMR",
      ledger,
      rest,
      () => now,
      () => now,
      initialMetadata = Some(metadata -> now)
    )
    worker.observe(Event.SocketUp)
    worker.observe(
      Event.BookUpdate(
        "XMR",
        Book(Vector(Level(bid, "50", 1)), Vector(Level(ask, "50", 1)), now),
        true
      )
    )
    worker

  test("fills move stables only by fees and results and saved profit reconciles to wallet value") {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      // Independently calculated quantities, entry prices, cash, realized profit, fees and net.
      val steps = Vector(
        (Side.Buy, "2", "100", "100", "2", "100", "999.8", "0", "0.2", "-0.2"),
        (Side.Buy, "1", "130", "120", "3", "110", "999.67", "0", "0.33", "29.67"),
        (Side.Sell, "1", "120", "120", "2", "110", "1009.55", "10", "0.45", "29.55"),
        (Side.Sell, "2", "90", "90", "0", "0", "969.37", "-30", "0.63", "-30.63")
      )
      var paid = BigDecimal(0)
      var signedCosts = BigDecimal(0)
      var held = BigDecimal(0)
      try
        steps.zipWithIndex.foreach { (step, index) =>
          val (side, amount, execution, current, quantity, entry, cash, realized, fees, net) = step
          val q = BigDecimal(amount)
          val price = BigDecimal(execution)
          val mark = BigDecimal(current)
          val signed = if side == Side.Buy then q else -q
          paid += q * price * fee
          signedCosts += signed * price
          held += signed
          val before = ledger.account
          assert(ledger.fill(plan(side, q, price), now + index).isRight)
          val saved = ledger.account
          val view = PortfolioView.prop(saved, "XMR", Some(mark))
          assertEquals(saved.cash, BigDecimal(cash))
          assertEquals(saved.cash - before.cash, saved.realized - before.realized - q * price * fee)
          assertEquals(
            saved.positions.get("XMR").map(_.quantity).getOrElse(BigDecimal(0)),
            BigDecimal(quantity)
          )
          assertEquals(
            saved.positions.get("XMR").map(_.entry).getOrElse(BigDecimal(0)),
            BigDecimal(entry)
          )
          assertEquals(saved.realized, BigDecimal(realized))
          assertEquals(saved.fees, BigDecimal(fees))
          assertEquals(view.stables, Some(saved.cash))
          assertEquals(view.tokenValue, Some(held * mark))
          assertEquals(view.net, Some(BigDecimal(net)))
          assertEquals(view.net, Some(-signedCosts - paid + held * mark))
          assertEquals(view.net, Some(view.stables.get + view.openProfit.get - 1000))
        }
      finally ledger.close()
      val reopened = PropLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      try
        assertEquals(reopened.account.count, 4L)
        assertEquals(reopened.account.cash, BigDecimal("969.37"))
        assertEquals(
          PortfolioView.prop(reopened.account, "XMR", None).net,
          Some(BigDecimal("-30.63"))
        )
      finally reopened.close()
    }
  }

  test(
    "wallet rejects reducing absent or insufficient tokens and margin beyond cash including fees"
  ) {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        val empty = ledger.account
        assert(ledger.fill(plan(Side.Sell, 1, 100).copy(reduceOnly = true), now).isLeft)
        assertEquals(ledger.account, empty)
        // 50 at 100 needs all 1000 as margin and its fee on top, long or short.
        for side <- Vector(Side.Buy, Side.Sell) do
          assertEquals(
            ledger.fill(plan(side, 50, 100), now).left.toOption,
            Some(
              "Prop limit: not enough stables including fees. Lower order sizes in settings. " +
                "The change applies to the next order."
            )
          )
          assertEquals(ledger.account, empty)
        assert(ledger.fill(plan(Side.Buy, 2, 100), now).isRight)
        val held = ledger.account
        assert(ledger.fill(plan(Side.Sell, 3, 100).copy(reduceOnly = true), now).isLeft)
        assertEquals(ledger.account, held)
      finally ledger.close()
    }
  }

  test("a break-even round trip with a repeating average entry nets only the entry's rounding") {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(100000), BigDecimal(0))
      try
        for (side, quantity, price) <- Vector(
            (Side.Buy, 1, 100),
            (Side.Buy, 2, 101),
            (Side.Sell, 1, 100),
            (Side.Sell, 2, 101)
          )
        do assert(ledger.fill(plan(side, quantity, price), now).isRight)
        // The entry 302 / 3 keeps 34 digits, so the two reduces realize -1e-31 between them,
        // and cash, realized and net agree on it.
        val rounding = BigDecimal("-1e-31")
        assertEquals(ledger.account.realized, rounding)
        assertEquals(ledger.account.cash, BigDecimal("99999.9999999999999999999999999999999"))
        assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
        assertEquals(PortfolioView.prop(ledger.account, "XMR", None).net, Some(rounding))
      finally ledger.close()
      val reopened = PropLedger.open(directory, "mainnet", BigDecimal(500000), BigDecimal(0))
      try
        assertEquals(
          PortfolioView.prop(reopened.account, "XMR", None).net,
          Some(BigDecimal("-1e-31"))
        )
      finally reopened.close()
    }
  }

  for (balanceText, feeText) <- Vector(
      ("1000", "0.0000000000000000000000000000000001"),
      ("1000.00000000000000000000000000000001", "0.001")
    )
  do
    test("accepted precise deposit and fee stay exact: " + balanceText + ", " + feeText) {
      temporary { directory =>
        val balance = new java.math.BigDecimal(balanceText)
        val cost = new java.math.BigDecimal("100").multiply(new java.math.BigDecimal(feeText))
        val expected = balance.subtract(cost)
        val ledger =
          PropLedger.open(directory, "mainnet", BigDecimal(balance), BigDecimal(feeText))
        try
          assert(ledger.fill(plan(Side.Buy, 1, 100), now).isRight)
          assertEquals(ledger.account.cash, BigDecimal(expected))
          assertEquals(ledger.account.fees, BigDecimal(cost))
          assertEquals(
            PortfolioView.prop(ledger.account, "XMR", Some(100)).net,
            Some(BigDecimal(cost.negate()))
          )
        finally ledger.close()
      }
    }

  test(
    "same-price round trip loses exactly fees and a price move changes profit by held quantity"
  ) {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assert(ledger.fill(plan(Side.Buy, 2, 100), now).isRight)
        val same = PortfolioView.prop(ledger.account, "XMR", Some(BigDecimal(100)))
        val moved = PortfolioView.prop(ledger.account, "XMR", Some(BigDecimal(110)))
        assertEquals(same.net, Some(BigDecimal("-0.2")))
        assertEquals(moved.net.get - same.net.get, BigDecimal(20))
        assertEquals(PortfolioView.prop(ledger.account, "XMR", None).net, None)
        assertEquals(PortfolioView.prop(ledger.account, "BTC", Some(BigDecimal(100))).net, None)
        assert(ledger.fill(plan(Side.Sell, 2, 100), now + 1).isRight)
        assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
        assertEquals(ledger.account.cash, BigDecimal("999.6"))
        assertEquals(PortfolioView.prop(ledger.account, "XMR", None).net, Some(BigDecimal("-0.4")))
      finally ledger.close()
    }
  }

  test(
    "partial fills move only actual tokens, proceeds and fees and preserve the remaining entry"
  ) {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assert(
          ledger
            .fill(plan(Side.Buy, 10, 100).copy(estimatedQuantity = 3, estimatedValue = 300), now)
            .isRight
        )
        assertEquals(ledger.account.cash, BigDecimal("999.7"))
        assert(
          ledger
            .fill(
              plan(Side.Sell, 2, 110)
                .copy(estimatedQuantity = BigDecimal("0.5"), estimatedValue = 55),
              now
            )
            .isRight
        )
        assertEquals(ledger.account.cash, BigDecimal("1004.645"))
        assertEquals(
          ledger.account.positions("XMR"),
          PropPosition(BigDecimal("2.5"), BigDecimal(100))
        )
        assertEquals(ledger.account.realized, BigDecimal(5))
        assertEquals(ledger.account.fees, BigDecimal("0.355"))
        assertEquals(
          PortfolioView.prop(ledger.account, "XMR", Some(BigDecimal(110))).net,
          Some(BigDecimal("29.645"))
        )
      finally ledger.close()
    }
  }

  test("a selected dollar sell closes a smaller holding without opening a short") {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      assert(ledger.fill(plan(Side.Buy, BigDecimal("0.75"), 100), now).isRight)
      val worker = workerFor(ledger)
      try
        val selected = worker.preview(OrderIntent(Side.Sell, OrderKind.Hard), terms).toOption.get
        assertEquals(selected.quantity, BigDecimal("0.75"))
        assertEquals(selected.estimatedQuantity, BigDecimal("0.75"))
        assertEquals(selected.estimatedValue, BigDecimal("74.25"))
        assertEquals(selected.positionAfter, BigDecimal(0))
        assert(ledger.fill(selected, now).isRight)
        assertEquals(ledger.account.cash, BigDecimal("999.10075"))
        assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
        // The next sell opens a short. It is not a reduce of the closed holding.
        assertEquals(
          worker.preview(OrderIntent(Side.Sell, OrderKind.Hard), terms).map(_.reduceOnly),
          Right(false)
        )
      finally worker.close()
    }
  }

  test("the compact wallet values add up and no extra trading text is needed") {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assert(ledger.fill(plan(Side.Buy, 2, 100), now).isRight)
        val view = TradingView(
          "XMR",
          Some(BigDecimal(2)),
          "ready",
          ready = true,
          portfolio = PortfolioView.prop(ledger.account, "XMR", Some(BigDecimal(100)))
        )
        val frame = Workspace.draw(
          Snapshot.initial("XMR", now),
          view,
          PanelView(mode = "PROP"),
          100,
          30,
          now,
          ZoneOffset.UTC,
          Style.mono
        )
        val text = frame.lines.map(Chart.plain).mkString("\n")
        assert(text.contains("$200"), text)
        assert(text.contains("$999.8"), text)
        assert(text.contains("$-0.2"), text)
        assertEquals(frame.buttons.count(_.action.isInstanceOf[DeskAction.Prepare]), 2)
      finally ledger.close()
    }
  }

  test("the marked wallet uses an exact midpoint for accepted large integer prices") {
    temporary { directory =>
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal(0))
      assert(ledger.fill(plan(Side.Buy, 1, 100), now).isRight)
      val bid = new java.math.BigDecimal("1e40")
      val ask = bid.add(java.math.BigDecimal.ONE)
      assert(Trading.validPrice(BigDecimal(bid), MarketRules(0, 2, 5)))
      assert(Trading.validPrice(BigDecimal(ask), MarketRules(0, 2, 5)))
      val worker = workerFor(ledger, bid.toPlainString, ask.toPlainString)
      try
        val midpoint = bid.add(ask).divide(new java.math.BigDecimal("2"))
        assert(worker.view.ready)
        assertEquals(worker.view.portfolio.tokenValue, Some(BigDecimal(midpoint)))
        assertEquals(
          worker.view.portfolio.net,
          Some(BigDecimal(midpoint.subtract(new java.math.BigDecimal("100"))))
        )
      finally worker.close()
    }
  }

  test("a new wallet preserves old margin files and saved wallet cash survives settings changes") {
    temporary { directory =>
      val legacy = directory.resolve("paper-mainnet.json")
      val old =
        """{"cash":"900","realized":"0","fees":"0","positions":{"XMR":{"quantity":"-1","entry":"100"}},"orders":[],"count":1}"""
      Files.writeString(legacy, old)
      Platform.setMode(legacy, "rw-------")
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assertEquals(ledger.account, PropAccount(BigDecimal(1000), deposit = BigDecimal(1000)))
        assert(ledger.fill(plan(Side.Buy, 1, 100), now).isRight)
        assert(Files.exists(directory.resolve(PropLedger.file("mainnet"))))
        assertEquals(Files.readString(legacy), old)
        intercept[Exception](PropLedger.open(directory, "mainnet", BigDecimal(1000), fee))
      finally ledger.close()
      val reopened = PropLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      try assertEquals(reopened.account.cash, BigDecimal("999.9"))
      finally reopened.close()
    }
  }

  test("position quantity and money use one saved account when a fill commits during the view") {
    val armed = new AtomicBoolean(false)
    var duringRead = () => ()
    val original = Map("XMR" -> PropPosition(BigDecimal(2), BigDecimal(100)))
    val positions = new scala.collection.immutable.AbstractMap[String, PropPosition]:
      def get(key: String): Option[PropPosition] =
        val result = original.get(key)
        if armed.compareAndSet(true, false) then duringRead()
        result
      def iterator: Iterator[(String, PropPosition)] = original.iterator
      def removed(key: String): Map[String, PropPosition] = original.removed(key)
      def updated[V1 >: PropPosition](key: String, value: V1): Map[String, V1] =
        original.updated(key, value)
    val save: PropAccount => Unit = _ => ()
    val lease: AutoCloseable = () => ()
    val ledger = new PropLedger(
      PropAccount(
        BigDecimal("999.8"),
        fees = BigDecimal("0.2"),
        positions = positions,
        deposit = BigDecimal(1000)
      ),
      fee,
      save,
      lease
    )
    val worker = workerFor(ledger, "109", "111")
    duringRead = () => assert(ledger.fill(plan(Side.Sell, 1, 110), now).isRight)
    try
      armed.set(true)
      val first = worker.view
      assertEquals(ledger.account.count, 1L)
      assertEquals(first.portfolio.tokenValue, first.position.map(_ * 110))
      assertEquals(first.position, Some(BigDecimal(2)))
      assertEquals(first.portfolio.stables, Some(BigDecimal("999.8")))
      assertEquals(first.portfolio.net, Some(BigDecimal("19.8")))
      val next = worker.view
      assertEquals(next.position, Some(BigDecimal(1)))
      assertEquals(next.portfolio.tokenValue, Some(BigDecimal(110)))
      assertEquals(next.portfolio.stables, Some(BigDecimal("1009.69")))
      assertEquals(next.portfolio.net, Some(BigDecimal("19.69")))
    finally worker.close()
  }

  test("current wallet audit fields survive loading, another fill and reload") {
    temporary { directory =>
      LocalFiles.write(
        directory.resolve(PropLedger.file("mainnet")),
        """{"cash":"799.8","deposit":"1000","realized":"12","fees":"0.2","positions":{"XMR":{"quantity":"2","entry":"100"}},"orders":["previous fill"],"count":1}"""
      )
      val ledger = PropLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      val expected = PropAccount(
        cash = BigDecimal("799.8"),
        deposit = BigDecimal(1000),
        realized = BigDecimal(12),
        fees = BigDecimal("0.2"),
        positions = Map("XMR" -> PropPosition(BigDecimal(2), BigDecimal(100))),
        orders = Vector("previous fill"),
        count = 1L
      )
      val filled = try
        assertEquals(ledger.account, expected)
        assert(ledger.fill(plan(Side.Buy, BigDecimal(1), BigDecimal(100)), now).isRight)
        ledger.account
      finally ledger.close()
      val reopened = PropLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      try
        assertEquals(reopened.account, filled)
        assertEquals(reopened.account.cash, BigDecimal("799.7"))
        assertEquals(reopened.account.positions("XMR").quantity, BigDecimal(3))
        assertEquals(reopened.account.orders.head, "previous fill")
        assertEquals(reopened.account.count, 2L)
      finally reopened.close()
    }
  }

  // Live's Usable never reads below zero, so prop's does not either. The ledger keeps the signed
  // figure for its checks.
  test("Usable reads $0.00 while the free collateral is below zero, as live's does") {
    val buy =
      PropOrder("0x" + "1" * 32, "XMR", Side.Buy, BigDecimal(100), BigDecimal(1), false, now)
    val long = PropAccount(
      BigDecimal("119.775"),
      fees = BigDecimal("0.225"),
      positions = Map("XMR" -> PropPosition(BigDecimal(1), BigDecimal(500))),
      deposit = BigDecimal(120)
    )
    for (account, bid, ask, free) <- Vector(
        // A long of 1 from 500 at mark 449.95: 119.775 - 50.05 - 89.99. Maintenance is 44.995.
        (long, "449.9", "450", "-20.265"),
        // Flat, with a resting buy that reserves 100 * (1/5 + 0.00015) = 20.015 of the 10.
        (
          PropAccount(BigDecimal(10), deposit = BigDecimal(10), resting = Vector(buy)),
          "99",
          "100",
          "-10.015"
        )
      )
    do
      val ledger = new PropLedger(account, fee, _ => (), () => ())
      val worker = workerFor(ledger, bid, ask)
      try
        val mark = (BigDecimal(bid) + BigDecimal(ask)) / 2
        assertEquals(PropMargin.free(ledger.account, mark), BigDecimal(free))
        val view = worker.view
        assert(view.ready, view.message)
        assertEquals(view.usableMargin, Some(BigDecimal(0)))
        val frame = Workspace.draw(
          Snapshot.initial("XMR", now),
          view,
          PanelView(mode = "PROP"),
          100,
          30,
          now,
          ZoneOffset.UTC,
          Style.mono
        )
        val text = frame.lines.map(Chart.plain).mkString("\n")
        assert(text.contains("Usable    $0.00"), text)
      finally worker.close()
  }
