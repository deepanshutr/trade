package desk

import java.nio.file.{Files, Path}
import java.time.ZoneOffset
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*

class PaperAccountingSpec extends munit.FunSuite:
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
      BigDecimal(0)
    )

  private def temporary[A](body: Path => A): A =
    val directory = Files.createTempDirectory("desk-accounting-check-")
    try body(directory)
    finally
      val paths = Files.walk(directory)
      try paths.iterator().asScala.toVector.reverse.foreach(Files.delete)
      finally paths.close()

  private def workerFor(ledger: PaperLedger, bid: String = "99", ask: String = "100"): PaperWorker =
    val rest = new Rest:
      def post(body: String): Option[String] = None
    val metadata = ujson.read(
      """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""
    )
    val worker = new PaperWorker(
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

  test("buys spend stables, sells return stables and saved profit reconciles to wallet value") {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      // Independently calculated quantities, entry prices, cash, realized profit, fees and net.
      val steps = Vector(
        (Side.Buy, "2", "100", "100", "2", "100", "799.8", "0", "0.2", "-0.2"),
        (Side.Buy, "1", "130", "120", "3", "110", "669.67", "0", "0.33", "29.67"),
        (Side.Sell, "1", "120", "120", "2", "110", "789.55", "10", "0.45", "29.55"),
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
          val before = ledger.account.cash
          assert(ledger.fill(plan(side, q, price), now + index).isRight)
          val saved = ledger.account
          val view = PortfolioView.paper(saved, "XMR", Some(mark))
          assertEquals(saved.cash, BigDecimal(cash))
          assertEquals(saved.cash - before, -signed * price - q * price * fee)
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
          assertEquals(view.net, Some(view.stables.get + view.tokenValue.get - 1000))
        }
      finally ledger.close()
      val reopened = PaperLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      try
        assertEquals(reopened.account.count, 4L)
        assertEquals(reopened.account.cash, BigDecimal("969.37"))
        assertEquals(
          PortfolioView.paper(reopened.account, "XMR", None).net,
          Some(BigDecimal("-30.63"))
        )
      finally reopened.close()
    }
  }

  test(
    "wallet rejects selling absent or insufficient tokens and spending more than cash including fees"
  ) {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        val empty = ledger.account
        assert(ledger.fill(plan(Side.Sell, 1, 100), now).isLeft)
        assertEquals(ledger.account, empty)
        assert(ledger.fill(plan(Side.Buy, 10, 100), now).isLeft)
        assertEquals(ledger.account, empty)
        assert(ledger.fill(plan(Side.Buy, 2, 100), now).isRight)
        val held = ledger.account
        assert(ledger.fill(plan(Side.Sell, 3, 100), now).isLeft)
        assertEquals(ledger.account, held)
      finally ledger.close()
    }
  }

  test("an exact break-even round trip has zero net even with a repeating average entry") {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(100000), BigDecimal(0))
      try
        for (side, quantity, price) <- Vector(
            (Side.Buy, 1, 100),
            (Side.Buy, 2, 101),
            (Side.Sell, 1, 100),
            (Side.Sell, 2, 101)
          )
        do assert(ledger.fill(plan(side, quantity, price), now).isRight)
        assertEquals(ledger.account.cash, BigDecimal(100000))
        assertEquals(ledger.account.positions, Map.empty[String, PaperPosition])
        assertEquals(PortfolioView.paper(ledger.account, "XMR", None).net, Some(BigDecimal(0)))
      finally ledger.close()
      val reopened = PaperLedger.open(directory, "mainnet", BigDecimal(500000), BigDecimal(0))
      try assertEquals(PortfolioView.paper(reopened.account, "XMR", None).net, Some(BigDecimal(0)))
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
        val expected = balance.subtract(new java.math.BigDecimal("100")).subtract(cost)
        val ledger =
          PaperLedger.open(directory, "mainnet", BigDecimal(balance), BigDecimal(feeText))
        try
          assert(ledger.fill(plan(Side.Buy, 1, 100), now).isRight)
          assertEquals(ledger.account.cash, BigDecimal(expected))
          assertEquals(ledger.account.fees, BigDecimal(cost))
          assertEquals(
            PortfolioView.paper(ledger.account, "XMR", Some(100)).net,
            Some(BigDecimal(cost.negate()))
          )
        finally ledger.close()
      }
    }

  test(
    "same-price round trip loses exactly fees and a price move changes profit by held quantity"
  ) {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assert(ledger.fill(plan(Side.Buy, 2, 100), now).isRight)
        val same = PortfolioView.paper(ledger.account, "XMR", Some(BigDecimal(100)))
        val moved = PortfolioView.paper(ledger.account, "XMR", Some(BigDecimal(110)))
        assertEquals(same.net, Some(BigDecimal("-0.2")))
        assertEquals(moved.net.get - same.net.get, BigDecimal(20))
        assertEquals(PortfolioView.paper(ledger.account, "XMR", None).net, None)
        assertEquals(PortfolioView.paper(ledger.account, "BTC", Some(BigDecimal(100))).net, None)
        assert(ledger.fill(plan(Side.Sell, 2, 100), now + 1).isRight)
        assertEquals(ledger.account.positions, Map.empty[String, PaperPosition])
        assertEquals(ledger.account.cash, BigDecimal("999.6"))
        assertEquals(PortfolioView.paper(ledger.account, "XMR", None).net, Some(BigDecimal("-0.4")))
      finally ledger.close()
    }
  }

  test(
    "partial fills move only actual tokens, proceeds and fees and preserve the remaining entry"
  ) {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assert(
          ledger
            .fill(plan(Side.Buy, 10, 100).copy(estimatedQuantity = 3, estimatedValue = 300), now)
            .isRight
        )
        assertEquals(ledger.account.cash, BigDecimal("699.7"))
        assert(
          ledger
            .fill(
              plan(Side.Sell, 2, 110)
                .copy(estimatedQuantity = BigDecimal("0.5"), estimatedValue = 55),
              now
            )
            .isRight
        )
        assertEquals(ledger.account.cash, BigDecimal("754.645"))
        assertEquals(
          ledger.account.positions("XMR"),
          PaperPosition(BigDecimal("2.5"), BigDecimal(100))
        )
        assertEquals(ledger.account.realized, BigDecimal(5))
        assertEquals(ledger.account.fees, BigDecimal("0.355"))
        assertEquals(
          PortfolioView.paper(ledger.account, "XMR", Some(BigDecimal(110))).net,
          Some(BigDecimal("29.645"))
        )
      finally ledger.close()
    }
  }

  test("a selected dollar sell closes a smaller holding without opening a short") {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      assert(ledger.fill(plan(Side.Buy, BigDecimal("0.75"), 100), now).isRight)
      val worker = workerFor(ledger)
      try
        val selected = worker.preview(OrderIntent(Side.Sell, OrderKind.Hard)).toOption.get
        assertEquals(selected.quantity, BigDecimal("0.75"))
        assertEquals(selected.estimatedQuantity, BigDecimal("0.75"))
        assertEquals(selected.estimatedValue, BigDecimal("74.25"))
        assertEquals(selected.positionAfter, BigDecimal(0))
        assert(ledger.fill(selected, now).isRight)
        assertEquals(ledger.account.cash, BigDecimal("999.10075"))
        assertEquals(ledger.account.positions, Map.empty[String, PaperPosition])
        assert(worker.preview(OrderIntent(Side.Sell, OrderKind.Hard)).isLeft)
      finally worker.close()
    }
  }

  test("the compact wallet values add up and no extra trading text is needed") {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assert(ledger.fill(plan(Side.Buy, 2, 100), now).isRight)
        val view = TradingView(
          "XMR",
          Some(BigDecimal(2)),
          "ready",
          ready = true,
          portfolio = PortfolioView.paper(ledger.account, "XMR", Some(BigDecimal(100)))
        )
        val frame = Workspace.draw(
          Snapshot.initial("XMR", now),
          view,
          PanelView(mode = "PAPER"),
          100,
          30,
          now,
          ZoneOffset.UTC,
          Style.mono
        )
        val text = frame.lines.map(Chart.plain).mkString("\n")
        assert(text.contains("$200"), text)
        assert(text.contains("$799.8"), text)
        assert(text.contains("$-0.2"), text)
        assertEquals(frame.buttons.count(_.action.isInstanceOf[DeskAction.Prepare]), 2)
      finally ledger.close()
    }
  }

  test("the marked wallet uses an exact midpoint for accepted large integer prices") {
    temporary { directory =>
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal(0))
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
      Files.setPosixFilePermissions(
        legacy,
        java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
      )
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee)
      try
        assertEquals(ledger.account, PaperAccount(BigDecimal(1000), deposit = BigDecimal(1000)))
        assert(ledger.fill(plan(Side.Buy, 1, 100), now).isRight)
        assert(Files.exists(directory.resolve("wallet-mainnet.json")))
        assertEquals(Files.readString(legacy), old)
        intercept[Exception](PaperLedger.open(directory, "mainnet", BigDecimal(1000), fee))
      finally ledger.close()
      val reopened = PaperLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      try assertEquals(reopened.account.cash, BigDecimal("899.9"))
      finally reopened.close()
    }
  }

  test("position quantity and money use one saved account when a fill commits during the view") {
    val armed = new AtomicBoolean(false)
    var duringRead = () => ()
    val original = Map("XMR" -> PaperPosition(BigDecimal(2), BigDecimal(100)))
    val positions = new scala.collection.immutable.AbstractMap[String, PaperPosition]:
      def get(key: String): Option[PaperPosition] =
        val result = original.get(key)
        if armed.compareAndSet(true, false) then duringRead()
        result
      def iterator: Iterator[(String, PaperPosition)] = original.iterator
      def removed(key: String): Map[String, PaperPosition] = original.removed(key)
      def updated[V1 >: PaperPosition](key: String, value: V1): Map[String, V1] =
        original.updated(key, value)
    val constructor =
      classOf[PaperLedger].getDeclaredConstructors.find(_.getParameterCount == 4).get
    constructor.setAccessible(true)
    val save: PaperAccount => Unit = _ => ()
    val lease: AutoCloseable = () => ()
    val ledger = constructor
      .newInstance(
        PaperAccount(
          BigDecimal("799.8"),
          fees = BigDecimal("0.2"),
          positions = positions,
          deposit = BigDecimal(1000)
        ),
        fee,
        save,
        lease
      )
      .asInstanceOf[PaperLedger]
    val worker = workerFor(ledger, "109", "111")
    duringRead = () => assert(ledger.fill(plan(Side.Sell, 1, 110), now).isRight)
    try
      armed.set(true)
      val first = worker.view
      assertEquals(ledger.account.count, 1L)
      assertEquals(first.portfolio.tokenValue, first.position.map(_ * 110))
      assertEquals(first.position, Some(BigDecimal(2)))
      assertEquals(first.portfolio.stables, Some(BigDecimal("799.8")))
      assertEquals(first.portfolio.net, Some(BigDecimal("19.8")))
      val next = worker.view
      assertEquals(next.position, Some(BigDecimal(1)))
      assertEquals(next.portfolio.tokenValue, Some(BigDecimal(110)))
      assertEquals(next.portfolio.stables, Some(BigDecimal("909.69")))
      assertEquals(next.portfolio.net, Some(BigDecimal("19.69")))
    finally worker.close()
  }

  test("current wallet audit fields survive loading, another fill and reload") {
    temporary { directory =>
      LocalFiles.write(
        directory.resolve("wallet-mainnet.json"),
        """{"cash":"799.8","deposit":"1000","realized":"12","fees":"0.2","positions":{"XMR":{"quantity":"2","entry":"100"}},"orders":["previous fill"],"count":1}"""
      )
      val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      val expected = PaperAccount(
        cash = BigDecimal("799.8"),
        deposit = BigDecimal(1000),
        realized = BigDecimal(12),
        fees = BigDecimal("0.2"),
        positions = Map("XMR" -> PaperPosition(BigDecimal(2), BigDecimal(100))),
        orders = Vector("previous fill"),
        count = 1L
      )
      val filled = try
        assertEquals(ledger.account, expected)
        assert(ledger.fill(plan(Side.Buy, BigDecimal(1), BigDecimal(100)), now).isRight)
        ledger.account
      finally ledger.close()
      val reopened = PaperLedger.open(directory, "mainnet", BigDecimal(50000), fee)
      try
        assertEquals(reopened.account, filled)
        assertEquals(reopened.account.cash, BigDecimal("699.7"))
        assertEquals(reopened.account.positions("XMR").quantity, BigDecimal(3))
        assertEquals(reopened.account.orders.head, "previous fill")
        assertEquals(reopened.account.count, 2L)
      finally reopened.close()
    }
  }
