package desk

import java.nio.file.Files

class PropSpec extends TempDirectorySuite:
  private def plan(side: Side, quantity: String, price: String): OrderPlan =
    val q = BigDecimal(quantity)
    val p = BigDecimal(price)
    OrderPlan("XMR", 0, side, OrderKind.Hard, q, p, p, q, q * p, Some(p), BigDecimal(0), false)

  test("prop fills charge fees and persist cash, average entry and realized outcome") {
    val directory = tempDirectory("desk-prop-test")
    val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      assert(ledger.fill(plan(Side.Buy, "2", "100"), 1000L).isRight)
      assertEquals(ledger.account.cash, BigDecimal("799.8"))
      assertEquals(ledger.account.positions("XMR").quantity, BigDecimal(2))
      assert(ledger.fill(plan(Side.Sell, "1", "110"), 2000L).isRight)
      assertEquals(ledger.account.cash, BigDecimal("909.69"))
      assertEquals(ledger.account.realized, BigDecimal(10))
      assertEquals(ledger.account.positions("XMR").quantity, BigDecimal(1))
      assertEquals(ledger.account.positions("XMR").entry, BigDecimal(100))
    finally ledger.close()
    val reopened = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      assertEquals(reopened.account.cash, BigDecimal("909.69"))
      assertEquals(reopened.account.orders.size, 2)
    finally reopened.close()
  }

  test("a prop market fill names the money it put up at the fill price, as a live fill does") {
    val ledger = PropLedger.open(
      tempDirectory("desk-prop-test"),
      "mainnet",
      BigDecimal(1000),
      BigDecimal("0.001")
    )
    def first(text: Either[String, String]): String = text.toOption.get.takeWhile(_ != '\n')
    try
      assertEquals(
        first(ledger.fill(plan(Side.Buy, "0.183", "546.09"), 1000L)),
        "PROP XMR buy filled $99.93 at 546.09"
      )
      // Half a cent rounds up: 0.5 at 40.01 is 20.005.
      assertEquals(
        first(ledger.fill(plan(Side.Buy, "0.5", "40.01"), 2000L)),
        "PROP XMR buy filled $20.01 at 40.01"
      )
      assertEquals(
        first(ledger.fill(plan(Side.Sell, "0.1", "550"), 3000L)),
        "PROP XMR sell filled $55.00 at 550"
      )
      val part = plan(Side.Buy, "3", "100")
        .copy(estimatedQuantity = BigDecimal("1.5"), estimatedValue = BigDecimal(150))
      assertEquals(first(ledger.fill(part, 4000L)), "PROP XMR buy partial $150.00 at 100")
      // The log keeps the same text, the fee and the count after it, and no quantity.
      assert(
        ledger.account.orders.last
          .startsWith("PROP XMR buy partial $150.00 at 100\nFee $0.15 | #4 "),
        ledger.account.orders.last
      )
      assert(!ledger.account.orders.exists(_.contains(" 0.183 ")), ledger.account.orders)
    finally ledger.close()
  }

  test("a prop fill of a thousand dollars or more names them in the live compact form") {
    def first(p: OrderPlan): String =
      PropLedger.fillText(p, BigDecimal(1), 1L, 0L).takeWhile(_ != '\n')
    assertEquals(first(plan(Side.Buy, "2", "1000")), "PROP XMR buy filled $2000 at 1000")
    assertEquals(first(plan(Side.Buy, "1", "1234.56")), "PROP XMR buy filled $~1234 at 1234.56")
  }

  test("partial fills use actual depth quantity and reject excess exposure without mutation") {
    val ledger = PropLedger.open(
      tempDirectory("desk-prop-test"),
      "testnet",
      BigDecimal(1000),
      BigDecimal("0.001")
    )
    try
      val requested = plan(Side.Buy, "3", "100")
        .copy(estimatedQuantity = BigDecimal("1.5"), estimatedValue = BigDecimal(150))
      assert(ledger.fill(requested, 1000L).isRight)
      assertEquals(ledger.account.positions("XMR").quantity, BigDecimal("1.5"))
      assert(ledger.account.orders.last.contains("partial"))
      val before = ledger.account
      assert(ledger.fill(plan(Side.Buy, "30", "100"), 2000L).isLeft)
      assertEquals(ledger.account, before)
      assert(
        ledger
          .fill(
            requested.copy(
              estimatedQuantity = BigDecimal(0),
              estimatedValue = BigDecimal(0),
              average = None
            ),
            2000L
          )
          .isLeft
      )
      assertEquals(ledger.account, before)
    finally ledger.close()
  }

  test("prop account refuses a second writer and corruption never resets a balance") {
    val directory = tempDirectory("desk-prop-test")
    val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      intercept[Exception](
        PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
      )
      assert(ledger.fill(plan(Side.Buy, "1", "100"), 1000L).isRight)
    finally ledger.close()
    Files.writeString(directory.resolve("wallet-mainnet.json"), "broken")
    intercept[Exception](
      PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    )
    assertEquals(Files.readString(directory.resolve("wallet-mainnet.json")), "broken")
  }
