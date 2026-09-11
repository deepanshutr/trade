package desk

import java.nio.file.Files

class PaperSpec extends munit.FunSuite:
  private def plan(side: Side, quantity: String, price: String): OrderPlan =
    val q = BigDecimal(quantity)
    val p = BigDecimal(price)
    OrderPlan("XMR", 0, side, OrderKind.Hard, q, p, p, q, q * p, Some(p), BigDecimal(0))

  test("paper fills charge fees and persist cash, average entry and realized outcome") {
    val directory = Files.createTempDirectory("desk-paper-test")
    val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      assert(ledger.fill(plan(Side.Buy, "2", "100"), 1000L).isRight)
      assertEquals(ledger.account.cash, BigDecimal("999.8"))
      assertEquals(ledger.account.positions("XMR").quantity, BigDecimal(2))
      assert(ledger.fill(plan(Side.Sell, "3", "110"), 2000L).isRight)
      assertEquals(ledger.account.cash, BigDecimal("1019.47"))
      assertEquals(ledger.account.realized, BigDecimal(20))
      assertEquals(ledger.account.positions("XMR").quantity, BigDecimal(-1))
      assertEquals(ledger.account.positions("XMR").entry, BigDecimal(110))
    finally ledger.close()
    val reopened = PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      assertEquals(reopened.account.cash, BigDecimal("1019.47"))
      assertEquals(reopened.account.orders.size, 2)
    finally reopened.close()
  }

  test("partial fills use actual depth quantity and reject excess exposure without mutation") {
    val ledger = PaperLedger.open(
      Files.createTempDirectory("desk-paper-test"),
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

  test("paper account refuses a second writer and corruption never resets a balance") {
    val directory = Files.createTempDirectory("desk-paper-test")
    val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      intercept[Exception](
        PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
      )
      assert(ledger.fill(plan(Side.Buy, "1", "100"), 1000L).isRight)
    finally ledger.close()
    Files.writeString(directory.resolve("paper-mainnet.json"), "broken")
    intercept[Exception](
      PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    )
    assertEquals(Files.readString(directory.resolve("paper-mainnet.json")), "broken")
  }
