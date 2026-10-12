package desk

class PropMarginSpec extends munit.FunSuite:
  private def d(text: String): BigDecimal = BigDecimal(text)
  private def held(cash: String, quantity: String, entry: String, coin: String = "XMR") =
    PropAccount(
      d(cash),
      positions = Map(coin -> PropPosition(d(quantity), d(entry))),
      deposit = d(cash)
    )
  private def order(side: Side, price: String, quantity: String, reduceOnly: Boolean = false) =
    PropOrder("0x" + "1" * 32, "XMR", side, d(price), d(quantity), reduceOnly, 0L)
  private val flat = PropAccount(d("10"), deposit = d("10"))

  test("maintenance is half the initial margin at the market's maximum"):
    assertEquals(PropMargin.maintenance(MarketRules(0, 3, 5)), d("0.1"))
    assertEquals(PropMargin.maintenance(MarketRules(0, 5, 40)), d("0.0125"))
    assertEquals(
      PropMargin.maintenance(MarketRules(0, 2, 3)),
      d("0.1666666666666666666666666666666667")
    )
    intercept[IllegalArgumentException](PropMargin.maintenance(MarketRules(0, 3, 0)))

  test("equity, used and free value the position at the given price"):
    val long = held("140", "1", "500")
    assertEquals(PropMargin.equity(long, d("400")), d("40"))
    assertEquals(PropMargin.used(long, d("400")), d("80"))
    assertEquals(PropMargin.free(long, d("400")), d("-40"))
    assertEquals(PropMargin.equity(long, d("600")), d("240"))
    assertEquals(PropMargin.used(long, d("600")), d("120"))
    assertEquals(PropMargin.free(long, d("600")), d("120"))
    val short = held("160", "-1", "500")
    assertEquals(PropMargin.equity(short, d("600")), d("60"))
    assertEquals(PropMargin.used(short, d("600")), d("120"))
    assertEquals(PropMargin.free(short, d("600")), d("-60"))
    for price <- Vector("0.5", "400", "600", "100000") do
      assertEquals(PropMargin.equity(flat, d(price)), d("10"))
      assertEquals(PropMargin.used(flat, d(price)), d("0"))
      assertEquals(PropMargin.free(flat, d(price)), d("10"))

  test("an opening order reserves margin and maker fee, a reduce-only order nothing"):
    assertEquals(PropMargin.reserve(order(Side.Buy, "245", "0.2")), d("9.80735"))
    assertEquals(PropMargin.reserve(order(Side.Sell, "600", "0.1")), d("12.009"))
    assertEquals(PropMargin.reserve(order(Side.Sell, "600", "0.1", reduceOnly = true)), d("0"))

  test("free subtracts every reserve"):
    val one =
      PropAccount(d("10"), deposit = d("10"), resting = Vector(order(Side.Buy, "245", "0.2")))
    assertEquals(PropMargin.free(one, d("246")), d("0.19265"))
    // 100 - 9.80735 - 12.009 - 0: every resting order counts, a reduce-only one holds nothing.
    val several = PropAccount(
      d("100"),
      deposit = d("100"),
      resting = Vector(
        order(Side.Buy, "245", "0.2"),
        order(Side.Sell, "600", "0.1"),
        order(Side.Sell, "600", "0.1", reduceOnly = true)
      )
    )
    assertEquals(PropMargin.free(several, d("246")), d("78.18365"))

  test("liquidation prices for a long, a short and a covered long"):
    assertEquals(
      PropMargin.liquidationPrice(held("140", "1", "500"), "XMR", d("0.1")),
      Some(d("400"))
    )
    assertEquals(
      PropMargin.liquidationPrice(held("160", "-1", "500"), "XMR", d("0.1")),
      Some(d("600"))
    )
    assertEquals(PropMargin.liquidationPrice(held("500", "1", "500"), "XMR", d("0.1")), None)
    assertEquals(PropMargin.liquidationPrice(held("600", "1", "500"), "XMR", d("0.1")), None)
    val btc = held("210", "0.01", "100000", "BTC")
    assertEquals(PropMargin.liquidationPrice(btc, "BTC", d("0.0125")), Some(d("80000")))
    assertEquals(PropMargin.liquidationPrice(flat, "XMR", d("0.1")), None)
    assertEquals(PropMargin.liquidationPrice(btc, "XMR", d("0.1")), None)
    // (100000 + 215 / 0.01) / 1.0125: a short whose size is not one.
    val shortBtc = held("215", "-0.01", "100000", "BTC")
    assertEquals(PropMargin.liquidationPrice(shortBtc, "BTC", d("0.0125")), Some(d("120000")))
    // 100 / 3 and the outer division do not end, so both round to 34 digits half even.
    assertEquals(
      PropMargin.liquidationPrice(held("100", "3", "500"), "XMR", d("0.1")),
      Some(d("518.5185185185185185185185185185185"))
    )
    assertEquals(
      PropMargin.liquidationPrice(held("100", "-3", "500"), "XMR", d("0.1")),
      Some(d("484.8484848484848484848484848484848"))
    )

  test("the equity test flips exactly at the liquidation price"):
    val long = held("140", "1", "500")
    assert(PropMargin.liquidated(long, "XMR", d("400"), d("0.1")))
    assert(!PropMargin.liquidated(long, "XMR", d("400.01"), d("0.1")))
    val short = held("160", "-1", "500")
    assert(PropMargin.liquidated(short, "XMR", d("600"), d("0.1")))
    assert(!PropMargin.liquidated(short, "XMR", d("599.99"), d("0.1")))
    val btc = held("210", "0.01", "100000", "BTC")
    assert(PropMargin.liquidated(btc, "BTC", d("80000"), d("0.0125")))
    assert(!PropMargin.liquidated(btc, "BTC", d("80000.1"), d("0.0125")))
    val shortBtc = held("215", "-0.01", "100000", "BTC")
    assert(PropMargin.liquidated(shortBtc, "BTC", d("120000"), d("0.0125")))
    assert(!PropMargin.liquidated(shortBtc, "BTC", d("119999.9"), d("0.0125")))
    // No position on the coin asked for is never liquidated, even where a held one would be.
    assert(!PropMargin.liquidated(flat, "XMR", d("400"), d("0.1")))
    assert(!PropMargin.liquidated(held("140", "1", "500", "BTC"), "XMR", d("400"), d("0.1")))

  test("funding is quantity times oracle times rate, signed"):
    assertEquals(PropMargin.payment(d("2"), d("500"), d("0.0000125")), d("0.0125"))
    assertEquals(PropMargin.payment(d("-2"), d("500"), d("0.0000125")), d("-0.0125"))
    assertEquals(PropMargin.payment(d("2"), d("500"), d("-0.0000125")), d("-0.0125"))

  test("maintenance accepts a maximum of one and refuses a negative maximum"):
    assertEquals(PropMargin.maintenance(MarketRules(0, 3, 1)), d("0.5"))
    intercept[IllegalArgumentException](PropMargin.maintenance(MarketRules(0, 3, -1)))

  test("a short pays at a negative rate"):
    assertEquals(PropMargin.payment(d("-2"), d("500"), d("-0.0000125")), d("0.0125"))

  test("prop trades at 5x cross"):
    assertEquals(PropMargin.leverage, 5)
    assertEquals(PropMargin.mode, "cross")

  test("products and sums stay exact past 34 digits"):
    // A weighted entry the ledger rounds to 34 digits, held at 0.183.
    val account = held("1000", "0.183", "518.5185185185185185185185185185185")
    assertEquals(PropMargin.equity(account, d("600")), d("1014.9111111111111111111111111111111145"))
    assertEquals(PropMargin.free(account, d("600")), d("992.9511111111111111111111111111111145"))
