package desk

class TradingSpec extends munit.FunSuite:
  private def d(value: String): BigDecimal = BigDecimal(value)
  private val rules = MarketRules(7, 3, 5)
  private val sizes = TradeSizes()
  private def level(price: String, size: String): Level = Level(price, size, 1)
  private def book(
      bids: Vector[(String, String)] = Vector("99.99" -> "20"),
      asks: Vector[(String, String)] = Vector("100" -> "20")
  ): Book = Book(bids.map(level), asks.map(level), 1L)
  private def result(
      intent: OrderIntent,
      depth: Book = book(),
      reference: String = "100",
      position: String = "0",
      amounts: TradeSizes = sizes,
      market: MarketRules = rules
  ): Either[String, OrderPlan] =
    Trading.plan("XMR", market, depth, d(reference), d(position), intent, amounts)
  private def accepted(value: Either[String, OrderPlan]): OrderPlan =
    value.fold(message => fail(message), identity)
  private val sellBook = book(
    Vector("100" -> "20"),
    Vector("100.01" -> "20")
  )

  test("the default dollar sizes are small 500, big 1000 and target 1000") {
    assertEquals(sizes, TradeSizes(d("500"), d("1000"), d("1000")))
  }

  test("eight actions keep their side, kind, requested size and asset") {
    for
      side <- Vector(Side.Buy, Side.Sell)
      kind <- Vector(OrderKind.Hard, OrderKind.Best)
      big <- Vector(false, true)
    do
      val p = accepted(
        result(OrderIntent(side, kind, big), if side == Side.Buy then book() else sellBook)
      )
      assertEquals(p.coin, "XMR")
      assertEquals(p.asset, 7)
      assertEquals(p.side, side)
      assertEquals(p.kind, kind)
      val ceiling = if big then d("1000") else d("500")
      if kind == OrderKind.Hard then assertEquals(p.quantity, ceiling / d("100"))
      else assert(p.quantity * p.reference < ceiling)
      assert(p.quantity > 0)
  }

  test("Hard buy and sell limits include an exactly representable boundary") {
    val buy = accepted(result(OrderIntent(Side.Buy, OrderKind.Hard)))
    val sell = accepted(result(OrderIntent(Side.Sell, OrderKind.Hard), sellBook))
    assertEquals(buy.limit, d("100.1"))
    assertEquals(sell.limit, d("99.9"))
    assertEquals(buy.quantity, d("5"))
    assertEquals(sell.quantity, d("5"))
  }

  test("Best moves an exact boundary inward by the valid price step") {
    val buy = accepted(result(OrderIntent(Side.Buy, OrderKind.Best)))
    val sell = accepted(result(OrderIntent(Side.Sell, OrderKind.Best), sellBook))
    assertEquals(buy.limit, d("100.09"))
    assertEquals(sell.limit, d("99.901"))
    assert(buy.limit < buy.reference * d("1.001"))
    assert(sell.limit > sell.reference * d("0.999"))
    assertEquals(buy.quantity, d("4.995"))
    assertEquals(sell.quantity, d("4.999"))
    assert(buy.quantity * buy.limit <= sizes.small)
  }

  test("Best excludes equality while Hard estimates those same visible fills") {
    val asks = book(asks = Vector("100" -> "1", "100.1" -> "20"))
    val bids = book(Vector("100" -> "1", "99.9" -> "20"), Vector("100.01" -> "20"))
    for (side, depth) <- Vector(Side.Buy -> asks, Side.Sell -> bids) do
      val best = accepted(result(OrderIntent(side, OrderKind.Best), depth))
      val hard = accepted(result(OrderIntent(side, OrderKind.Hard), depth))
      assertEquals(best.quantity, d("1"))
      assertEquals(hard.estimatedQuantity, d("5"))
      assertEquals(hard.estimatedValue, if side == Side.Buy then d("500.4") else d("499.6"))
  }

  test("Best can equal Hard coin quantity when both reference values are below the ceiling") {
    val depth = book(Vector("101" -> "20"), Vector("101.01" -> "20"))
    val hard = accepted(result(OrderIntent(Side.Sell, OrderKind.Hard), depth, "101"))
    val best = accepted(result(OrderIntent(Side.Sell, OrderKind.Best), depth, "101"))
    assertEquals(hard.quantity, d("4.950"))
    assertEquals(best.quantity, hard.quantity)
    assert(best.quantity * best.reference < sizes.small)
  }

  test("Hard keeps its full request with only a partial visible estimate") {
    val p = accepted(
      result(
        OrderIntent(Side.Sell, OrderKind.Hard),
        book(Vector("100" -> "1"), Vector("100.01" -> "20")),
        position = "3"
      )
    )
    assertEquals(p.quantity, d("5"))
    assertEquals(p.estimatedQuantity, d("1"))
    assertEquals(p.estimatedValue, d("100"))
    assertEquals(p.average, Some(d("100")))
    assertEquals(p.positionAfter, d("2"))
  }

  test("Hard can have zero visible fills without inventing an average or position change") {
    val p = accepted(
      result(
        OrderIntent(Side.Buy, OrderKind.Hard),
        book(asks = Vector("101" -> "20")),
        position = "-3"
      )
    )
    assertEquals(p.quantity, d("5"))
    assertEquals(p.estimatedQuantity, d("0"))
    assertEquals(p.estimatedValue, d("0"))
    assertEquals(p.average, None)
    assertEquals(p.positionAfter, d("-3"))
  }

  test("Best has no request without eligible visible quantity") {
    assert(result(OrderIntent(Side.Buy, OrderKind.Best), book(asks = Vector("101" -> "20"))).isLeft)
  }

  test("Hard rounds requested quantity down without clamping to an opposite position") {
    val depth = book(Vector("101" -> "20"), Vector("101.01" -> "20"))
    val p = accepted(result(OrderIntent(Side.Sell, OrderKind.Hard), depth, "101", "1"))
    assertEquals(p.quantity, d("4.950"))
    assertEquals(p.positionAfter, d("-3.950"))
    assert(p.quantity * p.reference <= sizes.small)
    assert((p.quantity + d("0.001")) * p.reference > sizes.small)
  }

  test("visible fills reduce an opposite position before opening excess") {
    for (side, depth, before, after) <- Vector(
        (Side.Buy, book(), "-7", "-2"),
        (Side.Buy, book(), "-3", "2"),
        (Side.Sell, sellBook, "7", "2"),
        (Side.Sell, sellBook, "3", "-2")
      )
    do
      val p = accepted(result(OrderIntent(side, OrderKind.Hard), depth, position = before))
      assertEquals(p.positionAfter, d(after))
  }

  test("Target sums exact visible levels and reports their weighted average") {
    val depth = book(asks = Vector("100" -> "1", "102" -> "1"))
    val p =
      accepted(result(OrderIntent(Side.Buy, OrderKind.Target, target = Some(d("102"))), depth))
    assertEquals(p.quantity, d("2"))
    assertEquals(p.estimatedQuantity, d("2"))
    assertEquals(p.estimatedValue, d("202"))
    assertEquals(p.average, Some(d("101")))
    assertEquals(p.limit, d("102"))
  }

  test("Target stops before prices outside the selected limit on either side") {
    val asks = book(asks = Vector("100" -> "1", "101" -> "2", "102" -> "3"))
    val bids = book(Vector("100" -> "1", "99" -> "2", "98" -> "3"), Vector("100.01" -> "20"))
    for (side, depth, target, value) <- Vector(
        (Side.Buy, asks, "101", "302"),
        (Side.Sell, bids, "99", "298")
      )
    do
      val p = accepted(result(OrderIntent(side, OrderKind.Target, target = Some(d(target))), depth))
      assertEquals(p.quantity, d("3"))
      assertEquals(p.estimatedValue, d(value))
  }

  test("a Target beyond exact coverage rejects an unknown remaining quantity") {
    val asks = book(asks = Vector("100" -> "1", "102" -> "1"))
    val bids = book(Vector("100" -> "1", "98" -> "1"), Vector("100.01" -> "20"))
    assert(result(OrderIntent(Side.Buy, OrderKind.Target, target = Some(d("103"))), asks).isLeft)
    assert(result(OrderIntent(Side.Sell, OrderKind.Target, target = Some(d("97"))), bids).isLeft)
  }

  test("a Target beyond coverage is allowed when the visible prefix fills its capped quantity") {
    val depth = book(asks = Vector("100" -> "1", "102" -> "1"))
    val p = accepted(
      result(
        OrderIntent(Side.Buy, OrderKind.Target, target = Some(d("103"))),
        depth,
        amounts = sizes.copy(target = d("150"))
      )
    )
    assertEquals(p.quantity, d("1.456"))
    assertEquals(p.estimatedValue, d("146.512"))
    assert(p.quantity * p.limit <= d("150"))
    assert((p.quantity + d("0.001")) * p.limit > d("150"))
    assertEquals(p.estimatedQuantity, p.quantity)
  }

  test("a Target accepts coverage exactly equal to its capped quantity") {
    val p = accepted(
      result(
        OrderIntent(Side.Sell, OrderKind.Target, target = Some(d("97"))),
        book(Vector("100" -> "1", "98" -> "0.5"), Vector("100.01" -> "20")),
        amounts = sizes.copy(target = d("150"))
      )
    )
    assertEquals(p.quantity, d("1.5"))
    assertEquals(p.estimatedValue, d("149"))
  }

  test("better sell proceeds can exceed the requested reference dollar ceiling") {
    val depth = book(Vector("101" -> "20"), Vector("101.01" -> "20"))
    val p = accepted(result(OrderIntent(Side.Sell, OrderKind.Best), depth))
    assertEquals(p.quantity, d("4.999"))
    assert(p.quantity * p.reference < sizes.small)
    assert(p.estimatedValue > sizes.small)
  }

  test("prices keep the five-figure rule and the integer exception") {
    assert(Trading.validPrice(d("99.999"), rules))
    assert(!Trading.validPrice(d("100.001"), rules))
    assert(Trading.validPrice(d("123456"), rules))
    assert(!Trading.validPrice(d("0.0001"), rules))
    assert(!Trading.validPrice(d("0"), rules))
    assert(!Trading.validPrice(d("-1"), rules))
    assert(Trading.validPrice(d("0.000001"), rules.copy(szDecimals = 0)))
    assert(!Trading.validPrice(d("0.000001"), rules.copy(szDecimals = 1)))
    assert(Trading.validPrice(d("1"), rules.copy(szDecimals = 6)))
    assert(!Trading.validPrice(d("1.1"), rules.copy(szDecimals = 6)))
  }

  test("arrow price steps cross powers of ten in both directions") {
    assertEquals(Trading.priceStep(d("99.999"), rules), Some(d("0.001")))
    assertEquals(Trading.priceStep(d("100"), rules), Some(d("0.01")))
    assertEquals(Trading.nextPrice(d("99.999"), rules, up = true), Some(d("100")))
    assertEquals(Trading.nextPrice(d("100"), rules, up = false), Some(d("99.999")))
    assertEquals(Trading.nextPrice(d("100"), rules, up = true), Some(d("100.01")))
    assertEquals(Trading.nextPrice(d("100.01"), rules, up = false), Some(d("100")))
    assertEquals(Trading.nextPrice(d("0.001"), rules, up = false), None)
    assertEquals(Trading.nextPrice(d("100.001"), rules, up = true), None)
    assertEquals(Trading.priceStep(d("0"), rules), None)
    assertEquals(Trading.priceStep(d("100"), rules.copy(szDecimals = 7)), None)
  }

  test("an unrepresentable price boundary rounds inward on both sides") {
    val buy = accepted(
      result(
        OrderIntent(Side.Buy, OrderKind.Best),
        book(Vector("99.998" -> "20"), Vector("99.999" -> "20")),
        "99.999"
      )
    )
    val sell = accepted(
      result(
        OrderIntent(Side.Sell, OrderKind.Best),
        book(Vector("99.999" -> "20"), Vector("100" -> "20")),
        "99.999"
      )
    )
    assertEquals(buy.limit, d("100.09"))
    assertEquals(sell.limit, d("99.9"))
  }

  test("malformed, nonpositive, crossed, duplicate and unsorted levels are rejected") {
    val invalid = Vector(
      book(asks = Vector("NaN" -> "1")),
      book(asks = Vector("1e2" -> "1")),
      book(asks = Vector("0" -> "1")),
      book(asks = Vector("100" -> "0")),
      book(asks = Vector("100" -> "-1")),
      book(asks = Vector("100" -> "0.0001")),
      book(asks = Vector("100.001" -> "1")),
      book(Vector("101" -> "1"), Vector("100" -> "1")),
      book(Vector("100" -> "1"), Vector("100" -> "1")),
      book(asks = Vector("101" -> "1", "100" -> "1")),
      book(asks = Vector("100" -> "1", "100" -> "2")),
      book(Vector("99" -> "1", "99.9" -> "1")),
      book(bids = Vector.empty),
      book(asks = Vector.empty),
      book().copy(asks = Vector(Level("100", "1", 0))),
      book().copy(asks = Vector(Level(null, "1", 1)))
    )
    invalid.foreach(depth => assert(result(OrderIntent(Side.Buy, OrderKind.Hard), depth).isLeft))
  }

  test("bad market rules and amounts never produce an order") {
    val intent = OrderIntent(Side.Buy, OrderKind.Hard)
    for market <- Vector(
        rules.copy(asset = -1),
        rules.copy(szDecimals = -1),
        rules.copy(szDecimals = 7),
        rules.copy(maxLeverage = 4)
      )
    do assert(result(intent, market = market).isLeft)
    for amounts <- Vector(
        sizes.copy(small = d("0")),
        sizes.copy(big = d("499")),
        sizes.copy(target = d("-1"))
      )
    do assert(result(intent, amounts = amounts).isLeft)
    assert(result(intent, reference = "0").isLeft)
    assert(result(intent, reference = "100.001").isLeft)
    assert(result(intent, position = "0.0001").isLeft)
    assert(Trading.plan("", rules, book(), d("100"), d("0"), intent, sizes).isLeft)
  }

  test("an order below ten dollars is rejected even when it only reduces a position") {
    assert(
      result(
        OrderIntent(Side.Sell, OrderKind.Hard),
        sellBook,
        position = "0.05",
        amounts = sizes.copy(small = d("5"))
      ).isLeft
    )
    assert(
      result(
        OrderIntent(Side.Sell, OrderKind.Hard),
        sellBook,
        amounts = sizes.copy(small = d("10"))
      ).isLeft
    )
    assert(
      result(OrderIntent(Side.Buy, OrderKind.Hard), amounts = sizes.copy(small = d("10"))).isRight
    )
  }

  test("Target needs a valid selected price that reaches visible orders") {
    assert(result(OrderIntent(Side.Buy, OrderKind.Target)).isLeft)
    assert(result(OrderIntent(Side.Buy, OrderKind.Target, target = Some(d("100.001")))).isLeft)
    assert(result(OrderIntent(Side.Buy, OrderKind.Target, target = Some(d("99")))).isLeft)
    assert(result(OrderIntent(Side.Buy, OrderKind.Hard, target = Some(d("100")))).isLeft)
  }

  test("plain decimal formatting removes trailing zeroes without adding exponents") {
    assertEquals(Trading.format(d("0.00000100")), "0.000001")
    assertEquals(Trading.format(d("1000000.000")), "1000000")
    assertEquals(Trading.format(d("-1.2300")), "-1.23")
    assertEquals(Trading.format(d("0.000")), "0")
  }

  test("weighted average precision never changes exact quantity or value") {
    val p = accepted(
      result(
        OrderIntent(Side.Buy, OrderKind.Target, target = Some(d("101"))),
        book(asks = Vector("100" -> "1", "101" -> "2"))
      )
    )
    assertEquals(p.quantity, d("3"))
    assertEquals(p.estimatedValue, d("302"))
    assertEquals(Trading.format(p.average.get), "100.6666666666666666666666666666667")
  }
