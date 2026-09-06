package desk

class TradingScreenSpec extends munit.FunSuite:
  private val rules = MarketRules(0, 3, 5)
  private val ready = TradingView(
    "XMR",
    Some(BigDecimal(1)),
    "five times leverage",
    true,
    false,
    Some(rules),
    Some(BigDecimal("99.99")),
    Some(BigDecimal("100")),
    None
  )
  private val plan = OrderPlan(
    "XMR",
    0,
    Side.Buy,
    OrderKind.Target,
    BigDecimal(2),
    BigDecimal("100.1"),
    BigDecimal(100),
    BigDecimal("1.5"),
    BigDecimal(150),
    Some(BigDecimal(100)),
    BigDecimal("2.5")
  )

  test("four rows retain separate actions and the configured hard dollar sizes") {
    val rows = TradingScreen.rows(
      ready,
      None,
      _ => Right(plan),
      TradeSizes(BigDecimal("501.01"), BigDecimal(999), BigDecimal(800))
    )
    assertEquals(rows.size, 4)
    assert(rows(0).startsWith("Hard 1 buy $501.01 2 buy $999 3 sell $501.01 4 sell $999"))
    assert(rows(1).startsWith("Best 5 buy $150 6 buy $150 7 sell $150 8 sell $150"))
    assert(rows(2).contains("Target 9 buy 0 sell"))
    assert(rows(0).contains("position long 1"))
    assertEquals(rows(3), "ready | five times leverage")
  }

  test("the best row asks for the correct direction and size for every action") {
    val calls = scala.collection.mutable.ArrayBuffer.empty[OrderIntent]
    val _ = TradingScreen.rows(
      ready,
      None,
      intent => {
        calls += intent
        Right(plan)
      }
    )
    assertEquals(
      calls.toVector,
      Vector(
        OrderIntent(Side.Buy, OrderKind.Best),
        OrderIntent(Side.Buy, OrderKind.Best, big = true),
        OrderIntent(Side.Sell, OrderKind.Best),
        OrderIntent(Side.Sell, OrderKind.Best, big = true)
      )
    )
  }

  test("a preview from a changed market cannot enter the current footer") {
    val other = plan.copy(coin = "BTC")
    val rows = TradingScreen.rows(ready, Some(Side.Buy -> BigDecimal("100.1")), _ => Right(other))
    assert(rows(1).contains("5 buy --") && rows(1).contains("8 sell --"))
    assert(rows(2).contains("no estimated fill"))
    assert(!rows.exists(_.contains("average")))
    assert(!rows.exists(_.contains("target estimate")))
  }

  test("target details use estimated filled quantity and show the resulting position") {
    val rows = TradingScreen.rows(ready, Some(Side.Buy -> BigDecimal("100.1")), _ => Right(plan))
    assertEquals(
      rows(2),
      "Target 9 buy 0 sell | buy $100.1 request 2 estimate 1.5 value $150 average $100"
    )
    assert(rows(1).contains("target estimate long 2.5"))
    assert(rows(0).contains("position long 1"))
    assertEquals(rows(3), "ready | five times leverage")
    assert(rows.forall(_.length <= 100))
  }

  test("a target price retains small valid price steps instead of rounding to cents") {
    val rows =
      TradingScreen.rows(ready, Some(Side.Buy -> BigDecimal("0.012345")), _ => Left("not ready"))
    assert(rows(2).contains("buy $0.012345"))
    val filled = TradingScreen.rows(
      ready,
      Some(Side.Buy -> BigDecimal("0.012345")),
      _ => Right(plan.copy(average = Some(BigDecimal("0.012345"))))
    )
    assert(filled(2).contains("average $0.012345"))
  }

  test("unknown positions and missing estimates never appear as flat or a zero average") {
    val unknown = ready.copy(position = None, ready = false, message = "viewing only")
    val rows =
      TradingScreen.rows(unknown, Some(Side.Sell -> BigDecimal(99)), _ => Left("not ready"))
    assert(rows(0).contains("$500") && rows(0).contains("$1000"))
    assert(rows(1).contains("5 buy --") && rows(1).contains("8 sell --"))
    assertEquals(rows(2), "Target 9 buy 0 sell | sell $99 no estimated fill")
    assert(rows(0).contains("position unavailable"))
    assertEquals(rows(3), "unavailable | viewing only")
    assert(!rows.exists(_.contains("flat")))
    assert(!rows.exists(_.contains("average $0")))
    val empty =
      plan.copy(estimatedQuantity = BigDecimal(0), estimatedValue = BigDecimal(0), average = None)
    val noFill = TradingScreen.rows(ready, Some(Side.Buy -> BigDecimal(100)), _ => Right(empty))
    assert(noFill(2).endsWith("request 2 estimate 0"))
    assert(!noFill(2).contains("average"))
    assert(!noFill(1).contains("target estimate"))
  }

  test("a successful public estimate cannot turn an unknown position into an after position") {
    val unknown = ready.copy(position = None, ready = false, message = "viewing only")
    for selected <- Vector(None, Some(Side.Buy -> BigDecimal("100.1"))) do
      val rows = TradingScreen.rows(
        unknown.copy(lastPlan = Some(plan), pending = true),
        selected,
        _ => Right(plan)
      )
      assert(rows(2).contains("request 2 estimate 1.5"))
      assert(rows(2).contains("value $150 average $100"))
      assert(rows(0).contains("position unavailable"))
      assert(!rows(1).contains("press estimate") && !rows(1).contains("target estimate"))
      assert(!rows.exists(_.contains("flat")))
  }

  test("hard and best pressed plans remain visible during a pending reply") {
    for (kind, label) <- Vector(OrderKind.Hard -> "Hard", OrderKind.Best -> "Best") do
      val pressed = plan.copy(kind = kind, side = Side.Sell, positionAfter = BigDecimal("-0.5"))
      val rows = TradingScreen.rows(
        ready.copy(pending = true, lastPlan = Some(pressed), message = "XMR submitting"),
        None,
        _ => Left("pending")
      )
      assert(rows(0).contains("1 buy") && rows(0).contains("4 sell"))
      assert(rows(1).contains("5 buy") && rows(1).contains("8 sell"))
      assert(rows(2).contains(label + " sell request 2 estimate 1.5 value $150 average $100"))
      assert(rows(2).contains("Target 9 buy 0 sell"))
      assertEquals(rows(3), "pending | XMR submitting")
      assert(rows(0).contains("position long 1"))
      assert(rows(1).contains("press estimate short 0.5"))
      assert(rows.forall(_.length <= 100))
  }

  test("a selected target supersedes the last press and never borrows its failed estimate") {
    val last = ready.copy(lastPlan = Some(plan.copy(kind = OrderKind.Hard)))
    val rows = TradingScreen.rows(last, Some(Side.Sell -> BigDecimal(99)), _ => Left("not ready"))
    assert(rows(2).contains("sell $99 no estimated fill"))
    assert(!rows(2).contains("value"))
    assert(!rows(1).contains("target estimate"))
    val changed =
      TradingScreen.rows(last.copy(coin = "BTC", pending = true), None, _ => Left("not ready"))
    assert(!changed(2).contains("request"))
  }

  test("pending and short positions remain distinct from ready and flat") {
    val rows = TradingScreen.rows(
      ready.copy(position = Some(BigDecimal("-0.25")), pending = true),
      None,
      _ => Left("pending")
    )
    assert(rows.head.contains("position short 0.25") && rows.last.startsWith("pending"))
    val flat =
      TradingScreen.rows(ready.copy(position = Some(BigDecimal(0))), None, _ => Left("none"))
    assert(flat.head.contains("position flat"))
  }

  test("maximum configured dollar values leave every action label visible at the minimum width") {
    val maximum = BigDecimal("1000000000")
    val rows = TradingScreen.rows(
      ready,
      None,
      _ => Right(plan.copy(estimatedValue = maximum)),
      TradeSizes(maximum, maximum, maximum)
    )
    assert(rows.take(2).forall(_.length <= 100), rows.take(2))
    assert(rows.head.contains("4 sell") && rows(1).contains("8 sell"))
  }

  test("maximum sizes and the longest trading market retain a valid small-price target") {
    val coin = "X" * 20
    val tiny = BigDecimal("0.000001")
    val maximum = BigDecimal("1000000000")
    val sizes = TradeSizes(maximum, maximum, maximum)
    val market = MarketRules(0, 0, 5)
    val book = Book(
      Vector(Level("0.000001", "1000000000000000", 1)),
      Vector(Level("0.000002", "1000000000000000", 1)),
      1000L
    )
    val current = BigDecimal("999999999999999")
    val view = ready.copy(
      coin = coin,
      position = Some(current),
      rules = Some(market),
      bid = Some(tiny),
      ask = Some(tiny * 2),
      message = coin + " order rejected"
    )
    val preview: OrderIntent => Either[String, OrderPlan] = intent =>
      Trading.plan(coin, market, book, view.reference(intent.side).get, current, intent, sizes)
    val target = OrderIntent(Side.Sell, OrderKind.Target, target = Some(tiny))
    assert(preview(target).isRight)
    val rows = TradingScreen.rows(view, Some(Side.Sell -> tiny), preview, sizes)
    assert(rows.forall(_.length <= 100))
    assert(rows(0).contains("1 buy") && rows(0).contains("4 sell"))
    assert(rows(1).contains("5 buy") && rows(1).contains("8 sell"))
    assert(rows(2).contains("Target 9 buy 0 sell | sell $0.000001"))
    assert(rows(2).contains("request ~"))
    assert(rows(2).contains("estimate ~"))
    assert(rows(2).contains("value $~"))
    assert(rows(2).endsWith("average $0.000001"), rows(2))
    assert(rows(0).contains("position long ~"))
    assert(rows(1).contains("target estimate short 1"))
    assertEquals(rows(3), "ready | " + coin + " order rejected")
  }

  test("resolved outcomes retain their own row beside a large precise confirmed position") {
    val current = BigDecimal("123456789.123")
    assert(current.scale <= rules.szDecimals)
    val captured = plan.copy(kind = OrderKind.Hard, positionAfter = BigDecimal("2.5"))
    for outcome <- Vector("XMR order rejected", "XMR filled 0.375 at 100") do
      val rows = TradingScreen.rows(
        ready.copy(position = Some(current), lastPlan = Some(captured), message = outcome),
        None,
        _ => Right(plan)
      )
      assert(rows.forall(_.length <= 100))
      assert(rows(0).contains("position long ~1.234e8"))
      assert(rows(0).contains("1 buy") && rows(0).contains("4 sell"))
      assert(rows(1).contains("5 buy") && rows(1).contains("8 sell"))
      assert(!rows(1).contains("press estimate"))
      assert(!rows(2).contains("request") && !rows(2).contains("average"))
      assertEquals(rows(3), "ready | " + outcome)
  }

  test("a pending press estimate stays labelled when the confirmed position changes") {
    val rows = TradingScreen.rows(
      ready.copy(
        position = Some(BigDecimal(40)),
        pending = true,
        message = "XMR waiting for order result",
        lastPlan = Some(plan.copy(kind = OrderKind.Best))
      ),
      None,
      _ => Left("pending")
    )
    assert(rows(0).contains("position long 40"))
    assert(rows(1).contains("press estimate long 2.5"))
    assert(!rows.exists(_.contains("after")))
    assertEquals(rows(3), "pending | XMR waiting for order result")
  }

  test("an outcome remains visible while a new target has a full preview") {
    val outcome = "XMR filled 0.375 at 100"
    val rows = TradingScreen.rows(
      ready.copy(
        message = outcome,
        position = Some(BigDecimal("1.375")),
        lastPlan = Some(plan.copy(kind = OrderKind.Hard))
      ),
      Some(Side.Buy -> BigDecimal("100.1")),
      _ => Right(plan.copy(positionAfter = BigDecimal("2.875")))
    )
    assert(rows(0).contains("position long 1.375"))
    assert(rows(1).contains("target estimate long 2.875"))
    assert(rows(2).contains("request 2 estimate 1.5 value $150 average $100"))
    assertEquals(rows(3), "ready | " + outcome)
  }
