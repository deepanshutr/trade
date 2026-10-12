package desk

import java.nio.file.{Files, Path}

class PropLedgerMarginSpec extends TempDirectorySuite:
  private val at = 1800000000000L
  private val fee = BigDecimal("0.00045")
  private val exceeds = "Prop rejected: reduce exceeds the position"
  private def d(text: String): BigDecimal = BigDecimal(text)
  private def id(n: Int): String = "0x%032x".format(n)
  private def xmr(quantity: String, entry: String): Map[String, PropPosition] =
    Map("XMR" -> PropPosition(d(quantity), d(entry)))

  // A market plan whose limit is wide enough to pass the reviewed limit.
  private def plan(
      side: Side,
      quantity: String,
      price: String,
      held: String = "0",
      reduceOnly: Boolean = false
  ): OrderPlan =
    val q = d(quantity)
    val p = d(price)
    val limit = if side == Side.Buy then p * 2 else p / 2
    val after = if side == Side.Buy then d(held) + q else d(held) - q
    OrderPlan("XMR", 0, side, OrderKind.Hard, q, limit, p, q, q * p, Some(p), after, reduceOnly)

  private def opened(balance: String, directory: Path = tempDirectory("prop-margin-")) =
    PropLedger.open(directory, "mainnet", d(balance), fee)
  private def memory(account: PropAccount, rate: BigDecimal = fee): PropLedger =
    new PropLedger(account, rate, _ => (), new AutoCloseable { def close(): Unit = () })
  private def filled(ledger: PropLedger, plan: OrderPlan): String =
    ledger.fill(plan, at).fold(reason => fail(reason), identity)
  private def position(ledger: PropLedger): Option[PropPosition] =
    ledger.account.positions.get("XMR")
  private def balanced(account: PropAccount): Unit =
    assertEquals(account.cash, account.deposit + account.realized - account.fees)
  private def withLedger[A](ledger: PropLedger)(run: PropLedger => A): A =
    try run(ledger)
    finally ledger.close()

  test("a long at 5x spends only its fee") {
    withLedger(opened("1000")) { ledger =>
      val text = filled(ledger, plan(Side.Buy, "1", "500"))
      assertEquals(ledger.account.cash, d("999.775"))
      assertEquals(ledger.account.fees, d("0.225"))
      assertEquals(ledger.account.realized, d("0"))
      assertEquals(position(ledger), Some(PropPosition(d("1"), d("500"))))
      val lines = text.split("\n").toVector
      assertEquals(lines.head, "PROP XMR buy filled $100.00 at 500")
      assert(lines(1).startsWith("Fee $0.225 | #1 "), lines(1))
    }
  }

  test("a short from flat opens a negative position") {
    withLedger(opened("1000")) { ledger =>
      val text = filled(ledger, plan(Side.Sell, "1", "500"))
      assertEquals(ledger.account.cash, d("999.775"))
      assertEquals(position(ledger), Some(PropPosition(d("-1"), d("500"))))
      assertEquals(text.takeWhile(_ != '\n'), "PROP XMR sell filled $100.00 at 500")
    }
  }

  test("a reduce realizes with the position's sign") {
    withLedger(opened("1000")) { ledger =>
      filled(ledger, plan(Side.Buy, "1", "500"))
      filled(ledger, plan(Side.Sell, "0.4", "520", held = "1", reduceOnly = true))
      assertEquals(ledger.account.realized, d("8"))
      assertEquals(ledger.account.fees, d("0.225") + d("0.0936"))
      assertEquals(ledger.account.cash, d("1007.6814"))
      assertEquals(position(ledger), Some(PropPosition(d("0.6"), d("500"))))
    }
    withLedger(opened("1000")) { ledger =>
      filled(ledger, plan(Side.Sell, "1", "500"))
      filled(ledger, plan(Side.Buy, "0.4", "480", held = "-1", reduceOnly = true))
      assertEquals(ledger.account.realized, d("8"))
      assertEquals(ledger.account.fees, d("0.225") + d("0.0864"))
      assertEquals(ledger.account.cash, d("1007.6886"))
      assertEquals(position(ledger), Some(PropPosition(d("-0.6"), d("500"))))
    }
  }

  test("an opening fill needs free collateral at its price") {
    withLedger(opened("100")) { ledger =>
      assertEquals(ledger.fill(plan(Side.Buy, "1", "500"), at), Left(PropLedger.noStables))
      assertEquals(ledger.account, PropAccount(d("100"), deposit = d("100")))
      filled(ledger, plan(Side.Buy, "0.99", "500"))
      assertEquals(ledger.account.cash, d("99.77725"))
      assertEquals(ledger.account.count, 1L)
    }
  }

  test("open profit adds buying power at the fill price") {
    withLedger(opened("100.225")) { ledger =>
      filled(ledger, plan(Side.Buy, "1", "500"))
      assertEquals(ledger.account.cash, d("100"))
      // Free is 100 + 200 - 140 = 160 at 700 against 140 + 0.315.
      filled(ledger, plan(Side.Buy, "1", "700", held = "1"))
      assertEquals(ledger.account.cash, d("99.685"))
      assertEquals(position(ledger), Some(PropPosition(d("2"), d("600"))))
      // Free is 99.685 + 200 - 280 = 19.685 at 700.
      val before = ledger.account
      assertEquals(
        ledger.fill(plan(Side.Buy, "1", "700", held = "2"), at),
        Left(PropLedger.noStables)
      )
      assertEquals(ledger.account, before)
    }
  }

  test("a reduce that would leave cash below zero is refused") {
    val account = PropAccount(d("10"), deposit = d("10"), positions = xmr("1", "500"))
    withLedger(memory(account)) { ledger =>
      assertEquals(
        ledger.fill(plan(Side.Sell, "1", "480", held = "1", reduceOnly = true), at),
        Left(PropLedger.noStables)
      )
      assertEquals(ledger.account, account)
    }
  }

  test("a reduce-only plan larger than the position is refused") {
    val account = PropAccount(d("1000"), deposit = d("1000"), positions = xmr("1", "500"))
    withLedger(memory(account)) { ledger =>
      assertEquals(
        ledger.fill(plan(Side.Sell, "2", "500", held = "1", reduceOnly = true), at),
        Left(exceeds)
      )
      assertEquals(ledger.account, account)
    }
    // A reduce-only buy on a flat account has nothing to reduce, so it never opens a long.
    val flat = PropAccount(d("1000"), deposit = d("1000"))
    withLedger(memory(flat)) { ledger =>
      assertEquals(ledger.fill(plan(Side.Buy, "1", "500", reduceOnly = true), at), Left(exceeds))
      assertEquals(ledger.account, flat)
    }
  }

  // Each plan with whether it fills. At 100 a buy of 1 at 500 needs 100 margin and a 0.225 fee.
  // At 100.225 the third buy needs 140.315 at 700, where 299.685 equity less 280 leaves 19.685.
  test("cash equals deposit plus realized less fees after every save") {
    val runs = Vector(
      "1000" -> Vector(plan(Side.Buy, "1", "500") -> true),
      "1000" -> Vector(plan(Side.Sell, "1", "500") -> true),
      "1000" -> Vector(
        plan(Side.Buy, "1", "500") -> true,
        plan(Side.Sell, "0.4", "520", held = "1", reduceOnly = true) -> true
      ),
      "1000" -> Vector(
        plan(Side.Sell, "1", "500") -> true,
        plan(Side.Buy, "0.4", "480", held = "-1", reduceOnly = true) -> true
      ),
      "100" -> Vector(plan(Side.Buy, "1", "500") -> false, plan(Side.Buy, "0.99", "500") -> true),
      "100.225" -> Vector(
        plan(Side.Buy, "1", "500") -> true,
        plan(Side.Buy, "1", "700", held = "1") -> true,
        plan(Side.Buy, "1", "700", held = "2") -> false
      )
    )
    for (balance, plans) <- runs do
      val directory = tempDirectory("prop-margin-balanced-")
      withLedger(opened(balance, directory)) { ledger =>
        balanced(ledger.account)
        plans.foreach { (plan, fills) =>
          val result = ledger.fill(plan, at)
          if fills then assert(result.isRight, s"$balance $plan: $result")
          else assertEquals(result, Left(PropLedger.noStables), s"$balance $plan")
          balanced(ledger.account)
        }
      }
      withLedger(opened(balance, directory))(ledger => balanced(ledger.account))
  }

  test("a resting opening order reserves margin, a reduce-only one nothing") {
    withLedger(memory(PropAccount(d("10"), deposit = d("10")))) { ledger =>
      // 0.2 * 245 * (1/5 + 0.00015) = 9.80735 of the 10 free.
      val buy = PropOrder(id(1), "XMR", Side.Buy, d("245"), d("0.2"), false, at)
      assertEquals(ledger.checkResting(buy, ledger.account), Right(()))
      assert(ledger.rest(buy, 0L).isRight)
      assertEquals(PropMargin.free(ledger.account, d("245")), d("0.19265"))
      val second = buy.copy(id = id(2))
      assertEquals(ledger.checkResting(second, ledger.account), Left(PropLedger.restingNoStables))
      assertEquals(ledger.rest(second, 1L), Left(PropLedger.restingNoStables))
      // An opening sell holds the same margin as a buy.
      val sell = buy.copy(id = id(3), side = Side.Sell)
      assertEquals(ledger.rest(sell, 1L), Left(PropLedger.restingNoStables))
      assertEquals(ledger.account.resting, Vector(buy))
      // The order's own reserve is left out when it is checked again.
      assertEquals(ledger.checkResting(buy, ledger.account), Right(()))
    }
    val long = PropAccount(d("10"), deposit = d("10"), positions = xmr("1", "500"))
    withLedger(memory(long)) { ledger =>
      // Free at 600 is 10 + 100 - 120 = -10, and a reduce-only order still rests.
      val reduce = PropOrder(id(4), "XMR", Side.Sell, d("600"), d("1"), true, at)
      assert(ledger.rest(reduce, 0L).isRight)
      assertEquals(ledger.account.resting, Vector(reduce))
      val opening = reduce.copy(id = id(5), reduceOnly = false)
      assertEquals(ledger.checkResting(opening, ledger.account), Left(PropLedger.restingNoStables))
    }
  }

  test("a resting order that faces and exceeds the position flips it") {
    val sell = PropOrder(id(1), "XMR", Side.Sell, d("520"), d("3"), false, at)
    val account = PropAccount(
      d("999.775"),
      fees = d("0.225"),
      positions = xmr("1", "500"),
      deposit = d("1000"),
      resting = Vector(sell)
    )
    withLedger(memory(account)) { ledger =>
      assertEquals(ledger.resolve(sell.id, at + 1), Right(("XMR filled $312.00 at 520", d("3"))))
      assertEquals(ledger.account.realized, d("20"))
      assertEquals(ledger.account.fees, d("0.459"))
      assertEquals(ledger.account.cash, d("1019.541"))
      assertEquals(position(ledger), Some(PropPosition(d("-2"), d("520"))))
      assertEquals(ledger.account.resting, Vector.empty)
      balanced(ledger.account)
    }
  }

  test("a reduce-only buy larger than the short fills the short and cancels the rest") {
    val buy = PropOrder(id(1), "XMR", Side.Buy, d("480.00"), d("1.5"), true, at)
    val account = PropAccount(
      d("999.775"),
      fees = d("0.225"),
      positions = xmr("-1", "500"),
      deposit = d("1000"),
      resting = Vector(buy)
    )
    withLedger(memory(account)) { ledger =>
      assertEquals(
        ledger.resolve(buy.id, at + 1),
        Right(
          (
            "XMR part filled $96.00 of $144.00 at 480.00\n" +
              "XMR canceled $48.00 at 480.00 (nothing left to reduce)",
            d("1")
          )
        )
      )
      assertEquals(ledger.account.realized, d("20"))
      assertEquals(ledger.account.cash, d("1019.703"))
      assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
      assertEquals(ledger.account.resting, Vector.empty)
      balanced(ledger.account)
    }
  }

  test("a reduce-only order with nothing to reduce is cancelled whole") {
    val sell = PropOrder(id(1), "XMR", Side.Sell, d("480.00"), d("1"), true, at)
    val account = PropAccount(d("1000"), deposit = d("1000"), resting = Vector(sell))
    withLedger(memory(account)) { ledger =>
      assertEquals(
        ledger.resolve(sell.id, at + 1),
        Right(("XMR canceled $96.00 at 480.00 (nothing left to reduce)", d("0")))
      )
      assertEquals(ledger.account.cash, d("1000"))
      assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
      assertEquals(ledger.account.resting, Vector.empty)
    }
  }

  test("a reduce-only order on the position's own side is cancelled whole") {
    val buy = PropOrder(id(1), "XMR", Side.Buy, d("480.00"), d("1"), true, at)
    val long = PropAccount(
      d("1000"),
      deposit = d("1000"),
      positions = xmr("1", "500"),
      resting = Vector(buy)
    )
    withLedger(memory(long)) { ledger =>
      assertEquals(
        ledger.resolve(buy.id, at + 1),
        Right(("XMR canceled $96.00 at 480.00 (nothing left to reduce)", d("0")))
      )
      assertEquals(position(ledger), Some(PropPosition(d("1"), d("500"))))
      assertEquals(ledger.account.cash, d("1000"))
    }
  }

  test("a flip opens only on the free collateral the reduce leaves") {
    def flip(cash: String): PropLedger =
      val sell = PropOrder(id(1), "XMR", Side.Sell, d("500"), d("2"), false, at)
      memory(
        PropAccount(
          d(cash),
          deposit = d(cash),
          positions = xmr("1", "500"),
          resting = Vector(sell)
        )
      )
    // With the long closed at 500 free is the cash, against 100 of margin and 0.15 of fee.
    withLedger(flip("150")) { ledger =>
      assertEquals(ledger.resolve(id(1), at + 1).map(_._2), Right(d("2")))
      assertEquals(position(ledger), Some(PropPosition(d("-1"), d("500"))))
      assertEquals(ledger.account.cash, d("149.85"))
    }
    withLedger(flip("100.1")) { ledger =>
      assertEquals(
        ledger.resolve(id(1), at + 1),
        Right(("XMR canceled $200.00 at 500 (not enough stables)", d("0")))
      )
      assertEquals(position(ledger), Some(PropPosition(d("1"), d("500"))))
      assertEquals(ledger.account.cash, d("100.1"))
    }
  }

  test("the margin wallet is its own file") {
    assertEquals(PropLedger.file("mainnet"), "margin-mainnet.json")
    assertEquals(PropLedger.file("testnet"), "margin-testnet.json")
    val directory = tempDirectory("prop-margin-file-")
    val spot = directory.resolve("wallet-mainnet.json")
    val spotText = ujson.write(
      ujson.Obj(
        "cash" -> "5",
        "deposit" -> "5",
        "realized" -> "0",
        "fees" -> "0",
        "orders" -> ujson.Arr(),
        "count" -> "0",
        "positions" -> ujson.Obj()
      )
    )
    LocalFiles.write(spot, spotText)
    val spotBytes = Files.readAllBytes(spot)
    // A spot lock held, as an older build holds it, does not stop the margin wallet.
    val spotLock = LocalFiles.lock(directory.resolve("wallet-mainnet.lock"))
    try
      withLedger(opened("1000", directory)) { ledger =>
        assertEquals(ledger.account, PropAccount(d("1000"), deposit = d("1000")))
        filled(ledger, plan(Side.Sell, "1", "500"))
        assert(Files.exists(directory.resolve("margin-mainnet.json")))
        assert(Files.exists(directory.resolve("margin-mainnet.lock")))
        intercept[Exception](opened("1000", directory))
      }
    finally spotLock.close()
    assert(java.util.Arrays.equals(Files.readAllBytes(spot), spotBytes))
    // The short saved with its sign and loads again.
    withLedger(opened("50", directory)) { ledger =>
      assertEquals(position(ledger), Some(PropPosition(d("-1"), d("500"))))
      assertEquals(ledger.account.cash, d("999.775"))
    }
    val margin = directory.resolve("margin-mainnet.json")
    val zero = ujson.read(Files.readString(margin))
    zero("positions")("XMR")("quantity") = "0"
    val zeroText = ujson.write(zero)
    LocalFiles.write(margin, zeroText)
    intercept[IllegalArgumentException](opened("1000", directory))
    assertEquals(Files.readString(margin), zeroText)
    assert(java.util.Arrays.equals(Files.readAllBytes(spot), spotBytes))
  }

  test("a losing flip opens only on the collateral its loss leaves") {
    def flip(cash: String): PropLedger =
      val sell = PropOrder(id(1), "XMR", Side.Sell, d("450"), d("2"), false, at)
      memory(
        PropAccount(
          d(cash),
          deposit = d(cash),
          positions = xmr("1", "500"),
          resting = Vector(sell)
        )
      )
    // The reduce at 450 loses 50. The short of 1 at 450 needs 90 of margin and 0.135 of fee.
    withLedger(flip("140.135")) { ledger =>
      assertEquals(ledger.resolve(id(1), at + 1).map(_._2), Right(d("2")))
      assertEquals(position(ledger), Some(PropPosition(d("-1"), d("450"))))
      assertEquals(ledger.account.cash, d("90"))
      balanced(ledger.account)
    }
    withLedger(flip("140.1")) { ledger =>
      assertEquals(
        ledger.resolve(id(1), at + 1),
        Right(("XMR canceled $180.00 at 450 (not enough stables)", d("0")))
      )
      assertEquals(position(ledger), Some(PropPosition(d("1"), d("500"))))
      assertEquals(ledger.account.cash, d("140.1"))
    }
  }

  test("a market opening fill leaves the margin that resting orders reserve") {
    // 0.9 at 500 needs 90 of margin and 0.2025 of fee.
    val flat = PropAccount(d("100"), deposit = d("100"))
    withLedger(memory(flat))(ledger =>
      assert(ledger.fill(plan(Side.Buy, "0.9", "500"), at).isRight)
    )
    // A resting buy of 0.1 at 500 reserves 0.1 * 500 * (1/5 + 0.00015) = 10.0075 of the 100.
    val resting = PropOrder(id(1), "XMR", Side.Buy, d("500"), d("0.1"), false, at)
    val reserved = flat.copy(resting = Vector(resting))
    withLedger(memory(reserved)) { ledger =>
      assertEquals(ledger.fill(plan(Side.Buy, "0.9", "500"), at), Left(PropLedger.noStables))
      assertEquals(ledger.fill(plan(Side.Sell, "0.9", "500"), at), Left(PropLedger.noStables))
      assertEquals(ledger.account, reserved)
    }
  }

  test("a resting opening order is judged at its own price") {
    // Long 1 from 400 with cash 100. At 546 free is 100 + 146 - 109.2 = 136.8.
    val long = PropAccount(d("100"), deposit = d("100"), positions = xmr("1", "400"))
    withLedger(memory(long)) { ledger =>
      // 0.5 at 546 reserves 0.5 * 546 * 0.20015 = 54.64095.
      val high = PropOrder(id(1), "XMR", Side.Buy, d("546"), d("0.5"), false, at)
      assertEquals(ledger.checkResting(high, ledger.account), Right(()))
      // At 300 the open loss leaves 100 - 100 - 60 = -60 free.
      val low = PropOrder(id(2), "XMR", Side.Buy, d("300"), d("0.05"), false, at)
      assertEquals(ledger.checkResting(low, ledger.account), Left(PropLedger.restingNoStables))
    }
  }

  test("a short grows at the weighted entry") {
    withLedger(opened("1000")) { ledger =>
      filled(ledger, plan(Side.Sell, "1", "500"))
      filled(ledger, plan(Side.Sell, "1", "520", held = "-1"))
      assertEquals(position(ledger), Some(PropPosition(d("-2"), d("510"))))
      assertEquals(ledger.account.realized, d("0"))
      assertEquals(ledger.account.cash, d("999.541"))
    }
  }

  test("a reduce that leaves cash at exactly zero fills") {
    // A loss of 20 and a fee of 0.216 take all 20.216.
    val account = PropAccount(d("20.216"), deposit = d("20.216"), positions = xmr("1", "500"))
    withLedger(memory(account)) { ledger =>
      filled(ledger, plan(Side.Sell, "1", "480", held = "1", reduceOnly = true))
      assertEquals(ledger.account.cash, d("0"))
      assertEquals(ledger.account.realized, d("-20"))
      assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
      balanced(ledger.account)
    }
  }
