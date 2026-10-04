package desk

import desk.VenueOrders.*

class VenueOrdersSpec extends munit.FunSuite:
  private def reply(status: String): Option[ujson.Value] =
    Some(
      ujson.read(s"""{"status":"ok","response":{"type":"order","data":{"statuses":[$status]}}}""")
    )

  test("a cancel by client id is written with the exact keys in order") {
    assertEquals(
      ujson.write(cancelAction(32767, "0x0123456789abcdef0123456789abcdef")),
      """{"type":"cancelByCloid","cancels":[{"asset":32767,"cloid":"0x0123456789abcdef0123456789abcdef"}]}"""
    )
  }

  test("a placed reply reads resting, filled and rejected orders") {
    assertEquals(
      placed(reply("""{"resting":{"oid":4294967296}}""")),
      Some(Placed.Resting(4294967296L))
    )
    assertEquals(
      placed(reply("""{"filled":{"totalSz":"0.183","avgPx":"546.09","oid":77738308}}""")),
      Some(Placed.Filled(BigDecimal("0.183"), BigDecimal("546.09"), 77738308L))
    )
    assertEquals(
      placed(reply("""{"error":"Post only order would have immediately matched"}""")),
      Some(Placed.Rejected("Post only order would have immediately matched"))
    )
    assertEquals(placed(reply("\"success\"")), None)
    assertEquals(placed(reply("""{"resting":{"oid":1.5}}""")), None)
    assertEquals(placed(reply("""{"resting":{"oid":-1}}""")), None)
    assertEquals(placed(reply("""{"resting":{"oid":1}},{"resting":{"oid":2}}""")), None)
    assertEquals(placed(None), None)
    assertEquals(placed(reply("""{"filled":{"totalSz":"0","avgPx":"546.09","oid":1}}""")), None)
    assertEquals(placed(reply("""{"filled":{"totalSz":"0.183","avgPx":"0","oid":1}}""")), None)
    assertEquals(
      placed(
        Some(
          ujson.read(
            """{"status":"err","response":{"type":"order","data":{"statuses":[{"resting":{"oid":1}}]}}}"""
          )
        )
      ),
      None
    )
    assertEquals(placed(Some(ujson.read("""{"status":"err","response":"x"}"""))), None)
  }

  test("a cancel reply reads success and the error text") {
    assertEquals(cancelOutcome(reply("\"success\"")), Some(Right(())))
    assertEquals(
      cancelOutcome(reply("""{"error":"Order was never placed, already canceled, or filled."}""")),
      Some(Left("Order was never placed, already canceled, or filled."))
    )
    assertEquals(cancelOutcome(reply("""{"resting":{"oid":1}}""")), None)
    assertEquals(cancelOutcome(reply("\"success\",\"success\"")), None)
    assertEquals(cancelOutcome(None), None)
    assertEquals(
      cancelOutcome(
        Some(
          ujson.read(
            """{"status":"err","response":{"type":"cancel","data":{"statuses":["success"]}}}"""
          )
        )
      ),
      None
    )
  }

  test("a pushed order update parses and one without a client id does not") {
    val value = ujson.read(
      """{"order":{"coin":"XMR","side":"B","limitPx":"546.05","sz":"0.15","oid":77738308,
        |"timestamp":1699999999999,"origSz":"0.183","cloid":"0x0123456789abcdef0123456789abcdef"},
        |"status":"open","statusTimestamp":1700000000000}""".stripMargin
    )
    assertEquals(
      orderUpdate(value),
      Some(
        OrderUpdate(
          "0x0123456789abcdef0123456789abcdef",
          "XMR",
          "open",
          BigDecimal("0.15"),
          77738308L,
          1700000000000L
        )
      )
    )
    value("order").obj.remove("cloid")
    assertEquals(orderUpdate(value), None)
  }

  private def lookupReply(status: String, limit: String = "546.05"): Option[ujson.Value] =
    Some(
      ujson.read(
        s"""{"status":"order","order":{"order":{"coin":"XMR","side":"B","limitPx":"$limit",
           |"sz":"0.15","oid":77738308,"origSz":"0.183",
           |"cloid":"0x0123456789abcdef0123456789abcdef"},"status":"$status","statusTimestamp":1}}""".stripMargin
      )
    )

  test("a status lookup reads open, every terminal status and unknown") {
    assertEquals(lookup(lookupReply("open")), Some(Lookup.Open(77738308L, BigDecimal("0.15"))))
    assertEquals(
      lookup(lookupReply("filled")),
      Some(Lookup.Settled("filled", Some(BigDecimal("546.05"))))
    )
    Vector(
      "canceled",
      "rejected",
      "marginCanceled",
      "vaultWithdrawalCanceled",
      "openInterestCapCanceled",
      "selfTradeCanceled",
      "reduceOnlyCanceled",
      "siblingFilledCanceled",
      "delistedCanceled",
      "liquidatedCanceled",
      "scheduledCancel",
      "tickRejected",
      "minTradeNtlRejected",
      "perpMarginRejected",
      "reduceOnlyRejected",
      "badAloPxRejected",
      "iocCancelRejected",
      "badTriggerPxRejected",
      "marketOrderNoLiquidityRejected",
      "positionIncreaseAtOpenInterestCapRejected",
      "positionFlipAtOpenInterestCapRejected",
      "tooAggressiveAtOpenInterestCapRejected",
      "openInterestIncreaseRejected",
      "insufficientSpotBalanceRejected",
      "oracleRejected",
      "perpMaxPositionRejected"
    ).foreach { status =>
      assertEquals(lookup(lookupReply(status)), Some(Lookup.Settled(status, None)), status)
    }
    assertEquals(lookup(lookupReply("somethingNew")), None)
    assertEquals(lookup(Some(ujson.read("""{"status":"unknownOid"}"""))), Some(Lookup.Unknown))
    assertEquals(lookup(None), None)
  }

  test("a whole number is accepted below two to the 53rd power") {
    assertEquals(wholeLong(ujson.Num(4294967296.0)), Some(4294967296L))
    assertEquals(wholeLong(ujson.Str("77738308")), Some(77738308L))
    assertEquals(wholeLong(ujson.Num(9007199254740991.0)), Some(9007199254740991L))
    assertEquals(wholeLong(ujson.Str("9007199254740991")), Some(9007199254740991L))
    assertEquals(wholeLong(ujson.Num(9007199254740992.0)), None)
    assertEquals(wholeLong(ujson.Str("9007199254740992")), None)
    assertEquals(wholeLong(ujson.Num(9007199254740994.0)), None)
    assertEquals(wholeLong(ujson.Str("9007199254740993")), None)
    assertEquals(wholeLong(ujson.Num(1.5)), None)
    assertEquals(wholeLong(ujson.Num(-1.0)), None)
    assertEquals(wholeLong(ujson.Str("-1")), None)
    assertEquals(wholeLong(ujson.Str("1.0")), None)
    assertEquals(wholeLong(ujson.Str("")), None)
    assertEquals(wholeLong(ujson.Null), None)
  }
