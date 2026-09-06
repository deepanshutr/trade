package desk

class ExchangeSpec extends munit.FunSuite:
  private val saved =
    SavedOrder("0x" + "1" * 32, 1000, "XMR", 7, Side.Buy, "5", "100.1", "100", 6000, 1000)
  private def reply(status: ujson.Value): Option[ujson.Value] = Some(
    ujson.Obj(
      "status" -> "ok",
      "response" -> ujson.Obj(
        "type" -> "order",
        "data" -> ujson.Obj("statuses" -> ujson.Arr(status))
      )
    )
  )

  test("an immediate action preserves exact strings and client identity") {
    val action = Exchange.action(saved)
    assertEquals(action.obj.keys.toVector, Vector("type", "orders", "grouping"))
    val order = action("orders")(0)
    assertEquals(order.obj.keys.toVector, Vector("a", "b", "p", "s", "r", "t", "c"))
    assertEquals(order("p").str, "100.1")
    assertEquals(order("s").str, "5")
    assertEquals(order("c").str, saved.id)
    assertEquals(order("t")("limit")("tif").str, "Ioc")
    assertEquals(order("r").bool, false)
  }

  test("outer success does not turn an inner error into a fill") {
    assertEquals(
      Exchange.outcome(reply(ujson.Obj("error" -> "rejected")), saved),
      Some("XMR order rejected")
    )
  }

  test("partial fills remain partial and unexpected replies remain unresolved") {
    val fill = ujson.Obj("filled" -> ujson.Obj("totalSz" -> "1", "avgPx" -> "100", "oid" -> 9))
    assertEquals(Exchange.outcome(reply(fill), saved), Some("XMR filled 1 at 100"))
    assertEquals(Exchange.outcome(None, saved), None)
    assertEquals(Exchange.outcome(Some(ujson.Obj()), saved), None)
    assertEquals(
      Exchange.outcome(reply(ujson.Obj("resting" -> ujson.Obj("oid" -> 9))), saved),
      None
    )
    val tooMuch = ujson.Obj("filled" -> ujson.Obj("totalSz" -> "6", "avgPx" -> "100", "oid" -> 9))
    assertEquals(Exchange.outcome(reply(tooMuch), saved), None)
  }

  test("recovery needs a matching terminal order and unknown never completes it") {
    def status(state: String, id: String = saved.id): ujson.Value = ujson.Obj(
      "status" -> "order",
      "order" -> ujson.Obj(
        "status" -> state,
        "order" -> ujson.Obj(
          "cloid" -> id,
          "coin" -> "XMR",
          "side" -> "B",
          "limitPx" -> "100.1",
          "origSz" -> "5"
        )
      )
    )
    assertEquals(Exchange.recovered(Some(ujson.Obj("status" -> "unknownOid")), saved), None)
    assertEquals(Exchange.recovered(Some(status("open")), saved), None)
    assertEquals(Exchange.recovered(Some(status("filled", "wrong")), saved), None)
    assertEquals(Exchange.recovered(Some(status("filled")), saved), Some("XMR recovered filled"))
    assertEquals(
      Exchange.recovered(Some(status("iocCancelRejected")), saved),
      Some("XMR recovered iocCancelRejected")
    )
  }

  test("metadata resolves the current index and rejects unsupported markets") {
    val meta = ujson.Obj(
      "universe" -> ujson.Arr(
        ujson.Obj("name" -> "BTC", "szDecimals" -> 5, "maxLeverage" -> 40),
        ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5)
      )
    )
    assertEquals(Exchange.market(meta, "XMR"), MarketRules(1, 3, 5))
    intercept[Exception](Exchange.market(meta, "other:XMR"))
    meta("universe")(1)("isDelisted") = ujson.Bool(true)
    intercept[Exception](Exchange.market(meta, "XMR"))
  }

  test("a replaced stream cannot deliver old data or disconnect its replacement") {
    val guard = new StreamGuard
    val old = guard.begin()
    val current = guard.begin()
    var value = "current"
    assert(!guard.deliver(old) { value = "old data" })
    assert(!guard.deliver(old) { value = "disconnected" })
    assertEquals(value, "current")
    assert(guard.deliver(current) { value = "new data" })
    assertEquals(value, "new data")
    guard.invalidate()
    assert(!guard.deliver(current) { value = "late data" })
    assertEquals(value, "new data")
  }
