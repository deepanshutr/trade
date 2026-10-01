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

  test("a top-level err leaves the saved order unresolved") {
    assertEquals(
      Exchange.outcome(Some(ujson.Obj("status" -> "err", "response" -> "x")), saved),
      None
    )
  }

  test("an unknownOid confirms an err reply without expiry permission") {
    assertEquals(
      Exchange.recovered(
        Some(ujson.Obj("status" -> "unknownOid")),
        saved,
        1000L,
        allowUnknown = false,
        errReply = true
      ),
      Some("XMR order rejected")
    )
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

  test("recovery preserves known status handling after expiry") {
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
    assertEquals(
      Exchange.recovered(Some(status("canceled")), saved, 66001L, allowUnknown = true),
      Some("XMR recovered canceled")
    )
    assertEquals(
      Exchange.recovered(Some(status("futureStatus")), saved, 66001L, allowUnknown = true),
      None
    )
    assertEquals(Exchange.recovered(Some(status("open")), saved, 66001L, allowUnknown = true), None)
    assertEquals(
      Exchange.recovered(Some(status("filled", "wrong")), saved, 66001L, allowUnknown = true),
      None
    )
    assertEquals(
      Exchange.recovered(Some(status("filled")), saved, 66001L, allowUnknown = true),
      Some("XMR recovered filled")
    )
    assertEquals(
      Exchange.recovered(Some(status("iocCancelRejected")), saved, 66001L, allowUnknown = true),
      Some("XMR recovered iocCancelRejected")
    )
  }

  test("unknownOid releases only strictly past signed expiry plus sixty seconds") {
    val unknown = Some(ujson.Obj("status" -> "unknownOid"))
    assertEquals(Exchange.recovered(unknown, saved, 65999L, allowUnknown = true), None)
    assertEquals(Exchange.recovered(unknown, saved, 66000L, allowUnknown = true), None)
    assertEquals(
      Exchange.recovered(unknown, saved, 66001L, allowUnknown = true),
      Some("XMR not placed")
    )
  }

  test("unknownOid cannot release without permission even well past expiry") {
    assertEquals(
      Exchange.recovered(
        Some(ujson.Obj("status" -> "unknownOid")),
        saved,
        saved.expiresAfter + 600000L,
        allowUnknown = false
      ),
      None
    )
  }

  test("failed and malformed lookups never release after expiry") {
    Vector(
      None,
      Some(ujson.Obj()),
      Some(ujson.Str("unknownOid")),
      Some(ujson.Obj("status" -> "err"))
    ).foreach { reply =>
      assertEquals(Exchange.recovered(reply, saved, 66001L, allowUnknown = true), None)
    }
  }

  test("HTTP failures and unparsable bodies are neither refusals nor unknown orders") {
    val server = com.sun.net.httpserver.HttpServer.create(
      new java.net.InetSocketAddress("127.0.0.1", 0),
      0
    )
    val response = new java.util.concurrent.atomic.AtomicReference((200, ""))
    server.createContext(
      "/",
      request => {
        request.getRequestBody.close()
        val (code, body) = response.get()
        val bytes = body.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        request.sendResponseHeaders(code, bytes.length.toLong)
        request.getResponseBody.write(bytes)
        request.close()
      }
    )
    server.start()
    val exchange = new JdkExchange(s"http://127.0.0.1:${server.getAddress.getPort}")
    def unresolved(): Unit =
      val post = exchange.post(ujson.Obj())
      val lookup = exchange.info(ujson.Obj())
      assertEquals(post, None)
      assertEquals(lookup, None)
      assertEquals(Exchange.outcome(post, saved), None)
      assertEquals(Exchange.recovered(lookup, saved, 66001L, allowUnknown = true), None)
    try
      val refusal = """{"status":"err","response":"x"}"""
      val unknown = """{"status":"unknownOid"}"""
      response.set((200, refusal))
      assertEquals(exchange.post(ujson.Obj()), Some(ujson.read(refusal)))
      response.set((200, unknown))
      assertEquals(exchange.info(ujson.Obj()), Some(ujson.read(unknown)))
      for status <- Vector(429, 500, 503); body <- Vector(refusal, unknown) do
        response.set((status, body))
        unresolved()
      for invalid <- Vector("not json", "") do
        response.set((200, invalid))
        unresolved()
      server.stop(0)
      unresolved()
    finally
      exchange.close()
      server.stop(0)
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

  private def recoveredWith(field: String, value: String): Option[String] =
    val order = ujson.Obj(
      "cloid" -> saved.id,
      "coin" -> saved.coin,
      "side" -> "B",
      "limitPx" -> saved.limit,
      "origSz" -> saved.quantity
    )
    order(field) = ujson.Str(value)
    Exchange.recovered(
      Some(
        ujson.Obj(
          "status" -> "order",
          "order" -> ujson.Obj("status" -> "filled", "order" -> order)
        )
      ),
      saved,
      66001L,
      allowUnknown = true
    )

  test(
    "recovery rejects a mismatched original quantity with the same client identifier"
  ) {
    assertEquals(recoveredWith("origSz", "4"), None)
    assertEquals(
      recoveredWith("origSz", saved.quantity),
      Some("XMR recovered filled")
    )
  }

  test("recovery rejects a mismatched limit with the same client identifier") {
    assertEquals(recoveredWith("limitPx", "100.2"), None)
    assertEquals(
      recoveredWith("limitPx", saved.limit),
      Some("XMR recovered filled")
    )
  }

  test("filled average prices respect both buy and sell saved limits") {
    def fill(price: String): Option[ujson.Value] =
      reply(
        ujson.Obj(
          "filled" -> ujson.Obj("totalSz" -> "1", "avgPx" -> price, "oid" -> 9)
        )
      )
    assertEquals(Exchange.outcome(fill("100.2"), saved), None)
    assertEquals(
      Exchange.outcome(fill("100.1"), saved),
      Some("XMR filled 1 at 100.1")
    )
    val sell = saved.copy(side = Side.Sell)
    assertEquals(Exchange.outcome(fill("100"), sell), None)
    assertEquals(
      Exchange.outcome(fill("100.1"), sell),
      Some("XMR filled 1 at 100.1")
    )
  }

  test(
    "validated fill text fits long markets and marks rounded numbers without changing the fill guards"
  ) {
    val long = saved.copy(
      coin = "X" * 40,
      quantity = "99999999999999999999999999999999999999",
      limit = "99999999999999999999999999999999999999"
    )
    val quantity = "123456789012345678901234567890"
    val price = "234567890123456789012345678901"
    val filled =
      ujson.Obj("filled" -> ujson.Obj("totalSz" -> quantity, "avgPx" -> price, "oid" -> 9))
    val shown = Exchange.outcome(reply(filled), long).get
    assert(shown.startsWith(long.coin + " filled ~"), shown)
    assert(shown.contains(" at ~"), shown)
    assert(shown.length <= 64, shown)
    assertEquals(BigDecimal(quantity), BigDecimal(filled("filled")("totalSz").str))
    assertEquals(BigDecimal(price), BigDecimal(filled("filled")("avgPx").str))
    assertEquals(Exchange.outcome(reply(filled), long.copy(quantity = "1")), None)
    assertEquals(Exchange.outcome(reply(filled), long.copy(limit = "1")), None)
  }

  test("metadata accepts any positive maximum leverage and rejects zero") {
    def meta(maximum: Int): ujson.Value = ujson.Obj(
      "universe" -> ujson.Arr(
        ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> maximum)
      )
    )
    (1 to 4).foreach(maximum =>
      assertEquals(Exchange.market(meta(maximum), "XMR"), MarketRules(0, 3, maximum))
    )
    intercept[Exception](Exchange.market(meta(0), "XMR"))
  }
