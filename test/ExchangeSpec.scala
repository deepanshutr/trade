package desk

class ExchangeSpec extends munit.FunSuite:
  private val saved =
    SavedOrder("0x" + "1" * 32, 1000, "XMR", 7, Side.Buy, "5", "100.1", "100", 6000, 1000)
  // Enough monotonic time, so these cases test the wall time rule.
  private val waited = 65000L
  private def reply(status: ujson.Value): Option[ujson.Value] = Some(
    ujson.Obj(
      "status" -> "ok",
      "response" -> ujson.Obj(
        "type" -> "order",
        "data" -> ujson.Obj("statuses" -> ujson.Arr(status))
      )
    )
  )

  private def lookup(state: String, id: String = saved.id): ujson.Value = ujson.Obj(
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

  test("a posted order is sent add liquidity only and every other order immediate or cancel") {
    def tif(order: SavedOrder): String =
      Exchange.action(order)("orders")(0)("t")("limit")("tif").str
    assertEquals(tif(saved.copy(post = true)), "Alo")
    assertEquals(tif(saved), "Ioc")
    assertEquals(tif(saved.copy(reduceOnly = true)), "Ioc")
    val posted = Exchange.action(saved.copy(post = true))
    posted("orders")(0)("t")("limit")("tif") = "Ioc"
    assertEquals(ujson.write(posted), ujson.write(Exchange.action(saved)))
  }

  test("the action carries the saved reduce-only flag and nothing else changes") {
    assertEquals(saved.reduceOnly, false)
    val opening = Exchange.action(saved)
    val reducing = Exchange.action(saved.copy(reduceOnly = true))
    assertEquals(opening("orders")(0)("r"), ujson.Bool(false))
    assertEquals(reducing("orders")(0)("r"), ujson.Bool(true))
    reducing("orders")(0)("r") = false
    assertEquals(ujson.write(reducing), ujson.write(opening))
  }

  test("a top-level err leaves the saved order unresolved") {
    assertEquals(
      Exchange.placed(Some(ujson.Obj("status" -> "err", "response" -> "x")), saved),
      None
    )
  }

  test("an err reply plus unknownOid releases only strictly past expiry plus sixty seconds") {
    Vector(saved.expiresAfter + 1, saved.expiresAfter + 60000L).foreach { now =>
      assertEquals(
        Exchange.released(saved, now, elapsedMs = waited, allowUnknown = false, errReply = true),
        None
      )
    }
    assertEquals(
      Exchange.released(
        saved,
        saved.expiresAfter + 60001L,
        elapsedMs = waited,
        allowUnknown = false,
        errReply = true
      ),
      Some("XMR order rejected")
    )
  }

  test("failed, malformed, open and other-order lookups are never an unknown order") {
    Vector(None, Some(ujson.Str("unknownOid")), Some(ujson.Obj()), Some(lookup("open"))).foreach {
      reply =>
        assert(!VenueOrders.lookup(reply).contains(VenueOrders.Lookup.Unknown), reply.toString)
    }
    assert(!Exchange.sameOrder(Some(lookup("filled", "wrong")), saved))
    assertEquals(
      VenueOrders.lookup(Some(ujson.Obj("status" -> "unknownOid"))),
      Some(VenueOrders.Lookup.Unknown)
    )
  }

  test("outer success does not turn an inner error into a fill") {
    assertEquals(
      Exchange.placed(reply(ujson.Obj("error" -> "rejected")), saved),
      Some(VenueOrders.Placed.Rejected("rejected"))
    )
  }

  test("a fill within the order and its limit is read, and unexpected replies stay unresolved") {
    val fill = ujson.Obj("filled" -> ujson.Obj("totalSz" -> "1", "avgPx" -> "100", "oid" -> 9))
    assertEquals(
      Exchange.placed(reply(fill), saved),
      Some(VenueOrders.Placed.Filled(BigDecimal(1), BigDecimal(100), 9L))
    )
    assertEquals(Exchange.placed(None, saved), None)
    assertEquals(Exchange.placed(Some(ujson.Obj()), saved), None)
    assertEquals(
      Exchange
        .placed(reply(ujson.Obj("resting" -> ujson.Obj("oid" -> 4294967296L.toDouble))), saved),
      Some(VenueOrders.Placed.Resting(4294967296L))
    )
    val tooMuch = ujson.Obj("filled" -> ujson.Obj("totalSz" -> "6", "avgPx" -> "100", "oid" -> 9))
    assertEquals(Exchange.placed(reply(tooMuch), saved), None)
    val other = reply(fill).get
    other("response")("type") = "cancel"
    assertEquals(Exchange.placed(Some(other), saved), None)
  }

  test("a lookup names the saved order only by its identity, side, size and limit") {
    assert(Exchange.sameOrder(Some(lookup("canceled")), saved))
    assert(Exchange.sameOrder(Some(lookup("filled")), saved))
    assert(!Exchange.sameOrder(Some(lookup("filled", "wrong")), saved))
    assert(!Exchange.sameOrder(None, saved))
    assert(!Exchange.sameOrder(Some(ujson.Obj("status" -> "unknownOid")), saved))
    assert(!Exchange.sameOrder(Some(lookup("filled")), saved.copy(side = Side.Sell)))
    assert(!Exchange.sameOrder(Some(lookup("filled")), saved.copy(coin = "BTC")))
  }

  test("unknownOid releases only strictly past signed expiry plus sixty seconds") {
    Vector(65999L, 66000L).foreach { now =>
      assertEquals(
        Exchange.released(saved, now, elapsedMs = waited, allowUnknown = true, errReply = false),
        None
      )
    }
    assertEquals(
      Exchange.released(saved, 66001L, elapsedMs = waited, allowUnknown = true, errReply = false),
      Some("XMR not placed")
    )
  }

  test("unknownOid releases only after sixty-five monotonic seconds even far past wall expiry") {
    val late = saved.expiresAfter + 600000L
    for (errReply, text) <- Vector(false -> "XMR not placed", true -> "XMR order rejected") do
      Vector(0L, 64999L).foreach { elapsed =>
        assertEquals(
          Exchange.released(saved, late, elapsed, allowUnknown = true, errReply = errReply),
          None,
          s"released after $elapsed ms with err $errReply"
        )
      }
      assertEquals(
        Exchange.released(saved, late, 65000L, allowUnknown = true, errReply = errReply),
        Some(text)
      )
  }

  test("unknownOid cannot release without permission even well past expiry") {
    assertEquals(
      Exchange.released(
        saved,
        saved.expiresAfter + 600000L,
        elapsedMs = waited,
        allowUnknown = false,
        errReply = false
      ),
      None
    )
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
      assertEquals(Exchange.placed(post, saved), None)
      assertEquals(VenueOrders.lookup(lookup), None)
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

  private def lookupWith(field: String, value: String): Option[ujson.Value] =
    val order = ujson.Obj(
      "cloid" -> saved.id,
      "coin" -> saved.coin,
      "side" -> "B",
      "limitPx" -> saved.limit,
      "origSz" -> saved.quantity
    )
    order(field) = ujson.Str(value)
    Some(
      ujson.Obj("status" -> "order", "order" -> ujson.Obj("status" -> "filled", "order" -> order))
    )

  test("a lookup with a mismatched original quantity is not the saved order") {
    assert(!Exchange.sameOrder(lookupWith("origSz", "4"), saved))
    assert(Exchange.sameOrder(lookupWith("origSz", saved.quantity), saved))
  }

  test("a lookup with a mismatched limit is not the saved order") {
    assert(!Exchange.sameOrder(lookupWith("limitPx", "100.2"), saved))
    assert(Exchange.sameOrder(lookupWith("limitPx", saved.limit), saved))
  }

  test("filled average prices respect both buy and sell saved limits") {
    def fill(price: String): Option[ujson.Value] =
      reply(ujson.Obj("filled" -> ujson.Obj("totalSz" -> "1", "avgPx" -> price, "oid" -> 9)))
    def filled(price: String): Option[VenueOrders.Placed] =
      Some(VenueOrders.Placed.Filled(BigDecimal(1), BigDecimal(price), 9L))
    assertEquals(Exchange.placed(fill("100.2"), saved), None)
    assertEquals(Exchange.placed(fill("100.1"), saved), filled("100.1"))
    val sell = saved.copy(side = Side.Sell)
    assertEquals(Exchange.placed(fill("100"), sell), None)
    assertEquals(Exchange.placed(fill("100.1"), sell), filled("100.1"))
  }

  test("a fill keeps exact long numbers and the guards still hold for them") {
    val long = saved.copy(
      coin = "X" * 40,
      quantity = "99999999999999999999999999999999999999",
      limit = "99999999999999999999999999999999999999"
    )
    val quantity = "123456789012345678901234567890"
    val price = "234567890123456789012345678901"
    val filled =
      ujson.Obj("filled" -> ujson.Obj("totalSz" -> quantity, "avgPx" -> price, "oid" -> 9))
    assertEquals(
      Exchange.placed(reply(filled), long),
      Some(VenueOrders.Placed.Filled(BigDecimal(quantity), BigDecimal(price), 9L))
    )
    assertEquals(Exchange.placed(reply(filled), long.copy(quantity = "1")), None)
    assertEquals(Exchange.placed(reply(filled), long.copy(limit = "1")), None)
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
