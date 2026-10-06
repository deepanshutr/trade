package desk

import java.net.http.WebSocket
import java.nio.ByteBuffer
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicLong

class ActiveDataReadsSpec extends munit.FunSuite:
  private val owner = "0x" + "a" * 40
  private val current = ActiveDataSession("XMR", owner, 7L, 11L)
  private def data(coin: String = "XMR", user: String = owner): ujson.Value =
    ujson.Obj(
      "user" -> user,
      "coin" -> coin,
      "leverage" -> ujson.Obj("type" -> "isolated", "value" -> 5),
      "maxTradeSzs" -> ujson.Arr("1", "2")
    )
  private def reply(id: ujson.Value, value: ujson.Value = data()): ujson.Value =
    ujson.Obj(
      "channel" -> "post",
      "data" -> ujson.Obj(
        "id" -> id,
        "response" -> ujson.Obj(
          "type" -> "info",
          "payload" -> ujson.Obj("type" -> "activeAssetData", "data" -> value)
        )
      )
    )
  private def fresh(): (ActiveDataReads, ujson.Value) =
    val reads = new ActiveDataReads
    reads.open(current)
    (reads, reads.request(current, 1000L).get)

  test(
    "quiet account reads use one correlated info post and retain request-start age"
  ) {
    val (reads, request) = fresh()
    assertEquals(request("method").str, "post")
    assertEquals(request("request")("type").str, "info")
    assertEquals(
      request("request")("payload"),
      ujson.Obj(
        "type" -> "activeAssetData",
        "user" -> owner,
        "coin" -> "XMR"
      )
    )
    assertEquals(reads.request(current, 1200L), None)
    assertEquals(
      reads.accept(current, reply(request("id")), 1600L),
      Some((Some(data()), 1000L))
    )
    assertEquals(reads.accept(current, reply(request("id")), 1601L), None)
  }

  test(
    "a newer stream update supersedes the pending read even when the read replies later"
  ) {
    val (reads, request) = fresh()
    val newer = data()
    newer("maxTradeSzs") = ujson.Arr("0", "0")
    val stream = ujson.Obj("channel" -> "activeAssetData", "data" -> newer)
    assertEquals(
      reads.accept(current, stream, 1200L),
      Some((Some(newer), 1200L))
    )
    assertEquals(reads.accept(current, reply(request("id")), 1300L), None)
    assertEquals(reads.request(current, 2199L), None)
    assert(reads.request(current, 2200L).nonEmpty)
  }

  test("wrong or malformed identifiers cannot consume the matching read") {
    Vector(ujson.Num(88), ujson.Num(1.5), ujson.Str("1"), ujson.Null).foreach { id =>
      val (reads, request) = fresh()
      assertEquals(reads.accept(current, reply(id), 1100L), None)
      assertEquals(
        reads.accept(current, reply(request("id")), 1101L),
        Some((Some(data()), 1000L))
      )
    }
  }

  test("wrong response and payload types never renew active readiness") {
    Vector("response", "payload").foreach { part =>
      val (reads, request) = fresh()
      val invalid = reply(request("id"))
      if part == "response" then invalid("data")("response")("type") = "action"
      else invalid("data")("response")("payload")("type") = "meta"
      assertEquals(reads.accept(current, invalid, 1100L), Some((None, 1000L)))
      assertEquals(reads.accept(current, reply(request("id")), 1200L), None)
    }
  }

  test(
    "a correlated read still requires the configured owner and selected market"
  ) {
    Vector(data("BTC"), data(user = "0x" + "b" * 40)).foreach { invalid =>
      val (reads, request) = fresh()
      assertEquals(
        reads.accept(current, reply(request("id"), invalid), 1100L),
        Some((None, 1000L))
      )
    }
  }

  test(
    "replacement selection and connection cannot be overwritten or expire one another"
  ) {
    Vector(
      current.copy(coin = "BTC"),
      current.copy(owner = "0x" + "b" * 40),
      current.copy(generation = 8L),
      current.copy(connection = 12L)
    ).foreach { replacement =>
      val (reads, old) = fresh()
      reads.open(replacement)
      val next = reads.request(replacement, 1200L).get
      assert(next("id") != old("id"))
      assertEquals(reads.accept(current, reply(old("id")), 1300L), None)
      assertEquals(reads.request(current, 4000L), None)
      assert(!reads.expire(current, 5000L))
      val value = data(replacement.coin, replacement.owner)
      assertEquals(
        reads.accept(replacement, reply(next("id"), value), 1400L),
        Some((Some(value), 1200L))
      )
    }
  }

  test("timeout and late replies cannot refresh account data") {
    val (reads, request) = fresh()
    assert(!reads.expire(current, 2999L))
    assert(reads.expire(current, 3001L))
    assertEquals(reads.accept(current, reply(request("id")), 3001L), None)
    assertEquals(
      reads.request(current, 3001L),
      None,
      "timed-out transport must close before retry"
    )
    val replacement = current.copy(connection = 12L)
    reads.open(replacement)
    val next = reads.request(replacement, 3001L).get
    assert(next("id") != request("id"))
    assertEquals(reads.request(replacement, 3100L), None)
  }

  test("late replies are refused even before the next timeout check") {
    val (reads, request) = fresh()
    assertEquals(
      reads.accept(current, reply(request("id")), 3001L),
      Some((None, 1000L))
    )
  }

  test(
    "errors invalidate only their pending read and pong never renews account data"
  ) {
    val (reads, request) = fresh()
    assertEquals(
      reads.accept(current, ujson.Obj("channel" -> "pong"), 1050L),
      None
    )
    val error = reply(request("id"))
    error("data")("response") = ujson.Obj("type" -> "error", "payload" -> "failed")
    assertEquals(reads.accept(current, error, 1100L), Some((None, 1000L)))
    assertEquals(reads.request(current, 1999L), None)
    assert(reads.request(current, 2000L).nonEmpty)
  }

  test("mismatched stream updates do not supersede a matching pending read") {
    val (reads, request) = fresh()
    assertEquals(
      reads.accept(
        current,
        ujson.Obj("channel" -> "activeAssetData", "data" -> data("BTC")),
        1100L
      ),
      None
    )
    assertEquals(
      reads.accept(current, reply(request("id")), 1101L),
      Some((Some(data()), 1000L))
    )
  }

  test(
    "closing the account socket invalidates pending and unsolicited active data"
  ) {
    val (reads, request) = fresh()
    reads.close()
    assertEquals(reads.accept(current, reply(request("id")), 1100L), None)
    assertEquals(
      reads.accept(
        current,
        ujson.Obj("channel" -> "activeAssetData", "data" -> data()),
        1100L
      ),
      None
    )
    assertEquals(reads.request(current, 2000L), None)
  }

  test("a reversed clock cannot turn an old pending read fresh") {
    val (reads, request) = fresh()
    assertEquals(
      reads.accept(current, reply(request("id")), 999L),
      Some((None, 1000L))
    )
  }

  test(
    "stream supersession cannot start a second read before the first response or timeout"
  ) {
    val (reads, request) = fresh()
    val stream = ujson.Obj("channel" -> "activeAssetData", "data" -> data())
    assertEquals(
      reads.accept(current, stream, 1200L),
      Some((Some(data()), 1200L))
    )
    assertEquals(
      reads.request(current, 2200L),
      None,
      "the superseded wire read is still in flight"
    )
    assertEquals(reads.accept(current, reply(request("id")), 2300L), None)
    assert(reads.request(current, 2300L).nonEmpty)
  }

  private final class FakeSocket(val listener: WebSocket.Listener) extends WebSocket:
    var messages = Vector.empty[ujson.Value]
    var aborted = false
    private def done(): CompletableFuture[WebSocket] =
      CompletableFuture.completedFuture(this)
    def sendText(
        text: CharSequence,
        last: Boolean
    ): CompletableFuture[WebSocket] =
      assert(last)
      messages :+= ujson.read(text.toString)
      done()
    def sendBinary(
        data: ByteBuffer,
        last: Boolean
    ): CompletableFuture[WebSocket] =
      fail("unexpected binary account request")
    def sendPing(data: ByteBuffer): CompletableFuture[WebSocket] = fail(
      "unexpected binary ping"
    )
    def sendPong(data: ByteBuffer): CompletableFuture[WebSocket] = fail(
      "unexpected binary pong"
    )
    def sendClose(code: Int, reason: String): CompletableFuture[WebSocket] =
      done()
    def request(count: Long): Unit = assert(count > 0)
    def getSubprotocol(): String = ""
    def isOutputClosed(): Boolean = aborted
    def isInputClosed(): Boolean = aborted
    def abort(): Unit = aborted = true
    def emit(value: ujson.Value): Unit =
      val _ = listener.onText(this, ujson.write(value), true)
    def disconnect(): Unit =
      val _ = listener.onClose(this, 1000, "closed")
    def posts: Vector[ujson.Value] = messages.filter(_("method").str == "post")

  private final class SocketFixture:
    val clock = new AtomicLong(1000L)
    var sockets = Vector.empty[FakeSocket]
    var events = Vector.empty[(Option[ujson.Value], Long)]
    // Each order event with the number of messages its socket had sent when it arrived.
    var orders = Vector.empty[(OrderEvent, Int)]
    val exchange = new JdkExchange(
      "https://example.invalid",
      Some((_, address, listener) =>
        assertEquals(address.toString, "wss://example.invalid/ws")
        val socket = new FakeSocket(listener)
        sockets :+= socket
        listener.onOpen(socket)
        socket
      )
    )
    exchange.watchOrders(event => orders :+= ((event, sockets.lastOption.fold(0)(_.messages.size))))
    def watch(coin: String = "XMR", generation: Long = 7L): Unit =
      exchange.watchTimed(
        coin,
        owner,
        generation,
        () => clock.get(),
        (value, at) => events :+= (value, at)
      )

  test(
    "the real socket path posts quietly and dates a valid response at request start"
  ) {
    val f = new SocketFixture
    try
      f.watch()
      val socket = f.sockets.head
      assertEquals(socket.messages.head("method").str, "subscribe")
      assertEquals(socket.posts.size, 1)
      assertEquals(f.events, Vector((None, 1000L)))
      f.clock.set(1600L)
      socket.emit(reply(socket.posts.head("id")))
      assertEquals(f.events.last, (Some(data()), 1000L))
      f.watch()
      assertEquals(socket.posts.size, 1)
      f.clock.set(2000L)
      f.watch()
      assertEquals(socket.posts.size, 2)
    finally f.exchange.close()
    assert(f.sockets.forall(_.aborted))
  }

  test("the real socket path keeps a superseded wire read until its response") {
    val f = new SocketFixture
    try
      f.watch()
      val socket = f.sockets.head
      val oldId = socket.posts.head("id")
      f.clock.set(1200L)
      val newer = data()
      newer("maxTradeSzs") = ujson.Arr("0", "0")
      socket.emit(ujson.Obj("channel" -> "activeAssetData", "data" -> newer))
      f.clock.set(2200L)
      f.watch()
      assertEquals(socket.posts.size, 1)
      socket.emit(reply(oldId))
      assertEquals(f.events.last, (Some(newer), 1200L))
      f.watch()
      assertEquals(socket.posts.size, 2)
    finally f.exchange.close()
  }

  test(
    "a socket timeout aborts the outstanding transport before another post"
  ) {
    val f = new SocketFixture
    try
      f.watch()
      val old = f.sockets.head
      val oldId = old.posts.head("id")
      f.clock.set(3001L)
      f.watch()
      assert(old.aborted, "the timed-out wire read was left outstanding")
      assertEquals(old.posts.size, 1)
      assertEquals(f.events.last, (None, 3001L))
      f.clock.set(3002L)
      f.watch()
      val next = f.sockets.last
      assert(next ne old)
      assertEquals(next.posts.size, 1)
      assert(next.posts.head("id") != oldId)
      next.emit(reply(next.posts.head("id")))
      val accepted = f.events
      old.emit(reply(oldId))
      old.disconnect()
      assertEquals(f.events, accepted)
      assertEquals(f.events.last, (Some(data()), 3002L))
    finally f.exchange.close()
  }

  test("selection replacement rejects old active updates and old disconnects") {
    val f = new SocketFixture
    try
      f.watch()
      val old = f.sockets.head
      f.clock.set(1200L)
      f.watch("BTC", 8L)
      assert(old.aborted)
      val next = f.sockets.last
      next.emit(reply(next.posts.head("id"), data("BTC")))
      val accepted = f.events
      old.emit(ujson.Obj("channel" -> "activeAssetData", "data" -> data()))
      old.emit(reply(old.posts.head("id")))
      old.disconnect()
      assertEquals(f.events, accepted)
      assertEquals(f.events.last, (Some(data("BTC")), 1200L))
    finally f.exchange.close()
  }

  test(
    "pong and unrelated response identifiers cannot make the account ready"
  ) {
    val f = new SocketFixture
    try
      f.watch()
      val socket = f.sockets.head
      val before = f.events
      f.clock.set(1500L)
      socket.emit(ujson.Obj("channel" -> "pong"))
      socket.emit(reply(ujson.Num(999L)))
      assertEquals(f.events, before)
      val error = reply(socket.posts.head("id"))
      error("data")("response") = ujson.Obj("type" -> "error", "payload" -> "unavailable")
      socket.emit(error)
      assertEquals(f.events.last, (None, 1000L))
    finally f.exchange.close()
  }

  private def update(cloid: String): ujson.Value = ujson.Obj(
    "order" -> ujson.Obj(
      "coin" -> "XMR",
      "side" -> "B",
      "limitPx" -> "546.09",
      "sz" -> "0.15",
      "oid" -> 77738308,
      "timestamp" -> 1700000000000.0,
      "origSz" -> "0.2",
      "cloid" -> cloid
    ),
    "status" -> "open",
    "statusTimestamp" -> 1700000000001.0
  )

  test("the account socket subscribes to order updates and reports each open after both") {
    val f = new SocketFixture
    try
      f.watch()
      val socket = f.sockets.head
      val subscribes = socket.messages.filter(_("method").str == "subscribe")
      assertEquals(
        subscribes.map(_("subscription")),
        Vector(
          ujson.Obj("type" -> "activeAssetData", "user" -> owner, "coin" -> "XMR"),
          ujson.Obj("type" -> "orderUpdates", "user" -> owner)
        )
      )
      assertEquals(socket.messages.take(2), subscribes)
      assertEquals(f.orders, Vector((OrderEvent.Opened(1000L), 2)))
      f.clock.set(1200L)
      f.watch()
      assertEquals(f.orders.size, 1, "a kept socket reported another open")
      f.watch("BTC", 8L)
      val next = f.sockets.last
      assert(next ne socket)
      assertEquals(
        next.messages.filter(_("method").str == "subscribe").map(_("subscription")("type").str),
        Vector("activeAssetData", "orderUpdates")
      )
      assertEquals(f.orders.last, (OrderEvent.Opened(1200L), 2))
      assertEquals(f.orders.size, 2)
    finally f.exchange.close()
  }

  test("order updates reach the orders receiver and never the active data receiver") {
    val f = new SocketFixture
    try
      f.watch()
      val socket = f.sockets.head
      val before = f.events
      f.clock.set(1500L)
      val values = ujson.Arr(update("0x" + "1" * 32), update("0x" + "2" * 32))
      socket.emit(ujson.Obj("channel" -> "orderUpdates", "data" -> values))
      assertEquals(f.events, before)
      assertEquals(f.orders.last._1, OrderEvent.Updates(values.value.toVector, 1500L))
      socket.emit(ujson.Obj("channel" -> "orderUpdates", "data" -> ujson.Obj()))
      socket.emit(ujson.Obj("channel" -> "userFills", "data" -> values))
      assertEquals(f.orders.size, 2)
      f.watch("BTC", 8L)
      val count = f.orders.size
      socket.emit(ujson.Obj("channel" -> "orderUpdates", "data" -> values))
      assertEquals(f.orders.size, count, "a replaced socket delivered order updates")
    finally f.exchange.close()
  }

  test("order updates pass only for the current session") {
    val reads = new ActiveDataReads
    val message =
      ujson.Obj("channel" -> "orderUpdates", "data" -> ujson.Arr(update("0x" + "1" * 32)))
    assertEquals(reads.orders(current, message, 1000L), None)
    reads.open(current)
    assertEquals(
      reads.orders(current, message, 1000L),
      Some(OrderEvent.Updates(Vector(update("0x" + "1" * 32)), 1000L))
    )
    assertEquals(reads.orders(current.copy(connection = 12L), message, 1000L), None)
    assertEquals(
      reads.orders(current, ujson.Obj("channel" -> "activeAssetData", "data" -> data()), 1000L),
      None
    )
    reads.close()
    assertEquals(reads.orders(current, message, 1000L), None)
  }
