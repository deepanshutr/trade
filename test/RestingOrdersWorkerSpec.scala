package desk

import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.collection.mutable.ArrayBuffer

class RestingOrdersWorkerSpec extends TempDirectorySuite:
  private val owner = "0x" + "2" * 40
  private val other = "0x" + "3" * 40
  private val sizes =
    TradeSizes(small = BigDecimal(20), big = BigDecimal(100), target = BigDecimal(100))
  private val venueTime = 1700000000000L

  // One call held inside the fake until the test releases it.
  private final class Hold:
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    def pass(): Unit =
      entered.countDown()
      val _ = release.await(10, TimeUnit.SECONDS)
    def await(): Unit = assert(entered.await(5, TimeUnit.SECONDS), "held call never started")
    def open(): Unit = release.countDown()

  private final case class Seed(
      side: Side,
      quantity: String,
      limit: String,
      oid: Long,
      coin: String = "XMR",
      remaining: Option[String] = None,
      reduceOnly: Boolean = false,
      holder: String = "0x" + "2" * 40
  )

  private def sideText(side: Side): String = if side == Side.Buy then "B" else "A"

  private def placedReply(status: ujson.Value): ujson.Value = ujson.Obj(
    "status" -> "ok",
    "response" -> ujson.Obj("type" -> "order", "data" -> ujson.Obj("statuses" -> ujson.Arr(status)))
  )

  private def cancelReply(status: ujson.Value): ujson.Value = ujson.Obj(
    "status" -> "ok",
    "response" -> ujson.Obj(
      "type" -> "cancel",
      "data" -> ujson.Obj("statuses" -> ujson.Arr(status))
    )
  )

  private val gone = ujson.Obj("error" -> "Order was never placed, already canceled, or filled.")

  // A venue order as the orderUpdates channel sends it.
  private def ws(
      cloid: String,
      side: Side,
      limit: String,
      quantity: String,
      oid: Long,
      status: String,
      size: String,
      at: Long = 1700000000000L,
      coin: String = "XMR"
  ): ujson.Value = ujson.Obj(
    "order" -> ujson.Obj(
      "coin" -> coin,
      "side" -> sideText(side),
      "limitPx" -> limit,
      "sz" -> size,
      "oid" -> ujson.Num(oid.toDouble),
      "timestamp" -> ujson.Num(1699999990000.0),
      "origSz" -> quantity,
      "cloid" -> cloid
    ),
    "status" -> status,
    "statusTimestamp" -> ujson.Num(at.toDouble)
  )

  private def ws(order: SavedOrder, status: String, size: String, at: Long): ujson.Value =
    ws(
      order.id,
      order.side,
      order.limit,
      order.quantity,
      order.oid.get,
      status,
      size,
      at,
      order.coin
    )

  private def lookupReply(order: SavedOrder, status: String, size: String): ujson.Value =
    ujson.Obj(
      "status" -> "order",
      "order" -> ujson.Obj(
        "order" -> ujson.Obj(
          "coin" -> order.coin,
          "side" -> sideText(order.side),
          "limitPx" -> order.limit,
          "sz" -> size,
          "oid" -> ujson.Num(order.oid.get.toDouble),
          "timestamp" -> ujson.Num(1699999990000.0),
          "origSz" -> order.quantity,
          "cloid" -> order.id
        ),
        "status" -> status,
        "statusTimestamp" -> ujson.Num(venueTime.toDouble)
      )
    )

  private def listed(order: SavedOrder, size: String): ujson.Value = ujson.Obj(
    "coin" -> order.coin,
    "limitPx" -> order.limit,
    "oid" -> ujson.Num(order.oid.get.toDouble),
    "side" -> sideText(order.side),
    "sz" -> size,
    "timestamp" -> ujson.Num(1699999990000.0)
  )

  private final class Fixture(seeds: Vector[Seed] = Vector.empty, firstReadHeld: Boolean = false):
    private val key = Array.fill[Byte](32)(1)
    val config = TradingConfig(
      sizes = sizes,
      owner = Some(owner),
      agent = Some(OrderSigning.address(key))
    )
    val terms = TradeTerms(config.sizes, config.slippage)
    val clock = new AtomicLong(10000L)
    val directory = tempDirectory("desk-resting")
    @volatile var position = "0"
    @volatile var publishActive = true
    @volatile var infoDown = false
    @volatile var lostOrders = false
    @volatile var orderError = Option.empty[String]
    @volatile var openOrders: Option[ujson.Value] = Some(ujson.Arr())
    @volatile var cancels: String => Option[ujson.Value] = _ => Some(cancelReply("success"))
    @volatile var inGate = false
    // Makes every journal write fail, as a full disk would.
    @volatile var failWrites = false
    @volatile var orders: OrderEvent => Unit = _ => ()
    @volatile var journal: OrderJournal = null
    // Holds the first account read when asked, so a test acts while the position is unknown.
    val firstHold: Hold = if firstReadHeld then new Hold else null
    val accountHold = new AtomicReference[Hold](firstHold)
    val postHold = new AtomicReference[Hold](null)
    val signHold = new AtomicReference[Hold](null)
    val cancelHolds = new ConcurrentHashMap[String, Hold]()
    val statuses = new ConcurrentHashMap[String, ujson.Value]()
    val failing = ConcurrentHashMap.newKeySet[String]()
    val accountReads = new AtomicInteger()
    val openReads = new AtomicInteger()
    val gateEntries = new AtomicInteger()
    private val nextOid = new AtomicLong(77738308L)
    private val record = new Object
    private val postsSeen = ArrayBuffer.empty[(ujson.Value, Boolean)]
    private val lookupsSeen = ArrayBuffer.empty[String]
    private val writersSeen = ArrayBuffer.empty[String]
    def posts: Vector[ujson.Value] = record.synchronized(postsSeen.toVector.map(_._1))
    def postedInGate: Vector[Boolean] = record.synchronized(postsSeen.toVector.map(_._2))
    def orderPosts: Vector[ujson.Value] = posts.filter(_("action")("type").str == "order")
    def cancelPosts: Vector[ujson.Value] =
      posts.filter(_("action")("type").str == "cancelByCloid")
    def lookups: Vector[String] = record.synchronized(lookupsSeen.toVector)
    def writers: Vector[String] = record.synchronized(writersSeen.toVector)

    val seeded: Vector[SavedOrder] =
      if seeds.isEmpty then Vector.empty
      else
        val opened = OrderJournal.open(directory, config.identity)
        try
          seeds.map { s =>
            val plan = OrderPlan(
              s.coin,
              if s.coin == "BTC" then 1 else 0,
              s.side,
              OrderKind.Hard,
              BigDecimal(s.quantity),
              BigDecimal(s.limit),
              BigDecimal(s.limit),
              BigDecimal(0),
              BigDecimal(0),
              None,
              BigDecimal(0),
              s.reduceOnly,
              post = true
            )
            val reserved = opened.reserve(plan, s.holder, clock.get())
            val rested = opened.rest(reserved.id, s.oid)
            s.remaining.foreach(opened.remaining(reserved.id, _))
            rested.copy(remaining = s.remaining.orElse(rested.remaining))
          }
        finally opened.close()

    private def active(coin: String): ujson.Value = ujson.Obj(
      "user" -> owner,
      "coin" -> coin,
      "leverage" -> ujson.Obj("type" -> "cross", "value" -> 5),
      "maxTradeSzs" -> ujson.Arr("100", "100"),
      "availableToTrade" -> ujson.Arr("1000", "1000")
    )

    val exchange: Exchange = new Exchange:
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit = ()
      override def watchTimed(
          coin: String,
          owner: String,
          generation: Long,
          now: () => Long,
          receive: (Option[ujson.Value], Long) => Unit
      ): Unit = if publishActive then receive(Some(active(coin)), now())
      override def watchOrders(receive: OrderEvent => Unit): Unit = orders = receive
      def info(body: ujson.Value): Option[ujson.Value] =
        val kind = body("type").str
        if kind == "clearinghouseState" then
          accountReads.incrementAndGet()
          Option(accountHold.getAndSet(null)).foreach(_.pass())
        if kind == "openOrders" then
          val _ = openReads.incrementAndGet()
        if kind == "orderStatus" then record.synchronized { lookupsSeen += body("oid").str }
        if infoDown then None
        else
          kind match
            case "meta" =>
              Some(
                ujson.Obj(
                  "universe" -> ujson.Arr(
                    ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5),
                    ujson.Obj("name" -> "BTC", "szDecimals" -> 5, "maxLeverage" -> 40)
                  )
                )
              )
            case "userRole" =>
              Some(ujson.Obj("role" -> "agent", "data" -> ujson.Obj("user" -> owner)))
            case "clearinghouseState" =>
              Some(
                ujson.Obj(
                  "marginSummary" -> ujson.Obj("accountValue" -> "1000"),
                  "assetPositions" -> ujson.Arr(
                    ujson.Obj(
                      "position" -> ujson.Obj(
                        "coin" -> "XMR",
                        "szi" -> position,
                        "positionValue" -> "0",
                        "entryPx" -> "546",
                        "liquidationPx" -> "100",
                        "unrealizedPnl" -> "0"
                      )
                    )
                  )
                )
              )
            case "userAbstraction" => Some(ujson.Str("disabled"))
            case "openOrders"      => openOrders
            case "orderStatus"     =>
              val cloid = body("oid").str
              if failing.contains(cloid) then None
              else Some(Option(statuses.get(cloid)).getOrElse(ujson.Obj("status" -> "unknownOid")))
            case _ => None
      def post(body: ujson.Value): Option[ujson.Value] =
        record.synchronized { postsSeen += ((body, inGate)) }
        val action = body("action")
        action("type").str match
          case "order" =>
            Option(postHold.getAndSet(null)).foreach(_.pass())
            if lostOrders then None
            else if orderError.nonEmpty then Some(placedReply(ujson.Obj("error" -> orderError.get)))
            else
              val order = action("orders")(0)
              if order("t")("limit")("tif").str == "Alo" then
                Some(
                  placedReply(
                    ujson.Obj(
                      "resting" -> ujson.Obj("oid" -> ujson.Num(nextOid.getAndIncrement().toDouble))
                    )
                  )
                )
              else
                Some(
                  placedReply(
                    ujson.Obj(
                      "filled" -> ujson.Obj(
                        "totalSz" -> order("s").str,
                        "avgPx" -> "546.09",
                        "oid" -> 9
                      )
                    )
                  )
                )
          case "cancelByCloid" =>
            val cloid = action("cancels")(0)("cloid").str
            Option(cancelHolds.remove(cloid)).foreach(_.pass())
            cancels(cloid)
          case _ => None

    val worker: TradingWorker = new TradingWorker(
      config,
      true,
      "XMR",
      exchange,
      () => key.clone(),
      () => {
        journal = OrderJournal.open(
          directory,
          config.identity,
          beforeReplace = () => {
            if failWrites then throw new java.io.IOException("disk full")
            record.synchronized { writersSeen += Thread.currentThread().getName }
          }
        )
        journal
      },
      () => clock.get(),
      () => clock.get(),
      beforeOrderSign = () => Option(signHold.getAndSet(null)).foreach(_.pass()),
      warmSigning = () => (),
      orderPermission = run => {
        gateEntries.incrementAndGet()
        inGate = true
        try run()
        finally inGate = false
      }
    )
    worker.start()
    book("546.09", "546.10", 1000L)

    def book(bid: String, ask: String, time: Long): Unit =
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level(bid, "20", 1)), Vector(Level(ask, "20", 1)), time),
          true
        )
      )
    def ready(): Unit = Wait.until("worker never became ready")(worker.view.ready)
    // Two more account reads, so at least one whole refresh ran after this call.
    def refreshes(): Unit =
      val start = accountReads.get()
      Wait.until("account reads stalled")(accountReads.get() >= start + 2)
    def priced(side: Side, price: String): OrderIntent =
      OrderIntent(side, OrderKind.Hard, price = Some(BigDecimal(price)))
    def press(intent: OrderIntent): Boolean =
      val current = worker.view
      worker.preview(intent, terms).toOption.exists { plan =>
        val at = clock.get()
        worker.submitReviewed(
          OrderReview(
            intent,
            plan,
            terms,
            at,
            at + TradingWorker.maximumReviewLifetimeMs,
            current.leverage,
            current.marginMode
          )
        )
      }
    def push(values: ujson.Value*): Unit = orders(OrderEvent.Updates(values.toVector, clock.get()))
    def opened(): Unit = orders(OrderEvent.Opened(clock.get()))
    // The journal's resting orders once the worker shows the same ones, so a test never reads
    // the journal between its write and the worker's own update.
    def resting: Vector[SavedOrder] =
      def snapshot = (
        Option(journal).fold(Vector.empty[SavedOrder])(_.resting),
        worker.view.resting.map(o => (o.id, o.remaining))
      )
      def same(s: (Vector[SavedOrder], Vector[(String, BigDecimal)])): Boolean =
        s._1.map(o => (o.id, BigDecimal(o.remaining.get))) == s._2
      val deadline = System.nanoTime() + 5000000000L
      var seen = snapshot
      while !same(seen) && System.nanoTime() < deadline do
        Thread.sleep(2)
        seen = snapshot
      assertEquals(
        seen._2,
        seen._1.map(o => (o.id, BigDecimal(o.remaining.get))),
        "the journal and the worker disagree"
      )
      seen._1
    def holdAccount(): Hold =
      val hold = new Hold
      accountHold.set(hold)
      hold
    def close(): Unit =
      Vector(firstHold, accountHold.get(), postHold.get(), signHold.get()).foreach(h =>
        if h != null then h.open()
      )
      cancelHolds.values().forEach(_.open())
      worker.close()

  private def using(f: Fixture)(body: Fixture => Unit): Unit =
    try body(f)
    finally f.close()

  // Test 9: placing.

  test("a limit press rests, busy clears and a second press at the same price rests again") {
    using(new Fixture()) { f =>
      f.ready()
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("order did not rest")(f.resting.size == 1)
      val first = f.resting.head
      assertEquals(first.oid, Some(77738308L))
      assertEquals(first.remaining, Some("0.183"))
      assertEquals(f.journal.pending, None)
      val sent = f.orderPosts.head("action")("orders")(0)
      assertEquals(sent("t")("limit")("tif").str, "Alo")
      assertEquals(sent("p").str, "546.09")
      assertEquals(sent("s").str, "0.183")
      Wait.until("busy never cleared")(!f.worker.view.pending && f.worker.view.ready)
      val view = f.worker.view
      assertEquals(view.outcome, Some("XMR resting $19.99 at 546.09"))
      assertEquals(
        view.resting,
        Vector(
          RestingOrder(
            first.id,
            "XMR",
            0,
            Side.Buy,
            BigDecimal("546.09"),
            BigDecimal("0.183"),
            BigDecimal("0.183"),
            false,
            first.createdAt
          )
        )
      )
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms).isRight)
      assert(f.worker.preview(OrderIntent(Side.Sell, OrderKind.Hard), f.terms).isRight)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("second order did not rest")(f.resting.size == 2)
      assertNotEquals(f.resting(1).id, first.id)
      assertEquals(f.resting(1).oid, Some(77738309L))
      assertEquals(f.orderPosts.size, 2)
    }
  }

  test("a reserved limit order is saved with its post flag and reloads with it") {
    val directory = tempDirectory("desk-post-flag")
    val identity = "mainnet-" + "1" * 40
    val file = directory.resolve(identity + ".json")
    def plan(post: Boolean): OrderPlan = OrderPlan(
      "XMR",
      0,
      Side.Buy,
      OrderKind.Hard,
      BigDecimal("0.183"),
      BigDecimal("546.09"),
      BigDecimal("546.09"),
      BigDecimal(0),
      BigDecimal(0),
      None,
      BigDecimal(0),
      false,
      post = post
    )
    val first = OrderJournal.open(directory, identity)
    val saved =
      try first.reserve(plan(post = true), owner, 10000L)
      finally first.close()
    assert(saved.post)
    assertEquals(
      ujson.read(java.nio.file.Files.readString(file))("pending")("post"),
      ujson.Bool(true)
    )
    val second = OrderJournal.open(directory, identity)
    try
      assertEquals(second.pending.map(_.post), Some(true))
      assert(second.rest(saved.id, 77738308L).post)
    finally second.close()
    val third = OrderJournal.open(directory, identity)
    try
      assertEquals(third.resting.map(_.post), Vector(true))
      third.settle(saved.id)
      val market = third.reserve(plan(post = false), owner, 20000L)
      assert(!market.post)
      assert(!ujson.read(java.nio.file.Files.readString(file))("pending").obj.contains("post"))
    finally third.close()
  }

  test("a limit press is journalled as posted while in flight and after it rests") {
    using(new Fixture()) { f =>
      f.ready()
      val hold = new Hold
      f.postHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      assertEquals(f.journal.pending.map(_.post), Some(true))
      hold.open()
      Wait.until("order did not rest")(f.resting.size == 1)
      assert(f.resting.head.post)
    }
  }

  test("a market press is sent immediate and its fill is marked from the reply") {
    using(new Fixture()) { f =>
      f.ready()
      assert(f.press(OrderIntent(Side.Buy, OrderKind.Hard)))
      Wait.until("market order did not fill")(f.worker.view.outcome.nonEmpty)
      val sent = f.orderPosts.head("action")("orders")(0)
      assertEquals(sent("t")("limit")("tif").str, "Ioc")
      val outcome = f.worker.view.outcome.get
      assertEquals(outcome, "XMR filled $19.99 at 546.09")
      assert(!outcome.contains("0.183"), outcome)
      assertEquals(
        f.worker.view.fills,
        Vector(Fill("XMR", Side.Buy, 10000L, BigDecimal("546.09"), BigDecimal("0.183")))
      )
      assertEquals(f.resting, Vector.empty)
    }
  }

  test("the twenty first resting order is refused with the cap text") {
    val seeds = (0 until 19).toVector.map(i =>
      Seed(Side.Buy, "0.183", (BigDecimal("540.00") + BigDecimal(i) / 100).toString, 100L + i)
    )
    using(new Fixture(seeds)) { f =>
      f.ready()
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("twentieth order did not rest")(f.resting.size == 20)
      Wait.until("worker never became ready")(f.worker.view.ready)
      assertEquals(
        f.worker.preview(f.priced(Side.Buy, "546.08"), f.terms),
        Left("20 orders rest. Cancel one first")
      )
      assert(!f.press(f.priced(Side.Buy, "546.08")))
      assertEquals(f.orderPosts.size, 1)
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms).isRight)
    }
  }

  test("a press whose reserved quantity covers the position is refused") {
    using(new Fixture(Vector(Seed(Side.Sell, "0.2", "550", 5L)))) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      val covered = Left("Resting orders cover the position")
      assertEquals(f.worker.preview(OrderIntent(Side.Sell, OrderKind.Hard), f.terms), covered)
      assertEquals(f.worker.preview(f.priced(Side.Sell, "548"), f.terms), covered)
      assertEquals(
        f.worker.preview(OrderIntent(Side.Sell, OrderKind.Close), f.terms).map(_.quantity),
        Right(BigDecimal("0.2"))
      )
    }
  }

  test("a crossing price at send time is canceled before sending") {
    using(new Fixture()) { f =>
      f.ready()
      val hold = new Hold
      f.signHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      f.book("546.08", "546.09", 1001L)
      hold.open()
      Wait.until("crossing order was not canceled")(
        f.worker.view.outcome.contains("XMR canceled before sending")
      )
      assertEquals(f.orderPosts.size, 0)
      assertEquals(f.resting, Vector.empty)
    }
  }

  test("a touch move that does not cross between review and send does not refuse") {
    using(new Fixture()) { f =>
      f.ready()
      val hold = new Hold
      f.signHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      f.book("546.00", "546.20", 1001L)
      hold.open()
      Wait.until("order did not rest")(f.resting.size == 1)
      assertEquals(f.worker.view.outcome, Some("XMR resting $19.99 at 546.09"))
    }
  }

  test("an error reply is rejected and a rejected CLOSE points at the resting orders") {
    using(new Fixture(Vector(Seed(Side.Sell, "0.1", "550", 5L, reduceOnly = true)))) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      f.orderError = Some("Post only order would have immediately matched, bbo was 546.09@546.10")
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("rejection not shown")(
        f.worker.view.outcome.contains(
          "XMR order rejected | venue: Post only order would have immediately matched, " +
            "bbo was 546.09@546.10"
        )
      )
      assertEquals(f.journal.pending, None)
      assertEquals(f.resting.size, 1)
      f.ready()
      f.orderError = Some("Reduce only order would increase position")
      assert(f.press(OrderIntent(Side.Sell, OrderKind.Close)))
      Wait.until("close rejection not shown")(
        f.worker.view.outcome.contains(
          "XMR order rejected. Resting reduce orders may block it, press o | venue: " +
            "Reduce only order would increase position"
        )
      )
      assertEquals(f.cancelPosts, Vector.empty)
    }
  }

  test("the CLOSE refusal is shown only for an immediate CLOSE while reduce only orders rest") {
    // A REDUCE ticket whose free remainder is under $10 becomes a posted plan of kind Close.
    using(new Fixture(Vector(Seed(Side.Sell, "0.01", "560", 5L, reduceOnly = true)))) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      val ticket = f.priced(Side.Sell, "546.20")
      val plan = f.worker.preview(ticket, f.terms).toOption.get
      assertEquals((plan.kind, plan.post, plan.reduceOnly), (OrderKind.Close, true, true))
      f.orderError = Some("Post only order would have immediately matched")
      assert(f.press(ticket))
      Wait.until("rejection not shown")(f.worker.view.outcome.nonEmpty)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR order rejected | venue: Post only order would have immediately matched")
      )
    }
    // An immediate CLOSE with an entry resting here and a reduce only order on another market.
    val seeds = Vector(
      Seed(Side.Buy, "0.183", "540", 5L),
      Seed(Side.Sell, "0.001", "99000", 6L, coin = "BTC", reduceOnly = true)
    )
    using(new Fixture(seeds)) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      f.orderError = Some("Reduce only order would increase position")
      assert(f.press(OrderIntent(Side.Sell, OrderKind.Close)))
      Wait.until("rejection not shown")(f.worker.view.outcome.nonEmpty)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR order rejected | venue: Reduce only order would increase position")
      )
    }
  }

  test("a resting reply of a reduce-only order starts no pause") {
    using(new Fixture()) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      assert(f.press(f.priced(Side.Sell, "546.20")))
      Wait.until("reduce did not rest")(f.resting.size == 1)
      assert(f.resting.head.reduceOnly)
      assertEquals(
        f.orderPosts.head("action")("orders")(0)("t")("limit")("tif").str,
        "Alo"
      )
      f.ready()
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms).isRight)
    }
  }

  test("a lost reply moves into the resting list when the lookup answers open") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(
        f.worker.view.message == "XMR waiting for order result"
      )
      val pending = f.journal.pending.get
      f.statuses.put(pending.id, lookupReply(pending.copy(oid = Some(4242L)), "open", "0.1"))
      Wait.until("open order not recovered")(f.resting.size == 1)
      Wait.until("venue size not applied")(f.resting.head.remaining.contains("0.1"))
      assertEquals(f.resting.head.id, pending.id)
      assertEquals(f.resting.head.oid, Some(4242L))
      assertEquals(f.journal.pending, None)
      assertEquals(f.orderPosts.size, 1)
    }
  }

  test("a non-fill update never completes a lost order as filled") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(
        f.worker.view.message == "XMR waiting for order result"
      )
      val pending = f.journal.pending.get
      f.push(
        ws(pending.id, Side.Buy, "546.09", "0.183", 4242L, "open", "0.183", venueTime),
        ws(pending.id, Side.Buy, "546.09", "0.183", 4242L, "canceled", "0.183", venueTime + 1)
      )
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(pending.id))
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.worker.view.fills, Vector.empty)
      f.statuses.put(pending.id, lookupReply(pending.copy(oid = Some(4242L)), "open", "0.183"))
      Wait.until("held cancel not applied once the order rested")(
        f.worker.view.outcome.contains("XMR canceled $19.99 at 546.09")
      )
      assertEquals(f.journal.pending, None)
      assertEquals(f.resting, Vector.empty)
      assertEquals(f.worker.view.fills, Vector.empty)
    }
  }

  test("a lost reply fill of a reducing order starts the pause") {
    using(new Fixture()) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Sell, "546.20")))
      Wait.until("lost reply not kept")(
        f.worker.view.message == "XMR waiting for order result"
      )
      val pending = f.journal.pending.get
      f.push(ws(pending.id, Side.Sell, "546.2", "0.2", 4242L, "filled", "0", venueTime))
      Wait.until("lost reply fill not applied")(f.journal.pending.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR filled $21.85 at 546.2"))
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
  }

  test("the cap counts resting orders on every market") {
    val seeds = (0 until 20).toVector.map(i =>
      Seed(Side.Buy, "0.001", (90000 + i).toString, 100L + i, coin = "BTC")
    )
    using(new Fixture(seeds)) { f =>
      f.ready()
      assertEquals(
        f.worker.preview(f.priced(Side.Buy, "546.08"), f.terms),
        Left("20 orders rest. Cancel one first")
      )
      assert(!f.press(f.priced(Side.Buy, "546.08")))
      assertEquals(f.orderPosts, Vector.empty)
    }
  }

  test("orders on another market neither reserve this position nor take its sign") {
    val seeds = Vector(
      Seed(Side.Sell, "0.2", "99000", 5L, coin = "BTC"),
      Seed(Side.Buy, "0.001", "90000", 6L, coin = "BTC")
    )
    using(new Fixture(seeds)) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      assertEquals(
        f.worker.preview(OrderIntent(Side.Sell, OrderKind.Hard), f.terms).map(_.quantity),
        Right(BigDecimal("0.2"))
      )
      f.push(ws(f.seeded(1), "filled", "0", venueTime))
      Wait.until("BTC fill not settled")(f.resting.size == 1)
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
  }

  test("view stale follows the active data age") {
    using(new Fixture()) { f =>
      f.ready()
      assert(!f.worker.view.stale)
      f.publishActive = false
      f.clock.addAndGet(f.config.positionAgeMs + 1)
      Wait.until("stale active data not shown")(f.worker.view.stale)
      f.publishActive = true
      Wait.until("fresh active data not shown")(!f.worker.view.stale)
    }
  }

  test("a fatal worker dims the amounts and the status row keeps the restart text") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      assert(!f.worker.view.stale)
      f.failWrites = true
      f.push(ws(f.seeded.head, "open", "0.15", venueTime))
      Wait.until("storage failure not shown")(
        f.worker.view.block.contains(TradingWorker.storageFailedMessage)
      )
      // Account reads and active data go on, but fills are no longer applied.
      f.refreshes()
      val view = f.worker.view
      assert(view.stale, "a fatal worker drew its orders as current")
      val frame = Workspace.draw(
        Snapshot.initial("XMR", 0L),
        view,
        PanelView(mode = "LIVE"),
        100,
        30,
        0L,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      assertEquals(
        Chart.plain(frame.lines(30 - 12 + 3)).take(38).trim,
        TradingWorker.storageFailedMessage
      )
    }
  }

  // Test 10: order updates.

  test("a partial fill updates the remaining size and the text and marks the fill") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      val order = f.seeded.head
      f.push(ws(order, "open", "0.15", venueTime))
      Wait.until("partial fill not applied")(f.resting.head.remaining.contains("0.15"))
      val view = f.worker.view
      assertEquals(view.outcome, Some("XMR part filled $5.46 of $21.84 at 546.09"))
      assertEquals(
        view.fills,
        Vector(Fill("XMR", Side.Buy, venueTime, BigDecimal("546.09"), BigDecimal("0.05")))
      )
      assertEquals(view.resting.head.remaining, BigDecimal("0.15"))
      assertEquals(view.resting.head.original, BigDecimal("0.2"))
      val outcomeId = view.outcomeId
      f.push(ws(order, "open", "0.15", venueTime + 1), ws(order, "open", "0.18", venueTime + 2))
      f.refreshes()
      assertEquals(f.resting.head.remaining, Some("0.15"))
      assertEquals(f.worker.view.outcomeId, outcomeId)
      assertEquals(f.worker.view.fills.size, 1)
      f.push(ws(order, "canceled", "0.15", venueTime + 3))
      Wait.until("cancel not applied")(f.resting.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR canceled $16.38 at 546.09"))
      assertEquals(f.worker.view.fills.size, 1)
    }
  }

  test("a fill settles and starts the pause only when it reduced") {
    val seeds = Vector(Seed(Side.Buy, "0.1", "540", 5L), Seed(Side.Sell, "0.1", "550", 6L))
    using(new Fixture(seeds)) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      val opening = OrderIntent(Side.Buy, OrderKind.Hard)
      f.push(ws(f.seeded(0), "filled", "0", venueTime))
      Wait.until("buy fill not settled")(f.resting.size == 1)
      assertEquals(f.worker.view.outcome, Some("XMR filled $10.80 at 540"))
      assert(f.worker.preview(opening, f.terms).isRight, "an entry fill started the pause")
      f.push(ws(f.seeded(1), "filled", "0", venueTime + 1))
      Wait.until("sell fill not settled")(f.resting.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR filled $11.00 at 550"))
      assertEquals(
        f.worker.view.fills,
        Vector(
          Fill("XMR", Side.Buy, venueTime, BigDecimal("540"), BigDecimal("0.1")),
          Fill("XMR", Side.Sell, venueTime + 1, BigDecimal("550"), BigDecimal("0.1"))
        )
      )
      assertEquals(f.worker.preview(opening, f.terms), Left(TradingWorker.openingPausedMessage))
      f.clock.addAndGet(TradingWorker.openingPauseMs)
      f.book("546.09", "546.10", 3000L)
      f.ready()
      assert(f.worker.preview(opening, f.terms).isRight)
    }
  }

  test("a reducing partial fill refuses an opening preview for 2 seconds") {
    // Long 0.2 with an unflagged sell of 0.5 resting, which reads flip. A sweep fills 0.3 of it.
    using(new Fixture(Vector(Seed(Side.Sell, "0.5", "550", 5L)))) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      f.push(ws(f.seeded.head, "open", "0.2", venueTime))
      Wait.until("partial fill not applied")(f.resting.head.remaining.contains("0.2"))
      val paused = Left(TradingWorker.openingPausedMessage)
      assertEquals(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms), paused)
      // The account now reads short 0.1, so a sell opens.
      f.position = "-0.1"
      Wait.until("short position not read")(
        f.worker.view.position.contains(BigDecimal("-0.1"))
      )
      val opening = OrderIntent(Side.Sell, OrderKind.Hard)
      assertEquals(f.worker.preview(opening, f.terms), paused)
      f.clock.addAndGet(TradingWorker.openingPauseMs - 1)
      f.book("546.09", "546.10", 3000L)
      assertEquals(f.worker.preview(opening, f.terms), paused)
      f.clock.addAndGet(1L)
      f.book("546.09", "546.10", 3001L)
      f.ready()
      assert(f.worker.preview(opening, f.terms).isRight)
    }
  }

  test("a lookup or an open orders read that shrinks a facing order starts the pause") {
    val seed = Vector(Seed(Side.Sell, "0.5", "550", 5L))
    using(new Fixture(seed, firstReadHeld = true)) { f =>
      f.position = "0.2"
      f.statuses.put(f.seeded.head.id, lookupReply(f.seeded.head, "open", "0.2"))
      f.firstHold.open()
      Wait.until("looked up size not applied")(
        f.resting.headOption.exists(_.remaining.contains("0.2"))
      )
      f.ready()
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
    using(new Fixture(seed)) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms).isRight)
      f.openOrders = Some(ujson.Arr(listed(f.seeded.head, "0.2")))
      f.opened()
      Wait.until("lower open size not applied")(f.resting.head.remaining.contains("0.2"))
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
  }

  test("venue cancels settle with their own texts") {
    val seeds = Vector(
      Seed(Side.Buy, "0.183", "546.09", 5L),
      Seed(Side.Sell, "0.2", "546.09", 6L, remaining = Some("0.15"), reduceOnly = true)
    )
    using(new Fixture(seeds)) { f =>
      f.ready()
      f.push(ws(f.seeded(0), "selfTradeCanceled", "0.183", venueTime))
      Wait.until("self-trade cancel not applied")(f.resting.size == 1)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR resting buy $19.99 at 546.09 canceled by self-trade prevention")
      )
      f.push(ws(f.seeded(1), "reduceOnlyCanceled", "0.15", venueTime))
      Wait.until("reduce-only cancel not applied")(f.resting.isEmpty)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR canceled by the venue $16.38 at 546.09 (reduceOnlyCanceled)")
      )
      assertEquals(f.worker.view.fills, Vector.empty)
    }
  }

  test("two settles in one batch reach the result log as two lines and the status row in turn") {
    val seeds = Vector(
      Seed(Side.Buy, "0.183", "546.09", 5L),
      Seed(Side.Sell, "0.2", "546.09", 6L, remaining = Some("0.15"), reduceOnly = true)
    )
    using(new Fixture(seeds)) { f =>
      f.ready()
      // The status row of each result as the controller records it, drawn on a ready panel.
      val drawnReady = TradingView("XMR", message = "Ready", ready = true)
      var log = Vector.empty[(Long, String)]
      var rows = Vector.empty[String]
      lazy val controller: DeskController = new DeskController(
        PanelView(mode = "LIVE"),
        () => f.clock.get(),
        f.worker.preview,
        f.worker.submitReviewed,
        _ => Left("no mode switch"),
        result = (number, text) => {
          log :+= ((number, text))
          val frame = Workspace.draw(
            Snapshot.initial("XMR", 0L),
            drawnReady,
            controller.panel,
            100,
            30,
            0L,
            java.time.ZoneOffset.UTC,
            Style.mono
          )
          rows :+= Chart.plain(frame.lines(30 - 12 + 3)).take(38).trim
        }
      )
      controller.orders(f.worker.view)
      f.push(
        ws(f.seeded(0), "selfTradeCanceled", "0.183", venueTime),
        ws(f.seeded(1), "reduceOnlyCanceled", "0.15", venueTime)
      )
      Wait.until("both settles not applied")(f.resting.isEmpty)
      controller.orders(f.worker.view)
      controller.orders(f.worker.view)
      assertEquals(
        log,
        Vector(
          (1L, "XMR resting buy $19.99 at 546.09 canceled by self-trade prevention"),
          (2L, "XMR canceled by the venue $16.38 at 546.09 (reduceOnlyCanceled)")
        )
      )
      assertEquals(rows, Vector("Canceled $19.99 #1", "Canceled $16.38 #2"))
    }
  }

  test("the worker keeps the last 50 outcomes with rising ids") {
    using(new Fixture(Vector(Seed(Side.Buy, "1", "546.09", 5L)))) { f =>
      f.ready()
      val order = f.seeded.head
      f.push((1 to 60).map(i => ws(order, "open", (1 - BigDecimal(i) / 1000).toString, i))*)
      Wait.until("part fills not applied")(f.resting.head.remaining.contains("0.94"))
      val view = f.worker.view
      assertEquals(view.outcomes.size, 50)
      assertEquals(view.outcomes.map(_._1), view.outcomes.map(_._1).sorted.distinct)
      assertEquals(view.outcomes.last, (view.outcomeId, view.outcome.get))
      assertEquals(view.outcomes.head._2, "XMR part filled $0.11 of $109.22 at 546.09")
    }
  }

  test("an update for a finished order is dropped silently and an unknown one is counted") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      assert(f.press(OrderIntent(Side.Buy, OrderKind.Hard)))
      Wait.until("market order did not fill")(f.worker.view.outcome.nonEmpty)
      val cloid = f.orderPosts.head("action")("orders")(0)("c").str
      f.push(ws(cloid, Side.Buy, "546.64", "0.183", 9L, "filled", "0"))
      val order = f.seeded.head
      f.push(ws(order, "canceled", "0.2", venueTime))
      Wait.until("cancel not applied")(f.resting.isEmpty)
      f.push(ws(order, "canceled", "0.2", venueTime + 1))
      f.refreshes()
      assertEquals(f.worker.view.ignoredOrderUpdates, 0L)
      f.push(ws("0x" + "9" * 32, Side.Buy, "500", "1", 44L, "filled", "0"))
      Wait.until("foreign update not counted")(f.worker.view.ignoredOrderUpdates == 1L)
      f.push(ujson.Obj("order" -> ujson.Obj("coin" -> "XMR"), "status" -> "open"))
      Wait.until("unreadable update not counted")(f.worker.view.ignoredOrderUpdates == 2L)
    }
  }

  test("an update or a lookup for another order never changes a resting one") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)), firstReadHeld = true)) { f =>
      val order = f.seeded.head
      f.statuses.put(order.id, lookupReply(order.copy(quantity = "0.3"), "filled", "0"))
      f.firstHold.open()
      Wait.until("start lookup missing")(f.lookups.size == 1)
      f.ready()
      f.push(
        ws(order.copy(oid = Some(6L)), "canceled", "0.2", venueTime),
        ws(order.copy(coin = "BTC"), "canceled", "0.2", venueTime)
      )
      Wait.until("mismatched updates not counted")(f.worker.view.ignoredOrderUpdates == 2L)
      f.refreshes()
      assertEquals(f.resting.map(o => (o.id, o.remaining)), Vector((order.id, Some("0.2"))))
      assertEquals(f.worker.view.outcome, None)
    }
  }

  test("a full update queue asks for an open orders read") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      f.refreshes()
      assertEquals(f.openReads.get(), 0)
      val order = f.seeded.head
      f.push((1 to 257).map(i => ws(order, "open", "0.2", venueTime + i))*)
      Wait.until("no open orders read after the queue filled")(f.openReads.get() == 1)
      assertEquals(f.resting.head.remaining, Some("0.2"))
      assertEquals(f.worker.view.ignoredOrderUpdates, 0L)
    }
  }

  test("a full held list asks for an open orders read") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "540", 5L)))) { f =>
      f.ready()
      f.refreshes()
      assertEquals(f.openReads.get(), 0)
      val hold = new Hold
      f.postHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      val cloid = f.orderPosts.head("action")("orders")(0)("c").str
      def update(i: Int): ujson.Value =
        ws(cloid, Side.Buy, "546.09", "0.183", 77738308L, "open", "0.183", venueTime + i)
      f.push((1 to 256).map(update)*)
      f.refreshes()
      assertEquals(f.openReads.get(), 0)
      f.push(update(257))
      Wait.until("no open orders read after the held list filled")(f.openReads.get() == 1)
      hold.open()
      Wait.until("order did not rest")(f.resting.size == 2)
    }
  }

  test("an update for the pending order is held until the resting reply, then applied") {
    using(new Fixture()) { f =>
      f.ready()
      val hold = new Hold
      f.postHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      val cloid = f.orderPosts.head("action")("orders")(0)("c").str
      f.push(ws(cloid, Side.Buy, "546.09", "0.183", 77738308L, "open", "0.1", venueTime))
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(cloid))
      assertEquals(f.resting, Vector.empty)
      assertEquals(f.worker.view.fills, Vector.empty)
      assertEquals(f.worker.view.ignoredOrderUpdates, 0L)
      hold.open()
      Wait.until("held partial fill not applied")(
        f.resting.headOption.exists(_.remaining.contains("0.1"))
      )
      assertEquals(f.worker.view.outcome, Some("XMR part filled $9.07 of $19.99 at 546.09"))
      assertEquals(
        f.worker.view.fills,
        Vector(Fill("XMR", Side.Buy, venueTime, BigDecimal("546.09"), BigDecimal("0.083")))
      )
    }
  }

  test("a held fill completes a pending order whose reply was lost, never while sending") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      val hold = new Hold
      f.postHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      val cloid = f.orderPosts.head("action")("orders")(0)("c").str
      f.push(ws(cloid, Side.Buy, "546.09", "0.183", 77738308L, "filled", "0", venueTime))
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(cloid))
      assertEquals(f.worker.view.outcome, None)
      assert(f.worker.view.pending)
      hold.open()
      Wait.until("held fill did not complete the lost order")(f.journal.pending.isEmpty)
      assertEquals(f.resting, Vector.empty)
      assertEquals(f.worker.view.outcome, Some("XMR filled $19.99 at 546.09"))
      assertEquals(
        f.worker.view.fills,
        Vector(Fill("XMR", Side.Buy, venueTime, BigDecimal("546.09"), BigDecimal("0.183")))
      )
      assert(!f.worker.view.block.contains(TradingWorker.storageFailedMessage))
      Wait.until("busy never cleared")(!f.worker.view.pending)
    }
  }

  test("updates for another order apply while one is held and a settle leaves busy alone") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "540", 5L)))) { f =>
      f.ready()
      val hold = new Hold
      f.postHold.set(hold)
      assert(f.press(f.priced(Side.Buy, "546.09")))
      hold.await()
      val cloid = f.orderPosts.head("action")("orders")(0)("c").str
      f.push(
        ws(cloid, Side.Buy, "546.09", "0.183", 77738308L, "open", "0.1", venueTime),
        ws(f.seeded.head, "canceled", "0.2", venueTime)
      )
      Wait.until("update behind the held one not applied")(f.resting.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR canceled $21.60 at 540"))
      assert(f.worker.view.pending, "a settle cleared busy while an order was in flight")
      hold.open()
      Wait.until("order did not rest")(f.resting.size == 1)
      Wait.until("busy never cleared")(!f.worker.view.pending)
      f.push(ws(f.resting.head, "canceled", "0.1", venueTime + 1))
      Wait.until("cancel not applied")(f.resting.isEmpty)
      f.refreshes()
      assert(!f.worker.view.pending, "a settle set busy with nothing in flight")
    }
  }

  test("updates apply on the account thread before any network call") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "540", 5L), Seed(Side.Buy, "0.2", "541", 6L))
    using(new Fixture(seeds)) { f =>
      f.ready()
      val hold = f.holdAccount()
      hold.await()
      f.push(ws(f.seeded(0), "canceled", "0.2", venueTime))
      assertEquals(f.resting.size, 2, "an update was applied on the socket thread")
      hold.open()
      Wait.until("update not applied")(f.resting.size == 1)
      // Every read fails from here, the cached metadata and role included.
      f.infoDown = true
      f.clock.addAndGet(60001L)
      f.push(ws(f.seeded(1), "canceled", "0.2", venueTime))
      Wait.until("an info outage held the update")(f.resting.isEmpty)
      assertEquals(f.writers.takeRight(2), Vector("account", "account"))
    }
  }

  test("a settle resets the account and a read that started before it cannot restore it") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "540", 5L), Seed(Side.Buy, "0.2", "541", 6L))
    using(new Fixture(seeds)) { f =>
      f.ready()
      val first = f.holdAccount()
      first.await()
      f.push(ws(f.seeded(0), "canceled", "0.2", venueTime))
      val second = f.holdAccount()
      first.open()
      second.await()
      assertEquals(f.resting.size, 1)
      assert(!f.worker.view.ready, "an update settle left the account fresh")
      assertEquals(f.worker.view.block, Some("account data stale"))
      assert(!f.worker.view.pending)
      second.open()
      f.ready()
      val third = f.holdAccount()
      third.await()
      assert(f.worker.cancel(Vector(f.seeded(1).id)))
      Wait.until("cancel not settled")(f.resting.isEmpty)
      val fourth = f.holdAccount()
      third.open()
      fourth.await()
      assert(!f.worker.view.ready, "a read from before the settle restored the account")
      assertEquals(f.worker.view.block, Some("account data stale"))
      fourth.open()
      f.ready()
    }
  }

  test("a press queued before a resting fill is canceled before sending") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.1", "540", 5L)))) { f =>
      f.ready()
      val hold = new Hold
      f.signHold.set(hold)
      assert(f.press(OrderIntent(Side.Buy, OrderKind.Hard)))
      hold.await()
      f.publishActive = false
      f.push(ws(f.seeded.head, "filled", "0", venueTime))
      Wait.until("fill not settled")(f.resting.isEmpty)
      hold.open()
      Wait.until("press was not canceled")(
        f.worker.view.outcome.contains("XMR canceled before sending")
      )
      assertEquals(f.orderPosts, Vector.empty)
    }
  }

  test("open orders lower the remaining size and never raise it") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "546.09", 5L, remaining = Some("0.15")))
    using(new Fixture(seeds)) { f =>
      f.ready()
      val order = f.seeded.head
      f.openOrders = Some(ujson.Arr(listed(order, "0.2")))
      f.opened()
      Wait.until("open orders not read")(f.openReads.get() == 1)
      f.refreshes()
      assertEquals(f.resting.head.remaining, Some("0.15"))
      f.openOrders = Some(ujson.Arr(listed(order, "0.1")))
      f.opened()
      Wait.until("lower open size not applied")(f.resting.head.remaining.contains("0.1"))
      assertEquals(f.worker.view.outcome, Some("XMR part filled $5.46 of $21.84 at 546.09"))
      assertEquals(f.openReads.get(), 2)
    }
  }

  test("the pause follows the position stamped on the update") {
    using(new Fixture(Vector(Seed(Side.Sell, "0.2", "550", 5L)))) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      val hold = f.holdAccount()
      hold.await()
      f.position = "0"
      f.push(ws(f.seeded.head, "filled", "0", venueTime))
      hold.open()
      Wait.until("fill not settled")(f.resting.isEmpty)
      Wait.until("flat position not read")(f.worker.view.position.contains(BigDecimal(0)))
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
  }

  test("an update with no stamped position starts the pause") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.1", "540", 5L)), firstReadHeld = true)) { f =>
      val hold = f.firstHold
      hold.await()
      assertEquals(f.worker.view.position, None)
      f.push(ws(f.seeded.head, "filled", "0", venueTime))
      hold.open()
      Wait.until("fill not settled")(f.resting.isEmpty)
      f.ready()
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
  }

  test("an update with a flat stamped position starts no pause") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.1", "540", 5L)))) { f =>
      f.ready()
      f.push(ws(f.seeded.head, "filled", "0", venueTime))
      Wait.until("fill not settled")(f.resting.isEmpty)
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms).isRight)
    }
  }

  test("fills are kept per market, at most 200 each") {
    val seeds =
      Vector(Seed(Side.Buy, "1", "546.09", 5L), Seed(Side.Buy, "0.001", "99000", 6L, coin = "BTC"))
    using(new Fixture(seeds)) { f =>
      f.ready()
      val order = f.seeded(0)
      val updates = (1 to 201).map(i =>
        ws(order, "open", (BigDecimal(1) - BigDecimal(i) / 1000).toString, 1000L + i)
      )
      f.push(updates*)
      Wait.until("partial fills not applied", 30)(f.resting.head.remaining.contains("0.799"))
      val xmr = f.worker.view.fills.filter(_.coin == "XMR")
      assertEquals(xmr.size, 200)
      assertEquals(xmr.head.time, 1002L)
      assertEquals(xmr.last.time, 1201L)
      f.push(ws(f.seeded(1), "filled", "0", venueTime))
      Wait.until("BTC fill not applied")(f.worker.view.fills.exists(_.coin == "BTC"))
      assertEquals(f.worker.view.fills.count(_.coin == "XMR"), 200)
      assertEquals(f.worker.view.fills.count(_.coin == "BTC"), 1)
    }
  }

  // Test 11: recovery.

  test(
    "a restart looks each resting order up once, keeps the open one and settles the filled one"
  ) {
    val seeds = Vector(Seed(Side.Buy, "0.2", "546.09", 5L), Seed(Side.Buy, "0.2", "540", 6L))
    using(new Fixture(seeds, firstReadHeld = true)) { f =>
      f.statuses.put(f.seeded(0).id, lookupReply(f.seeded(0), "open", "0.15"))
      f.statuses.put(f.seeded(1).id, lookupReply(f.seeded(1), "filled", "0"))
      f.firstHold.open()
      Wait.until("filled order not settled")(f.resting.size == 1)
      Wait.until("open order not kept with its size")(
        f.resting.head.remaining.contains("0.15")
      )
      assertEquals(f.resting.head.id, f.seeded(0).id)
      f.refreshes()
      assertEquals(f.lookups.sorted, Vector(f.seeded(0).id, f.seeded(1).id).sorted)
      assert(
        f.worker.view.outcome.exists(_.startsWith("XMR ")),
        f.worker.view.outcome.toString
      )
    }
  }

  test("an open socket reads open orders once and looks up only the missing order") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "546.09", 5L), Seed(Side.Buy, "0.2", "540", 6L))
    using(new Fixture(seeds, firstReadHeld = true)) { f =>
      f.seeded.foreach(o => f.statuses.put(o.id, lookupReply(o, "open", "0.2")))
      f.firstHold.open()
      Wait.until("start lookups missing")(f.lookups.size == 2)
      f.openOrders = Some(ujson.Arr(listed(f.seeded(0), "0.2")))
      f.opened()
      Wait.until("missing order not looked up")(f.lookups.size == 3)
      assertEquals(f.lookups.last, f.seeded(1).id)
      f.refreshes()
      assertEquals(f.openReads.get(), 1)
      assertEquals(f.lookups.size, 3)
      assertEquals(f.resting.size, 2)
    }
  }

  test("the minute read of open orders runs only while orders rest") {
    using(new Fixture()) { f =>
      f.ready()
      f.opened()
      f.refreshes()
      f.clock.addAndGet(60001L)
      f.refreshes()
      f.clock.addAndGet(60001L)
      f.refreshes()
      assertEquals(f.openReads.get(), 0)
    }
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.statuses.put(f.seeded.head.id, lookupReply(f.seeded.head, "open", "0.2"))
      f.ready()
      f.refreshes()
      assertEquals(f.openReads.get(), 0)
      f.openOrders = Some(ujson.Arr(listed(f.seeded.head, "0.2")))
      f.clock.addAndGet(30000L)
      f.refreshes()
      assertEquals(f.openReads.get(), 0)
      f.clock.addAndGet(30001L)
      Wait.until("minute read missing")(f.openReads.get() == 1)
      f.refreshes()
      assertEquals(f.openReads.get(), 1)
    }
  }

  test("a failed lookup retries no sooner than ten seconds") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.failing.add(f.seeded.head.id)
      Wait.until("first lookup missing")(f.lookups.size == 1)
      f.clock.addAndGet(9999L)
      f.refreshes()
      assertEquals(f.lookups.size, 1)
      f.clock.addAndGet(1L)
      Wait.until("retry missing")(f.lookups.size == 2)
      assertEquals(f.resting.size, 1)
    }
  }

  test("three unknown answers after the margin settle a missing order as not found") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      Wait.until("start lookup missing")(f.lookups.size == 1)
      f.opened()
      Wait.until("open orders not read")(f.openReads.get() == 1)
      f.clock.set(20000L)
      Wait.until("second lookup missing")(f.lookups.size == 2)
      f.clock.set(70001L)
      Wait.until("third lookup missing")(f.lookups.size == 3)
      f.clock.set(80001L)
      Wait.until("fourth lookup missing")(f.lookups.size == 4)
      f.refreshes()
      assertEquals(f.resting.size, 1, "two unknown answers after the margin settled the order")
      assertEquals(f.worker.view.notFoundOrders, 0L)
      f.clock.set(90001L)
      Wait.until("order not settled as not found")(f.resting.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR $21.84 at 546.09 not found at the venue"))
      assertEquals(f.worker.view.notFoundOrders, 1L)
      // An order settled as not found is not remembered, so its later fill is named and counted.
      f.push(ws(f.seeded.head, "filled", "0", venueTime))
      Wait.until("late fill of a not found order not counted")(
        f.worker.view.ignoredOrderUpdates == 1L
      )
      assertEquals(f.worker.view.outcome, Some("XMR filled $21.84 at 546.09 after release"))
    }
  }

  // Test 11b: a lost post only order.

  // Presses a limit buy at 546.09 whose reply is lost and walks it to its release: past the
  // margin, an open orders read that misses it, then three unknownOid answers ten seconds apart.
  // The open orders read lists only the given orders.
  private def releaseLost(
      f: Fixture,
      listedOrders: Vector[ujson.Value] = Vector.empty
  ): SavedOrder =
    f.ready()
    f.lostOrders = true
    assert(f.press(f.priced(Side.Buy, "546.09")))
    Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
    val pending = f.journal.pending.get
    assert(pending.post)
    // A refresh sees the order before time moves on, as one does every second.
    f.refreshes()
    f.openOrders = Some(ujson.Arr.from(listedOrders))
    // Past the signed expiry plus the margin and 65 s after the reservation.
    f.clock.set(pending.expiresAfter + Exchange.ExpiryMarginMs + 1)
    Wait.until("open orders not read past the margin")(f.openReads.get() >= 1)
    f.refreshes()
    f.refreshes()
    assertEquals(f.journal.pending.map(_.id), Some(pending.id), "released on one unknownOid")
    f.clock.addAndGet(10000L)
    f.refreshes()
    assertEquals(f.journal.pending.map(_.id), Some(pending.id), "released on two unknownOid")
    f.clock.addAndGet(10000L)
    Wait.until("lost post only order not released")(f.journal.pending.isEmpty)
    assertEquals(f.worker.view.outcome, Some("XMR not placed"))
    pending

  test("a lost post only order waits for the resting bound and its late fill is named") {
    // A Desk order resting at the same price and size is not the lost one, so its listing
    // does not hold the lost order back.
    using(new Fixture(Vector(Seed(Side.Buy, "0.183", "546.09", 5L)))) { f =>
      val pending = releaseLost(f, Vector(listed(f.seeded.head, "0.183")))
      assertEquals(f.resting.map(_.id), Vector(f.seeded.head.id))
      assertEquals(f.worker.view.ignoredOrderUpdates, 0L)
      f.push(ws(pending.id, Side.Buy, "546.09", "0.183", 4242L, "filled", "0", venueTime))
      Wait.until("late fill not counted")(f.worker.view.ignoredOrderUpdates == 1L)
      assertEquals(f.worker.view.outcome, Some("XMR filled $19.99 at 546.09 after release"))
      assert(
        f.worker.view.fills
          .contains(Fill("XMR", Side.Buy, venueTime, BigDecimal("546.09"), BigDecimal("0.183")))
      )
      assertEquals(f.resting.map(_.id), Vector(f.seeded.head.id))
    }
  }

  test("an open orders read listing a lost post only order holds its release") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
      val pending = f.journal.pending.get
      f.refreshes()
      // A partly filled order lists a smaller size, and still holds the release.
      f.openOrders = Some(ujson.Arr(listed(pending.copy(oid = Some(4242L)), "0.1")))
      f.clock.set(pending.expiresAfter + Exchange.ExpiryMarginMs + 1)
      Wait.until("open orders not read past the margin")(f.openReads.get() >= 1)
      for _ <- 1 to 4 do
        f.refreshes()
        f.clock.addAndGet(10000L)
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(pending.id))
    }
  }

  test("an open orders read before the margin does not count toward the release") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
      val pending = f.journal.pending.get
      f.refreshes()
      // A socket open reads the open orders just before the margin. The order is missing.
      f.clock.set(70000L)
      f.opened()
      Wait.until("open orders not read on the socket open")(f.openReads.get() == 1)
      // Past the margin the read runs again at once and lists the order, so nothing counts.
      f.openOrders = Some(ujson.Arr(listed(pending.copy(oid = Some(4242L)), "0.183")))
      f.clock.set(75001L)
      Wait.until("open orders not read at the margin")(f.openReads.get() == 2)
      f.refreshes()
      f.clock.set(85001L)
      f.refreshes()
      f.clock.set(95001L)
      f.refreshes()
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(pending.id))
      // The next minute read misses it, and three spaced answers then release it.
      f.openOrders = Some(ujson.Arr())
      f.clock.set(135002L)
      Wait.until("minute read missing")(f.openReads.get() == 3)
      f.refreshes()
      f.clock.set(145002L)
      f.refreshes()
      f.clock.set(155002L)
      Wait.until("lost post only order not released")(f.journal.pending.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR not placed"))
    }
  }

  test("a lost posted order is read at once past the margin and released 85 s after sending") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
      val pending = f.journal.pending.get
      assertEquals(pending.createdAt, 10000L)
      f.refreshes()
      // The minute read lands just before the margin and cannot count.
      f.clock.set(70001L)
      Wait.until("minute read missing")(f.openReads.get() == 1)
      f.refreshes()
      // Past the margin the next read runs at once, not a minute later.
      f.clock.set(75001L)
      Wait.until("open orders not read at the margin")(f.openReads.get() == 2)
      f.refreshes()
      f.clock.set(85001L)
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(pending.id))
      assert(f.worker.view.pending, "trading was free while the order was pending")
      f.clock.set(95001L)
      Wait.until("lost post only order not released 85 s after sending")(
        f.journal.pending.isEmpty
      )
      assertEquals(f.worker.view.outcome, Some("XMR not placed"))
    }
  }

  test("a late open update for a released order rests again in the list and the journal") {
    using(new Fixture()) { f =>
      val pending = releaseLost(f)
      f.push(ws(pending.id, Side.Buy, "546.09", "0.183", 4242L, "open", "0.15", venueTime))
      Wait.until("late open order not adopted")(f.resting.size == 1)
      val adopted = f.resting.head
      assertEquals(
        (adopted.id, adopted.oid, adopted.remaining, adopted.post),
        (pending.id, Some(4242L), Some("0.15"), true)
      )
      assertEquals(f.worker.view.outcome, Some("XMR resting $16.38 at 546.09 after release"))
      assertEquals(f.worker.view.ignoredOrderUpdates, 1L)
      // From here its updates apply as any resting order's.
      f.push(ws(adopted, "filled", "0", venueTime + 1))
      Wait.until("adopted order fill not applied")(f.resting.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR filled $16.38 at 546.09"))
      assertEquals(f.worker.view.ignoredOrderUpdates, 1L)
    }
    // A venue size above the order's own never shows more than Desk placed.
    using(new Fixture()) { f =>
      val pending = releaseLost(f)
      f.push(ws(pending.id, Side.Buy, "546.09", "0.183", 4242L, "open", "0.2", venueTime))
      Wait.until("late open order not adopted")(f.resting.size == 1)
      assertEquals(f.resting.head.remaining, Some("0.183"))
      assertEquals(f.worker.view.outcome, Some("XMR resting $19.99 at 546.09 after release"))
    }
  }

  test("a resting order of another owner blocks live") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "546.09", 5L, holder = other))
    using(new Fixture(seeds)) { f =>
      Wait.until("owner mismatch not shown")(
        f.worker.view.block.contains(TradingWorker.savedOwnerMismatchMessage)
      )
      f.refreshes()
      assert(!f.worker.view.ready)
      assertEquals(f.lookups, Vector.empty)
      assert(!f.worker.cancel(Vector(f.seeded.head.id)))
      assertEquals(f.cancelPosts, Vector.empty)
      assertEquals(f.resting.size, 1)
    }
  }

  // Test 12: cancelling.

  test("cancel sends one cancel per id in order inside one request, each in the gate") {
    val seeds = Vector(
      Seed(Side.Buy, "0.2", "540", 5L),
      Seed(Side.Buy, "0.15", "541", 6L),
      Seed(Side.Buy, "0.2", "542", 7L, coin = "BTC")
    )
    using(new Fixture(seeds)) { f =>
      val Vector(a, b, c) = f.seeded
      f.ready()
      f.statuses.put(c.id, lookupReply(c, "canceled", "0.2"))
      f.cancels =
        cloid => if cloid == c.id then Some(cancelReply(gone)) else Some(cancelReply("success"))
      val hold = new Hold
      f.cancelHolds.put(b.id, hold)
      val entries = f.gateEntries.get()
      Wait.until("start lookups missing")(f.lookups.size == 3)
      assert(f.worker.cancel(Vector(a.id, b.id, c.id)))
      hold.await()
      assertEquals(f.worker.view.message, "XMR canceling 2 of 3")
      assertEquals(f.worker.view.resting.filter(_.cancelling).map(_.id), Vector(b.id))
      hold.open()
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      val sent = f.cancelPosts.map(_("action"))
      assertEquals(sent.map(_("cancels")(0)("cloid").str), Vector(a.id, b.id, c.id))
      assertEquals(sent.map(_("cancels")(0)("asset").num.toInt), Vector(0, 0, 1))
      assert(sent.forall(_("cancels").arr.size == 1))
      assertEquals(f.postedInGate, Vector(true, true, true))
      assertEquals(f.gateEntries.get() - entries, 3)
      assertEquals(f.lookups.drop(3), Vector(c.id))
      assertEquals(f.resting, Vector.empty)
      assertEquals(f.worker.view.resting.filter(_.cancelling), Vector.empty)
    }
  }

  test("a row cancel stops at the first cancel without a clear success and leaves the rest") {
    val seeds = Vector(
      Seed(Side.Buy, "0.2", "540", 5L),
      Seed(Side.Buy, "0.15", "541", 6L),
      Seed(Side.Buy, "0.2", "542", 7L)
    )
    using(new Fixture(seeds)) { f =>
      val Vector(a, b, c) = f.seeded
      Wait.until("start lookups missing")(f.lookups.size == 3)
      f.ready()
      // The venue answers b with an error and its lookup finds it canceled. The loop still
      // stops, since the reply itself was not a clear success.
      f.statuses.put(b.id, lookupReply(b, "canceled", "0.15"))
      f.cancels =
        cloid => if cloid == b.id then Some(cancelReply(gone)) else Some(cancelReply("success"))
      assert(f.worker.cancel(Vector(a.id, b.id, c.id)))
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      def cloids = f.cancelPosts.map(_("action")("cancels")(0)("cloid").str)
      assertEquals(cloids, Vector(a.id, b.id))
      assertEquals(f.resting.map(_.id), Vector(c.id))
      // No reply at all stops it the same way.
      f.cancels = _ => None
      f.ready()
      assert(f.worker.cancel(Vector(c.id)))
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assertEquals(f.worker.view.outcome, Some("XMR cancel result unknown"))
      assertEquals(f.resting.map(_.id), Vector(c.id))
      f.cancels = _ => Some(cancelReply("success"))
      f.ready()
      assert(f.worker.cancel(Vector(c.id)))
      Wait.until("the next Enter did not cancel the rest")(f.resting.isEmpty)
      assertEquals(cloids, Vector(a.id, b.id, c.id, c.id))
    }
  }

  test("a CLOSE pressed during a three order cancel is sent after the current cancel") {
    val seeds = Vector(
      Seed(Side.Buy, "0.1", "540", 5L),
      Seed(Side.Buy, "0.1", "541", 6L),
      Seed(Side.Buy, "0.1", "542", 7L)
    )
    using(new Fixture(seeds)) { f =>
      val Vector(a, b, c) = f.seeded
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      val controller = new DeskController(
        PanelView(mode = "LIVE", terms = f.terms),
        () => f.clock.get(),
        f.worker.preview,
        f.worker.submitReviewed,
        _ => Left("no mode switch"),
        cancel = f.worker.cancel,
        closeFirst = f.worker.closeFirst
      )
      val hold = new Hold
      f.cancelHolds.put(a.id, hold)
      assert(f.worker.cancel(Vector(a.id, b.id, c.id)))
      hold.await()
      val running = f.worker.view
      assert(running.pending && !running.ready)
      // The frame drawn while the cancel runs shows CLOSE faint, as the panel draws it.
      val close = DeskButton("x Close", DeskAction.Close, 0, 23, 9)
      controller.publish(
        WorkspaceFrame(Vector.empty, Vector.empty, "XMR", 120, 40, "LIVE", None, Vector(close))
      )
      val snapshot = Snapshot.initial("XMR", f.clock.get())
      assert(controller.handle(Command.Close, snapshot, running, (120, 40), false))
      assertEquals(f.worker.view.message, TradingWorker.closeNextMessage)
      assertEquals(f.orderPosts, Vector.empty)
      hold.open()
      // The render pass hands each view to the controller, which sends CLOSE once live is ready.
      Wait.until("CLOSE was not sent after the current cancel") {
        controller.orders(f.worker.view)
        f.orderPosts.nonEmpty
      }
      Wait.until("CLOSE did not finish")(!f.worker.view.pending)
      assertEquals(f.posts.map(_("action")("type").str), Vector("cancelByCloid", "order"))
      assertEquals(f.cancelPosts.map(_("action")("cancels")(0)("cloid").str), Vector(a.id))
      val sent = f.orderPosts.head("action")("orders")(0)
      assertEquals(sent("b").bool, false)
      assertEquals(sent("r").bool, true)
      assertEquals(sent("s").str, "0.2")
      assertEquals(sent("t")("limit")("tif").str, "Ioc")
      assertEquals(f.resting.map(_.id), Vector(b.id, c.id))
    }
  }

  test("a cancel skips an order that settled before its turn") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "540", 5L), Seed(Side.Buy, "0.2", "541", 6L))
    using(new Fixture(seeds)) { f =>
      val Vector(a, b) = f.seeded
      f.ready()
      val hold = new Hold
      f.cancelHolds.put(a.id, hold)
      assert(f.worker.cancel(Vector(a.id, b.id)))
      hold.await()
      f.push(ws(b, "canceled", "0.2", venueTime))
      Wait.until("update not applied")(f.resting.map(_.id) == Vector(a.id))
      hold.open()
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assertEquals(f.cancelPosts.map(_("action")("cancels")(0)("cloid").str), Vector(a.id))
      assertEquals(f.resting, Vector.empty)
    }
  }

  test("cancels out of journal order carry each order's own cloid and asset") {
    val seeds = Vector(
      Seed(Side.Buy, "0.2", "540", 5L),
      Seed(Side.Buy, "0.2", "541", 6L),
      Seed(Side.Buy, "0.001", "90000", 7L, coin = "BTC")
    )
    using(new Fixture(seeds)) { f =>
      val Vector(a, b, c) = f.seeded
      f.ready()
      assert(f.worker.cancel(Vector(c.id, b.id)))
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assertEquals(
        f.cancelPosts
          .map(_("action")("cancels")(0))
          .map(o => (o("cloid").str, o("asset").num.toInt)),
        Vector((c.id, 1), (b.id, 0))
      )
      assertEquals(f.resting.map(_.id), Vector(a.id))
    }
  }

  test("any cancel error settles only from the order's looked up status") {
    val seeds = Vector(Seed(Side.Buy, "0.2", "540", 5L), Seed(Side.Buy, "0.15", "541", 6L))
    using(new Fixture(seeds)) { f =>
      val Vector(a, b) = f.seeded
      Wait.until("start lookups missing")(f.lookups.size == 2)
      f.ready()
      f.cancels = _ => Some(cancelReply(ujson.Obj("error" -> "Something the docs never named")))
      f.statuses.put(a.id, lookupReply(a, "canceled", "0.2"))
      f.statuses.put(b.id, lookupReply(b, "open", "0.15"))
      assert(f.worker.cancel(Vector(a.id)))
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assertEquals(f.lookups.drop(2), Vector(a.id))
      assertEquals(f.resting.map(_.id), Vector(b.id))
      val named = " | venue: Something the docs never named"
      assertEquals(f.worker.view.outcome, Some("XMR canceled $21.60 at 540" + named))
      assert(f.worker.cancel(Vector(b.id)))
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assertEquals(f.lookups.drop(2), Vector(a.id, b.id))
      assertEquals(f.resting.map(_.id), Vector(b.id))
      assertEquals(f.worker.view.outcome, Some("XMR cancel rejected" + named))
    }
  }

  test("a cancel answer of success settles with the canceled text") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L, remaining = Some("0.15"))))) { f =>
      f.ready()
      assert(f.worker.cancel(Vector(f.seeded.head.id)))
      Wait.until("cancel not settled")(f.resting.isEmpty)
      assertEquals(f.worker.view.outcome, Some("XMR canceled $16.38 at 546.09"))
    }
  }

  test("a cancel with no reply keeps the order and clears its cancelling mark") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      f.cancels = _ => None
      val hold = new Hold
      f.cancelHolds.put(f.seeded.head.id, hold)
      assert(f.worker.cancel(Vector(f.seeded.head.id)))
      hold.await()
      assert(f.worker.view.resting.head.cancelling)
      hold.open()
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assertEquals(f.resting.size, 1)
      assert(!f.worker.view.resting.head.cancelling)
    }
  }

  test("a cancel is built only from the journal") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      val foreign = "0x" + "7" * 32
      assert(!f.worker.cancel(Vector(foreign)))
      assertEquals(f.cancelPosts, Vector.empty)
      assert(f.worker.cancel(Vector(foreign, f.seeded.head.id)))
      Wait.until("cancel not settled")(f.resting.isEmpty)
      assertEquals(
        f.cancelPosts.map(_("action")("cancels")(0)("cloid").str),
        Vector(f.seeded.head.id)
      )
    }
  }

  test("cancel works from another market and with stale depth") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "546.09", 5L)))) { f =>
      f.ready()
      f.worker.select("BTC")
      f.clock.addAndGet(5000L)
      assert(!f.worker.view.ready)
      assert(f.worker.cancel(Vector(f.seeded.head.id)))
      Wait.until("cancel not settled")(f.resting.isEmpty)
      assertEquals(f.cancelPosts.size, 1)
    }
  }

  test("cancel is refused while an order is pending with the fixed text") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "540", 5L)))) { f =>
      f.ready()
      val hold = new Hold
      f.postHold.set(hold)
      assert(f.press(OrderIntent(Side.Buy, OrderKind.Hard)))
      hold.await()
      assert(!f.worker.cancel(Vector(f.seeded.head.id)))
      assertEquals(f.worker.view.message, "Wait for the pending order")
      hold.open()
      Wait.until("market order did not fill")(f.worker.view.outcome.nonEmpty)
      assertEquals(f.cancelPosts, Vector.empty)
    }
  }

  // Test 13: CLOSE with resting orders.

  test("a CLOSE with resting orders sends only the close and a self-trade cancel shows") {
    val seeds = Vector(
      Seed(Side.Sell, "0.1", "550", 5L, reduceOnly = true),
      Seed(Side.Buy, "0.183", "546.05", 6L)
    )
    using(new Fixture(seeds)) { f =>
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      assert(f.press(OrderIntent(Side.Sell, OrderKind.Close)))
      Wait.until("close did not fill")(f.worker.view.outcome.nonEmpty)
      f.refreshes()
      assertEquals(f.posts.size, 1)
      val sent = f.orderPosts.head("action")("orders")(0)
      assertEquals(sent("t")("limit")("tif").str, "Ioc")
      assertEquals(sent("r").bool, true)
      assertEquals(sent("s").str, "0.2")
      f.push(ws(f.seeded(1), "selfTradeCanceled", "0.183", venueTime))
      Wait.until("self-trade cancel not applied")(f.resting.size == 1)
      assertEquals(
        f.worker.view.outcome,
        Some("XMR resting buy $19.99 at 546.05 canceled by self-trade prevention")
      )
      assertEquals(f.posts.size, 1)
    }
  }

  test("Esc withdraws a CLOSE waiting on a cancel, and the other cancels stay unsent") {
    val seeds = Vector(
      Seed(Side.Buy, "0.1", "540", 5L),
      Seed(Side.Buy, "0.1", "541", 6L),
      Seed(Side.Buy, "0.1", "542", 7L)
    )
    using(new Fixture(seeds)) { f =>
      val Vector(a, b, c) = f.seeded
      f.position = "0.2"
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.ready()
      val controller = new DeskController(
        PanelView(mode = "LIVE", terms = f.terms),
        () => f.clock.get(),
        f.worker.preview,
        f.worker.submitReviewed,
        _ => Left("no mode switch"),
        cancel = f.worker.cancel,
        closeFirst = f.worker.closeFirst,
        withdrawClose = f.worker.withdrawClose
      )
      val hold = new Hold
      f.cancelHolds.put(a.id, hold)
      assert(f.worker.cancel(Vector(a.id, b.id, c.id)))
      hold.await()
      val close = DeskButton("x Close", DeskAction.Close, 0, 23, 9)
      controller.publish(
        WorkspaceFrame(Vector.empty, Vector.empty, "XMR", 120, 40, "LIVE", None, Vector(close))
      )
      val snapshot = Snapshot.initial("XMR", f.clock.get())
      assert(controller.handle(Command.Close, snapshot, f.worker.view, (120, 40), false))
      assertEquals(f.worker.view.message, TradingWorker.closeNextMessage)
      assert(controller.handle(Command.Cancel, snapshot, f.worker.view, (120, 40), false))
      assertEquals(f.worker.view.message, TradingWorker.closeWithdrawnMessage)
      hold.open()
      Wait.until("cancel did not finish")(!f.worker.view.pending)
      f.refreshes()
      f.ready()
      controller.orders(f.worker.view)
      controller.orders(f.worker.view)
      assertEquals(f.orderPosts, Vector.empty)
      assertEquals(f.cancelPosts.map(_("action")("cancels")(0)("cloid").str), Vector(a.id))
      assertEquals(f.resting.map(_.id), Vector(b.id, c.id))
    }
  }

  // R4j. A loop that ended leaves no cancel for a later CLOSE to wait on.
  test("CLOSE waits on a cancel only while one runs") {
    val seeds = Vector(Seed(Side.Buy, "0.1", "540", 5L), Seed(Side.Buy, "0.1", "541", 6L))
    using(new Fixture(seeds)) { f =>
      f.ready()
      assert(!f.worker.closeFirst())
      val hold = new Hold
      f.cancelHolds.put(f.seeded(0).id, hold)
      assert(f.worker.cancel(Vector(f.seeded(0).id)))
      hold.await()
      assert(f.worker.closeFirst())
      hold.open()
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      assert(!f.worker.closeFirst(), "a finished loop still took CLOSE")
      f.ready()
      assert(f.worker.cancel(Vector(f.seeded(1).id)))
      Wait.until("second cancel not settled")(f.resting.isEmpty)
      assert(!f.worker.closeFirst())
    }
  }

  // R2e. The lookup that moves a lost order into resting at a smaller size found a part fill.
  test("a lost order that rests at a smaller size starts the opening pause") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
      val pending = f.journal.pending.get
      assert(f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms).isRight)
      f.statuses.put(pending.id, lookupReply(pending.copy(oid = Some(4242L)), "open", "0.1"))
      Wait.until("venue size not applied")(
        f.resting.headOption.exists(_.remaining.contains("0.1"))
      )
      assertEquals(
        f.worker.preview(OrderIntent(Side.Buy, OrderKind.Hard), f.terms),
        Left(TradingWorker.openingPausedMessage)
      )
    }
  }

  // R3d. A listing starts the count of unknownOid answers again.
  test("a listing after a counted answer starts the count of a lost posted order again") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
      val pending = f.journal.pending.get
      f.refreshes()
      f.clock.set(70001L)
      Wait.until("minute read missing")(f.openReads.get() == 1)
      f.refreshes()
      // Past the margin the read misses it and the next lookup counts one answer.
      f.clock.set(75001L)
      Wait.until("open orders not read at the margin")(f.openReads.get() == 2)
      f.refreshes()
      // A socket open reads again, and this time the venue lists the order.
      f.openOrders = Some(ujson.Arr(listed(pending.copy(oid = Some(4242L)), "0.183")))
      f.opened()
      Wait.until("open orders not read on the socket open")(f.openReads.get() == 3)
      f.refreshes()
      f.clock.set(85001L)
      f.refreshes()
      f.clock.set(95001L)
      f.refreshes()
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(pending.id))
    }
  }

  // R3h. Only an order at the lost order's own limit can be it.
  test("a listing at another limit does not hold a lost posted order") {
    val elsewhere = ujson.Obj(
      "coin" -> "XMR",
      "limitPx" -> "546.08",
      "oid" -> ujson.Num(999.0),
      "side" -> "B",
      "sz" -> "0.183",
      "timestamp" -> ujson.Num(1699999990000.0)
    )
    using(new Fixture()) { f =>
      val pending = releaseLost(f, Vector(elsewhere))
      assertEquals(f.journal.pending, None)
      assertEquals(f.worker.view.outcome, Some("XMR not placed"))
      assert(pending.post)
    }
  }

  // R3m. A late fill counts as any fill: one that faced the stamped position pauses.
  test("a late fill of a released order that faced the position starts the pause") {
    using(new Fixture(Vector(Seed(Side.Sell, "0.1", "550", 5L)))) { f =>
      f.position = "0.2"
      Wait.until("start lookup missing")(f.lookups.size == 1)
      f.opened()
      Wait.until("open orders not read")(f.openReads.get() == 1)
      f.clock.set(20000L)
      Wait.until("second lookup missing")(f.lookups.size == 2)
      f.clock.set(70001L)
      Wait.until("third lookup missing")(f.lookups.size == 3)
      f.clock.set(80001L)
      Wait.until("fourth lookup missing")(f.lookups.size == 4)
      f.clock.set(90001L)
      Wait.until("order not settled as not found")(f.resting.isEmpty)
      Wait.until("long position not read")(f.worker.view.position.contains(BigDecimal("0.2")))
      f.book("546.09", "546.10", 5000L)
      val opening = OrderIntent(Side.Buy, OrderKind.Hard)
      assert(f.worker.preview(opening, f.terms).isRight)
      f.push(ws(f.seeded.head, "filled", "0", venueTime))
      Wait.until("late fill not counted")(f.worker.view.ignoredOrderUpdates == 1L)
      assertEquals(f.worker.preview(opening, f.terms), Left(TradingWorker.openingPausedMessage))
    }
  }

  // R3n. A lost order's count never carries into its life as a resting order.
  test("a lost posted order that rests starts with no count") {
    using(new Fixture()) { f =>
      f.ready()
      f.lostOrders = true
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("lost reply not kept")(f.worker.view.message == "XMR waiting for order result")
      val pending = f.journal.pending.get
      f.refreshes()
      f.clock.set(75001L)
      Wait.until("open orders not read past the margin")(f.openReads.get() >= 1)
      f.refreshes()
      f.refreshes()
      f.clock.set(85001L)
      f.refreshes()
      assertEquals(f.journal.pending.map(_.id), Some(pending.id))
      // Two answers counted. The order then turns up resting.
      f.statuses.put(pending.id, lookupReply(pending.copy(oid = Some(4242L)), "open", "0.183"))
      Wait.until("lost order did not rest")(f.resting.size == 1)
      f.statuses.remove(pending.id)
      f.clock.set(95001L)
      f.refreshes()
      f.refreshes()
      assertEquals(f.resting.map(_.id), Vector(pending.id))
      assertEquals(f.worker.view.notFoundOrders, 0L)
    }
  }

  test("the venue's own rejection words reach the order result log line") {
    using(new Fixture(Vector(Seed(Side.Buy, "0.2", "540", 5L)))) { f =>
      f.ready()
      val drawnReady = TradingView("XMR", message = "Ready", ready = true)
      var log = Vector.empty[(Long, String)]
      var rows = Vector.empty[String]
      lazy val controller: DeskController = new DeskController(
        PanelView(mode = "LIVE"),
        () => f.clock.get(),
        f.worker.preview,
        f.worker.submitReviewed,
        _ => Left("no mode switch"),
        result = (number, text) => {
          log :+= ((number, text))
          val frame = Workspace.draw(
            Snapshot.initial("XMR", 0L),
            drawnReady,
            controller.panel,
            100,
            30,
            0L,
            java.time.ZoneOffset.UTC,
            Style.mono
          )
          rows :+= Chart.plain(frame.lines(30 - 12 + 3)).take(38).trim
        }
      )
      controller.orders(f.worker.view)
      f.orderError = Some("Order has insufficient margin, not enough to fill")
      assert(f.press(f.priced(Side.Buy, "546.09")))
      Wait.until("rejection not shown")(f.worker.view.outcome.nonEmpty)
      controller.orders(f.worker.view)
      f.ready()
      val order = f.seeded.head
      f.cancels = _ => Some(cancelReply(ujson.Obj("error" -> "Order was never placed, or filled.")))
      f.statuses.put(order.id, lookupReply(order, "open", "0.2"))
      assert(f.worker.cancel(Vector(order.id)))
      Wait.until("cancel request never finished")(!f.worker.view.pending)
      controller.orders(f.worker.view)
      assertEquals(
        log,
        Vector(
          (1L, "XMR order rejected | venue: Order has insufficient margin, not enough to fill"),
          (2L, "XMR cancel rejected | venue: Order was never placed, or filled.")
        )
      )
      // The status row keeps Desk's own word, whatever the venue said.
      assertEquals(rows, Vector("Not sent #1", "Cancel refused #2"))
    }
  }

  test("the venue's words are the first line, free of control bytes and at most 160 long") {
    assertEquals(TradingWorker.venueWords(""), "")
    assertEquals(TradingWorker.venueWords("  \nsecond"), "")
    val clear = 27.toChar.toString + "[2J"
    assertEquals(
      TradingWorker.venueWords(s"Bad ${clear}price\nsecond line"),
      " | venue: Bad price"
    )
    assertEquals(TradingWorker.venueWords("x" * 300), " | venue: " + "x" * 160)
  }

  // Diagnostics.

  private class Memory extends Diagnostics.Sink:
    private val saved = ArrayBuffer.empty[String]
    def append(line: String): Unit = synchronized { saved += line }
    def lines: Vector[String] = synchronized(saved.toVector)
    def close(): Unit = ()

  test("diagnostics count ignored updates and write a line for each order not found") {
    val clock = new AtomicLong(10000L)
    val sink = new Memory
    val log = new Diagnostics(() => sink, clock.get, 2000L, monotonic = clock.get)
    log.start()
    try
      val base = TradingView("XMR", message = "Ready")
      log.execution("LIVE", base)
      log.execution("LIVE", base.copy(ignoredOrderUpdates = 2L))
      log.execution("LIVE", base.copy(ignoredOrderUpdates = 2L).forMarket("BTC"))
      log.execution("LIVE", base.copy(ignoredOrderUpdates = 2L, notFoundOrders = 1L))
      log.execution("PROP", TradingView("XMR", message = "Ready"))
      log.execution("LIVE", base.copy(ignoredOrderUpdates = 2L, notFoundOrders = 1L))
      // A new live worker counts from zero again.
      log.execution("LIVE", base)
      log.execution("LIVE", base.copy(ignoredOrderUpdates = 1L))
      Wait.until("diagnostics lines missing")(
        sink.lines.count(_.contains("order_updates_ignored")) == 2 &&
          sink.lines.count(_.contains("order_not_found")) == 1
      )
      val ignored = sink.lines.filter(_.contains("order_updates_ignored")).map(ujson.read(_))
      assertEquals(ignored.map(_("ignoredOrderUpdates").num.toLong), Vector(2L, 1L))
      assertEquals(
        sink.lines.filter(_.contains("order_not_found")).map(ujson.read(_)("count").num.toLong),
        Vector(1L)
      )
    finally log.close()
  }
