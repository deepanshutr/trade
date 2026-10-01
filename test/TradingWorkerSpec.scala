package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class TradingWorkerSpec extends TempDirectorySuite:
  private def workspaceText(view: TradingView): String =
    val controller = new DeskController(
      PanelView(mode = "LIVE"),
      () => 1000L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused")
    )
    controller.orders(view)
    Workspace
      .draw(
        Snapshot.initial(view.coin, 1000L),
        view,
        controller.panel,
        160,
        45,
        1000L,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      .lines
      .mkString("\n")

  private final class Fixture(
      enabled: Boolean = true,
      @volatile var roleMatches: Boolean = true,
      beforeReplace: () => Unit = () => (),
      afterReserve: () => Unit = () => (),
      afterReplace: () => Unit = () => (),
      beforeOrderSign: () => Unit = () => (),
      initialLeverage: Int = 5,
      marketMaximum: Int = 5,
      loseLeverage: Boolean = false,
      marginMode: String = "cross",
      warmSigning: () => Unit = () => OrderSigning.warm(),
      autoStart: Boolean = true,
      orderPermission: (() => Unit) => Unit = run => run(),
      accountPaused: () => Boolean = () => false,
      balanceMode: String = "disabled",
      beforeAccountRead: Int => Unit = _ => (),
      missing: String = "",
      failJournalOpen: Boolean = false
  ):
    private val key = Array.fill[Byte](32)(1)
    val agent = OrderSigning.address(key)
    val owner = "0x" + "2" * 40
    val config = TradingConfig(
      owner = Option.when(missing != "owner")(owner),
      agent = Option.when(missing != "agent")(agent)
    )
    val clock = new AtomicLong(10000L)
    val loads = new AtomicInteger()
    val journalOpens = new AtomicInteger()
    val warmCalls = new AtomicInteger()
    val posts = new AtomicInteger()
    val orderPosts = new AtomicInteger()
    val roleReads = new AtomicInteger()
    val recoveryReads = new AtomicInteger()
    val modeReads = new AtomicInteger()
    val spotReads = new AtomicInteger()
    val coreReads = new AtomicInteger()
    val infoReads = new AtomicInteger()
    val spotEntered = new CountDownLatch(1)
    val spotRelease = new CountDownLatch(1)
    val secondSpotEntered = new CountDownLatch(1)
    val secondSpotRelease = new CountDownLatch(1)
    val recoveryEntered = new CountDownLatch(1)
    val recoveryRelease = new CountDownLatch(1)
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val accountEntered = new CountDownLatch(1)
    val accountRelease = new CountDownLatch(1)
    val lastPost = new AtomicReference[ujson.Value](ujson.Null)
    @volatile var hold = false
    @volatile var lose = false
    @volatile var reject = false
    @volatile var lookupFailure = false
    var openedJournal: OrderJournal = null
    @volatile var position = "0"
    @volatile var readFailure = false
    @volatile var failedRead = ""
    @volatile var holdAccount = false
    @volatile var holdBalance = false
    @volatile var holdSecondBalance = false
    @volatile var balanceFailure = false
    @volatile var stable = "1000"
    @volatile var leverage = initialLeverage
    @volatile var activeMode = marginMode
    @volatile var holdRecovery = false
    @volatile var recoverFilled = false
    @volatile var publishActive = true
    @volatile var maximums = Vector("100", "100")
    @volatile var available = Vector("123.45", "123.45")
    @volatile var activeCoin = "XMR"
    @volatile var activeReceive: Option[ujson.Value] => Unit = _ => ()
    @volatile var timedActiveReceive: (Option[ujson.Value], Long) => Unit = (_, _) => ()
    private val activeDelivery = new Object
    def pauseActive(): Unit = activeDelivery.synchronized { publishActive = false }
    def activePayload: Option[ujson.Value] = Some(
      ujson.Obj(
        "user" -> owner,
        "coin" -> activeCoin,
        "leverage" -> ujson.Obj("type" -> activeMode, "value" -> leverage),
        "maxTradeSzs" -> ujson.Arr.from(maximums),
        "availableToTrade" -> ujson.Arr.from(available)
      )
    )
    def sendActive(): Unit = activeReceive(activePayload)
    val directory = tempDirectory("desk-worker")
    val exchange = new Exchange:
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit = activeDelivery.synchronized {
        require(failedRead != "socket", "account socket failed")
        activeCoin = coin
        activeReceive = receive
        if publishActive then sendActive()
      }
      override def watchTimed(
          coin: String,
          owner: String,
          generation: Long,
          clock: () => Long,
          receive: (Option[ujson.Value], Long) => Unit
      ): Unit =
        timedActiveReceive = receive
        watch(coin, owner, generation, data => receive(data, clock()))
      def info(body: ujson.Value): Option[ujson.Value] =
        infoReads.incrementAndGet()
        if readFailure || body("type").str == failedRead then None
        else
          body("type").str match
            case "meta" =>
              Some(
                ujson.Obj(
                  "universe" -> ujson.Arr(
                    ujson.Obj(
                      "name" -> "XMR",
                      "szDecimals" -> 3,
                      "maxLeverage" -> marketMaximum
                    ),
                    ujson.Obj(
                      "name" -> "BTC",
                      "szDecimals" -> 5,
                      "maxLeverage" -> 40
                    )
                  )
                )
              )
            case "userRole" =>
              roleReads.incrementAndGet()
              Some(
                ujson.Obj(
                  "role" -> "agent",
                  "data" -> ujson.Obj("user" -> (if roleMatches then owner
                                                 else agent))
                )
              )
            case "clearinghouseState" =>
              val read = coreReads.incrementAndGet()
              beforeAccountRead(read)
              val captured = position
              if holdAccount then
                holdAccount = false
                accountEntered.countDown()
                accountRelease.await()
              Some(
                ujson.Obj(
                  "marginSummary" -> ujson.Obj("accountValue" -> stable),
                  "assetPositions" -> ujson.Arr(
                    ujson.Obj(
                      "position" -> ujson.Obj(
                        "coin" -> "XMR",
                        "szi" -> captured,
                        "positionValue" -> (BigDecimal(
                          captured
                        ).abs * 100).toString,
                        "entryPx" -> "100",
                        "liquidationPx" -> "50",
                        "unrealizedPnl" -> "0"
                      )
                    )
                  )
                )
              )
            case "userAbstraction" =>
              modeReads.incrementAndGet()
              if balanceFailure then None else Some(ujson.Str(balanceMode))
            case "spotClearinghouseState" =>
              val read = spotReads.incrementAndGet()
              val captured = stable
              if holdBalance then
                holdBalance = false
                spotEntered.countDown()
                spotRelease.await()
              else if holdSecondBalance && read == 2 then
                secondSpotEntered.countDown()
                secondSpotRelease.await()
              if balanceFailure then None
              else
                Some(
                  ujson.Obj(
                    "balances" -> ujson.Arr(
                      ujson.Obj(
                        "coin" -> "USDC",
                        "token" -> 0,
                        "total" -> captured
                      )
                    )
                  )
                )
            case "orderStatus" =>
              recoveryReads.incrementAndGet()
              if holdRecovery then
                recoveryEntered.countDown()
                recoveryRelease.await()
              if lookupFailure then None
              else if recoverFilled then
                val order = lastPost.get()("action")("orders")(0)
                Some(
                  ujson.Obj(
                    "status" -> "order",
                    "order" -> ujson.Obj(
                      "status" -> "filled",
                      "order" -> ujson.Obj(
                        "cloid" -> body("oid"),
                        "coin" -> "XMR",
                        "side" -> "B",
                        "limitPx" -> order("p"),
                        "origSz" -> order("s")
                      )
                    )
                  )
                )
              else Some(ujson.Obj("status" -> "unknownOid"))
            case _ => None
      def post(body: ujson.Value): Option[ujson.Value] =
        posts.incrementAndGet()
        if body("action")("type").str == "order" then orderPosts.incrementAndGet()
        lastPost.set(body)
        entered.countDown()
        if hold then release.await()
        if body("action")("type").str == "updateLeverage" then
          if loseLeverage then None
          else
            leverage = 5
            sendActive()
            Some(
              ujson.Obj(
                "status" -> "ok",
                "response" -> ujson.Obj("type" -> "default")
              )
            )
        else if reject then Some(ujson.Obj("status" -> "err", "response" -> "x"))
        else if lose then None
        else
          Some(
            ujson.Obj(
              "status" -> "ok",
              "response" -> ujson.Obj(
                "type" -> "order",
                "data" -> ujson.Obj(
                  "statuses" -> ujson.Arr(
                    ujson.Obj(
                      "filled" ->
                        ujson.Obj(
                          "totalSz" -> "1",
                          "avgPx" -> "100",
                          "oid" -> 9
                        )
                    )
                  )
                )
              )
            )
          )
    def fresh(
        worker: TradingWorker,
        coin: String = "XMR",
        time: Long = 1000L
    ): Unit =
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          coin,
          Book(
            Vector(Level("99.99", "20", 1)),
            Vector(Level("100", "20", 1)),
            time
          ),
          true
        )
      )
    def create(): TradingWorker = new TradingWorker(
      config,
      enabled,
      "XMR",
      exchange,
      () =>
        loads.incrementAndGet()
        key.clone()
      ,
      () =>
        journalOpens.incrementAndGet()
        require(!failJournalOpen, "journal open failed")
        openedJournal = OrderJournal.open(directory, config.identity, beforeReplace, afterReplace)
        openedJournal
      ,
      () => clock.get(),
      () => clock.get(),
      afterReserve,
      beforeOrderSign,
      () =>
        warmCalls.incrementAndGet()
        warmSigning()
      ,
      orderPermission,
      accountPaused
    )
    val worker = create()
    if autoStart then worker.start()
    fresh(worker)
    def ready(): Unit = Wait.until("worker never became ready")(worker.view.ready)
    def review(intent: OrderIntent = buy, lifetime: Long = 1000L): OrderReview =
      val current = worker.view
      OrderReview(
        intent,
        worker.preview(intent).toOption.get,
        clock.get(),
        clock.get() + lifetime,
        current.leverage,
        current.marginMode
      )
    def close(): Unit =
      release.countDown()
      accountRelease.countDown()
      recoveryRelease.countDown()
      spotRelease.countDown()
      secondSpotRelease.countDown()
      worker.close()

  private val buy = OrderIntent(Side.Buy, OrderKind.Hard)

  private def completeWithoutActive(f: Fixture): Unit =
    f.ready()
    f.pauseActive()
    assert(f.worker.submit(buy, "XMR"))
    if f.reject then
      Wait.until("err post missing")(f.orderPosts.get() > 0)
      f.clock.set(f.lastPost.get()("expiresAfter").num.toLong + 1)
    Wait.until("completion and fresh account snapshot missing")(!f.worker.view.pending)
    assertEquals(
      f.worker.view.outcome,
      Some(if f.reject then "XMR order rejected" else "XMR filled 1 at 100")
    )

  private def assertUsable(f: Fixture, expected: Option[String], cash: Boolean = false): Unit =
    val view = f.worker.view
    for (cols, rows) <- Vector((100, 30), (360, 99)) do
      val frame = Workspace.draw(
        Snapshot.initial("XMR", f.clock.get()),
        view,
        PanelView(mode = "LIVE"),
        cols,
        rows,
        f.clock.get(),
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      assertEquals(frame.lines.size, rows)
      assert(frame.lines.forall(_.length <= cols), s"frame exceeds $cols columns")
      val text = frame.lines.mkString("\n")
      expected match
        case Some(value) =>
          assert(text.contains("Usable"), s"${cols}x$rows: missing Usable label")
          assert(text.contains(value), s"${cols}x$rows: missing usable $value")
        case None =>
          assert(!text.contains("Usable"))
          assert(!text.contains("123.45"))
      if cash then
        assert(text.contains("Stables"))
        assert(text.contains("1000"))

  test("live usable margin appears beside cash and fits both frame sizes") {
    val f = new Fixture()
    try
      f.ready()
      Wait.until("cash missing")(f.worker.view.portfolio.stables.contains(BigDecimal(1000)))
      assertUsable(f, Some("123.45"), cash = true)
    finally f.close()
  }

  test("usable margin disappears after completion until new active data") {
    val f = new Fixture()
    try
      f.ready()
      assertUsable(f, Some("123.45"))
      val receive = f.timedActiveReceive
      val at = f.clock.get()
      f.clock.incrementAndGet()
      completeWithoutActive(f)
      assertUsable(f, None)
      receive(f.activePayload, at)
      assertUsable(f, None)
      f.sendActive()
      assertUsable(f, Some("123.45"))
    finally f.close()
  }

  test("stale active data hides usable margin") {
    val f = new Fixture()
    try
      f.ready()
      f.pauseActive()
      assertUsable(f, Some("123.45"))
      f.clock.addAndGet(f.config.positionAgeMs + 1)
      assertUsable(f, None)
    finally f.close()
  }

  test("usable margin takes the smaller side in either order") {
    val f = new Fixture()
    try
      f.ready()
      f.pauseActive()
      for sides <- Vector(Vector("123.45", "987.65"), Vector("987.65", "123.45")) do
        f.available = sides
        f.sendActive()
        assertUsable(f, Some("123.45"))
    finally f.close()
  }

  test("usable margin clears on invalid data, disconnect and market change") {
    val f = new Fixture()
    try
      f.ready()
      f.pauseActive()
      assertUsable(f, Some("123.45"))
      f.activeReceive(None)
      assertUsable(f, None)
      f.sendActive()
      assertUsable(f, Some("123.45"))
      val oldReceive = f.activeReceive
      f.worker.observe(Event.SocketDown)
      oldReceive(f.activePayload)
      assertUsable(f, None)
      f.worker.select("BTC")
      oldReceive(f.activePayload)
      assertUsable(f, None)
    finally f.close()
  }

  test("unavailable usable margin does not change order capacity") {
    val f = new Fixture()
    try
      f.ready()
      f.pauseActive()
      f.available = Vector.empty
      f.sendActive()
      assertUsable(f, None)
      assert(f.worker.view.ready)
      f.available = Vector("0", "0")
      f.sendActive()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("display margin blocked the order")(f.orderPosts.get() == 1)
    finally f.close()
  }

  test("paper and view panels ignore live usable margin") {
    val f = new Fixture()
    try
      f.ready()
      val view = f.worker.view
      for mode <- Vector("PAPER", "VIEW") do
        val frame = Workspace.draw(
          Snapshot.initial("XMR", f.clock.get()),
          view,
          PanelView(mode = mode),
          100,
          30,
          f.clock.get(),
          java.time.ZoneOffset.UTC,
          Style.mono
        )
        val text = frame.lines.mkString("\n")
        assert(!text.contains("Usable"))
        assert(!text.contains("123.45"))
    finally f.close()
  }

  test("a fill needs new active data even after a fresh account snapshot") {
    val f = new Fixture()
    try
      completeWithoutActive(f)
      assert(!f.worker.view.ready)
    finally f.close()
  }

  test("renewed active data restores readiness and replaces pre-fill maximums") {
    val f = new Fixture()
    try
      completeWithoutActive(f)
      assert(!f.worker.view.ready)
      f.maximums = Vector("0", "100")
      f.sendActive()
      f.ready()
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.worker.view.message, "Requested quantity exceeds confirmed account capacity")
      assertEquals(f.orderPosts.get(), 1)
      assert(f.worker.submit(buy.copy(side = Side.Sell), "XMR"))
      Wait.until("renewed sell capacity was not used")(f.orderPosts.get() == 2)
    finally f.close()
  }

  test("active data stamped before a fill cannot restore readiness or pre-fill capacity") {
    val f = new Fixture()
    try
      f.ready()
      val receive = f.timedActiveReceive
      val requestStartedAt = f.clock.get()
      val preFill = f.activePayload
      f.clock.incrementAndGet()
      completeWithoutActive(f)
      receive(preFill, requestStartedAt)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.orderPosts.get(), 1)
      f.maximums = Vector("0", "100")
      receive(f.activePayload, f.clock.incrementAndGet())
      f.ready()
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.worker.view.message, "Requested quantity exceeds confirmed account capacity")
      assert(f.worker.submit(buy.copy(side = Side.Sell), "XMR"))
      Wait.until("post-fill sell capacity was not used")(f.orderPosts.get() == 2)
    finally f.close()
  }

  test("a rejected order resumes after unchanged active data is renewed") {
    val f = new Fixture()
    try
      f.reject = true
      completeWithoutActive(f)
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.position, Some(BigDecimal(0)))
      f.fresh(f.worker, time = 2000L)
      f.sendActive()
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("order after rejection did not reach the exchange")(f.orderPosts.get() == 2)
    finally f.close()
  }

  test("an owner mismatch clears after the role matches with no saved order") {
    val f = new Fixture(roleMatches = false)
    try
      Wait.until("owner mismatch was not shown")(
        f.worker.view.message == TradingWorker.ownerMismatchMessage
      )
      assertEquals(f.openedJournal.pending, None)
      f.roleMatches = true
      f.clock.addAndGet(10001L)
      f.fresh(f.worker, time = 12001L)
      Wait.until("account did not refresh after the role matched")(
        f.worker.view.position.nonEmpty && f.worker.view.block.isEmpty
      )
      assertNotEquals(f.worker.view.message, TradingWorker.ownerMismatchMessage)
      assert(Set("Preparing trading", "Ready").contains(f.worker.view.message))
      assertEquals(f.openedJournal.pending, None)
    finally f.close()
  }

  test("a saved order stays busy after an owner mismatch and successful account refresh") {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace = () => { writes.incrementAndGet(); () })
    try
      f.ready()
      f.lose = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("lost result was not saved")(
        f.worker.view.message == "XMR waiting for order result"
      )
      val saved = f.openedJournal.pending
      assert(saved.nonEmpty)
      f.roleMatches = false
      f.worker.observe(Event.SocketDown)
      f.fresh(f.worker, time = 2000L)
      Wait.until("owner mismatch was not shown")(
        f.worker.view.message == TradingWorker.ownerMismatchMessage
      )
      f.roleMatches = true
      f.position = "1"
      f.clock.addAndGet(10001L)
      f.fresh(f.worker, time = 12001L)
      Wait.until("account did not refresh after the role matched")(
        f.worker.view.position.contains(BigDecimal(1)) && f.worker.view.block.isEmpty
      )
      assertNotEquals(f.worker.view.message, TradingWorker.ownerMismatchMessage)
      assertEquals(f.worker.view.message, "XMR waiting for order result")
      assertEquals(f.openedJournal.pending, saved)
      assert(f.worker.view.pending)
      val before = writes.get()
      val ready = f.worker.view.ready
      val accepted = f.worker.submit(buy, "XMR")
      assert(!ready, "saved order became ready after owner recovery")
      assert(!accepted, "new submit was accepted with an order still saved")
      assertEquals(writes.get(), before)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  private def saveAtStartup(f: Fixture): SavedOrder =
    val plan = Trading
      .plan(
        "XMR",
        MarketRules(0, 3, 5),
        Book(Vector(Level("99.99", "20", 1)), Vector(Level("100", "20", 1)), 1000L),
        BigDecimal(100),
        BigDecimal(0),
        buy,
        f.config.sizes
      )
      .toOption
      .get
    val journal = OrderJournal.open(f.directory, f.config.identity)
    try
      val saved = journal.reserve(plan, f.owner, f.clock.get())
      f.lastPost.set(ujson.Obj("action" -> Exchange.action(saved)))
      saved
    finally journal.close()

  test("a saved order of another owner keeps one message across failed role checks and recovery") {
    val f = new Fixture(autoStart = false)
    try
      val saved = saveAtStartup(f)
      val file = f.directory.resolve(f.config.identity + ".json")
      val data = ujson.read(Files.readString(file))
      data("pending")("owner") = "0x" + "3" * 40
      Files.writeString(file, ujson.write(data))
      f.clock.set(saved.expiresAfter + 60001L)
      f.worker.start()
      Wait.until("first refresh did not complete")(f.worker.view.position.nonEmpty)
      assertEquals(f.worker.view.message, TradingWorker.savedOwnerMismatchMessage)
      f.roleMatches = false
      f.worker.observe(Event.SocketDown)
      Wait.until("saved order and role were not checked")(f.roleReads.get() > 1)
      for round <- 1 to 3 do
        f.clock.addAndGet(10001L)
        val reads = f.recoveryReads.get()
        Wait.until(s"refresh $round did not complete") {
          assertEquals(f.worker.view.message, TradingWorker.savedOwnerMismatchMessage)
          f.roleReads.get() >= round + 2 && f.recoveryReads.get() >= reads + 2
        }
      f.position = "1"
      f.roleMatches = true
      f.clock.addAndGet(10001L)
      Wait.until("matching role did not refresh the account") {
        assertEquals(f.worker.view.message, TradingWorker.savedOwnerMismatchMessage)
        f.worker.view.position.contains(BigDecimal(1))
      }
      assert(f.worker.view.pending)
      assertEquals(f.openedJournal.pending.map(_.id), Some(saved.id))
    finally f.close()
  }

  Vector(Some("0x" + "3" * 40), None).foreach { owner =>
    test(s"expired unknown order stays saved with a different or missing owner: $owner") {
      val f = new Fixture(autoStart = false)
      try
        val saved = saveAtStartup(f)
        val file = f.directory.resolve(f.config.identity + ".json")
        val data = ujson.read(Files.readString(file))
        owner match
          case Some(value) => data("pending")("owner") = value
          case None        => data("pending").obj.remove("owner")
        Files.writeString(file, ujson.write(data))
        f.clock.set(saved.expiresAfter + 60001L)
        f.worker.start()
        Wait.until("matching role did not refresh the account")(
          f.roleReads.get() > 0 && f.worker.view.position.nonEmpty
        )
        val reads = f.recoveryReads.get()
        Wait.until("lookup after role check did not complete")(
          f.recoveryReads.get() >= reads + 2 || f.worker.view.outcome.nonEmpty
        )
        assertEquals(f.openedJournal.pending.map(_.id), Some(saved.id))
        assert(f.worker.view.pending)
        assert(!f.worker.submit(buy, "XMR"))
        assertEquals(f.posts.get(), 0)
        assertEquals(f.worker.view.outcome, None)
        if owner.nonEmpty then
          assertEquals(f.worker.view.message, TradingWorker.savedOwnerMismatchMessage)
        else assertEquals(f.worker.view.message, "XMR waiting for order result")
      finally f.close()
    }
  }

  test("an owner-less saved order stays saved and clears a recovered signer mismatch") {
    val f = new Fixture(autoStart = false, roleMatches = false)
    try
      val saved = saveAtStartup(f)
      val file = f.directory.resolve(f.config.identity + ".json")
      val data = ujson.read(Files.readString(file))
      data("pending").obj.remove("owner")
      Files.writeString(file, ujson.write(data))
      f.clock.set(saved.expiresAfter + 60001L)
      f.worker.start()
      Wait.until("signer mismatch was not shown")(
        f.worker.view.message == TradingWorker.ownerMismatchMessage
      )
      f.roleMatches = true
      f.clock.addAndGet(10001L)
      Wait.until("matching role did not refresh the account")(f.worker.view.position.nonEmpty)
      assertEquals(f.worker.view.message, "XMR waiting for order result")
      assertEquals(f.openedJournal.pending.map(_.id), Some(saved.id))
      assertEquals(f.openedJournal.pending.flatMap(_.owner), None)
      assert(f.worker.view.pending)
      assert(!f.worker.submit(buy, "XMR"))
    finally f.close()
  }

  test("startup matching fill settles a saved order belonging to another owner") {
    val f = new Fixture(autoStart = false)
    try
      val saved = saveAtStartup(f)
      val file = f.directory.resolve(f.config.identity + ".json")
      val data = ujson.read(Files.readString(file))
      data("pending")("owner") = "0x" + "3" * 40
      Files.writeString(file, ujson.write(data))
      f.clock.set(saved.expiresAfter + 60001L)
      f.recoverFilled = true
      f.worker.start()
      Wait.until("fill not recovered")(
        f.worker.view.outcome.contains("XMR recovered filled")
      )
      assertEquals(f.openedJournal.pending, None)
      Wait.until("settled order remained pending")(!f.worker.view.pending)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  Vector(false, true).foreach { roleMatches =>
    test(s"startup expired unknown order releases only after matching role: $roleMatches") {
      val f = new Fixture(autoStart = false, roleMatches = roleMatches)
      try
        val saved = saveAtStartup(f)
        f.clock.set(saved.expiresAfter + 60001L)
        f.worker.start()
        Wait.until("role check missing")(f.roleReads.get() > 0)
        if roleMatches then
          Wait.until("expired order not released")(
            f.worker.view.outcome.contains("XMR not placed")
          )
          assertEquals(f.openedJournal.pending, None)
        else
          Wait.until("lookup not completed")(
            f.recoveryReads.get() >= 2 || f.worker.view.outcome.nonEmpty
          )
          assertEquals(f.openedJournal.pending, Some(saved))
          assert(f.worker.view.pending)
          assertEquals(f.worker.view.message, TradingWorker.ownerMismatchMessage)
          assertEquals(f.worker.view.outcome, None)
        assertEquals(f.posts.get(), 0)
      finally f.close()
    }
  }

  test("an unknown saved order stays saved before a failed role check on a new connection") {
    val f = new Fixture(autoStart = false)
    try
      val saved = saveAtStartup(f)
      f.clock.set(saved.expiresAfter + 60001L)
      f.lookupFailure = true
      f.worker.start()
      Wait.until("matching role did not refresh the account")(
        f.roleReads.get() == 1 && f.worker.view.position.nonEmpty
      )
      f.roleMatches = false
      f.worker.observe(Event.SocketDown)
      f.lookupFailure = false
      f.fresh(f.worker, time = 2000L)
      Wait.until("owner mismatch was not shown")(
        f.worker.view.message == TradingWorker.ownerMismatchMessage
      )
      assertEquals(f.roleReads.get(), 2)
      assertEquals(f.openedJournal.pending, Some(saved))
      assert(f.worker.view.pending)
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("a failed role check drops recent proof for releasing an unknown saved order") {
    val f = new Fixture(autoStart = false)
    try
      val saved = saveAtStartup(f)
      f.clock.set(saved.expiresAfter + 60001L)
      f.lookupFailure = true
      f.worker.start()
      Wait.until("matching role did not refresh the account")(
        f.roleReads.get() == 1 && f.worker.view.position.nonEmpty
      )
      f.roleMatches = false
      f.worker.observe(Event.SocketDown)
      f.fresh(f.worker, time = 2000L)
      Wait.until("owner mismatch was not shown")(
        f.worker.view.message == TradingWorker.ownerMismatchMessage
      )
      val reads = f.recoveryReads.get()
      f.lookupFailure = false
      Wait.until("unknown order lookup did not complete")(
        f.recoveryReads.get() >= reads + 2 || f.worker.view.outcome.nonEmpty
      )
      assertEquals(f.openedJournal.pending, Some(saved))
      assert(f.worker.view.pending)
      assertEquals(f.worker.view.message, TradingWorker.ownerMismatchMessage)
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("startup matching fill settles even when the role belongs to another owner") {
    val f = new Fixture(autoStart = false, roleMatches = false)
    try
      val saved = saveAtStartup(f)
      f.clock.set(saved.expiresAfter + 60001L)
      f.recoverFilled = true
      f.worker.start()
      Wait.until("fill not recovered")(
        f.worker.view.outcome.contains("XMR recovered filled") && f.roleReads.get() > 0
      )
      assertEquals(f.openedJournal.pending, None)
      assertEquals(f.loads.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  Vector("owner", "agent").foreach { missing =>
    test(s"live startup missing $missing asks for configuration without opening journal") {
      val f = new Fixture(missing = missing)
      try
        Wait.until("configure message missing")(
          f.worker.view.message == "Configure owner and delegated signer"
        )
        assertEquals(f.journalOpens.get(), 0)
        assertEquals(f.posts.get(), 0)
      finally f.close()
    }
  }

  test("fatal journal open failure is not retried on later refreshes") {
    val f = new Fixture(failJournalOpen = true)
    try
      Wait.until("journal failure missing")(
        f.worker.view.message == "Saved order state failed. Restart to recover"
      )
      val reads = f.infoReads.get()
      f.worker.select("BTC")
      Wait.until("later refresh missing")(f.infoReads.get() > reads + 3)
      assertEquals(f.journalOpens.get(), 1)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  Vector("meta", "socket", "userRole").foreach { failure =>
    test(s"startup settles a saved order while $failure fails and waits for account data") {
      val f = new Fixture(autoStart = false)
      try
        saveAtStartup(f)
        f.failedRead = failure
        f.recoverFilled = true
        f.holdRecovery = true
        f.worker.start()
        assert(f.recoveryEntered.await(3, TimeUnit.SECONDS), "startup status lookup missing")
        assert(f.worker.view.pending)
        assertEquals(f.loads.get(), 0)
        intercept[Exception](OrderJournal.open(f.directory, f.config.identity))
        f.recoveryRelease.countDown()
        Wait.until("saved order was not settled")(
          f.worker.view.outcome.contains("XMR recovered filled")
        )
        assertEquals(f.openedJournal.pending, None)
        assert(f.worker.view.pending)
        assert(!f.worker.view.ready)
        assert(!f.worker.submit(buy, "XMR"))
        assertEquals(f.coreReads.get(), 0)
        if failure == "userRole" then assertEquals(f.loads.get(), 0)
        f.failedRead = ""
        f.clock.addAndGet(10001L)
        f.fresh(f.worker, time = 1001L)
        f.ready()
        assert(!f.worker.view.pending)
        assertEquals(f.worker.view.outcome, Some("XMR recovered filled"))
        assertEquals(f.posts.get(), 0)
      finally f.close()
    }
  }

  test("startup failed status lookup keeps the saved order while metadata fails") {
    val f = new Fixture(autoStart = false)
    try
      val saved = saveAtStartup(f)
      f.failedRead = "meta"
      f.lookupFailure = true
      f.worker.start()
      Wait.until("startup status lookup missing")(f.recoveryReads.get() >= 2)
      assertEquals(f.openedJournal.pending, Some(saved))
      assert(f.worker.view.pending)
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.posts.get(), 0)
      f.worker.close()
      val reopened = OrderJournal.open(f.directory, f.config.identity)
      try assertEquals(reopened.pending, Some(saved))
      finally reopened.close()
    finally f.close()
  }

  test(
    "optional held spot balance cannot delay readiness or publish a pre-fill balance after completion"
  ) {
    val f = new Fixture(autoStart = false, balanceMode = "unifiedAccount")
    f.holdBalance = true
    f.holdSecondBalance = true
    f.worker.start()
    try
      assert(f.spotEntered.await(3, TimeUnit.SECONDS))
      f.ready()
      assertEquals(f.worker.view.portfolio.stables, None)
      assert(f.worker.submitReviewed(f.review()))
      Wait.until("order did not finish")(f.worker.view.outcome.nonEmpty)
      f.stable = "999"
      f.position = "1"
      Wait.until("fresh core account did not return")(
        f.worker.view.ready && f.worker.view.position.contains(BigDecimal(1))
      )
      assertEquals(f.worker.view.portfolio.stables, None)
      f.spotRelease.countDown()
      assert(
        f.secondSpotEntered.await(3, TimeUnit.SECONDS),
        "replacement read did not start"
      )
      assertEquals(
        f.worker.view.portfolio.stables,
        None,
        "old balance escaped its completed-order version"
      )
      f.secondSpotRelease.countDown()
      Wait.until("post-fill balance missing")(
        f.worker.view.portfolio.stables.contains(BigDecimal(999))
      )
      assertEquals(f.worker.view.portfolio.entry, Some(BigDecimal(100)))
      assertEquals(f.worker.view.portfolio.liquidation, Some(BigDecimal(50)))
      assertEquals(f.worker.view.portfolio.net, None)
    finally f.close()
  }

  test(
    "expensive account-mode reads are cached while cheap balances and core accounts refresh"
  ) {
    val f = new Fixture(balanceMode = "unifiedAccount")
    try
      f.ready()
      Wait.until("optional balance did not refresh three times")(
        f.spotReads.get() >= 3
      )
      assertEquals(f.modeReads.get(), 1)
      assert(f.coreReads.get() >= 2)
      f.clock.addAndGet(60001L)
      Wait.until("expired mode was not rechecked")(f.modeReads.get() == 2)
    finally f.close()
  }

  test(
    "undocumented or failed balance modes remain unknown without blocking core readiness"
  ) {
    val f = new Fixture(balanceMode = "default")
    try
      f.ready()
      Wait.until("mode was not checked")(f.modeReads.get() > 0)
      assertEquals(f.worker.view.portfolio.stables, None)
      f.balanceFailure = true
      f.clock.addAndGet(60001L)
      f.fresh(f.worker, time = 2000L)
      Wait.until("failed optional request did not complete")(
        f.modeReads.get() == 2
      )
      f.ready()
      assertEquals(f.worker.view.portfolio.stables, None)
      f.clock.addAndGet(10000L)
      Thread.sleep(1200L)
      assertEquals(
        f.modeReads.get(),
        2,
        "failed mode lookup retried inside its ten-second delay"
      )
      f.clock.incrementAndGet()
      Wait.until("failed mode lookup was never retried")(f.modeReads.get() == 3)
    finally f.close()
  }

  test("a paused account stays visible and cannot unlock a key or submit") {
    val f = new Fixture(accountPaused = () => true)
    try
      Wait.until("account refresh did not finish")(f.worker.view.position.nonEmpty)
      assert(!f.worker.view.ready)
      assert(f.worker.view.block.exists(_.contains("Wallet change")))
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.loads.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("pausing a ready account disables submission after its key was loaded") {
    val paused = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(accountPaused = () => paused.get())
    try
      f.ready()
      assertEquals(f.loads.get(), 1)
      paused.set(true)
      val current = f.worker.view
      assert(!current.ready)
      assert(current.block.exists(_.contains("Wallet change")))
      assert(
        workspaceText(current)
          .contains("Account unavailable")
      )
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 0)
      assertEquals(f.loads.get(), 1)
      paused.set(false)
      f.ready()
    finally f.close()
  }

  test(
    "a durable pause which wins before the gate prevents reservation and posting"
  ) {
    val root = tempDirectory("desk-worker-account-")
    val owner = "0x" + "2" * 40
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val writes = new AtomicInteger()
    val f = new Fixture(
      beforeReplace = () => { writes.incrementAndGet(); () },
      orderPermission = run => {
        entered.countDown()
        release.await()
        AccountGate.withOrder(root, "mainnet", owner)(run())
      },
      accountPaused = () => AccountGate.paused(root, "mainnet", owner)
    )
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      Files.writeString(
        root.resolve("mainnet-" + owner.drop(2) + ".paused"),
        "paused"
      )
      release.countDown()
      Wait.until("pause was not shown")(
        f.worker.view.message.contains("Wallet change")
      )
      assertEquals(writes.get(), 0)
      assertEquals(f.posts.get(), 0)
      assert(!f.worker.view.pending)
    finally
      release.countDown()
      f.close()
  }

  test("the real account lock remains held until the exchange call finishes") {
    val root = tempDirectory("desk-worker-account-")
    val owner = "0x" + "2" * 40
    val f =
      new Fixture(orderPermission = run => AccountGate.withOrder(root, "mainnet", owner)(run()))
    def probe(): Int =
      val process = new ProcessBuilder(
        "/usr/bin/python3",
        "-c",
        "import fcntl,sys\nf=open(sys.argv[1],'r+')\ntry: fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)\nexcept BlockingIOError: sys.exit(73)",
        root.resolve("mainnet-" + owner.drop(2) + ".lock").toString
      ).start()
      try
        assert(process.waitFor(3, TimeUnit.SECONDS), "lock probe did not exit")
        process.exitValue()
      finally if process.isAlive then process.destroyForcibly()
    try
      f.ready()
      f.hold = true
      assert(f.worker.submit(buy, "XMR"))
      assert(f.entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.orderPosts.get(), 1)
      assertEquals(probe(), 73)
      f.release.countDown()
      Wait.until("order did not finish")(f.worker.view.message.contains("filled"))
      Wait.until("account lock was not released")(probe() == 0)
    finally f.close()
  }

  test("a wallet change which wins the account lock stops a queued order") {
    val attempts = new AtomicInteger()
    val f = new Fixture(orderPermission = _ => {
      attempts.incrementAndGet()
      throw new IllegalArgumentException("paused")
    })
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("account lock was not checked")(attempts.get() == 1)
      Wait.until("pause was not shown")(
        f.worker.view.message.contains("Wallet change")
      )
      assertEquals(f.posts.get(), 0)
      assert(!f.worker.view.pending)
    finally f.close()
  }

  test("view mode has previews without protected key access or submissions") {
    val f = new Fixture(enabled = false)
    try
      Wait.until("metadata missing")(f.worker.view.rules.nonEmpty)
      assert(f.worker.preview(buy).isRight)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.loads.get(), 0)
      assertEquals(f.warmCalls.get(), 0)
      assertEquals(f.journalOpens.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("a different owner never reaches the protected key") {
    val f = new Fixture(roleMatches = false)
    try
      Wait.until("role failure was not shown")(
        f.worker.view.message.contains("owner")
      )
      assert(!f.worker.view.ready)
      assertEquals(f.loads.get(), 0)
      Thread.sleep(1200)
      assertEquals(f.roleReads.get(), 1)
    finally f.close()
  }

  test(
    "other market events, pongs and top quotes do not refresh old exact depth"
  ) {
    val f = new Fixture()
    try
      f.ready()
      f.clock.addAndGet(1001)
      f.worker.observe(Event.Pong)
      f.worker.observe(
        Event.BboUpdate("BTC", Level("1", "1", 1), Level("2", "1", 1), 2001)
      )
      f.worker.observe(
        Event.BboUpdate(
          "XMR",
          Level("99.99", "20", 1),
          Level("100", "20", 1),
          2001
        )
      )
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.block, Some("selected depth stale"))
      assertEquals(f.worker.view.blockCode, BlockCode.Prices)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("an equal-time fuller book replaces a short exact book") {
    val f = new Fixture(enabled = false)
    try
      Wait.until("metadata missing")(f.worker.view.rules.nonEmpty)
      f.worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("99.99", "20", 1)),
            Vector(Level("100", "1", 1)),
            2000
          ),
          true
        )
      )
      f.worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("99.99", "20", 1)),
            Vector(Level("100", "1", 1), Level("100.01", "20", 1)),
            2000
          ),
          false
        )
      )
      assertEquals(
        f.worker.preview(buy).toOption.get.estimatedQuantity,
        BigDecimal("5")
      )
    finally f.close()
  }

  test(
    "held posts do not block observations, market selection or the production fold"
  ) {
    val f = new Fixture()
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", 10000))
    val feed = new Feed(
      new Connector {
        def connect(t: String => Unit, c: () => Unit): Socket =
          throw new IllegalStateException()
      },
      new Rest { def post(body: String): Option[String] = Some("[]") },
      queue,
      () => 10000L,
      "XMR"
    )
    val threads = Main.startFold(queue, state, feed, () => 10000L)
    try
      f.ready()
      f.hold = true
      assert(f.worker.submit(buy, "XMR"))
      assert(f.entered.await(3, TimeUnit.SECONDS))
      assert(!f.worker.submit(buy, "XMR"))
      f.worker.select("BTC")
      queue.put(Event.Resize(120, 40))
      queue.put(Event.Key(Command.Quit))
      Wait.until("fold stopped behind post")(state.get().quit)
      assertEquals(state.get().cols, 120)
      assertEquals(f.worker.view.coin, "BTC")
      assertEquals(f.posts.get(), 1)
      assertEquals(f.release.getCount, 1L)
    finally
      f.close()
      threads.foreach(_.interrupt())
      threads.foreach(_.join(3000))
  }

  test(
    "a lost reply persists across restart and unknown recovery never resubmits"
  ) {
    val f = new Fixture()
    var restarted = Option.empty[TradingWorker]
    try
      f.ready()
      f.lose = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("lost reply did not become unresolved")(
        f.worker.view.message.contains("waiting")
      )
      assert(!f.worker.submit(buy, "XMR"))
      f.worker.close()
      val again = f.create()
      restarted = Some(again)
      again.start()
      f.fresh(again)
      Wait.until("saved order not restored")(again.view.pending)
      Thread.sleep(600)
      assert(!again.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 1)
    finally
      restarted.foreach(_.close())
      f.close()
  }

  test("an err reply recovers a matching filled order without a second post") {
    val f = new Fixture()
    try
      f.ready()
      f.reject = true
      f.recoverFilled = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("err reply did not recover filled order")(
        f.worker.view.outcome.contains("XMR recovered filled")
      )
      assertEquals(f.openedJournal.pending, None)
      assertEquals(f.orderPosts.get(), 1)
      assertEquals(f.worker.view.outcome, Some("XMR recovered filled"))
    finally f.close()
  }

  test("err and unknownOid stay saved until the signed expiry, then release as rejected") {
    val f = new Fixture()
    try
      f.ready()
      f.reject = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("err post missing")(f.orderPosts.get() == 1)
      val expiresAfter = f.lastPost.get()("expiresAfter").num.toLong
      Wait.until("err status lookup missing")(f.recoveryReads.get() > 0)
      assert(f.clock.get() < expiresAfter)
      val early = f.recoveryReads.get()
      // Lookups run serially, so the second start proves the first lookup completed.
      Wait.until("lookup before expiry not completed")(
        f.recoveryReads.get() > early + 1 || !f.worker.view.pending
      )
      assert(f.worker.view.pending, "err and unknownOid released before the signed expiry")
      assert(f.openedJournal.pending.nonEmpty)
      assertEquals(f.worker.view.outcome, None)
      f.clock.set(expiresAfter)
      val boundary = f.recoveryReads.get()
      Wait.until("boundary lookup not completed")(
        f.recoveryReads.get() > boundary + 1 || !f.worker.view.pending
      )
      assert(f.worker.view.pending, "err and unknownOid released at the signed expiry")
      assert(f.openedJournal.pending.nonEmpty)
      f.clock.set(expiresAfter + 1)
      Wait.until("err and unknownOid did not release after expiry")(!f.worker.view.pending)
      assert(f.clock.get() < expiresAfter + 60000L)
      assertEquals(f.openedJournal.pending, None)
      assertEquals(f.worker.view.outcome, Some("XMR order rejected"))
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("an err on an earlier settled order does not release a later lost order") {
    val f = new Fixture()
    try
      f.ready()
      f.reject = true
      f.recoverFilled = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("err order did not settle as filled")(
        f.worker.view.outcome.contains("XMR recovered filled")
      )
      val first = f.lastPost.get()("action")("orders")(0)("c").str
      f.reject = false
      f.recoverFilled = false
      f.lose = true
      f.sendActive()
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("second post missing")(f.orderPosts.get() == 2)
      val reads = f.recoveryReads.get()
      Wait.until("second lookup missing")(f.recoveryReads.get() > reads)
      val saved = f.openedJournal.pending.get
      assertNotEquals(saved.id, first)
      f.clock.set(saved.expiresAfter + 1)
      val late = f.recoveryReads.get()
      Wait.until("lookup after expiry not completed")(
        f.recoveryReads.get() > late + 1 || !f.worker.view.pending
      )
      assert(f.worker.view.pending, "an earlier err released a later lost order")
      assertEquals(f.openedJournal.pending, Some(saved))
      assertEquals(f.orderPosts.get(), 2)
    finally f.close()
  }

  test("err reply with a failed lookup stays saved and pending") {
    val f = new Fixture()
    try
      f.ready()
      f.reject = true
      f.lookupFailure = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("err status lookup missing")(f.recoveryReads.get() > 0)
      Thread.sleep(100)
      assert(f.worker.view.pending)
      assert(f.openedJournal.pending.nonEmpty)
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("a failed post stays saved until unknownOid is past expiry and never resends") {
    val f = new Fixture()
    try
      f.ready()
      f.lose = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("lookup missing")(f.recoveryReads.get() > 0)
      val saved = f.openedJournal.pending.get
      f.clock.set(saved.expiresAfter + 60000L)
      val reads = f.recoveryReads.get()
      // Lookups run serially, so the second start proves the first boundary lookup completed.
      Wait.until("boundary lookup not completed")(f.recoveryReads.get() > reads + 1)
      assert(f.worker.view.pending)
      assertEquals(f.openedJournal.pending, Some(saved))
      f.clock.incrementAndGet()
      Wait.until("expired unknown order not released")(!f.worker.view.pending)
      assertEquals(f.openedJournal.pending, None)
      assertEquals(f.orderPosts.get(), 1)
      assert(f.worker.view.outcome.contains("XMR not placed"))
    finally f.close()
  }

  test("an expired unknown order stays saved after a successful role check goes stale") {
    val f = new Fixture()
    try
      f.ready()
      f.lose = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("lookup missing")(f.recoveryReads.get() > 0)
      val saved = f.openedJournal.pending.get
      val roles = f.roleReads.get()
      f.roleMatches = false
      f.clock.set(saved.expiresAfter + 60001L)
      Wait.until("stale role was not checked again")(
        f.roleReads.get() > roles &&
          f.worker.view.message == TradingWorker.ownerMismatchMessage
      )
      val reads = f.recoveryReads.get()
      Wait.until("unknown lookup after failed role check not completed")(
        f.recoveryReads.get() > reads + 1
      )
      assertEquals(f.openedJournal.pending, Some(saved))
      assert(f.worker.view.pending)
      assertEquals(f.worker.view.outcome, None)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("restart releases an expired unknown order without resending") {
    val f = new Fixture()
    var restarted = Option.empty[TradingWorker]
    try
      f.ready()
      f.lose = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("lookup missing")(f.recoveryReads.get() > 0)
      f.worker.close()
      f.clock.set(f.openedJournal.pending.get.expiresAfter + 60001L)
      val again = f.create()
      restarted = Some(again)
      again.start()
      Wait.until("restart did not release expired order")(
        again.view.outcome.contains("XMR not placed")
      )
      Wait.until("restart remained busy")(!again.view.pending)
      assertEquals(f.openedJournal.pending, None)
      assertEquals(f.orderPosts.get(), 1)
    finally
      restarted.foreach(_.close())
      f.close()
  }

  test("failed lookup after expiry keeps the order saved") {
    val f = new Fixture()
    try
      f.ready()
      f.lose = true
      f.lookupFailure = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("lookup missing")(f.recoveryReads.get() > 0)
      val saved = f.openedJournal.pending.get
      f.clock.set(saved.expiresAfter + 60001L)
      val reads = f.recoveryReads.get()
      Wait.until("late lookup missing")(f.recoveryReads.get() > reads + 1)
      assert(f.worker.view.pending)
      assertEquals(f.openedJournal.pending, Some(saved))
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test(
    "partial response size is shown but position comes from a later account read"
  ) {
    val f = new Fixture()
    try
      f.ready()
      f.position = "7"
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("partial response not shown")(
        f.worker.view.message.contains("filled 1 at 100")
      )
      Wait.until("account position was not refreshed")(f.worker.view.ready)
      assertEquals(f.worker.view.position, Some(BigDecimal("7")))
      assertEquals(f.posts.get(), 1)
    finally f.close()
  }

  test("a held reservation cannot send after a market change") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val held = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(beforeReplace =
      () =>
        if held.compareAndSet(false, true) then
          entered.countDown()
          release.await()
    )
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      f.worker.select("BTC")
      release.countDown()
      Wait.until("press was not canceled")(
        f.worker.view.message.contains("canceled before sending")
      )
      assertEquals(f.posts.get(), 0)
    finally
      release.countDown()
      f.close()
  }

  test(
    "reservation failure stays blocked even without an in-memory pending record"
  ) {
    val f = new Fixture(beforeReplace = () => throw new java.io.IOException())
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("storage failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      Thread.sleep(1200)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("failure after reservation leaves its durable order for restart") {
    val f = new Fixture(afterReplace = () => throw new java.io.IOException())
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("post-reservation failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      assertEquals(f.posts.get(), 0)
      f.worker.close()
      val disk = OrderJournal.open(f.directory, f.config.identity)
      try assert(disk.pending.nonEmpty)
      finally disk.close()
    finally f.close()
  }

  test(
    "completion storage failure cannot be unlocked by a later account poll"
  ) {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace =
      () => if writes.incrementAndGet() == 2 then throw new java.io.IOException()
    )
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("completion failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      Thread.sleep(1200)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 1)
    finally f.close()
  }

  test(
    "an account read started before completion cannot unlock the next press"
  ) {
    val freshEntered = new CountDownLatch(1)
    val freshRelease = new CountDownLatch(1)
    val f = new Fixture(beforeAccountRead =
      read =>
        if read == 3 then
          freshEntered.countDown()
          freshRelease.await()
    )
    try
      f.ready()
      f.holdAccount = true
      assert(f.accountEntered.await(3, TimeUnit.SECONDS))
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("order response was not processed")(
        f.worker.view.message.contains("filled 1 at 100")
      )
      f.position = "7"
      f.accountRelease.countDown()
      assert(
        freshEntered.await(300, TimeUnit.MILLISECONDS),
        "post-order wake was lost during the older read"
      )
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.position, Some(BigDecimal(0)))
      freshRelease.countDown()
      Wait.until("new account read did not unlock input")(f.worker.view.ready)
      assertEquals(f.worker.view.position, Some(BigDecimal("7")))
    finally
      freshRelease.countDown()
      f.close()
  }

  test("changed leverage after capture cancels the order before sending") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val held = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(beforeReplace =
      () =>
        if held.compareAndSet(false, true) then
          entered.countDown()
          release.await()
    )
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      f.leverage = 3
      f.sendActive()
      release.countDown()
      Wait.until("changed leverage did not cancel the captured press")(
        f.orderPosts.get() != 0 || f.worker.view.message
          .contains("canceled before sending")
      )
      assertEquals(f.orderPosts.get(), 0)
    finally
      release.countDown()
      f.close()
  }

  test(
    "an active-data failure does not repeat a still-current signer role request"
  ) {
    val f = new Fixture()
    try
      f.ready()
      assertEquals(f.roleReads.get(), 1)
      f.publishActive = false
      f.activeReceive(None)
      assert(!f.worker.view.ready)
      val before = f.coreReads.get()
      Wait.until("account did not refresh after active-data failure")(
        f.coreReads.get() > before
      )
      assertEquals(f.roleReads.get(), 1)
      assert(!f.worker.view.ready)
    finally f.close()
  }

  test("quiet active data cannot be refreshed by a position read or quote") {
    val f = new Fixture()
    try
      f.ready()
      f.publishActive = false
      f.clock.addAndGet(2001)
      f.fresh(f.worker, time = 3001)
      Thread.sleep(1200)
      assert(!f.worker.view.ready)
      f.sendActive()
      assert(f.worker.view.ready)
    finally f.close()
  }

  test(
    "role checks are cached and a connection generation change invalidates the cache"
  ) {
    val f = new Fixture()
    try
      f.ready()
      Thread.sleep(1200)
      assertEquals(f.roleReads.get(), 1)
      f.worker.observe(Event.SocketDown)
      assert(!f.worker.view.ready)
      f.fresh(f.worker, time = 2000L)
      Wait.until("connection generation did not verify the owner again")(
        f.roleReads.get() == 2
      )
      assertEquals(f.loads.get(), 1)
    finally f.close()
  }

  test("active-only failures cannot extend the existing signer role lifetime") {
    val f = new Fixture()
    try
      f.ready()
      assertEquals(f.roleReads.get(), 1)
      f.activeReceive(None)
      f.clock.addAndGet(60001L)
      f.fresh(f.worker, time = 61001L)
      Wait.until("expired role verification was not refreshed")(
        f.roleReads.get() == 2
      )
      assertEquals(f.loads.get(), 1)
      f.ready()
    finally f.close()
  }

  test("recovery cannot complete a current submission twice") {
    val signingEntered = new CountDownLatch(1)
    val signingRelease = new CountDownLatch(1)
    val f = new Fixture(beforeOrderSign = () =>
      signingEntered.countDown()
      signingRelease.await())
    try
      f.ready()
      f.holdRecovery = true
      f.recoverFilled = true
      assert(f.worker.submit(buy, "XMR"))
      assert(signingEntered.await(3, TimeUnit.SECONDS))
      val _ = f.recoveryEntered.await(1500, TimeUnit.MILLISECONDS)
      signingRelease.countDown()
      Wait.until("the direct order result did not complete")(
        f.worker.view.message.contains("filled 1 at 100")
      )
      f.recoveryRelease.countDown()
      Wait.until("the completed submission did not settle")(
        f.worker.view.ready || f.worker.view.message
          .contains("Restart to recover")
      )
      assert(
        !f.worker.view.message.contains("Restart to recover"),
        f.worker.view.message
      )
      assert(f.worker.view.ready)
      assertEquals(f.posts.get(), 1)
      assertEquals(f.recoveryReads.get(), 0)
      assert(!f.worker.view.message.contains("Restart to recover"))
    finally
      signingRelease.countDown()
      f.close()
  }

  test(
    "held signing preparation leaves startup and view responsive before readiness"
  ) {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val returned = new CountDownLatch(1)
    val f = new Fixture(
      warmSigning = () =>
        entered.countDown()
        release.await()
      ,
      autoStart = false
    )
    val caller = new Thread(() =>
      f.worker.start()
      returned.countDown()
    )
    val answers = new LinkedBlockingQueue[(TradingView, Boolean)]()
    val reader = new Thread(() => answers.put((f.worker.view, f.worker.submit(buy, "XMR"))))
    caller.setDaemon(true)
    reader.setDaemon(true)
    try
      caller.start()
      assert(entered.await(3, TimeUnit.SECONDS))
      assert(
        returned.await(3, TimeUnit.SECONDS),
        "start waited for signing preparation"
      )
      reader.start()
      val answer = answers.poll(3, TimeUnit.SECONDS)
      assert(answer != null, "view or submit waited for signing preparation")
      assert(!answer._1.ready)
      assert(!answer._2)
      assertEquals(f.loads.get(), 0)
      assertEquals(f.journalOpens.get(), 0)
      assertEquals(f.posts.get(), 0)
      release.countDown()
      f.ready()
      assertEquals(f.warmCalls.get(), 1)
    finally
      release.countDown()
      caller.join(3000)
      reader.join(3000)
      f.close()
  }

  test("signing preparation runs once and adds no order or saved entry") {
    val f = new Fixture()
    try
      f.ready()
      assertEquals(f.warmCalls.get(), 1)
      assertEquals(f.loads.get(), 1)
      assertEquals(f.journalOpens.get(), 1)
      assertEquals(f.posts.get(), 0)
      assert(!Files.exists(f.directory.resolve(f.config.identity + ".json")))
      f.worker.start()
      f.worker.observe(Event.SocketDown)
      f.fresh(f.worker, time = 2000)
      f.ready()
      assertEquals(f.warmCalls.get(), 1)
      assertEquals(f.posts.get(), 0)
      assert(f.worker.submit(buy, "XMR"))
      assert(f.entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.posts.get(), 1)
      assertEquals(f.orderPosts.get(), 1)
      assertEquals(f.warmCalls.get(), 1)
    finally f.close()
  }

  test(
    "failed signing preparation blocks trading before protected or saved state access"
  ) {
    val accountThread = new AtomicReference[Thread]()
    val f = new Fixture(warmSigning = () =>
      accountThread.set(Thread.currentThread())
      throw new IllegalStateException("test failure"))
    try
      Wait.until("signing preparation failure was not shown")(
        f.worker.view.message == "Order signing could not be prepared. Restart to retry"
      )
      val stopped = accountThread.get()
      assert(
        stopped != null,
        "failed preparation did not run on the account thread"
      )
      stopped.join(3000)
      assert(
        !stopped.isAlive,
        "account thread continued after signing preparation failed"
      )
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      f.worker.start()
      f.worker.select("BTC")
      f.fresh(f.worker, coin = "BTC", time = 2000)
      assert(!f.worker.view.ready)
      assertEquals(f.warmCalls.get(), 1)
      assertEquals(f.loads.get(), 0)
      assertEquals(f.journalOpens.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  private def checkCapturedBoundary(
      prepare: Fixture => Unit,
      changed: Fixture => Unit
  ): Unit =
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(beforeOrderSign = () =>
      entered.countDown()
      release.await())
    try
      f.ready()
      prepare(f)
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      changed(f)
      release.countDown()
      Wait.until("captured press did not settle")(
        f.worker.view.message.contains("canceled before sending") ||
          f.worker.view.message.contains("filled 1 at 100")
      )
      assertEquals(f.orderPosts.get(), 0)
      assert(f.worker.view.message.contains("canceled before sending"))
    finally
      release.countDown()
      f.close()

  test("a captured press cannot survive switching away and back") {
    checkCapturedBoundary(
      _ => (),
      f =>
        f.worker.select("BTC")
        f.worker.select("XMR")
        f.fresh(f.worker, time = 2000)
        Wait.until("new selected account was not received")(
          f.worker.view.position.nonEmpty && f.worker.view.rules.nonEmpty &&
            f.roleReads.get() >= 2
        )
    )
  }

  test("fresh exact depth cannot renew an older top quote receipt") {
    val f = new Fixture()
    try
      f.ready()
      f.worker.observe(
        Event.BboUpdate(
          "XMR",
          Level("99.99", "20", 1),
          Level("100", "20", 1),
          2000
        )
      )
      f.clock.addAndGet(1001)
      f.fresh(f.worker, time = 1001)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.orderPosts.get(), 0)
    finally f.close()
  }

  test("a same-time top quote cannot renew an old exact book receipt") {
    val f = new Fixture()
    try
      f.ready()
      f.clock.addAndGet(1001)
      f.worker.observe(
        Event.BboUpdate(
          "XMR",
          Level("99.99", "20", 1),
          Level("100", "20", 1),
          1000
        )
      )
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.orderPosts.get(), 0)
    finally f.close()
  }

  test("new current data cannot renew a captured old top quote") {
    checkCapturedBoundary(
      f =>
        f.worker.observe(
          Event.BboUpdate(
            "XMR",
            Level("99.99", "20", 1),
            Level("100", "20", 1),
            2000
          )
        )
        f.clock.addAndGet(800)
        f.fresh(f.worker, time = 1001)
      ,
      f =>
        f.clock.addAndGet(201)
        f.fresh(f.worker, time = 2001)
    )
  }

  test("new current data cannot renew a captured old exact book") {
    checkCapturedBoundary(
      f =>
        f.clock.addAndGet(800)
        f.worker.observe(
          Event.BboUpdate(
            "XMR",
            Level("99.99", "20", 1),
            Level("100", "20", 1),
            1000
          )
        )
      ,
      f =>
        f.clock.addAndGet(201)
        f.fresh(f.worker, time = 2001)
    )
  }

  test("fresh market and active data cannot renew an old account position") {
    val f = new Fixture()
    try
      f.ready()
      f.holdAccount = true
      assert(f.accountEntered.await(3, TimeUnit.SECONDS))
      f.clock.addAndGet(2001)
      f.sendActive()
      f.fresh(f.worker, time = 3001)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.orderPosts.get(), 0)
    finally f.close()
  }

  test("new current data cannot renew a captured old account position") {
    checkCapturedBoundary(
      f =>
        f.holdAccount = true
        assert(f.accountEntered.await(3, TimeUnit.SECONDS))
        f.clock.addAndGet(1800)
        f.sendActive()
        f.fresh(f.worker, time = 2800)
      ,
      f =>
        f.clock.addAndGet(201)
        f.position = "7"
        f.accountRelease.countDown()
        Wait.until("new account position was not received")(
          f.worker.view.position.contains(BigDecimal("7"))
        )
    )
  }

  test("new current data cannot renew captured old active data") {
    checkCapturedBoundary(
      f =>
        f.publishActive = false
        f.clock.addAndGet(1800)
        f.position = "7"
        f.fresh(f.worker, time = 2800)
        Wait.until("new account position was not received")(
          f.worker.view.position.contains(BigDecimal("7"))
        )
      ,
      f =>
        f.clock.addAndGet(201)
        f.sendActive()
    )
  }

  test("a fatal completion write cannot be healed by later terminal recovery") {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace =
      () => if writes.incrementAndGet() == 2 then throw new java.io.IOException()
    )
    try
      f.ready()
      f.recoverFilled = true
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("completion failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      f.position = "7"
      Wait.until("later account read did not run")(
        f.worker.view.position.contains(BigDecimal("7"))
      )
      Thread.sleep(1200)
      assertEquals(f.recoveryReads.get(), 0)
      assertEquals(writes.get(), 2)
      assert(!f.worker.view.ready)
      assert(f.worker.view.message.contains("Restart to recover"))
    finally f.close()
  }

  test(
    "a request cannot reach the exchange while durable reservation is held"
  ) {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val held = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(beforeReplace =
      () =>
        if held.compareAndSet(false, true) then
          entered.countDown()
          release.await()
    )
    try
      f.ready()
      f.hold = true
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      assert(!f.entered.await(1500, TimeUnit.MILLISECONDS))
      assertEquals(f.orderPosts.get(), 0)
      assertEquals(release.getCount, 1L)
    finally
      release.countDown()
      f.close()
  }

  test(
    "a retained fill cannot hide current selected depth or account freshness failures"
  ) {
    val f = new Fixture()
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("fill missing")(f.worker.view.message.contains("filled 1 at 100"))
      f.ready()
      f.publishActive = false
      f.clock.addAndGet(2001)
      f.worker.observe(Event.Pong)
      f.worker.observe(
        Event.BboUpdate("BTC", Level("1", "1", 1), Level("2", "1", 1), 3001)
      )
      val depth =
        workspaceText(f.worker.view)
      assert(depth.contains("Waiting for prices"), depth)
      assert(f.worker.view.outcome.exists(_.contains("XMR filled 1 at 100")))
      assert(!depth.contains("Filled #1"), depth)
      f.fresh(f.worker, time = 3001)
      val account =
        workspaceText(f.worker.view)
      assert(account.contains("Account unavailable"), account)
      assert(f.worker.view.outcome.exists(_.contains("XMR filled 1 at 100")))
      assert(!account.contains("Filled #1"), account)
      f.readFailure = true
      Wait.until("failed refresh not observed")(
        f.worker.view.message == "Waiting for market or account data"
      )
      val failed =
        workspaceText(f.worker.view)
      assert(failed.contains("Account unavailable"), failed)
      assert(f.worker.view.outcome.exists(_.contains("XMR filled 1 at 100")))
      assert(!failed.contains("Filled #1"), failed)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      f.worker.observe(Event.SocketDown)
      val down =
        workspaceText(f.worker.view)
      assert(down.contains("Reconnecting"), down)
      assert(f.worker.view.outcome.exists(_.contains("XMR filled 1 at 100")))
      assert(!down.contains("Filled #1"), down)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test(
    "a new failed attempt replaces an earlier fill while blocked input preserves it"
  ) {
    val f = new Fixture()
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      Wait.until("fill missing")(f.worker.view.message.contains("filled 1 at 100"))
      f.ready()
      assert(!f.worker.submit(buy, "BTC"))
      assert(
        workspaceText(f.worker.view)
          .contains("Filled #1")
      )
      f.worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("999999999", "20", 1)),
            Vector(Level("1000000000", "20", 1)),
            2000
          ),
          true
        )
      )
      assert(f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.worker.view.message, "No visible quantity fits this order")
      assertEquals(f.worker.view.outcome, None)
      val controller = new DeskController(
        PanelView(mode = "LIVE"),
        () => f.clock.get(),
        f.worker.preview,
        _ => fail("an invalid preview submitted an order"),
        _ => Left("unused")
      )
      val snapshot = Snapshot.initial("XMR", f.clock.get())
      def draw() = Workspace.draw(
        snapshot,
        f.worker.view,
        controller.panel,
        160,
        45,
        f.clock.get(),
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      controller.publish(draw())
      controller.handle(Command.PlaceOrder(buy), snapshot, f.worker.view, (160, 45), false)
      val text = draw().lines.mkString("\n")
      assert(text.contains("No visible quantity fits this order"), text)
      assert(!text.contains("Filled #1"), text)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("XMR at five permits either long or short from a flat live account") {
    Vector(Side.Buy, Side.Sell).foreach { side =>
      val f = new Fixture(marginMode = "isolated")
      try
        f.ready()
        assertEquals(f.worker.view.position, Some(BigDecimal(0)))
        assertEquals(f.worker.view.leverage, Some(5))
        assertEquals(f.worker.view.marginMode, Some("isolated"))
        assert(f.worker.submitReviewed(f.review(buy.copy(side = side))))
        assert(f.entered.await(3, TimeUnit.SECONDS))
        val action = f.lastPost.get()("action")
        assertEquals(action("type").str, "order")
        assertEquals(action("orders")(0)("b").bool, side == Side.Buy)
        assertEquals(action("orders")(0)("r").bool, false)
        assertEquals(f.posts.get(), 1)
      finally f.close()
    }
  }

  test(
    "startup observes leverage and margin without an account mutation or nonce write"
  ) {
    val writes = new AtomicInteger()
    val f = new Fixture(
      initialLeverage = 3,
      marginMode = "isolated",
      beforeReplace = () => { writes.incrementAndGet(); () }
    )
    try
      Wait.until("account was not observed")(f.worker.view.position.nonEmpty)
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.block, Some("Set XMR leverage to 5x"))
      assertEquals(f.worker.view.blockCode, BlockCode.Leverage)
      Thread.sleep(1200)
      assertEquals(f.posts.get(), 0)
      assertEquals(writes.get(), 0)
      assertEquals(f.leverage, 3)
      assertEquals(f.worker.view.leverage, Some(3))
      assertEquals(f.worker.view.marginMode, Some("isolated"))
    finally f.close()
  }

  test("XMR below five stays disabled without an account mutation") {
    val f = new Fixture(initialLeverage = 1, marketMaximum = 2)
    try
      Wait.until("account was not observed")(f.worker.view.position.nonEmpty)
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.block, Some("Set XMR leverage to 5x"))
      assertEquals(f.worker.view.blockCode, BlockCode.Leverage)
      assertEquals(f.worker.view.leverage, Some(1))
      assertEquals(f.posts.get(), 0)
      assert(f.worker.preview(buy).isRight)
    finally f.close()
  }

  test(
    "invalid leverage never becomes ready and never triggers a corrective post"
  ) {
    Vector(0, 6).foreach { leverage =>
      val f = new Fixture(initialLeverage = leverage, loseLeverage = true)
      try
        Wait.until("account was not observed")(f.worker.view.position.nonEmpty)
        Thread.sleep(100)
        assert(!f.worker.view.ready)
        assertEquals(f.worker.view.leverage, None)
        assertEquals(f.posts.get(), 0)
      finally f.close()
    }
  }

  test("an unchanged review submits exactly its reviewed signed order fields") {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      assert(f.worker.submitReviewed(review))
      assert(f.entered.await(3, TimeUnit.SECONDS))
      val action = f.lastPost.get()("action")
      val order = action("orders")(0)
      assertEquals(action("type").str, "order")
      assertEquals(order("a").num.toInt, review.plan.asset)
      assertEquals(order("b").bool, review.plan.side == Side.Buy)
      assertEquals(order("s").str, Trading.format(review.plan.quantity))
      assertEquals(order("p").str, Trading.format(review.plan.limit))
      assertEquals(order("t")("limit")("tif").str, "Ioc")
      assertEquals(order("r").bool, false)
      assertEquals(f.worker.view.lastPlan, Some(review.plan))
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("review equality covers every plan field and the intent") {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      val p = review.plan
      val changes = Vector(
        p.copy(coin = "BTC"),
        p.copy(asset = p.asset + 1),
        p.copy(side = Side.Sell),
        p.copy(kind = OrderKind.Best),
        p.copy(quantity = p.quantity + 1),
        p.copy(limit = p.limit + 1),
        p.copy(reference = p.reference + 1),
        p.copy(estimatedQuantity = p.estimatedQuantity + 1),
        p.copy(estimatedValue = p.estimatedValue + 1),
        p.copy(average = None),
        p.copy(positionAfter = p.positionAfter + 1)
      )
      changes.foreach { changed =>
        assert(
          !f.worker.submitReviewed(review.copy(plan = changed)),
          changed.toString
        )
      }
      assert(
        !f.worker.submitReviewed(
          review.copy(intent = buy.copy(side = Side.Sell))
        )
      )
      assertEquals(f.posts.get(), 0)
      assert(f.worker.submitReviewed(review))
    finally f.close()
  }

  test("expired and future-dated reviews are refused before any reservation") {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace = () => { writes.incrementAndGet(); () })
    try
      f.ready()
      val review = f.review()
      assert(!f.worker.submitReviewed(review.copy(expiresAt = f.clock.get())))
      assert(
        !f.worker.submitReviewed(review.copy(createdAt = f.clock.get() + 1))
      )
      assert(
        !f.worker.submitReviewed(review.copy(expiresAt = review.createdAt - 1))
      )
      assertEquals(writes.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("a fresh changed quote requires a new review") {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      f.worker.observe(
        Event.BboUpdate(
          "XMR",
          Level("100", "20", 1),
          Level("100.01", "20", 1),
          1001
        )
      )
      assert(f.worker.view.ready)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test(
    "fresh position changes require a new review even when signed size and limit stay equal"
  ) {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      f.position = "7"
      Wait.until("new position was not observed")(
        f.worker.view.position.contains(BigDecimal(7))
      )
      assert(f.worker.view.ready)
      assertEquals(
        f.worker.preview(buy).toOption.get.quantity,
        review.plan.quantity
      )
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test(
    "review captures observed leverage and margin mode including missing snapshots"
  ) {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      assert(!f.worker.submitReviewed(review.copy(leverage = None)))
      assert(!f.worker.submitReviewed(review.copy(marginMode = None)))
      f.leverage = 3
      f.sendActive()
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.block, Some("Set XMR leverage to 5x"))
      assertEquals(f.worker.view.blockCode, BlockCode.Leverage)
      assert(!f.worker.submitReviewed(review))
      f.leverage = 5
      f.activeMode = "isolated"
      f.sendActive()
      assert(f.worker.view.ready)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  private def heldReviewed(change: Fixture => Unit): Unit =
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(beforeOrderSign = () => {
      entered.countDown(); release.await()
    })
    try
      f.ready()
      assert(f.worker.submitReviewed(f.review(lifetime = 500L)))
      assert(entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.posts.get(), 0)
      change(f)
      release.countDown()
      Wait.until("reviewed order was not canceled")(
        f.orderPosts.get() != 0 || f.worker.view.message
          .contains("canceled before sending")
      )
      assertEquals(f.posts.get(), 0)
    finally
      release.countDown()
      f.close()

  test(
    "review expiry is rechecked after durable reservation and signing delay"
  ) {
    heldReviewed(_.clock.addAndGet(500L))
  }

  test("margin mode is rechecked after durable reservation and signing delay") {
    heldReviewed { f => f.activeMode = "isolated"; f.sendActive() }
  }

  test(
    "a supplied review cannot extend the controller's thirty second lifetime"
  ) {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace = () => { writes.incrementAndGet(); () })
    try
      f.ready()
      assert(!f.worker.submitReviewed(f.review(lifetime = 30001L)))
      assertEquals(writes.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test(
    "extreme timestamp subtraction cannot turn an oversized review into a valid one"
  ) {
    val f = new Fixture()
    try
      f.ready()
      val oversized =
        f.review().copy(createdAt = Long.MinValue, expiresAt = Long.MaxValue)
      assert(!f.worker.submitReviewed(oversized))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("a review at the thirty second boundary remains valid") {
    val f = new Fixture()
    try
      f.ready()
      assert(f.worker.submitReviewed(f.review(lifetime = 30000L)))
      assert(f.entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  private def heldReview(change: Fixture => Unit, cancel: Boolean): Unit =
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(beforeOrderSign = () => {
      entered.countDown(); release.await()
    })
    try
      f.ready()
      val review = f.review(lifetime = 30000L)
      assert(f.worker.submitReviewed(review))
      assert(entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.posts.get(), 0)
      f.clock.addAndGet(250L)
      change(f)
      release.countDown()
      Wait.until("held order neither posted nor canceled") {
        f.orderPosts.get() != 0 || f.worker.view.message
          .contains("canceled before sending")
      }
      assertEquals(f.orderPosts.get(), if cancel then 0 else 1)
      if !cancel then
        val order = f.lastPost.get()("action")("orders")(0)
        assertEquals(order("s").str, Trading.format(review.plan.quantity))
        assertEquals(order("p").str, Trading.format(review.plan.limit))
    finally
      release.countDown()
      f.close()

  test(
    "a fresh position change after acceptance cancels before exchange post"
  ) {
    heldReview(
      f => {
        val before = f.worker.preview(buy).toOption.get
        f.position = "7"
        Wait.until("new position was not observed during signing") {
          f.worker.view.position.contains(BigDecimal(7))
        }
        val after = f.worker.preview(buy).toOption.get
        assertEquals(after.quantity, before.quantity)
        assertEquals(after.limit, before.limit)
        assert(after.positionAfter != before.positionAfter)
      },
      cancel = true
    )
  }

  test("a fresh quote change after acceptance cancels before exchange post") {
    heldReview(
      f => {
        f.worker.observe(
          Event.BboUpdate(
            "XMR",
            Level("100", "20", 1),
            Level("100.01", "20", 1),
            1001L
          )
        )
      },
      cancel = true
    )
  }

  test("unchanged reviewed inputs survive an asynchronous signing delay") {
    heldReview(_ => (), cancel = false)
  }

  test("legacy capture also cancels when margin mode changes during signing") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(beforeOrderSign = () => {
      entered.countDown(); release.await()
    })
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      f.activeMode = "isolated"
      f.sendActive()
      release.countDown()
      Wait.until("held legacy order neither posted nor canceled") {
        f.orderPosts.get() != 0 || f.worker.view.message
          .contains("canceled before sending")
      }
      assertEquals(f.orderPosts.get(), 0)
    finally
      release.countDown()
      f.close()
  }
