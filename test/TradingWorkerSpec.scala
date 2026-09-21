package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class TradingWorkerSpec extends munit.FunSuite:
  private def waitFor(label: String)(condition: => Boolean): Unit =
    val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < end do Thread.sleep(2)
    assert(condition, label)

  private final class Fixture(
      enabled: Boolean = true,
      roleMatches: Boolean = true,
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
      accountPaused: () => Boolean = () => false
  ):
    private val key = Array.fill[Byte](32)(1)
    val agent = OrderSigning.address(key)
    val owner = "0x" + "2" * 40
    val config = TradingConfig(owner = Some(owner), agent = Some(agent))
    val clock = new AtomicLong(10000L)
    val loads = new AtomicInteger()
    val journalOpens = new AtomicInteger()
    val warmCalls = new AtomicInteger()
    val posts = new AtomicInteger()
    val orderPosts = new AtomicInteger()
    val roleReads = new AtomicInteger()
    val recoveryReads = new AtomicInteger()
    val recoveryEntered = new CountDownLatch(1)
    val recoveryRelease = new CountDownLatch(1)
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val accountEntered = new CountDownLatch(1)
    val accountRelease = new CountDownLatch(1)
    val lastPost = new AtomicReference[ujson.Value](ujson.Null)
    @volatile var hold = false
    @volatile var lose = false
    @volatile var position = "0"
    @volatile var readFailure = false
    @volatile var holdAccount = false
    @volatile var leverage = initialLeverage
    @volatile var activeMode = marginMode
    @volatile var holdRecovery = false
    @volatile var recoverFilled = false
    @volatile var publishActive = true
    @volatile var activeCoin = "XMR"
    @volatile var activeReceive: Option[ujson.Value] => Unit = _ => ()
    def sendActive(): Unit = activeReceive(
      Some(
        ujson.Obj(
          "user" -> owner,
          "coin" -> activeCoin,
          "leverage" -> ujson.Obj("type" -> activeMode, "value" -> leverage),
          "maxTradeSzs" -> ujson.Arr("100", "100")
        )
      )
    )
    val directory = Files.createTempDirectory("desk-worker")
    val exchange = new Exchange:
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        activeCoin = coin
        activeReceive = receive
        if publishActive then sendActive()
      def info(body: ujson.Value): Option[ujson.Value] =
        if readFailure then None
        else
          body("type").str match
            case "meta" =>
              Some(
                ujson.Obj(
                  "universe" -> ujson.Arr(
                    ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> marketMaximum),
                    ujson.Obj("name" -> "BTC", "szDecimals" -> 5, "maxLeverage" -> 40)
                  )
                )
              )
            case "userRole" =>
              roleReads.incrementAndGet()
              Some(
                ujson.Obj(
                  "role" -> "agent",
                  "data" -> ujson.Obj("user" -> (if roleMatches then owner else agent))
                )
              )
            case "clearinghouseState" =>
              val captured = position
              if holdAccount then
                holdAccount = false
                accountEntered.countDown()
                accountRelease.await()
              Some(
                ujson.Obj(
                  "assetPositions" -> ujson.Arr(
                    ujson.Obj("position" -> ujson.Obj("coin" -> "XMR", "szi" -> captured))
                  )
                )
              )
            case "orderStatus" =>
              recoveryReads.incrementAndGet()
              if holdRecovery then
                recoveryEntered.countDown()
                recoveryRelease.await()
              if recoverFilled then
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
            Some(ujson.Obj("status" -> "ok", "response" -> ujson.Obj("type" -> "default")))
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
                        ujson.Obj("totalSz" -> "1", "avgPx" -> "100", "oid" -> 9)
                    )
                  )
                )
              )
            )
          )
    def fresh(worker: TradingWorker, coin: String = "XMR", time: Long = 1000L): Unit =
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          coin,
          Book(Vector(Level("99.99", "20", 1)), Vector(Level("100", "20", 1)), time),
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
        OrderJournal.open(directory, config.identity, beforeReplace, afterReplace)
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
    def ready(): Unit = waitFor("worker never became ready")(worker.view.ready)
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
      worker.close()

  private val buy = OrderIntent(Side.Buy, OrderKind.Hard)

  test("a paused account stays visible and cannot unlock a key or submit") {
    val f = new Fixture(accountPaused = () => true)
    try
      waitFor("account refresh did not finish")(f.worker.view.position.nonEmpty)
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
      assert(TradingScreen.rows(current, None, f.worker.preview).mkString.contains("Wallet change"))
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 0)
      assertEquals(f.loads.get(), 1)
      paused.set(false)
      f.ready()
    finally f.close()
  }

  test("a durable pause which wins before the gate prevents reservation and posting") {
    val root = Files.createTempDirectory("desk-worker-account-")
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
      Files.writeString(root.resolve("mainnet-" + owner.drop(2) + ".paused"), "paused")
      release.countDown()
      waitFor("pause was not shown")(f.worker.view.message.contains("Wallet change"))
      assertEquals(writes.get(), 0)
      assertEquals(f.posts.get(), 0)
      assert(!f.worker.view.pending)
    finally
      release.countDown()
      f.close()
      val paths = Files.walk(root)
      try paths.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally paths.close()
  }

  test("the real account lock remains held until the exchange call finishes") {
    val root = Files.createTempDirectory("desk-worker-account-")
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
      waitFor("order did not finish")(f.worker.view.message.contains("filled"))
      waitFor("account lock was not released")(probe() == 0)
    finally
      f.close()
      val paths = Files.walk(root)
      try paths.sorted(java.util.Comparator.reverseOrder()).forEach(Files.deleteIfExists(_))
      finally paths.close()
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
      waitFor("account lock was not checked")(attempts.get() == 1)
      waitFor("pause was not shown")(f.worker.view.message.contains("Wallet change"))
      assertEquals(f.posts.get(), 0)
      assert(!f.worker.view.pending)
    finally f.close()
  }

  test("view mode has previews without protected key access or submissions") {
    val f = new Fixture(enabled = false)
    try
      waitFor("metadata missing")(f.worker.view.rules.nonEmpty)
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
      waitFor("role failure was not shown")(f.worker.view.message.contains("owner"))
      assert(!f.worker.view.ready)
      assertEquals(f.loads.get(), 0)
      Thread.sleep(1200)
      assertEquals(f.roleReads.get(), 1)
    finally f.close()
  }

  test("other market events, pongs and top quotes do not refresh old exact depth") {
    val f = new Fixture()
    try
      f.ready()
      f.clock.addAndGet(1001)
      f.worker.observe(Event.Pong)
      f.worker.observe(Event.BboUpdate("BTC", Level("1", "1", 1), Level("2", "1", 1), 2001))
      f.worker.observe(Event.BboUpdate("XMR", Level("99.99", "20", 1), Level("100", "20", 1), 2001))
      assert(!f.worker.view.ready)
      assertEquals(f.worker.view.block, Some("selected depth stale"))
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("an equal-time fuller book replaces a short exact book") {
    val f = new Fixture(enabled = false)
    try
      waitFor("metadata missing")(f.worker.view.rules.nonEmpty)
      f.worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99.99", "20", 1)), Vector(Level("100", "1", 1)), 2000),
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
      assertEquals(f.worker.preview(buy).toOption.get.estimatedQuantity, BigDecimal("5"))
    finally f.close()
  }

  test("held posts do not block observations, market selection or the production fold") {
    val f = new Fixture()
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", 10000))
    val feed = new Feed(
      new Connector {
        def connect(t: String => Unit, c: () => Unit): Socket = throw new IllegalStateException()
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
      waitFor("fold stopped behind post")(state.get().quit)
      assertEquals(state.get().cols, 120)
      assertEquals(f.worker.view.coin, "BTC")
      assertEquals(f.posts.get(), 1)
      assertEquals(f.release.getCount, 1L)
    finally
      f.close()
      threads.foreach(_.interrupt())
      threads.foreach(_.join(3000))
  }

  test("a lost reply persists across restart and unknown recovery never resubmits") {
    val f = new Fixture()
    var restarted = Option.empty[TradingWorker]
    try
      f.ready()
      f.lose = true
      assert(f.worker.submit(buy, "XMR"))
      waitFor("lost reply did not become unresolved")(f.worker.view.message.contains("waiting"))
      assert(!f.worker.submit(buy, "XMR"))
      f.worker.close()
      val again = f.create()
      restarted = Some(again)
      again.start()
      f.fresh(again)
      waitFor("saved order not restored")(again.view.pending)
      Thread.sleep(600)
      assert(!again.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 1)
    finally
      restarted.foreach(_.close())
      f.close()
  }

  test("partial response size is shown but position comes from a later account read") {
    val f = new Fixture()
    try
      f.ready()
      f.position = "7"
      assert(f.worker.submit(buy, "XMR"))
      waitFor("partial response not shown")(f.worker.view.message.contains("filled 1 at 100"))
      waitFor("account position was not refreshed")(f.worker.view.ready)
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
      waitFor("press was not canceled")(f.worker.view.message.contains("canceled before sending"))
      assertEquals(f.posts.get(), 0)
    finally
      release.countDown()
      f.close()
  }

  test("reservation failure stays blocked even without an in-memory pending record") {
    val f = new Fixture(beforeReplace = () => throw new java.io.IOException())
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      waitFor("storage failure was not latched")(
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
      waitFor("post-reservation failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      assertEquals(f.posts.get(), 0)
      f.worker.close()
      val disk = OrderJournal.open(f.directory, f.config.identity)
      try assert(disk.pending.nonEmpty)
      finally disk.close()
    finally f.close()
  }

  test("completion storage failure cannot be unlocked by a later account poll") {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace =
      () => if writes.incrementAndGet() == 2 then throw new java.io.IOException()
    )
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      waitFor("completion failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      Thread.sleep(1200)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.posts.get(), 1)
    finally f.close()
  }

  test("an account read started before completion cannot unlock the next press") {
    val f = new Fixture()
    try
      f.ready()
      f.holdAccount = true
      assert(f.accountEntered.await(3, TimeUnit.SECONDS))
      assert(f.worker.submit(buy, "XMR"))
      waitFor("order response was not processed")(f.worker.view.message.contains("filled 1 at 100"))
      f.position = "7"
      f.accountRelease.countDown()
      Thread.sleep(50)
      assert(!f.worker.view.ready)
      waitFor("new account read did not unlock input")(f.worker.view.ready)
      assertEquals(f.worker.view.position, Some(BigDecimal("7")))
    finally f.close()
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
      waitFor("changed leverage did not cancel the captured press")(
        f.orderPosts.get() != 0 || f.worker.view.message.contains("canceled before sending")
      )
      assertEquals(f.orderPosts.get(), 0)
    finally
      release.countDown()
      f.close()
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

  test("role checks are cached and an account stream reconnect invalidates the cache") {
    val f = new Fixture()
    try
      f.ready()
      Thread.sleep(1200)
      assertEquals(f.roleReads.get(), 1)
      f.activeReceive(None)
      assert(!f.worker.view.ready)
      waitFor("stream restart did not verify the owner again")(f.roleReads.get() == 2)
      assertEquals(f.loads.get(), 1)
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
      waitFor("the direct order result did not complete")(
        f.worker.view.message.contains("filled 1 at 100")
      )
      f.recoveryRelease.countDown()
      waitFor("the completed submission did not settle")(
        f.worker.view.ready || f.worker.view.message.contains("Restart to recover")
      )
      assert(!f.worker.view.message.contains("Restart to recover"), f.worker.view.message)
      assert(f.worker.view.ready)
      assertEquals(f.posts.get(), 1)
      assertEquals(f.recoveryReads.get(), 0)
      assert(!f.worker.view.message.contains("Restart to recover"))
    finally
      signingRelease.countDown()
      f.close()
  }

  test("held signing preparation leaves startup and view responsive before readiness") {
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
      assert(returned.await(3, TimeUnit.SECONDS), "start waited for signing preparation")
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

  test("failed signing preparation blocks trading before protected or saved state access") {
    val accountThread = new AtomicReference[Thread]()
    val f = new Fixture(warmSigning = () =>
      accountThread.set(Thread.currentThread())
      throw new IllegalStateException("test failure"))
    try
      waitFor("signing preparation failure was not shown")(
        f.worker.view.message == "Order signing could not be prepared. Restart to retry"
      )
      val stopped = accountThread.get()
      assert(stopped != null, "failed preparation did not run on the account thread")
      stopped.join(3000)
      assert(!stopped.isAlive, "account thread continued after signing preparation failed")
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
      waitFor("captured press did not settle")(
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
        waitFor("new selected account was not received")(
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
        waitFor("new account position was not received")(
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
        waitFor("new account position was not received")(
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
      waitFor("completion failure was not latched")(
        f.worker.view.message.contains("Restart to recover")
      )
      f.position = "7"
      waitFor("later account read did not run")(
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

  test("a retained fill cannot hide current selected depth or account freshness failures") {
    val f = new Fixture()
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      waitFor("fill missing")(f.worker.view.message.contains("filled 1 at 100"))
      f.ready()
      f.publishActive = false
      f.clock.addAndGet(2001)
      f.worker.observe(Event.Pong)
      f.worker.observe(Event.BboUpdate("BTC", Level("1", "1", 1), Level("2", "1", 1), 3001))
      val depth = TradingScreen.rows(f.worker.view, None, f.worker.preview).mkString("\n")
      assert(depth.contains("selected depth stale"), depth)
      assert(depth.contains("XMR filled 1 at 100"), depth)
      f.fresh(f.worker, time = 3001)
      val account = TradingScreen.rows(f.worker.view, None, f.worker.preview).mkString("\n")
      assert(account.contains("account data stale"), account)
      assert(account.contains("XMR filled 1 at 100"), account)
      f.readFailure = true
      waitFor("failed refresh not observed")(
        f.worker.view.message == "Waiting for market or account data"
      )
      val failed = TradingScreen.rows(f.worker.view, None, f.worker.preview).mkString("\n")
      assert(failed.contains("account data stale"), failed)
      assert(failed.contains("XMR filled 1 at 100"), failed)
      assert(!f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      f.worker.observe(Event.SocketDown)
      val down = TradingScreen.rows(f.worker.view, None, f.worker.preview).mkString("\n")
      assert(down.contains("connection disconnected"), down)
      assert(down.contains("XMR filled 1 at 100"), down)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("a new failed attempt replaces an earlier fill while blocked input preserves it") {
    val f = new Fixture()
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      waitFor("fill missing")(f.worker.view.message.contains("filled 1 at 100"))
      f.ready()
      assert(!f.worker.submit(buy, "BTC"))
      assert(
        TradingScreen
          .rows(f.worker.view, None, f.worker.preview)
          .mkString
          .contains("XMR filled 1 at 100")
      )
      f.worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("999999999", "20", 1)), Vector(Level("1000000000", "20", 1)), 2000),
          true
        )
      )
      assert(f.worker.view.ready)
      assert(!f.worker.submit(buy, "XMR"))
      val text = TradingScreen.rows(f.worker.view, None, f.worker.preview).mkString("\n")
      assert(text.contains("No visible quantity fits this order"), text)
      assert(!text.contains("filled 1 at 100"), text)
      assertEquals(f.orderPosts.get(), 1)
    finally f.close()
  }

  test("startup observes leverage and margin without an account mutation or nonce write") {
    val writes = new AtomicInteger()
    val f = new Fixture(
      initialLeverage = 3,
      marginMode = "isolated",
      beforeReplace = () => { writes.incrementAndGet(); () }
    )
    try
      f.ready()
      Thread.sleep(1200)
      assertEquals(f.posts.get(), 0)
      assertEquals(writes.get(), 0)
      assertEquals(f.leverage, 3)
      assertEquals(f.worker.view.leverage, Some(3))
      assertEquals(f.worker.view.marginMode, Some("isolated"))
    finally f.close()
  }

  test("positive observed leverage below five is valid within the market maximum") {
    val f = new Fixture(initialLeverage = 1, marketMaximum = 2)
    try
      f.ready()
      assertEquals(f.worker.view.leverage, Some(1))
      assertEquals(f.posts.get(), 0)
      assert(f.worker.preview(buy).isRight)
    finally f.close()
  }

  test("invalid leverage never becomes ready and never triggers a corrective post") {
    Vector(0, 6).foreach { leverage =>
      val f = new Fixture(initialLeverage = leverage, loseLeverage = true)
      try
        waitFor("account was not observed")(f.worker.view.position.nonEmpty)
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
        assert(!f.worker.submitReviewed(review.copy(plan = changed)), changed.toString)
      }
      assert(!f.worker.submitReviewed(review.copy(intent = buy.copy(side = Side.Sell))))
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
      assert(!f.worker.submitReviewed(review.copy(createdAt = f.clock.get() + 1)))
      assert(!f.worker.submitReviewed(review.copy(expiresAt = review.createdAt - 1)))
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
        Event.BboUpdate("XMR", Level("100", "20", 1), Level("100.01", "20", 1), 1001)
      )
      assert(f.worker.view.ready)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("fresh position changes require a new review even when signed size and limit stay equal") {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      f.position = "7"
      waitFor("new position was not observed")(f.worker.view.position.contains(BigDecimal(7)))
      assert(f.worker.view.ready)
      assertEquals(f.worker.preview(buy).toOption.get.quantity, review.plan.quantity)
      assert(!f.worker.submitReviewed(review))
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("review captures observed leverage and margin mode including missing snapshots") {
    val f = new Fixture()
    try
      f.ready()
      val review = f.review()
      assert(!f.worker.submitReviewed(review.copy(leverage = None)))
      assert(!f.worker.submitReviewed(review.copy(marginMode = None)))
      f.leverage = 3
      f.sendActive()
      assert(f.worker.view.ready)
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
    val f = new Fixture(beforeOrderSign = () => { entered.countDown(); release.await() })
    try
      f.ready()
      assert(f.worker.submitReviewed(f.review(lifetime = 500L)))
      assert(entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.posts.get(), 0)
      change(f)
      release.countDown()
      waitFor("reviewed order was not canceled")(
        f.orderPosts.get() != 0 || f.worker.view.message.contains("canceled before sending")
      )
      assertEquals(f.posts.get(), 0)
    finally
      release.countDown()
      f.close()

  test("review expiry is rechecked after durable reservation and signing delay") {
    heldReviewed(_.clock.addAndGet(500L))
  }

  test("margin mode is rechecked after durable reservation and signing delay") {
    heldReviewed { f => f.activeMode = "isolated"; f.sendActive() }
  }

  test("a supplied review cannot extend the controller's thirty second lifetime") {
    val writes = new AtomicInteger()
    val f = new Fixture(beforeReplace = () => { writes.incrementAndGet(); () })
    try
      f.ready()
      assert(!f.worker.submitReviewed(f.review(lifetime = 30001L)))
      assertEquals(writes.get(), 0)
      assertEquals(f.posts.get(), 0)
    finally f.close()
  }

  test("extreme timestamp subtraction cannot turn an oversized review into a valid one") {
    val f = new Fixture()
    try
      f.ready()
      val oversized = f.review().copy(createdAt = Long.MinValue, expiresAt = Long.MaxValue)
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
    val f = new Fixture(beforeOrderSign = () => { entered.countDown(); release.await() })
    try
      f.ready()
      val review = f.review(lifetime = 30000L)
      assert(f.worker.submitReviewed(review))
      assert(entered.await(3, TimeUnit.SECONDS))
      assertEquals(f.posts.get(), 0)
      f.clock.addAndGet(250L)
      change(f)
      release.countDown()
      waitFor("held order neither posted nor canceled") {
        f.orderPosts.get() != 0 || f.worker.view.message.contains("canceled before sending")
      }
      assertEquals(f.orderPosts.get(), if cancel then 0 else 1)
      if !cancel then
        val order = f.lastPost.get()("action")("orders")(0)
        assertEquals(order("s").str, Trading.format(review.plan.quantity))
        assertEquals(order("p").str, Trading.format(review.plan.limit))
    finally
      release.countDown()
      f.close()

  test("a fresh position change after acceptance cancels before exchange post") {
    heldReview(
      f => {
        val before = f.worker.preview(buy).toOption.get
        f.position = "7"
        waitFor("new position was not observed during signing") {
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
          Event.BboUpdate("XMR", Level("100", "20", 1), Level("100.01", "20", 1), 1001L)
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
    val f = new Fixture(beforeOrderSign = () => { entered.countDown(); release.await() })
    try
      f.ready()
      assert(f.worker.submit(buy, "XMR"))
      assert(entered.await(3, TimeUnit.SECONDS))
      f.activeMode = "isolated"
      f.sendActive()
      release.countDown()
      waitFor("held legacy order neither posted nor canceled") {
        f.orderPosts.get() != 0 || f.worker.view.message.contains("canceled before sending")
      }
      assertEquals(f.orderPosts.get(), 0)
    finally
      release.countDown()
      f.close()
  }
