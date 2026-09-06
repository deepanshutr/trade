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
      loseLeverage: Boolean = false,
      marginMode: String = "cross"
  ):
    private val key = Array.fill[Byte](32)(1)
    val agent = OrderSigning.address(key)
    val owner = "0x" + "2" * 40
    val config = TradingConfig(owner = Some(owner), agent = Some(agent))
    val clock = new AtomicLong(10000L)
    val loads = new AtomicInteger()
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
                    ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5),
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
      () => { loads.incrementAndGet(); key.clone() },
      () => OrderJournal.open(directory, config.identity, beforeReplace, afterReplace),
      () => clock.get(),
      () => clock.get(),
      afterReserve,
      beforeOrderSign
    )
    val worker = create()
    worker.start()
    fresh(worker)
    def ready(): Unit = waitFor("worker never became ready")(worker.view.ready)
    def close(): Unit =
      release.countDown()
      accountRelease.countDown()
      recoveryRelease.countDown()
      worker.close()

  private val buy = OrderIntent(Side.Buy, OrderKind.Hard)

  test("view mode has previews without protected key access or submissions") {
    val f = new Fixture(enabled = false)
    try
      waitFor("metadata missing")(f.worker.view.rules.nonEmpty)
      assert(f.worker.preview(buy).isRight)
      assert(!f.worker.submit(buy, "XMR"))
      assertEquals(f.loads.get(), 0)
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
      assertEquals(f.worker.view.message, "Waiting for fresh selected-market depth")
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

  test("five times preparation keeps the confirmed margin mode") {
    val f = new Fixture(initialLeverage = 3, marginMode = "isolated")
    try
      f.ready()
      val action = f.lastPost.get()("action")
      assertEquals(action("type").str, "updateLeverage")
      assertEquals(action("leverage").num, 5.0)
      assertEquals(action("isCross").bool, false)
      assertEquals(f.posts.get(), 1)
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
        f.worker.view.message.contains("canceled before sending")
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

  private def checkLeverageCanceled(change: Fixture => Unit): Unit =
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val held = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(
      initialLeverage = 3,
      beforeReplace = () =>
        if held.compareAndSet(false, true) then
          entered.countDown()
          release.await()
    )
    try
      assert(entered.await(3, TimeUnit.SECONDS))
      f.publishActive = false
      change(f)
      release.countDown()
      waitFor("held leverage preparation did not finish")(!f.worker.view.pending)
      assertEquals(f.posts.get(), 0)
    finally
      release.countDown()
      f.close()

  test("held leverage preparation cannot post for the previous market") {
    checkLeverageCanceled(_.worker.select("BTC"))
  }

  test("held leverage preparation cannot post after the active mode changes") {
    checkLeverageCanceled { f =>
      f.activeMode = "isolated"
      f.sendActive()
    }
  }

  test("held leverage preparation cannot post using expired active data") {
    checkLeverageCanceled(_.clock.addAndGet(2001))
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

  test("known unsent leverage preparation can resume when fresh data returns") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val held = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(
      initialLeverage = 3,
      beforeReplace = () =>
        if held.compareAndSet(false, true) then
          entered.countDown()
          release.await()
    )
    try
      assert(entered.await(3, TimeUnit.SECONDS))
      f.clock.addAndGet(2001)
      release.countDown()
      waitFor("stale leverage preparation did not finish")(!f.worker.view.pending)
      assertEquals(f.posts.get(), 0)
      f.fresh(f.worker, time = 3001)
      f.sendActive()
      f.ready()
      assertEquals(f.posts.get(), 1)
      assertEquals(f.lastPost.get()("action")("leverage").num, 5.0)
    finally
      release.countDown()
      f.close()
  }

  test("an uncertain leverage post is never repeated by later account reads") {
    val f = new Fixture(initialLeverage = 3, loseLeverage = true)
    try
      assert(f.entered.await(3, TimeUnit.SECONDS))
      waitFor("the next account cycle did not run")(f.roleReads.get() >= 2)
      Thread.sleep(1200)
      assertEquals(f.posts.get(), 1)
      assert(!f.worker.view.ready)
      assertEquals(f.leverage, 3)
    finally f.close()
  }
