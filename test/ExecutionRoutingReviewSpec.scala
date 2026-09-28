package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class ExecutionRoutingReviewSpec extends munit.FunSuite:

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
      autoStart: Boolean = true
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
      worker.close()

  private val buy = OrderIntent(Side.Buy, OrderKind.Hard)

  private def router(f: Fixture, mode: String = "LIVE"): ExecutionRouter =
    new ExecutionRouter(
      f.config,
      mode,
      "XMR",
      f.worker,
      _ => fail("unexpected live worker replacement"),
      new Rest { def post(body: String): Option[String] = fail("unexpected paper market request") },
      f.directory,
      () => f.clock.get(),
      () => f.clock.get()
    )

  test("live routing rejects a changed review instead of reducing it to an intent") {
    val f = new Fixture()
    val route = router(f)
    try
      f.ready()
      val review = f.review()
      assert(
        !route.submitReviewed(review.copy(plan = review.plan.copy(asset = review.plan.asset + 1)))
      )
      assertEquals(f.posts.get(), 0)
    finally
      route.close()
      f.close()
  }

  test("live routing submits the exact accepted reviewed wire fields") {
    val f = new Fixture()
    val route = router(f)
    try
      f.ready()
      val review = f.review()
      assert(route.submitReviewed(review))
      assert(f.entered.await(3000L, TimeUnit.MILLISECONDS))
      val order = f.lastPost.get()("action")("orders")(0)
      assertEquals(order("s").str, Trading.format(review.plan.quantity))
      assertEquals(order("p").str, Trading.format(review.plan.limit))
      assertEquals(f.orderPosts.get(), 1)
    finally
      route.close()
      f.close()
  }

  test("view routing rejects review submission even when the injected worker is ready") {
    val f = new Fixture()
    val route = router(f, "VIEW")
    try
      f.ready()
      assert(!route.submitReviewed(f.review()))
      assert(!route.view.ready)
      assertEquals(f.posts.get(), 0)
    finally
      route.close()
      f.close()
  }

  test("recorder startup reaches public storage without constructing an account worker") {
    val directory = Files.createTempDirectory("desk-record-routing")
    val config = directory.resolve("settings.json")
    Files.writeString(config, "{}")
    Files.setPosixFilePermissions(
      config,
      java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    )
    Files.writeString(directory.resolve("public"), "owned storage fixture failure")
    val factories = new AtomicInteger()
    val publicSources: TradingConfig => (Connector, Rest) = _ =>
      (
        new Connector {
          def connect(text: String => Unit, closed: () => Unit): Socket =
            fail("unexpected socket before storage failure")
        },
        new Rest {
          def post(body: String): Option[String] =
            fail("unexpected account or public call before storage failure")
        }
      )
    val result = scala.util.Try(
      Main.run(
        Main.Args(
          "XMR",
          true,
          config = Some(config.toString),
          record = true,
          stateDirectory = Some(directory.toString)
        ),
        publicSources,
        (_, _, _, _) => {
          factories.incrementAndGet(); throw new IllegalStateException("account factory called")
        }
      )
    )
    assert(result.isFailure)
    assertEquals(factories.get(), 0)
    assertEquals(Files.readString(directory.resolve("public")), "owned storage fixture failure")
  }
