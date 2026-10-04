package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class ExecutionRoutingReviewSpec extends TempDirectorySuite:

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
    val terms = TradeTerms(config.sizes, config.slippage)
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
    val directory = tempDirectory("desk-worker")
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
        worker.preview(intent, terms).toOption.get,
        terms,
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

  private def router(
      f: Fixture,
      mode: String = "LIVE",
      rest: Rest = new Rest {
        def post(body: String): Option[String] = fail("unexpected prop market request")
      }
  ): ExecutionRouter =
    new ExecutionRouter(
      f.config,
      mode,
      "XMR",
      f.worker,
      _ => fail("unexpected live worker replacement"),
      rest,
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

  test("live routing previews with the terms it is given") {
    val f = new Fixture()
    val route = router(f)
    try
      f.ready()
      val chosen = TradeTerms(TradeSizes(BigDecimal(20), BigDecimal(100)), BigDecimal("0.005"))
      val plan = route.preview(buy, chosen)
      assert(plan.isRight, plan)
      assertEquals(plan, f.worker.preview(buy, chosen))
      assertNotEquals(plan, f.worker.preview(buy, f.terms))
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

  // A router whose live factory counts the workers it makes.
  private def making(
      f: Fixture,
      mode: String,
      made: AtomicInteger,
      config: TradingConfig = null
  ): ExecutionRouter =
    new ExecutionRouter(
      Option(config).getOrElse(f.config),
      mode,
      "XMR",
      f.worker,
      _ =>
        made.incrementAndGet()
        f.create()
      ,
      new Rest { def post(body: String): Option[String] = fail("unexpected prop market request") },
      f.directory,
      () => f.clock.get(),
      () => f.clock.get()
    )

  private def journalFree(f: Fixture): Boolean =
    scala.util.Try(OrderJournal.open(f.directory, f.config.identity)).map(_.close()).isSuccess

  test("suspending live retires the worker and frees its journal, and nothing restarts it") {
    val f = new Fixture()
    val made = new AtomicInteger()
    val route = making(f, "LIVE", made)
    try
      Wait.until("the live worker never opened its journal")(f.journalOpens.get() == 1)
      assert(!journalFree(f), "the running worker should hold the journal lock")
      assertEquals(route.suspendLive(), Right(()))
      assert(f.worker.isRetired)
      assert(journalFree(f), "a suspended live worker still holds the journal lock")
      assertEquals(route.switchMode("LIVE"), Left("Settings are open"))
      Thread.sleep(200L)
      assertEquals(made.get(), 0)
      route.resumeLive()
      assertEquals(made.get(), 1)
      assertEquals(route.switchMode("LIVE"), Right(()))
    finally
      route.close()
      f.close()
  }

  test("a retirement that does not finish refuses the child and resume makes no second worker") {
    val released = new java.util.concurrent.atomic.AtomicBoolean(false)
    val f = new Fixture(warmSigning =
      () =>
        while !released.get() do
          try Thread.sleep(5L)
          catch case _: InterruptedException => ()
    )
    val made = new AtomicInteger()
    val route = making(f, "LIVE", made)
    try
      Wait.until("the live worker never started")(f.warmCalls.get() == 1)
      assertEquals(route.suspendLive(100L), Left("Trading did not stop. Restart Desk"))
      route.resumeLive()
      Thread.sleep(200L)
      assertEquals(made.get(), 0)
      assert(!f.worker.isRetired)
    finally
      released.set(true)
      route.close()
      f.close()
  }

  test("a router without an owner enters live after replaceLive with an approved config") {
    val f = new Fixture()
    val made = new AtomicInteger()
    val route = new ExecutionRouter(
      TradingConfig(),
      "VIEW",
      "XMR",
      f.worker,
      _ => fail("the first factory made a worker"),
      new Rest { def post(body: String): Option[String] = fail("unexpected prop market request") },
      f.directory,
      () => f.clock.get(),
      () => f.clock.get()
    )
    try
      route.start()
      assertEquals(route.switchMode("LIVE"), Left(TradingWorker.setupMessage))
      route.replaceLive(
        f.config,
        _ =>
          made.incrementAndGet()
          f.create()
      )
      assert(f.worker.isRetired)
      assertEquals(made.get(), 0)
      assertEquals(route.switchMode("LIVE"), Right(()))
      assertEquals(made.get(), 1)
    finally
      route.close()
      f.close()
  }

  test("replaceLive in live mode retires the old worker before the new factory starts one") {
    val f = new Fixture()
    val first = new AtomicInteger()
    val second = new AtomicInteger()
    val route = making(f, "LIVE", first)
    try
      Wait.until("the live worker never opened its journal")(f.journalOpens.get() == 1)
      route.replaceLive(
        f.config.copy(quoteAgeMs = 1500L),
        _ =>
          assert(f.worker.isRetired, "a new worker started before the old one retired")
          second.incrementAndGet()
          f.create()
      )
      assert(f.worker.isRetired)
      Wait.until("the new live worker never opened its journal")(f.journalOpens.get() == 2)
      assertEquals((first.get(), second.get()), (0, 1))
    finally
      route.close()
      f.close()
  }

  // The settings hand-off as Desk wires it, on a real router.

  // Reads the JVM's current SIGINT handler by swapping a probe in and the handler straight back.
  private def currentInt(): sun.misc.SignalHandler =
    val signal = new sun.misc.Signal("INT")
    val probe: sun.misc.SignalHandler = _ => ()
    val found = sun.misc.Signal.handle(signal, probe)
    val _ = sun.misc.Signal.handle(signal, found)
    found

  // Keys waiting in the terminal, and keys that come only when something blocks to read them.
  private final class Typed(waiting: String, later: String) extends java.io.InputStream:
    val queue = scala.collection.mutable.Queue.from(waiting.getBytes("UTF-8").map(_ & 0xff))
    val after = scala.collection.mutable.Queue.from(later.getBytes("UTF-8").map(_ & 0xff))
    def typed(text: String): Unit = queue ++= text.getBytes("UTF-8").map(_ & 0xff)
    override def available(): Int = queue.size
    override def read(): Int =
      if queue.nonEmpty then queue.dequeue()
      else if after.nonEmpty then after.dequeue()
      else -1

  private final class Exited(code: Int, onWait: () => Unit) extends Process:
    def getOutputStream: java.io.OutputStream = java.io.OutputStream.nullOutputStream()
    def getInputStream: java.io.InputStream = java.io.InputStream.nullInputStream()
    def getErrorStream: java.io.InputStream = java.io.InputStream.nullInputStream()
    def waitFor(): Int =
      onWait()
      code
    def exitValue(): Int = code
    def destroy(): Unit = ()
    override def isAlive: Boolean = false

  private final class Handing(
      started: Boolean = false,
      startFails: Boolean = false,
      waitFails: Boolean = false
  ):
    val f = new Fixture()
    val made = new AtomicInteger()
    val route = making(f, "LIVE", made)
    val events = scala.collection.mutable.ArrayBuffer.empty[String]
    // n was pressed while live stopped. The Enter for "Press Enter" comes after the child.
    val input = new Typed("n", "\n")
    val output = new java.io.ByteArrayOutputStream()
    val restoration = new WorkspaceApp.Restoration(() => (), () => ())
    if started then
      val _ = restoration.restore()
    private val signal = new sun.misc.Signal("INT")
    val desk: sun.misc.SignalHandler = _ => ()
    private val jvm = sun.misc.Signal.handle(signal, desk)
    def guarded: Boolean = !(currentInt() eq desk)
    def run(): Either[String, Int] =
      Wait.until("the live worker never opened its journal")(f.journalOpens.get() == 1)
      SettingsWiring.handOff(
        Seq("desk", "--keys"),
        route,
        () => {
          events += "reload"
          Right(())
        },
        restoration,
        "saved",
        input,
        new java.io.PrintStream(output, true, "UTF-8"),
        SettingsWiring.TermOps(
          saved => events += s"restore $saved guarded=$guarded",
          () => events += s"enter raw guarded=$guarded"
        ),
        command => {
          events += s"start ${command.mkString(" ")} journal free=${journalFree(f)} " +
            s"guarded=$guarded waiting=${input.available()} made=${made.get()}"
          if startFails then throw new java.io.IOException("missing program")
          new Exited(
            0,
            () => {
              events += "child"
              // A line typed while the child ran that the child never read.
              input.typed("y\n")
              if waitFails then throw new IllegalStateException("the wait failed")
            }
          )
        }
      )
    def close(): Unit =
      val _ = sun.misc.Signal.handle(signal, jvm)
      route.close()
      f.close()

  test("the settings hand-off frees the journal, guards Control C and restarts live after") {
    val h = new Handing()
    try
      assertEquals(h.run(), Right(0))
      assertEquals(
        h.events.toVector,
        Vector(
          "restore saved guarded=true",
          "start desk --keys journal free=true guarded=true waiting=0 made=0",
          "child",
          "enter raw guarded=true",
          "reload"
        )
      )
      assert(!h.guarded, "Desk's SIGINT handler was not put back")
      assertEquals(h.made.get(), 1)
      // The stale line was drained, so the pause read the Enter typed after it.
      assertEquals((h.input.queue.size, h.input.after.size), (0, 0))
      assert(h.output.toString("UTF-8").contains("Press Enter to return to Desk."))
    finally h.close()
  }

  test("a child that cannot start still gets the terminal back inside the guard and live back") {
    val h = new Handing(startFails = true)
    try
      assertEquals(h.run(), Left("The step did not start: missing program"))
      assertEquals(
        h.events.toVector,
        Vector(
          "restore saved guarded=true",
          "start desk --keys journal free=true guarded=true waiting=0 made=0",
          "enter raw guarded=true"
        )
      )
      assert(!h.guarded)
      assertEquals(h.made.get(), 1)
    finally h.close()
  }

  test("a wait that throws still takes the terminal back inside the guard and resumes live") {
    val h = new Handing(waitFails = true)
    try
      intercept[IllegalStateException](h.run())
      assertEquals(h.events.last, "enter raw guarded=true")
      assert(!h.events.contains("reload"))
      assert(!h.guarded)
      assertEquals(h.made.get(), 1)
    finally h.close()
  }

  test("while Desk restores the terminal itself the hand-off leaves raw mode to it") {
    val h = new Handing(started = true)
    try
      assertEquals(h.run(), Right(0))
      assert(!h.events.exists(_.startsWith("enter raw")), h.events)
      assert(h.events.contains("reload"))
    finally h.close()
  }

  test("recorder startup reaches public storage without constructing an account worker") {
    val directory = tempDirectory("desk-record-routing")
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
        },
        keys = KeyStartup.Hooks(home = directory)
      )
    )
    assert(result.isFailure)
    assertEquals(factories.get(), 0)
    assertEquals(Files.readString(directory.resolve("public")), "owned storage fixture failure")
  }

  // Test 15. A cancel goes to the worker of the selected mode. Each case holds a live order in
  // the fake journal file and a prop order in the prop ledger file, and names both ids, so a
  // cancel sent to the wrong worker, or to both, shows.

  // A prop worker asks for market details. These tests need none.
  private val quiet = new Rest { def post(body: String): Option[String] = None }

  // Writes one resting live order into the journal file before any worker opens it.
  private def restLive(f: Fixture): String =
    val journal = OrderJournal.open(f.directory, f.config.identity)
    try
      val plan = OrderPlan(
        "XMR",
        0,
        Side.Buy,
        OrderKind.Hard,
        BigDecimal("0.2"),
        BigDecimal("99"),
        BigDecimal("99"),
        BigDecimal(0),
        BigDecimal(0),
        None,
        BigDecimal(0),
        false,
        post = true
      )
      val saved = journal.reserve(plan, f.owner, f.clock.get())
      journal.rest(saved.id, 77738308L).id
    finally journal.close()

  private val propId = "0x" + "a" * 32

  private def propLedger(f: Fixture): PropLedger =
    PropLedger.open(
      f.directory.resolve("prop"),
      f.config.network,
      f.config.propBalance,
      f.config.propFee
    )

  private def restProp(f: Fixture): Unit =
    val ledger = propLedger(f)
    try
      val order =
        PropOrder(propId, "XMR", Side.Buy, BigDecimal(99), BigDecimal("0.2"), false, 1000L)
      assert(ledger.rest(order, 0L).isRight)
    finally ledger.close()

  // The prop orders the ledger file holds, read once the router has closed it.
  private def propResting(f: Fixture): Vector[String] =
    val ledger = propLedger(f)
    try ledger.account.resting.map(_.id)
    finally ledger.close()

  // The ids of the last cancel the fake venue received.
  private def cancelSent(f: Fixture): Vector[String] =
    val body = f.lastPost.get()
    if body.objOpt.exists(_("action")("type").str == "cancelByCloid") then
      body("action")("cancels").arr.toVector.map(_("cloid").str)
    else Vector.empty

  test("cancel in live reaches the live worker and leaves the prop order") {
    val f = new Fixture(autoStart = false)
    val id = restLive(f)
    restProp(f)
    val route = router(f, "LIVE", quiet)
    try
      route.start()
      f.ready()
      assertEquals(route.view.resting.map(_.id), Vector(id))
      assert(route.cancel(Vector(id, propId)))
      Wait.until("the live worker never sent the cancel")(cancelSent(f) == Vector(id))
    finally
      route.close()
      f.close()
    assertEquals(propResting(f), Vector(propId))
  }

  test("cancel in prop reaches the prop worker and never the live worker") {
    val f = new Fixture(autoStart = false)
    val id = restLive(f)
    restProp(f)
    val route = router(f, "PROP", quiet)
    try
      // A live worker able to cancel its order, so a cancel routed to it would show.
      f.worker.start()
      f.ready()
      route.start()
      assertEquals(route.view.resting.map(_.id), Vector(propId))
      assert(route.cancel(Vector(id, propId)))
      assertEquals(route.view.resting, Vector.empty)
      Thread.sleep(200L)
      assertEquals(cancelSent(f), Vector.empty)
      assertEquals(f.worker.view.resting.map(_.id), Vector(id))
    finally
      route.close()
      f.close()
    assertEquals(propResting(f), Vector.empty)
  }

  test("cancel in view answers false and reaches neither worker") {
    val f = new Fixture(autoStart = false)
    val id = restLive(f)
    restProp(f)
    val route = router(f, "VIEW", quiet)
    try
      route.start()
      f.ready()
      assert(!route.cancel(Vector(id, propId)))
      Thread.sleep(200L)
      assertEquals(cancelSent(f), Vector.empty)
      assertEquals(f.worker.view.resting.map(_.id), Vector(id))
      // The positive control: the same live worker takes the same cancel when asked directly.
      assert(f.worker.cancel(Vector(id)))
      Wait.until("the live worker never sent the direct cancel")(cancelSent(f) == Vector(id))
    finally
      route.close()
      f.close()
    assertEquals(propResting(f), Vector(propId))
  }

  test("a resting order shows through the live worker restarted after a settings child") {
    val f = new Fixture(autoStart = false)
    val id = restLive(f)
    val made = new AtomicInteger()
    val route = making(f, "LIVE", made)
    try
      route.start()
      Wait.until("the first worker never showed the order")(
        route.view.resting.map(_.id) == Vector(id)
      )
      assertEquals(route.suspendLive(), Right(()))
      assert(f.worker.isRetired)
      assertEquals(route.view.resting, Vector.empty)
      // No worker runs, and the journal file still holds the order.
      val held = OrderJournal.open(f.directory, f.config.identity)
      try assertEquals(held.resting.map(_.id), Vector(id))
      finally held.close()
      route.resumeLive()
      assertEquals(made.get(), 1)
      Wait.until("the restarted worker never showed the order")(
        route.view.resting.map(_.id) == Vector(id)
      )
      assertEquals(f.journalOpens.get(), 2)
    finally
      route.close()
      f.close()
  }

  // The workspace builds its controller with WorkspaceApp.deskController, so an order row's
  // Enter reaches the router. A frame drawn by hand carries the axis, as the screen draws it.
  test("the workspace controller cancels a prop order row through the router") {
    val f = new Fixture(autoStart = false)
    restProp(f)
    val route = router(f, "PROP", quiet)
    val controller = WorkspaceApp.deskController(
      PanelView(mode = "PROP"),
      () => f.clock.get(),
      route,
      _ => (),
      (_, _) => ()
    )
    val snapshot = Snapshot.initial("XMR", f.clock.get())
    def frame(): Unit =
      val view = route.view
      controller.orders(view)
      controller.publish(
        WorkspaceFrame(
          Vector.empty,
          Vector.empty,
          "XMR",
          120,
          40,
          "PROP",
          axis = Some(OrdersAxis.marks(None, None, view.resting, None, view.leverage, 90, 110, 40)),
          focus = controller.panel.focus
        )
      )
    def key(command: Command): Unit =
      val _ = controller.handle(command, snapshot, route.view, (120, 40), false)
    try
      route.start()
      frame()
      key(Command.Orders)
      frame()
      assertEquals(route.view.resting.map(_.id), Vector(propId))
      key(Command.Confirm)
      assertEquals(route.view.resting, Vector.empty)
    finally
      route.close()
      f.close()
    assertEquals(propResting(f), Vector.empty)
  }
