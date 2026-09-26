package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

class LiveReadinessAuditSpec extends munit.FunSuite:
  private val root = Files.createTempDirectory("desk-live-readiness-")
  private val metadata = ujson.Obj(
    "universe" -> ujson.Arr(
      ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5)
    )
  )

  private def waitFor(label: String)(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while !condition && System.nanoTime() < until do Thread.sleep(2)
    assert(condition, label)

  private final class Fixture(
      keyDelayMs: Long = 0L,
      accountDelayMs: Long = 0L,
      activeAgeMs: Long = 0L,
      closeEntered: CountDownLatch = new CountDownLatch(0),
      closeRelease: CountDownLatch = new CountDownLatch(0),
      warmSigning: () => Unit = () => ()
  ):
    private val key = Array.fill[Byte](32)(1)
    private val owner = "0x" + "2" * 40
    val config = TradingConfig(
      owner = Some(owner),
      agent = Some(OrderSigning.address(key))
    )
    val clock = new AtomicLong(10000L)
    val reads = new AtomicInteger()
    val posts = new AtomicInteger()
    val secondRead = new CountDownLatch(1)
    val directory = Files.createTempDirectory(root, "case-")
    private var activeReceive: Option[ujson.Value] => Unit = _ => ()
    def renewActive(): Unit = activeReceive(
      Some(
        ujson.Obj(
          "user" -> owner,
          "coin" -> "XMR",
          "leverage" -> ujson.Obj("type" -> "cross", "value" -> 5),
          "maxTradeSzs" -> ujson.Arr("100", "100")
        )
      )
    )
    val exchange = new Exchange:
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        activeReceive = receive
        renewActive()
      override def watchTimed(
          coin: String,
          owner: String,
          generation: Long,
          now: () => Long,
          receive: (Option[ujson.Value], Long) => Unit
      ): Unit = watch(
        coin,
        owner,
        generation,
        data => receive(data, now() - activeAgeMs)
      )
      def info(body: ujson.Value): Option[ujson.Value] = body("type").str match
        case "meta"     => Some(metadata)
        case "userRole" =>
          Some(
            ujson.Obj("role" -> "agent", "data" -> ujson.Obj("user" -> owner))
          )
        case "clearinghouseState" =>
          val number = reads.incrementAndGet()
          if number == 2 then secondRead.countDown()
          clock.addAndGet(accountDelayMs)
          Some(
            ujson.Obj(
              "marginSummary" -> ujson.Obj("accountValue" -> "1000"),
              "assetPositions" -> ujson.Arr()
            )
          )
        case "userAbstraction" => Some(ujson.Str("disabled"))
        case _                 => None
      def post(body: ujson.Value): Option[ujson.Value] =
        posts.incrementAndGet()
        Some(
          ujson.Obj(
            "status" -> "ok",
            "response" -> ujson.Obj(
              "type" -> "order",
              "data" -> ujson.Obj(
                "statuses" -> ujson.Arr(
                  ujson.Obj(
                    "filled" -> ujson.Obj(
                      "totalSz" -> "1",
                      "avgPx" -> "100",
                      "oid" -> 1
                    )
                  )
                )
              )
            )
          )
        )
      override def close(): Unit =
        closeEntered.countDown()
        closeRelease.await()

    lazy val worker: TradingWorker = new TradingWorker(
      config,
      true,
      "XMR",
      exchange,
      () =>
        clock.addAndGet(keyDelayMs)
        fresh()
        key.clone()
      ,
      () => OrderJournal.open(directory, config.identity),
      () => clock.get(),
      () => clock.get(),
      warmSigning = warmSigning
    )
    def fresh(): Unit =
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("99.99", "20", 1)),
            Vector(Level("100", "20", 1)),
            clock.get()
          ),
          true
        )
      )
    def refreshOnce(): Unit =
      val method = classOf[TradingWorker].getDeclaredMethods
        .find(_.getName == "refresh")
        .get
      method.setAccessible(true)
      method.invoke(worker)
      ()

  test("a fast first account read stays fresh after a slow key preparation") {
    val f = new Fixture(keyDelayMs = 2500L)
    try
      f.refreshOnce()
      val view = f.worker.view
      assertEquals(f.reads.get(), 1)
      assertEquals(view.position, Some(BigDecimal(0)))
      println(
        s"FIRST_SNAPSHOT ready=${view.ready} block=${view.block} clock=${f.clock.get()}"
      )
      assert(
        view.ready,
        "key preparation time was incorrectly counted as account snapshot age"
      )
    finally f.worker.close()
  }

  test(
    "a slow account read remains too old even when its response just arrived"
  ) {
    val f = new Fixture(accountDelayMs = 2001L)
    try
      f.refreshOnce()
      f.fresh()
      f.renewActive()
      assertEquals(f.worker.view.position, Some(BigDecimal(0)))
      assert(
        !f.worker.view.ready,
        "response arrival must not renew an old account snapshot"
      )
    finally f.worker.close()
  }

  test(
    "mode switching cannot hold screen access while a real worker join waits"
  ) {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val warming = new CountDownLatch(1)
    val f = new Fixture(
      closeEntered = entered,
      warmSigning = () =>
        warming.countDown()
        var finished = false
        while !finished do
          try
            release.await()
            finished = true
          catch case _: InterruptedException => ()
    )
    val route = new ExecutionRouter(
      f.config,
      "LIVE",
      "XMR",
      f.worker,
      _ => fail("unexpected replacement"),
      new Rest {
        def post(body: String): Option[String] =
          if ujson.read(body)("type").str == "meta" then Some(ujson.write(metadata))
          else None
      },
      f.directory,
      () => f.clock.get(),
      () => f.clock.get()
    )
    val changed = new AtomicReference[Either[String, Unit]]()
    val switcher = new Thread(() => changed.set(route.switchMode("PAPER")))
    val readerReturned = new CountDownLatch(1)
    val reader = new Thread(() => { route.view; readerReturned.countDown() })
    switcher.setDaemon(true)
    reader.setDaemon(true)
    try
      f.worker.start()
      assert(
        warming.await(2, TimeUnit.SECONDS),
        "worker preparation did not begin"
      )
      switcher.start()
      assert(
        entered.await(2, TimeUnit.SECONDS),
        "mode switch never reached worker shutdown"
      )
      reader.start()
      val responsive = readerReturned.await(200, TimeUnit.MILLISECONDS)
      println(
        s"MODE_SWITCH screenReturnedWithin200ms=$responsive readerState=${reader.getState}"
      )
      assert(
        responsive,
        "the real worker join held the router monitor and prevented drawing"
      )
    finally
      release.countDown()
      switcher.join(3000)
      reader.join(3000)
      route.close()
      assert(!switcher.isAlive && !reader.isAlive)
  }

  test("a completed order promptly starts the next account read") {
    val f = new Fixture()
    try
      f.worker.start()
      waitFor("initial state did not become ready")(f.worker.view.ready)
      assertEquals(f.reads.get(), 1)
      Thread.sleep(20)
      assert(f.worker.submit(OrderIntent(Side.Buy, OrderKind.Hard), "XMR"))
      waitFor("fake order did not complete")(f.worker.view.outcome.nonEmpty)
      val began = f.secondRead.await(300, TimeUnit.MILLISECONDS)
      println(
        s"POST_RESULT accountReadWithin300ms=$began reads=${f.reads.get()} posts=${f.posts.get()}"
      )
      assert(began, "completed order waited for the fixed account poll sleep")
    finally f.worker.close()
  }

  test("the active read retains its request-start age in the worker") {
    val f = new Fixture(activeAgeMs = 2001L)
    try
      f.refreshOnce()
      f.fresh()
      assertEquals(f.worker.view.position, Some(BigDecimal(0)))
      assert(
        !f.worker.view.ready,
        "an old active read was dated at response arrival"
      )
    finally f.worker.close()
  }

  test(
    "mode requests coalesce while the disabled old worker keeps journal ownership"
  ) {
    val warming = new CountDownLatch(1)
    val warmRelease = new CountDownLatch(1)
    val closeEntered = new CountDownLatch(1)
    val closeRelease = new CountDownLatch(1)
    val replacements = new AtomicInteger()
    val f = new Fixture(
      closeEntered = closeEntered,
      closeRelease = closeRelease,
      warmSigning = () =>
        warming.countDown()
        var released = false
        while !released do
          try
            warmRelease.await()
            released = true
          catch case _: InterruptedException => ()
    )
    val next = new Fixture()
    val route = new ExecutionRouter(
      f.config,
      "LIVE",
      "XMR",
      f.worker,
      _ =>
        replacements.incrementAndGet()
        assert(
          f.worker.isRetired,
          "replacement began before prior journal release"
        )
        next.worker
      ,
      new Rest {
        def post(body: String): Option[String] = Some(ujson.write(metadata))
      },
      f.directory,
      () => f.clock.get(),
      () => f.clock.get()
    )
    try
      f.refreshOnce()
      assert(f.worker.view.ready)
      f.worker.start()
      assert(warming.await(2, TimeUnit.SECONDS))
      val start = System.nanoTime()
      assert(route.switchMode("PAPER").isRight)
      assert(System.nanoTime() - start < TimeUnit.MILLISECONDS.toNanos(200))
      assert(
        !f.worker.view.ready,
        "the old worker stayed enabled during retirement"
      )
      assert(closeEntered.await(2, TimeUnit.SECONDS))
      assert(!f.worker.submit(OrderIntent(Side.Buy, OrderKind.Hard), "XMR"))
      intercept[Exception](OrderJournal.open(f.directory, f.config.identity))
      assert(route.switchMode("LIVE").isRight)
      assert(!route.view.ready)
      assert(route.switchMode("PAPER").isRight)
      assert(route.switchMode("LIVE").isRight)
      assertEquals(replacements.get(), 0)
      warmRelease.countDown()
      closeRelease.countDown()
      waitFor("replacement did not start after retirement")(
        replacements.get() == 1
      )
      assertEquals(route.mode, "LIVE")
      assert(f.worker.isRetired)
    finally
      warmRelease.countDown()
      closeRelease.countDown()
      route.close()
      next.worker.close()
  }

  test(
    "a worker finishing after the close timeout releases ownership before replacement"
  ) {
    val warming = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val replacements = new AtomicInteger()
    val f = new Fixture(warmSigning = () =>
      warming.countDown()
      var finished = false
      while !finished do
        try
          release.await()
          finished = true
        catch case _: InterruptedException => ())
    val next = new Fixture()
    val route = new ExecutionRouter(
      f.config,
      "LIVE",
      "XMR",
      f.worker,
      _ =>
        assert(f.worker.isRetired)
        val reopened = OrderJournal.open(f.directory, f.config.identity)
        reopened.close()
        replacements.incrementAndGet()
        next.worker
      ,
      new Rest {
        def post(body: String): Option[String] = Some(ujson.write(metadata))
      },
      f.directory,
      () => f.clock.get(),
      () => f.clock.get()
    )
    try
      f.refreshOnce()
      f.worker.start()
      assert(warming.await(2, TimeUnit.SECONDS))
      assert(route.switchMode("PAPER").isRight)
      assert(route.switchMode("LIVE").isRight)
      Thread.sleep(4200L)
      assert(!route.view.ready)
      assertEquals(replacements.get(), 0)
      intercept[Exception](OrderJournal.open(f.directory, f.config.identity))
      release.countDown()
      waitFor(
        "finished worker did not release its journal and permit replacement"
      )(
        replacements.get() == 1
      )
    finally
      release.countDown()
      route.close()
      next.worker.close()
  }
