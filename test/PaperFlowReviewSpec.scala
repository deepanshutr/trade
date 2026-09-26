package desk

import java.nio.file.Files
import java.time.ZoneId
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class PaperFlowReviewSpec extends munit.FunSuite:
  private val start = 1800000000000L
  private val meta =
    """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5},{"name":"BTC","szDecimals":2,"maxLeverage":40}]}"""

  private def waitFor(label: String)(condition: => Boolean): Unit =
    val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < end do Thread.sleep(2)
    assert(condition, label)

  private class Fixture(
      initialMode: String = "PAPER",
      configured: Boolean = false,
      allowLive: Boolean = false,
      held: BigDecimal = BigDecimal(0)
  ):
    val clock = new AtomicLong(start)
    val directory = Files.createTempDirectory("flow-")
    val syntheticKey = Array.fill[Byte](32)(1)
    val config = TradingConfig(
      owner = Option.when(configured)("0x" + "1" * 40),
      agent = Option.when(configured)(OrderSigning.address(syntheticKey))
    )
    if held > 0 then
      LocalFiles.write(
        directory.resolve("paper/wallet-mainnet.json"),
        ujson.write(
          ujson.Obj(
            "cash" -> config.paperBalance.toString,
            "deposit" -> (config.paperBalance + held * 100).toString,
            "realized" -> "0",
            "fees" -> "0",
            "orders" -> ujson.Arr(),
            "count" -> "0",
            "positions" -> ujson.Obj(
              "XMR" -> ujson.Obj(
                "quantity" -> held.toString,
                "entry" -> "100"
              )
            )
          )
        )
      )
    val infoTypes = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val watched = new java.util.concurrent.atomic.AtomicInteger()
    val closed = new java.util.concurrent.atomic.AtomicInteger()
    val recreated = new java.util.concurrent.atomic.AtomicInteger()
    val keysRead = new java.util.concurrent.atomic.AtomicInteger()
    def exchange: Exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] =
        val kind = body("type").str
        infoTypes.add(kind)
        if kind == "meta" then Some(ujson.read(meta))
        else if kind == "clearinghouseState" then Some(ujson.Obj("assetPositions" -> ujson.Arr()))
        else if kind == "userRole" && allowLive then
          Some(
            ujson.Obj(
              "role" -> "agent",
              "data" -> ujson.Obj("user" -> config.owner.get)
            )
          )
        else if kind == "userAbstraction" then Some(ujson.Str("disabled"))
        else fail("unexpected account request: " + kind)
      def post(body: ujson.Value): Option[ujson.Value] = fail(
        "paper reached a live order endpoint"
      )
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        watched.incrementAndGet()
        if allowLive then
          receive(
            Some(
              ujson.Obj(
                "user" -> owner,
                "coin" -> coin,
                "leverage" -> ujson.Obj("type" -> "cross", "value" -> 5),
                "maxTradeSzs" -> ujson.Arr("100", "100")
              )
            )
          )
      override def close(): Unit =
        closed.incrementAndGet()
    val rest = new Rest:
      def post(body: String): Option[String] =
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some(meta)
    val live = new TradingWorker(
      config,
      false,
      "XMR",
      exchange,
      () => fail("paper loaded a key"),
      () => fail("paper opened a live journal"),
      () => clock.get(),
      () => clock.get()
    )
    val route = new ExecutionRouter(
      config,
      initialMode,
      "XMR",
      live,
      coin =>
        if !allowLive then fail("paper created enabled live worker")
        recreated.incrementAndGet()
        new TradingWorker(
          config,
          true,
          coin,
          exchange,
          () => { keysRead.incrementAndGet(); syntheticKey.clone() },
          () =>
            OrderJournal
              .open(directory.resolve("live-journal"), config.identity),
          () => clock.get(),
          () => clock.get(),
          warmSigning = () => ()
        )
      ,
      rest,
      directory,
      () => clock.get(),
      () => clock.get()
    )
    val controller = new DeskController(
      PanelView(mode = initialMode),
      () => clock.get(),
      route.preview,
      route.submitReviewed,
      route.switchMode
    )
    var snapshot = Snapshot.initial("XMR", start).copy(workspace = true)
    var latest = Book(
      Vector(Level("99.99", "50", 1)),
      Vector(Level("100", "50", 1)),
      start
    )
    def publish(book: Book = latest): Unit =
      latest = book.copy(time = clock.get())
      route.observe(Event.BookUpdate(snapshot.coin, latest, true))
      snapshot = snapshot.copy(book = Some(latest), feed = Health.Live(clock.get()))
    def draw(cols: Int = 120, rows: Int = 40): WorkspaceFrame =
      val frame = Workspace.draw(
        snapshot,
        route.view,
        controller.panel,
        cols,
        rows,
        clock.get(),
        ZoneId.of("UTC"),
        Style.mono
      )
      controller.publish(frame)
      frame
    def key(command: Command, cols: Int = 120, rows: Int = 40): Unit =
      controller.handle(command, snapshot, route.view, (cols, rows), false)
    private var selected = Trading.action(0)
    def prepare(
        intent: OrderIntent,
        cols: Int = 120,
        rows: Int = 40
    ): OrderReview =
      assertEquals(intent.kind, OrderKind.Hard)
      selected = intent
      if controller.panel.bigOrder != intent.big then
        val size = draw(cols, rows).buttons
          .find(_.action == DeskAction.OrderSize(intent.big))
          .get
        key(Command.PointerPress(size.x, size.y, 0), cols, rows)
        key(Command.Pointer(size.x, size.y, 0), cols, rows)
      assert(
        draw(cols, rows).buttons.exists(_.action == DeskAction.Prepare(intent))
      )
      OrderReview(
        intent,
        route.preview(intent).toOption.get,
        clock.get(),
        clock.get() + 30000L
      )
    def send(cols: Int = 120, rows: Int = 40, mouse: Boolean = false): Unit =
      val frame = draw(cols, rows)
      if mouse then
        val button =
          frame.buttons.find(_.action == DeskAction.Prepare(selected)).get
        key(Command.PointerPress(button.x, button.y, 0), cols, rows)
        draw(cols, rows)
        key(Command.Pointer(button.x, button.y, 0), cols, rows)
      else key(Command.PlaceOrder(selected), cols, rows)
    def open(): Unit =
      route.start()
      route.observe(Event.SocketUp)
      publish()
      if initialMode == "PAPER" then waitFor("paper did not become ready")(route.view.ready)
    def saved: ujson.Value =
      ujson.read(
        Files.readString(directory.resolve("paper/wallet-mainnet.json"))
      )
    def close(): Unit = route.close()

  for (cols, rows) <- Vector((100, 30), (120, 40), (160, 45)) do
    for index <- 0 until 4 do
      test(
        s"paper order ${index + 1} reaches persisted fill through visible controls at ${cols}x${rows}"
      ) {
        val f = new Fixture(held = if index >= 2 then BigDecimal(20)
        else BigDecimal(0))
        try
          f.open()
          val review = f.prepare(Trading.action(index), cols, rows)
          f.send(cols, rows, mouse = index % 2 == 1)
          waitFor("paper did not finish")(f.route.view.outcome.nonEmpty)
          assert(
            f.route.view.outcome.exists(_.contains("filled")),
            f.route.view.outcome.toString
          )
          assertEquals(f.saved("count").num.toLong, 1L)
          assertEquals(f.route.view.position, Some(review.plan.positionAfter))
          assertEquals(
            BigDecimal(f.saved("fees").str),
            review.plan.estimatedValue * f.config.paperFee
          )
          assert(
            f.draw(cols, rows)
              .lines
              .map(Chart.plain)
              .mkString("\n")
              .contains("Stables")
          )
        finally f.close()
      }

  test(
    "view-to-paper selection needs no enabled live worker and accepts a fresh subsequent quote"
  ) {
    val f = new Fixture(initialMode = "VIEW")
    try
      f.open()
      f.draw()
      f.key(Command.Mode("PAPER"))
      f.draw()
      f.key(Command.SubmitTarget)
      assertEquals(f.controller.panel.mode, "PAPER")
      assert(!f.route.view.ready)
      f.clock.incrementAndGet()
      f.publish()
      waitFor("selected paper did not become ready")(f.route.view.ready)
      f.prepare(Trading.action(0))
      f.send()
      waitFor("selected paper did not finish")(f.route.view.outcome.nonEmpty)
      assert(f.route.view.outcome.exists(_.contains("filled")))
      assertEquals(f.saved("count").num.toLong, 1L)
    finally f.close()
  }

  test(
    "paper fill, fee and position survive reopening under the same state directory"
  ) {
    val f = new Fixture()
    var expected = Option.empty[OrderPlan]
    try
      f.open()
      expected = Some(f.prepare(Trading.action(0)).plan)
      f.send()
      waitFor("fill did not finish")(f.route.view.outcome.nonEmpty)
      assertEquals(f.saved("count").num.toLong, 1L)
    finally f.close()
    val plan = expected.get
    val ledger =
      PaperLedger.open(
        f.directory.resolve("paper"),
        "mainnet",
        BigDecimal(20),
        f.config.paperFee
      )
    try
      assertEquals(ledger.account.count, 1L)
      assertEquals(ledger.account.positions("XMR").quantity, plan.positionAfter)
      assertEquals(
        ledger.account.cash,
        f.config.paperBalance - plan.estimatedValue * (1 + f.config.paperFee)
      )
      assertEquals(ledger.account.orders.size, 1)
    finally ledger.close()
  }

  test(
    "storage failure is surfaced and does not report a fill or silently reset saved cash"
  ) {
    val f = new Fixture()
    try
      f.open()
      f.prepare(Trading.action(0))
      val saved = f.directory.resolve("paper/wallet-mainnet.json")
      Files.createDirectory(saved)
      f.send()
      waitFor("storage failure not surfaced")(f.route.view.outcome.nonEmpty)
      assertEquals(
        f.route.view.outcome,
        Some("Paper storage failed. Restart to recover")
      )
      assertEquals(f.route.view.position, Some(BigDecimal(0)))
      assert(!f.route.view.ready)
      assert(Files.isDirectory(saved))
    finally f.close()
  }

  test("one click uses the current safe depth estimate") {
    val f = new Fixture()
    try
      f.open()
      f.clock.incrementAndGet()
      f.publish(
        Book(
          Vector(Level("99.99", "50", 1)),
          Vector(Level("100", "1", 1), Level("100.05", "50", 1)),
          f.clock.get()
        )
      )
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val reviewed = f.prepare(intent).plan
      f.clock.addAndGet(250)
      f.publish(
        Book(
          Vector(Level("99.99", "50", 1)),
          Vector(Level("100", "2", 1), Level("100.05", "50", 1)),
          f.clock.get()
        )
      )
      val current = Trading
        .plan(
          "XMR",
          f.route.view.rules.get,
          f.latest,
          BigDecimal(f.latest.asks.head.px),
          f.route.view.position.get,
          intent,
          f.config.sizes
        )
        .toOption
        .get
      assertEquals(current.quantity, reviewed.quantity)
      assertEquals(current.limit, reviewed.limit)
      assertEquals(current.reference, reviewed.reference)
      assertEquals(current.positionAfter, reviewed.positionAfter)
      assert(current.estimatedValue < reviewed.estimatedValue)
      assert(f.route.view.ready)
      f.send()
      assertEquals(f.controller.panel.actionNotice, Some("Sending"))
      waitFor("changed estimate was not filled")(
        f.route.view.outcome.exists(_.contains("filled"))
      )
      assertEquals(f.saved("count").num.toLong, 1L)
    finally f.close()
  }

  test(
    "paper startup with live addresses configured does not request account state"
  ) {
    val f = new Fixture(configured = true)
    try
      f.open()
      Thread.sleep(30)
      assertEquals(f.infoTypes.asScala.toVector, Vector.empty)
      assertEquals(f.watched.get(), 0)
      assertEquals(f.keysRead.get(), 0)
      assertEquals(f.recreated.get(), 0)
    finally f.close()
  }

  test(
    "switching VIEW to PAPER closes account polling and returning LIVE creates a fresh enabled worker"
  ) {
    val f =
      new Fixture(initialMode = "VIEW", configured = true, allowLive = true)
    try
      f.open()
      waitFor("view did not start configured account polling")(
        f.infoTypes.asScala.contains("clearinghouseState")
      )
      assert(f.route.switchMode("PAPER").isRight)
      waitFor("previous live worker did not retire")(f.closed.get() == 1)
      val readsAtPaper = f.infoTypes.size()
      val watchesAtPaper = f.watched.get()
      Thread.sleep(1100)
      assertEquals(f.infoTypes.size(), readsAtPaper)
      assertEquals(f.watched.get(), watchesAtPaper)
      assertEquals(f.keysRead.get(), 0)
      assert(f.route.switchMode("LIVE").isRight)
      waitFor("new live worker was not created")(f.recreated.get() == 1)
      f.clock.incrementAndGet()
      f.publish()
      waitFor("new live worker did not become ready with fake account data")(
        f.route.view.ready
      )
      assertEquals(f.keysRead.get(), 1)
      assert(f.infoTypes.asScala.contains("userRole"))
      assert(f.route.switchMode("PAPER").isRight)
      waitFor("second live worker did not retire")(f.closed.get() == 2)
      assert(f.route.switchMode("LIVE").isRight)
      waitFor("second live worker was not created")(f.recreated.get() == 2)
      f.clock.incrementAndGet()
      f.publish()
      waitFor("second live worker did not become ready")(f.route.view.ready)
      assertEquals(f.keysRead.get(), 2)
    finally f.close()
  }

  test("paper storage refusal leaves current live polling available") {
    val f = new Fixture(initialMode = "VIEW", configured = true)
    try
      f.open()
      waitFor("view did not start account polling")(
        f.infoTypes.asScala.contains("clearinghouseState")
      )
      val paper = f.directory.resolve("paper")
      LocalFiles.directory(paper)
      LocalFiles.write(paper.resolve("wallet-mainnet.json"), "broken")
      assert(f.route.switchMode("PAPER").isLeft)
      assertEquals(f.route.mode, "VIEW")
      assertEquals(f.closed.get(), 0)
      val readsBefore = f.infoTypes.size()
      waitFor("existing worker stopped on failed paper selection")(
        f.infoTypes.size() > readsBefore
      )
      assertEquals(
        Files.readString(paper.resolve("wallet-mainnet.json")),
        "broken"
      )
    finally f.close()
  }
