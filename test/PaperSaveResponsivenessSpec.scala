package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.nio.file.Files
import java.time.ZoneId

class PaperSaveResponsivenessSpec extends munit.FunSuite:
  test("router view, account label and controller draw stay responsive during a held paper save") {
    val at = 1800000000000L
    val saving = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val sent = new CountDownLatch(1)
    val readersStarted = new CountDownLatch(3)
    val viewed = new CountDownLatch(3)
    val shown = new AtomicReference[TradingView]()
    val label = new AtomicReference[String]()
    val drawn = new AtomicReference[WorkspaceFrame]()
    val saved = new AtomicReference[PaperAccount]()
    val errors = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]()
    val trace = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val began = System.nanoTime()
    def mark(text: String): Unit =
      val _ = trace.add(s"${TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began)} ms $text")
    val persist: PaperAccount => Unit = next =>
      mark("save entered")
      saving.countDown()
      if !release.await(5, TimeUnit.SECONDS) then
        throw new IllegalStateException("test did not release simulated storage")
      mark("save released")
      saved.set(next)
    val constructor =
      classOf[PaperLedger].getDeclaredConstructors.find(_.getParameterCount == 4).get
    constructor.setAccessible(true)
    val ledger = constructor
      .newInstance(
        PaperAccount(BigDecimal(10000)),
        BigDecimal(0),
        persist,
        new AutoCloseable { def close(): Unit = () }
      )
      .asInstanceOf[PaperLedger]
    val rest = new Rest:
      def post(body: String): Option[String] =
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val worker = new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => at, () => at)
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = fail("paper started a live account read")
      def post(body: ujson.Value): Option[ujson.Value] = fail("paper attempted a live order")
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        fail("paper opened a live account socket")
    val live = new TradingWorker(
      TradingConfig(),
      false,
      "XMR",
      exchange,
      () => fail("paper loaded a key"),
      () => fail("paper opened a live journal"),
      () => at,
      () => at
    )
    val router = new ExecutionRouter(
      TradingConfig(),
      "PAPER",
      "XMR",
      live,
      _ => fail("paper enabled live trading"),
      rest,
      Files.createTempDirectory("save-response-"),
      () => at,
      () => at
    )
    // Install only a synthetic paper store with a controllable save. Every read
    // and order below goes through the real router and controller interfaces.
    val paperField = classOf[ExecutionRouter].getDeclaredFields.find(_.getName == "paper").get
    paperField.setAccessible(true)
    paperField.set(router, Some(worker))
    val book = Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), at)
    val snapshot =
      Snapshot.initial("XMR", at).copy(book = Some(book), feed = Health.Live(at), workspace = true)
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => at,
      router.preview,
      router.submitReviewed,
      _ => false,
      router.switchMode
    )
    def draw(): WorkspaceFrame = Workspace.draw(
      snapshot,
      router.view,
      controller.panel,
      120,
      40,
      at,
      ZoneId.of("UTC"),
      Style.mono
    )
    def reader(name: String)(read: => Unit): Thread =
      val thread = new Thread(
        () => {
          mark(name + " started")
          readersStarted.countDown()
          try read
          catch
            case error: Throwable =>
              val _ = errors.add(error)
          finally
            mark(name + " returned")
            viewed.countDown()
        },
        name
      )
      thread.setDaemon(true)
      thread
    val readers = Vector(
      reader("synthetic-paper-view")(shown.set(router.view)),
      reader("synthetic-paper-label")(label.set(router.accountLabel)),
      reader("synthetic-paper-frame") {
        controller.status("synthetic data", "disabled", router.accountLabel)
        drawn.set(draw())
      }
    )
    var sender = Option.empty[Thread]
    try
      worker.start()
      router.start()
      router.observe(Event.SocketUp)
      router.observe(Event.BookUpdate("XMR", book, true))
      val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !worker.view.ready && System.nanoTime() < until do Thread.sleep(2)
      assert(worker.view.ready)
      controller.publish(draw())
      controller.handle(
        Command.PlaceOrder(Trading.action(0)),
        snapshot,
        router.view,
        (120, 40),
        false
      )
      val reviewFrame = draw()
      assert(reviewFrame.completeReview)
      controller.publish(reviewFrame)
      val reviewedView = router.view
      val submitter = new Thread(
        () => {
          mark("Send entered")
          try
            val _ =
              controller.handle(Command.SubmitTarget, snapshot, reviewedView, (120, 40), false)
          catch
            case error: Throwable =>
              val _ = errors.add(error)
          finally
            mark("Send returned")
            sent.countDown()
        },
        "synthetic-paper-send"
      )
      submitter.setDaemon(true)
      sender = Some(submitter)
      submitter.start()
      assert(saving.await(3, TimeUnit.SECONDS), "worker never reached the synthetic save")
      readers.foreach(_.start())
      assert(readersStarted.await(1, TimeUnit.SECONDS), "synthetic readers did not start")
      val sendReturned = sent.await(500, TimeUnit.MILLISECONDS)
      val readsReturned = viewed.await(500, TimeUnit.MILLISECONDS)
      assert(
        sendReturned && readsReturned,
        s"Send returned=$sendReturned, readers remaining=${viewed.getCount}. ${trace.toString}"
      )
      assert(errors.isEmpty, errors.toString)
      assert(shown.get().pending)
      assertEquals(shown.get().position, Some(BigDecimal(0)))
      assert(label.get().startsWith("Cash $10000"))
      assert(drawn.get().lines.map(Chart.plain).mkString("\n").contains("Paper order pending"))
      assertEquals(saved.get(), null)
      release.countDown()
      val completed = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while worker.view.pending && System.nanoTime() < completed do Thread.sleep(2)
      assert(!worker.view.pending)
      assertEquals(worker.view.position, Some(BigDecimal(5)))
    finally
      release.countDown()
      sender.foreach(_.join(1000L))
      readers.foreach(_.join(1000L))
      router.close()
  }
