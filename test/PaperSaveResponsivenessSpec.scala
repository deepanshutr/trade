package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.time.ZoneId

class PaperSaveResponsivenessSpec extends TempDirectorySuite:
  test("router view and controller draw stay responsive during a held paper save") {
    val at = 1800000000000L
    val saving = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val sent = new CountDownLatch(1)
    val readersStarted = new CountDownLatch(2)
    val viewed = new CountDownLatch(2)
    val shown = new AtomicReference[TradingView]()
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
    val ledger = new PaperLedger(
      PaperAccount(BigDecimal(10000), deposit = BigDecimal(10000)),
      BigDecimal(0),
      persist,
      new AutoCloseable { def close(): Unit = () }
    )
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
      tempDirectory("save-response-"),
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
      Snapshot.initial("XMR", at).copy(book = Some(book), feed = Health.Live(at))
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => at,
      router.preview,
      router.submitReviewed,
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
      reader("synthetic-paper-frame") {
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
      val reviewedView = router.view
      val submitter = new Thread(
        () => {
          mark("Send entered")
          try
            val _ =
              controller.handle(
                Command.PlaceOrder(Trading.action(0)),
                snapshot,
                reviewedView,
                (120, 40),
                false
              )
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
      val sendReturned = sent.await(1, TimeUnit.SECONDS)
      val readsReturned = viewed.await(1, TimeUnit.SECONDS)
      assert(
        sendReturned && readsReturned,
        s"Send returned=$sendReturned, readers remaining=${viewed.getCount}. ${trace.toString}"
      )
      assert(errors.isEmpty, errors.toString)
      assert(shown.get().pending)
      assertEquals(shown.get().position, Some(BigDecimal(0)))
      assert(drawn.get().lines.map(Chart.plain).mkString("\n").contains("Saving 0s"))
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
