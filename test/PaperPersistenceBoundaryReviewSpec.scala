package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.util.control.NonFatal

class PaperPersistenceBoundaryReviewSpec extends munit.FunSuite:
  private def waitFor(label: String)(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < until do Thread.sleep(2)
    assert(condition, label)

  private class Fixture(persist: PaperAccount => Unit):
    val clock = new AtomicLong(1800000000000L)
    val constructor =
      classOf[PaperLedger].getDeclaredConstructors.find(_.getParameterCount == 4).get
    constructor.setAccessible(true)
    val ledger = constructor
      .newInstance(
        PaperAccount(BigDecimal(10000), deposit = BigDecimal(10000)),
        BigDecimal(0),
        persist,
        new AutoCloseable { def close(): Unit = () }
      )
      .asInstanceOf[PaperLedger]
    val rest = new Rest:
      def post(body: String): Option[String] =
        require(ujson.read(body) == ujson.Obj("type" -> "meta"))
        Some(
          """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5},{"name":"BTC","szDecimals":2,"maxLeverage":40}]}"""
        )
    val worker =
      new PaperWorker(TradingConfig(), "XMR", ledger, rest, () => clock.get(), () => clock.get())
    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = throw new IllegalStateException(
        "unexpected live read"
      )
      def post(body: ujson.Value): Option[ujson.Value] = throw new IllegalStateException(
        "unexpected live order"
      )
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        throw new IllegalStateException("unexpected live socket")
    val live = new TradingWorker(
      TradingConfig(),
      false,
      "XMR",
      exchange,
      () => throw new IllegalStateException("unexpected key read"),
      () => throw new IllegalStateException("unexpected live journal"),
      () => clock.get(),
      () => clock.get()
    )
    val router = new ExecutionRouter(
      TradingConfig(),
      "PAPER",
      "XMR",
      live,
      _ => throw new IllegalStateException("unexpected live worker"),
      rest,
      Files.createTempDirectory("persist-boundary-"),
      () => clock.get(),
      () => clock.get()
    )
    val paperField = classOf[ExecutionRouter].getDeclaredFields.find(_.getName == "paper").get
    paperField.setAccessible(true)
    paperField.set(router, Some(worker))
    private var readers = Vector.empty[Thread]
    def book(coin: String): Unit =
      router.observe(
        Event.BookUpdate(
          coin,
          Book(
            Vector(Level("99.99", "50", 1)),
            Vector(Level("100", "50", 1)),
            clock.incrementAndGet()
          ),
          true
        )
      )
    def open(): Unit =
      worker.start()
      router.start()
      router.observe(Event.SocketUp)
      book("XMR")
      waitFor("paper did not become ready")(router.view.ready)
    def review(): OrderReview =
      val intent = Trading.action(0)
      OrderReview(intent, router.preview(intent).toOption.get, clock.get(), clock.get() + 30000)
    def quick[T](label: String)(operation: => T): T =
      val done = new CountDownLatch(1)
      val result = new AtomicReference[Either[Throwable, T]]()
      val thread = new Thread(
        () => {
          try result.set(Right(operation))
          catch case NonFatal(error) => result.set(Left(error))
          finally done.countDown()
        },
        "synthetic-paper-read"
      )
      thread.setDaemon(true)
      readers :+= thread
      thread.start()
      assert(done.await(1, TimeUnit.SECONDS), label + " blocked behind persistence")
      result.get() match
        case Right(value) => value
        case Left(error)  => throw error
    def orderThread: Thread =
      val field = classOf[PaperWorker].getDeclaredFields.find(_.getName == "threads").get
      field.setAccessible(true)
      field.get(worker).asInstanceOf[Vector[Thread]].find(_.getName == "paper-orders").get
    def complete(): Unit =
      waitFor("paper order remained pending")(!router.view.pending && router.view.outcome.nonEmpty)
    def close(): Unit =
      readers.foreach(_.join(1000L))
      router.close()

  private def held(entered: CountDownLatch, release: CountDownLatch): Unit =
    entered.countDown()
    if !release.await(5, TimeUnit.SECONDS) then
      throw new IllegalStateException("synthetic save timed out")

  test("a held failed save stays readable and reports failure without publishing a position") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(_ => {
      held(entered, release)
      throw new java.io.IOException("synthetic storage failure")
    })
    try
      f.open()
      assert(f.router.submitReviewed(f.review()))
      assert(entered.await(3, TimeUnit.SECONDS))
      f.clock.addAndGet(5000L)
      val pending = f.quick("view")(f.router.view)
      assert(pending.pending)
      assertEquals(pending.message, "PAPER saving 5s | XMR")
      assertEquals(pending.position, Some(BigDecimal(0)))
      assertEquals(pending.outcome, None)
      assertEquals(pending.portfolio.stables, Some(BigDecimal(10000)))
      release.countDown()
      f.complete()
      assert(f.router.view.outcome.exists(_.contains("storage failed")))
      assert(!f.router.view.ready)
      assertEquals(f.ledger.account.count, 0L)
      assertEquals(f.router.view.position, Some(BigDecimal(0)))
    finally
      release.countDown()
      f.close()
  }

  test("a held save promptly refuses mode changes, duplicate submissions and new previews") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(_ => held(entered, release))
    try
      f.open()
      val review = f.review()
      assert(f.router.submitReviewed(review))
      assert(entered.await(3, TimeUnit.SECONDS))
      val switched = f.quick("mode selection")(f.router.switchMode("LIVE"))
      assert(switched.left.toOption.exists(_.contains("pending order")))
      assert(!f.quick("duplicate submission")(f.router.submitReviewed(review)))
      assert(f.quick("new preview")(f.router.preview(Trading.action(3))).isLeft)
      release.countDown()
      f.complete()
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.router.view.position, Some(BigDecimal(5)))
    finally
      release.countDown()
      f.close()
  }

  test("a market selected after persistence starts cannot retarget the authorized fill") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val f = new Fixture(_ => held(entered, release))
    try
      f.open()
      assert(f.router.submitReviewed(f.review()))
      assert(entered.await(3, TimeUnit.SECONDS))
      f.quick("market selection")(f.router.select("BTC"))
      f.quick("market update")(f.book("BTC"))
      val pending = f.quick("selected view")(f.router.view)
      assertEquals(pending.coin, "BTC")
      assert(pending.pending)
      assertEquals(pending.position, Some(BigDecimal(0)))
      release.countDown()
      f.complete()
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.ledger.account.positions("XMR").quantity, BigDecimal(5))
      assert(!f.ledger.account.positions.contains("BTC"))
      assertEquals(f.router.view.coin, "BTC")
      assertEquals(f.router.view.position, Some(BigDecimal(0)))
      assert(f.router.view.outcome.exists(_.contains("XMR")))
    finally
      release.countDown()
      f.close()
  }

  test("ledger revision is rechecked under the writer lock after the worker computes its fill") {
    val f = new Fixture(_ => ())
    try
      f.open()
      val review = f.review()
      f.ledger.synchronized {
        assert(f.router.submitReviewed(review))
        val order = f.orderThread
        def atWriter: Boolean = order.getStackTrace.exists(frame =>
          frame.getClassName == "desk.PaperLedger" && frame.getMethodName == "fill"
        )
        val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(2)
        while !atWriter && System.nanoTime() < until do Thread.`yield`()
        assert(atWriter, "order never reached the blocked ledger writer after validation")
        val intervening = review.plan.copy(
          quantity = BigDecimal(1),
          estimatedQuantity = BigDecimal(1),
          estimatedValue = BigDecimal(100),
          average = Some(BigDecimal(100)),
          positionAfter = BigDecimal(1)
        )
        assert(f.ledger.fill(intervening, f.clock.get()).isRight)
      }
      f.complete()
      assertEquals(f.ledger.account.count, 1L)
      assertEquals(f.ledger.account.positions("XMR").quantity, BigDecimal(1))
      assert(
        f.router.view.outcome.exists(text => text.contains("changed") || text.contains("canceled"))
      )
    finally f.close()
  }
