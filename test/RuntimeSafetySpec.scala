package desk

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scala.jdk.CollectionConverters.*

class RuntimeSafetySpec extends munit.FunSuite:
  private val now = 1800000000000L
  private def waitFor(label: String)(condition: => Boolean): Unit =
    val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while !condition && System.nanoTime() < until do Thread.sleep(5)
    assert(condition, label)
  private def chunks(directory: java.nio.file.Path): String =
    val paths = Files.list(directory)
    try
      paths
        .iterator()
        .asScala
        .filter(_.getFileName.toString.endsWith(".jsonl"))
        .map(Files.readString(_))
        .mkString
    finally paths.close()

  test("overflow is visible during capture and remains visible after close") {
    val directory = Files.createTempDirectory("desk-recorder-overflow")
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val first = new AtomicBoolean(true)
    val recorder = new PublicRecorder(
      MarketArchive.open(directory),
      () => {
        if Thread.currentThread().getName == "public-recorder" && first.compareAndSet(true, false)
        then
          entered.countDown()
          release.await()
        now
      },
      capacity = 1
    )
    try
      recorder.start()
      assert(entered.await(3, TimeUnit.SECONDS))
      recorder.observe(Event.SocketUp)
      recorder.observe(Event.SocketDown)
      assert(recorder.status.toLowerCase.contains("gap"), recorder.status)
      release.countDown()
      recorder.close()
      assert(recorder.status.toLowerCase.contains("gap"), recorder.status)
      assert(chunks(directory).contains("invalid-or-dropped-event"))
    finally
      release.countDown()
      recorder.close()
  }

  test("shutdown drains an observation already accepted by the recorder") {
    val directory = Files.createTempDirectory("desk-recorder-close-race")
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val finished = new CountDownLatch(1)
    val recorder = new PublicRecorder(
      MarketArchive.open(directory),
      () => {
        if Thread.currentThread().getName == "recorder-producer-test" then
          entered.countDown()
          release.await()
        now
      }
    )
    val producer = new Thread(() => recorder.observe(Event.SocketDown), "recorder-producer-test")
    val closer = new Thread(() => { recorder.close(); finished.countDown() })
    try
      recorder.start()
      producer.start()
      assert(entered.await(3, TimeUnit.SECONDS))
      closer.start()
      assert(
        !finished.await(600, TimeUnit.MILLISECONDS),
        "close passed an in-flight accepted observation"
      )
      release.countDown()
      producer.join(3000L)
      closer.join(3000L)
      assert(!producer.isAlive && !closer.isAlive)
      assert(chunks(directory).contains("disconnected"))
    finally
      release.countDown()
      producer.join(3000L)
      closer.join(3000L)
      recorder.close()
  }

  test("recorder keeps public event categories only") {
    val directory = Files.createTempDirectory("desk-recorder-public-only")
    val recorder = new PublicRecorder(MarketArchive.open(directory), () => now)
    recorder.start()
    recorder.observe(Event.Bad)
    recorder.observe(Event.SocketDown)
    recorder.close()
    val encoded = chunks(directory)
    assert(encoded.contains("invalid-or-dropped-event"))
    assert(encoded.contains("disconnected"))
    assert(!encoded.contains("user") && !encoded.contains("signature"))
  }

  test("paper startup and fills request only metadata even with configured account identifiers") {
    val requests = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val clock = new AtomicLong(now)
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        assertEquals(ujson.read(body), ujson.Obj("type" -> "meta"))
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    val ledger = PaperLedger.open(
      Files.createTempDirectory("desk-paper-public-only"),
      "mainnet",
      BigDecimal(10000),
      BigDecimal(0)
    )
    val config = TradingConfig(owner = Some("0x" + "1" * 40), agent = Some("0x" + "2" * 40))
    val worker = new PaperWorker(config, "XMR", ledger, rest, () => clock.get(), () => clock.get())
    try
      worker.start()
      worker.start()
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), now),
          true
        )
      )
      waitFor("paper never became ready")(worker.view.ready)
      val intent = OrderIntent(Side.Buy, OrderKind.Hard)
      val review = OrderReview(intent, worker.preview(intent).toOption.get, now, now + 30000)
      worker.synchronized {
        assert(worker.submitReviewed(review))
        worker.observe(
          Event.BboUpdate("XMR", Level("100", "50", 1), Level("101", "50", 1), now + 1)
        )
      }
      waitFor("paper order outside the newer quote did not report no fill")(
        worker.view.outcome.exists(_.contains("no fill"))
      )
      assertEquals(ledger.account.count, 0L)
      assertEquals(worker.view.position, Some(BigDecimal(0)))
      assert(!worker.view.pending)
      assertEquals(requests.size(), 1)
    finally worker.close()
    assert(!worker.view.ready)
    worker.close()
  }
