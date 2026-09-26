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
  private val valid = Vector(DeskMessage(1, now, "analysis", "saved result", Some(true)))

  test("message writes preserve every invariant needed to reopen the same data") {
    val directory = Files.createTempDirectory("desk-message-validity")
    val history = MessageHistory.open(directory)
    try
      history.save(valid)
      val invalid = Vector(
        valid ++ valid,
        Vector(valid.head.copy(id = -1)),
        Vector(valid.head.copy(at = -1)),
        Vector(valid.head.copy(kind = "unknown"))
      )
      invalid.foreach { next =>
        intercept[IllegalArgumentException](history.save(next))
        assertEquals(history.messages, valid)
      }
    finally history.close()
    val again = MessageHistory.open(directory)
    try assertEquals(again.messages, valid)
    finally again.close()
  }

  test("encoded message byte limit is checked before replacing valid history") {
    val directory = Files.createTempDirectory("desk-message-byte-limit")
    val history = MessageHistory.open(directory)
    try
      history.save(valid)
      val large = Vector.tabulate(300)(i => DeskMessage(i, now, "analysis", "\u6f22" * 20000))
      intercept[IllegalArgumentException](history.save(large))
      assertEquals(history.messages, valid)
      assert(Files.size(directory.resolve("messages.json")) < 10000000L)
    finally history.close()
    val again = MessageHistory.open(directory)
    try assertEquals(again.messages, valid)
    finally again.close()
  }

  test("a closed message writer cannot overwrite a later holder of the lease") {
    val directory = Files.createTempDirectory("desk-message-closed")
    val first = MessageHistory.open(directory)
    first.save(valid)
    first.close()
    val second = MessageHistory.open(directory)
    val next = Vector(DeskMessage(2, now, "question", "new result"))
    try
      second.save(next)
      intercept[Exception](first.save(Vector(DeskMessage(3, now, "analysis", "stale writer"))))
    finally second.close()
    val again = MessageHistory.open(directory)
    try assertEquals(again.messages, next)
    finally again.close()
  }

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

  test("recorder discards arbitrary error payloads and keeps public event categories only") {
    val directory = Files.createTempDirectory("desk-recorder-public-only")
    val recorder = new PublicRecorder(MarketArchive.open(directory), () => now)
    recorder.start()
    recorder.observe(Event.Bad("synthetic-private-account-canary"))
    recorder.observe(Event.SocketDown)
    recorder.close()
    val encoded = chunks(directory)
    assert(encoded.contains("invalid-or-dropped-event"))
    assert(encoded.contains("disconnected"))
    assert(!encoded.contains("synthetic-private-account-canary"))
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

  private def rss(
      date: String = "Fri, 11 Sep 2026 08:00:00 GMT",
      link: String = "https://www.bls.gov/news.release/example.htm"
  ): String =
    s"<rss><channel><item><title>Public release</title><link>$link</link><pubDate>$date</pubDate></item></channel></rss>"

  test("all-undated or invalid-dated feeds report a parsing failure") {
    Vector("", "not a date").foreach(date =>
      intercept[Exception](News.feed(rss(date), News.bls, now))
    )
    assertEquals(News.feed(rss(), News.bls, now).size, 1)
  }

  test("feed rejects entity expansion and links outside the allowed secure origin") {
    val canary = Files.createTempFile("desk-xml-canary", ".txt")
    Files.writeString(canary, "synthetic-private-file-canary")
    val xml = "<!DOCTYPE rss [<!ENTITY test SYSTEM \"" + canary.toUri + "\">]>" + rss().replace(
      "Public release",
      "&test;"
    )
    intercept[Exception](News.feed(xml, News.bls, now))
    Vector(
      "javascript:alert(1)",
      "http://www.bls.gov/news",
      "https://www.bls.gov.evil.test/news",
      "https://user@www.bls.gov/news",
      "https://www.bls.gov:444/news"
    ).foreach { link =>
      intercept[Exception](News.feed(rss(link = link), News.bls, now))
    }
    assertEquals(
      News.feed(rss(link = "/news.release/example.htm"), News.bls, now).head.url,
      "https://www.bls.gov/news.release/example.htm"
    )
  }

  private def ics(date: String = "20260911T083000"): String =
    s"BEGIN:VCALENDAR\r\nBEGIN:VEVENT\r\nUID:example\r\nDTSTART;TZID=America/New_York:$date\r\nSUMMARY:Public release\r\nURL:https://www.bls.gov/news.release/example.htm\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

  test("malformed calendar structure and impossible dates are unavailable") {
    intercept[Exception](News.calendar(ics().replace("END:VCALENDAR", ""), News.bls, now))
    intercept[Exception](News.calendar(ics("invalid"), News.bls, now))
    intercept[Exception](News.calendar(ics("20260230T083000"), News.bls, now))
    assertEquals(News.calendar(ics(), News.bls, now).size, 1)
  }

  test("news context excludes future reports and reports older than ninety days") {
    val recent = NewsItem("recent", "recent report", News.bls, Some(now - 1), None, now)
    val future = recent.copy(id = "future", title = "future report", publishedAt = Some(now + 1))
    val old =
      recent.copy(id = "old", title = "old report", publishedAt = Some(now - 90L * 86400000L - 1))
    val context = News.context(Vector(recent, future, old), Vector.empty, now)
    assert(context.contains("recent report"))
    assert(!context.contains("future report") && !context.contains("old report"))
    assert(context.contains("Calendar absence does not prove no upcoming events"))
  }
