package desk

import java.nio.channels.FileChannel
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class PublicBudgetReviewSpec extends munit.FunSuite:
  private val at = 1800000000000L
  private val stoppedConnector = new Connector:
    def connect(text: String => Unit, closed: () => Unit): Socket =
      throw new UnsupportedOperationException("no socket in budget review")
  private def response(now: Long): String =
    s"""[{"t":${now - 60000L},"o":"100","h":"101","l":"99","c":"100","v":"2"}]"""

  test("public candle cost rounds partial blocks and includes both endpoints") {
    def body(count: Long) = Feed.historyBody("XMR", "1m", at, Some(at - (count - 1) * 60000L))
    assertEquals(PublicRequestBudget.cost(body(1)), 21)
    assertEquals(PublicRequestBudget.cost(body(60)), 21)
    assertEquals(PublicRequestBudget.cost(body(61)), 22)
    assertEquals(PublicRequestBudget.cost(body(5000)), 104)
    assertEquals(PublicRequestBudget.cost(body(10000)), 104)
  }

  test("mixed clients preserve reservations across reopen and idle time") {
    val root = Files.createTempDirectory("desk-review-public-budget")
    val now = new AtomicLong(at)
    val first = new PublicRequestBudget(root, () => now.get())
    assertEquals(first.reserve(2), 172L)
    val second = new PublicRequestBudget(root, () => now.get())
    assertEquals(second.reserve(104), 9087L)
    assertEquals(first.reserve(20), 10802L)
    assertEquals(Files.readString(root.resolve("schedule.txt")).toLong, at + 10802L)
    now.set(at + 20000L)
    assertEquals(second.reserve(2), 172L)
  }

  test("invalid budgets and unsafe schedules preserve the previous reservation") {
    val root = Files.createTempDirectory("desk-review-public-invalid")
    val now = new AtomicLong(at)
    val budget = new PublicRequestBudget(root, () => now.get())
    budget.reserve(20)
    val saved = root.resolve("schedule.txt")
    val before = Files.readString(saved)
    for weight <- Vector(-1, 0, 105, Int.MaxValue) do
      intercept[IllegalArgumentException](budget.reserve(weight))
    assertEquals(Files.readString(saved), before)
    Files.setPosixFilePermissions(saved, PosixFilePermissions.fromString("rw-r--r--"))
    intercept[IllegalArgumentException](budget.reserve(20))
    assertEquals(Files.readString(saved), before)
  }

  test("a symbolic schedule cannot overwrite a different private file") {
    val root = Files.createTempDirectory("desk-review-public-symlink")
    val target = root.resolve("foreign.txt")
    LocalFiles.write(target, at.toString)
    Files.createSymbolicLink(root.resolve("schedule.txt"), target)
    val budget = new PublicRequestBudget(root, () => at)
    intercept[IllegalArgumentException](budget.reserve(20))
    assertEquals(Files.readString(target), at.toString)
    assert(Files.isSymbolicLink(root.resolve("schedule.txt")))
  }

  test("successful history repairs ask only for recent overlap while failures retry full range") {
    val now = new AtomicLong(at)
    val requests = new ConcurrentLinkedQueue[String]()
    var available = true
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        if available then Some(response(now.get())) else None
    val feed = new Feed(
      stoppedConnector,
      rest,
      new ConcurrentLinkedQueue[Event](),
      () => now.get(),
      "XMR",
      Vector.empty
    )
    feed.repairHistory()
    assertEquals(requests.size(), 6)
    now.addAndGet(60000L)
    feed.repairHistory()
    val recent = ujson.read(requests.asScala.toVector.last)("req")
    assertEquals(requests.size(), 7)
    assertEquals(recent("interval").str, "1m")
    assertEquals(recent("startTime").num.toLong, at - 120000L)
    assertEquals(PublicRequestBudget.cost(requests.asScala.toVector.last), 21)
    available = false
    requests.clear()
    val failed = new Feed(
      stoppedConnector,
      rest,
      new ConcurrentLinkedQueue[Event](),
      () => now.get(),
      "XMR",
      Vector.empty
    )
    failed.repairHistory()
    assertEquals(requests.size(), 12)
    now.addAndGet(60000L)
    failed.repairHistory()
    assertEquals(requests.size(), 24)
    assertEquals(PublicRequestBudget.cost(requests.asScala.toVector.last), 104)
  }

  test("overlapping repairs coalesce every series across a due boundary") {
    val now = new AtomicLong(at)
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val requests = new ConcurrentLinkedQueue[String]()
    val errors = new ConcurrentLinkedQueue[Throwable]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        if ujson.read(body)("req")("interval").str == "1m" then
          entered.countDown()
          val released = release.await(10, TimeUnit.SECONDS)
          require(released, "owned repair probe was not released")
        Some(response(now.get()))
    val feed = new Feed(
      stoppedConnector,
      rest,
      new ConcurrentLinkedQueue[Event](),
      () => now.get(),
      "XMR",
      Vector.empty
    )
    def runRepair(): Unit =
      try feed.repairHistory()
      catch case error: Throwable => { val _ = errors.add(error) }
    val first = new Thread(() => runRepair())
    first.start()
    try
      val ready = entered.await(10, TimeUnit.SECONDS)
      assert(ready)
      now.addAndGet(60000L)
      val second = new Thread(() => runRepair())
      second.start()
      second.join(3000L)
      assert(!second.isAlive, "an already pending series should not make a duplicate request")
      release.countDown()
      first.join(3000L)
      assert(!first.isAlive)
      assertEquals(errors.asScala.toVector, Vector.empty)
      assertEquals(requests.size(), 6)
      now.addAndGet(60000L)
      feed.repairHistory()
      assertEquals(requests.size(), 7)
    finally
      release.countDown()
      first.interrupt()
      first.join(3000L)
  }

  test("empty public history remains eligible for a full repair after a minute") {
    val now = new AtomicLong(at)
    val requests = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        requests.add(body)
        Some("[]")
    val feed = new Feed(
      stoppedConnector,
      rest,
      new ConcurrentLinkedQueue[Event](),
      () => now.get(),
      "XMR",
      Vector.empty
    )
    feed.repairHistory()
    assertEquals(requests.size(), 6)
    now.addAndGet(60000L)
    feed.repairHistory()
    assertEquals(requests.size(), 12)
    assert(requests.asScala.forall(body => PublicRequestBudget.cost(body) == 104))
  }

  test("an unforced active archive chunk cannot advance the durable checkpoint") {
    val root = Files.createTempDirectory("desk-review-active-force")
    val archive = MarketArchive.open(root, maximumBytes = 100000L, chunkBytes = 20000L)
    val bar = Candle(at - 60000L, 100, 101, 99, 100, 1)
    try
      archive.record(Event.CandleHistory("XMR", "1m", Vector(bar)), at)
      archive.checkpoint()
      val before = Files.readString(root.resolve("candles.json"))
      archive.record(Event.CandleUpdate("XMR", "1m", bar.copy(t = at)), at + 1)
      val field = classOf[MarketArchive].getDeclaredField("output")
      field.setAccessible(true)
      val output = field.get(archive).asInstanceOf[Option[(Path, FileChannel, Long)]]
      assert(output.nonEmpty)
      output.get._2.close()
      intercept[java.nio.channels.ClosedChannelException](archive.checkpoint())
      assertEquals(Files.readString(root.resolve("candles.json")), before)
    finally scala.util.Try(archive.close())
  }
