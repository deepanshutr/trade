package desk

import java.nio.file.Files
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class BudgetFreshnessSpec extends munit.FunSuite:
  private val at = 1800000000000L

  test("a depth request can pass while a cold history request earns its allowance") {
    val directory = Files.createTempDirectory("fresh-budget-")
    val clock = new AtomicLong(at)
    val history = new PublicRequestBudget(directory, () => clock.get())
    val prices = new PublicRequestBudget(directory, () => clock.get())
    var first = true
    var priceAt = 0L
    var historyAt = 0L
    def sleep(delay: Long): Unit =
      val until = clock.get() + delay
      if first then
        first = false
        prices.execute(2, d => { val _ = clock.addAndGet(d) }) { priceAt = clock.get() }
      clock.set(clock.get().max(until))
    history.execute(104, sleep) { historyAt = clock.get() }
    assert(priceAt - at <= 344L, s"selected prices waited ${priceAt - at}ms")
    assert(historyAt > priceAt)
    assertEquals(Files.readString(directory.resolve("schedule.txt")).toLong - at, 9116L)
  }

  test("old large reservations remain charged rather than resetting the shared allowance") {
    val directory = Files.createTempDirectory("fresh-legacy-")
    val clock = new AtomicLong(at)
    val old = new PublicRequestBudget(directory, () => clock.get())
    val current = new PublicRequestBudget(directory, () => clock.get())
    assertEquals(old.reserve(104), 8915L)
    var sent = 0L
    current.execute(2, d => { val _ = clock.addAndGet(d) }) { sent = clock.get() }
    assertEquals(sent - at, 9087L)
  }

  test("the expensive request gate remains held through transport while prices can pass") {
    val directory = Files.createTempDirectory("fresh-transport-")
    val clock = new AtomicLong(at)
    val first = new PublicRequestBudget(directory, () => clock.get())
    val second = new PublicRequestBudget(directory, () => clock.get())
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val next = new CountDownLatch(1)
    val errors = new ConcurrentLinkedQueue[Throwable]()
    def sleep(delay: Long): Unit = { val _ = clock.addAndGet(delay) }
    val a = new Thread(() =>
      try
        first.execute(104, sleep) {
          entered.countDown()
          require(release.await(5, TimeUnit.SECONDS))
        }
      catch case e: Throwable => { val _ = errors.add(e) }
    )
    val b = new Thread(() =>
      try second.execute(20, sleep) { next.countDown() }
      catch case e: Throwable => { val _ = errors.add(e) }
    )
    a.setDaemon(true)
    b.setDaemon(true)
    a.start()
    try
      assert(entered.await(3, TimeUnit.SECONDS))
      b.start()
      assert(!next.await(100, TimeUnit.MILLISECONDS))
      var priceSent = false
      second.execute(2, sleep) { priceSent = true }
      assert(priceSent)
    finally
      release.countDown()
      a.join(3000)
      b.join(3000)
    assert(!a.isAlive && !b.isAlive)
    assert(errors.isEmpty, errors.asScala.toVector)
    assertEquals(next.getCount, 0L)
  }

  test("interruption keeps earned credits and releases the expensive gate") {
    val directory = Files.createTempDirectory("fresh-interrupt-")
    val clock = new AtomicLong(at)
    val budget = new PublicRequestBudget(directory, () => clock.get())
    var sent = false
    var interrupted = false
    try budget.execute(104, _ => throw new InterruptedException("cancelled")) { sent = true }
    catch case _: InterruptedException => interrupted = true
    assert(interrupted)
    assert(!sent)
    assertEquals(Files.readString(directory.resolve("schedule.txt")).toLong, at + 172L)
    budget.execute(20, d => { val _ = clock.addAndGet(d) }) { sent = true }
    assert(sent)
  }
