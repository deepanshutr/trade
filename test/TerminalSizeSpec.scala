package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

class TerminalSizeSpec extends munit.FunSuite:
  test("a delayed size observation cannot publish after a newer observation") {
    val firstRead = new CountDownLatch(1)
    val releaseFirst = new CountDownLatch(1)
    val secondRead = new CountDownLatch(1)
    val calls = new AtomicInteger
    val published = new java.util.concurrent.CopyOnWriteArrayList[(Int, Int)]
    val reader = new TerminalSize(
      () =>
        if calls.incrementAndGet() == 1 then
          firstRead.countDown()
          require(releaseFirst.await(3, TimeUnit.SECONDS))
          (100, 30)
        else
          secondRead.countDown()
          (120, 40)
      ,
      size => { val _ = published.add(size) }
    )
    val first = new Thread(() => { val _ = reader.read() })
    val second = new Thread(() => { val _ = reader.read() })
    first.setDaemon(true)
    second.setDaemon(true)
    first.start()
    try
      assert(firstRead.await(2, TimeUnit.SECONDS))
      second.start()
      assert(
        !secondRead.await(150, TimeUnit.MILLISECONDS),
        "second observation overtook the held first"
      )
      releaseFirst.countDown()
      first.join(2000L)
      second.join(2000L)
      assert(!first.isAlive && !second.isAlive)
      assertEquals(published.asScala.toVector, Vector((100, 30), (120, 40)))
    finally
      releaseFirst.countDown()
      first.join(2000L)
      if second.getState != Thread.State.NEW then second.join(2000L)
  }
