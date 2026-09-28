package desk

import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

class ThreadsSpec extends munit.FunSuite:
  test("daemon creation is unstarted and interruption exits a queue wait within 100 ms") {
    val entered = new CountDownLatch(1)
    val queue = new LinkedBlockingQueue[String]()
    val thread = Threads.daemon("test-daemon") {
      entered.countDown()
      val _ = queue.take()
    }
    assert(thread.isDaemon)
    assertEquals(thread.getName, "test-daemon")
    assertEquals(thread.getState, Thread.State.NEW)
    try
      thread.start()
      assert(entered.await(1, TimeUnit.SECONDS))
      thread.interrupt()
      thread.join(100L)
      assert(!thread.isAlive)
    finally
      thread.interrupt()
      thread.join(1000L)
  }

  test("unexpected failures reach the thread handler with the original name") {
    val problem = new IllegalStateException("worker failure")
    val seen = new AtomicReference[(String, Throwable)]()
    val thread = Threads.daemon("test-failure") { throw problem }
    thread.setUncaughtExceptionHandler((t, error) => seen.set(t.getName -> error))
    thread.start()
    thread.join(1000L)
    assert(!thread.isAlive)
    assertEquals(seen.get(), ("test-failure", problem: Throwable))
  }
