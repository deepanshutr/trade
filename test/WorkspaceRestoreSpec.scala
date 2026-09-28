package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

class WorkspaceRestoreSpec extends munit.FunSuite:
  test("a second restore waits until terminal restoration finishes") {
    val closing = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val returned = new CountDownLatch(1)
    val entered = new CountDownLatch(1)
    val terminal = new AtomicBoolean(false)
    val cleanup = new WorkspaceApp.Restoration(
      () => { closing.countDown(); release.await() },
      () => terminal.set(true)
    )
    val first = new Thread(() => { cleanup.restore(); () })
    val second = new Thread(() => {
      entered.countDown()
      cleanup.restore()
      returned.countDown()
    })
    first.start()
    try
      assert(closing.await(2, TimeUnit.SECONDS))
      second.start()
      assert(entered.await(2, TimeUnit.SECONDS))
      assert(!returned.await(200, TimeUnit.MILLISECONDS), "restore returned while close was held")
    finally
      release.countDown()
      first.join(2000)
      second.join(2000)
    assert(terminal.get())
    assertEquals(returned.getCount, 0L)
    assert(!cleanup.restore())
  }

  test("failed terminal entry runs the real shared cleanup list including diagnostics") {
    val closed = scala.collection.mutable.ArrayBuffer.empty[String]
    def close(name: String): AutoCloseable = () => { closed += name; () }
    val result = WorkspaceApp.enterTerminal(
      () => throw new IllegalStateException("no terminal"),
      () =>
        WorkspaceApp.closeAll(
          close("outlook"),
          close("announcements"),
          Some(close("recorder")),
          close("worker"),
          close("diagnostics")
        )
    )
    assert(result.isFailure)
    assertEquals(
      closed.toVector,
      Vector(
        "outlook",
        "announcements",
        "recorder",
        "worker",
        "diagnostics"
      )
    )
  }

  for failClose <- Vector(false, true) do
    test(s"main thread reports and exits 1 after restore, cleanup failure=$failClose") {
      val events = scala.collection.mutable.ArrayBuffer.empty[String]
      val cleanup = new WorkspaceApp.Restoration(
        () => if failClose then throw new IllegalArgumentException("private close detail"),
        () => { events += "terminal"; () }
      )
      val error = intercept[Exception] {
        try throw new IllegalStateException("private render detail")
        finally cleanup.restore()
      }
      val main = Thread.currentThread()
      val handler = WorkspaceApp.failureHandler(
        cleanup,
        main,
        (_, _) => { events += "record"; () },
        message => { events += message; () },
        status => { events += s"exit:$status"; () }
      )
      handler.uncaughtException(main, error)
      assertEquals(
        events.toVector,
        Vector("terminal", "record", "Desk stopped: " + error.getClass.getSimpleName, "exit:1")
      )
    }

  test("background handler records and returns quietly while restore is held") {
    val closing = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val returned = new CountDownLatch(1)
    val recorded = new AtomicBoolean(false)
    val reported = new AtomicBoolean(false)
    val exited = new AtomicBoolean(false)
    val cleanup = new WorkspaceApp.Restoration(
      () => { closing.countDown(); release.await() },
      () => ()
    )
    val handler = WorkspaceApp.failureHandler(
      cleanup,
      Thread.currentThread(),
      (_, _) => recorded.set(true),
      _ => reported.set(true),
      _ => exited.set(true)
    )
    val first = new Thread(() => { cleanup.restore(); () })
    val second = new Thread(() => {
      handler.uncaughtException(Thread.currentThread(), new IllegalStateException("background"))
      returned.countDown()
    })
    first.start()
    try
      assert(closing.await(2, TimeUnit.SECONDS))
      second.start()
      assert(returned.await(2, TimeUnit.SECONDS), "handler waited for cleanup")
      assert(recorded.get())
      assert(!reported.get())
      assert(!exited.get())
    finally
      release.countDown()
      first.join(2000)
      second.join(2000)
  }

  test("terminal restoration still runs when close fails") {
    var restored = false
    val cleanup = new WorkspaceApp.Restoration(
      () => throw new IllegalStateException("close"),
      () => restored = true
    )
    intercept[IllegalStateException](cleanup.restore())
    assert(restored)
    assert(!cleanup.restore())
  }
