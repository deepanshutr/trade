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

  test("failed terminal entry closes diagnostics through the shared cleanup") {
    var diagnosticsClosed = false
    val result = WorkspaceApp.enterTerminal(
      () => throw new IllegalStateException("no terminal"),
      () => diagnosticsClosed = true
    )
    assert(result.isFailure)
    assert(diagnosticsClosed, "diagnostics remained open")
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
