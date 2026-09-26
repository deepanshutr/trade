package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

class ExecutionReceiptOrderFinderSpec extends munit.FunSuite:
  test("frame result collection cannot overtake an input that already captured a pending view") {
    val at = 1800000000000L
    val snapshot = Snapshot.initial("XMR", at)
    val pending = TradingView(
      "XMR",
      message = "sending",
      pending = true,
      rules = Some(MarketRules(0, 3, 5)),
      bid = Some(BigDecimal(99)),
      ask = Some(BigDecimal(101))
    )
    val fill = "PAPER XMR buy filled 1 at 100\nFee $0.05 | #1 offline"
    val completed =
      pending.copy(pending = false, ready = true, outcome = Some(fill), outcomeId = 1L)
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => at,
      _ => throw new AssertionError("A pending input cannot request a new preview"),
      _ => throw new AssertionError("No submission is permitted in the receipt ordering check"),
      _ => Left("unused")
    )
    controller.publish(
      Workspace.draw(
        snapshot,
        pending,
        controller.panel,
        120,
        40,
        at,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
    )
    val inputLock = new Object
    val inputCaptured = new CountDownLatch(1)
    val releaseInput = new CountDownLatch(1)
    val frameStarted = new CountDownLatch(1)
    val frameFinished = new CountDownLatch(1)
    val contextLocked = new AtomicBoolean(false)
    val input = new Thread(() =>
      inputLock.synchronized {
        val captured = pending
        inputCaptured.countDown()
        require(releaseInput.await(3L, TimeUnit.SECONDS))
        controller.handle(
          Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Hard)),
          snapshot,
          captured,
          (120, 40),
          false
        )
        ()
      }
    )
    val frame = new Thread(() =>
      frameStarted.countDown()
      try
        WorkspaceApp.collectExecution(
          inputLock,
          controller,
          () => {
            contextLocked.set(Thread.holdsLock(inputLock))
            (snapshot, completed)
          }
        )
        ()
      finally frameFinished.countDown()
    )
    input.setDaemon(true)
    frame.setDaemon(true)
    try
      input.start()
      assert(inputCaptured.await(1L, TimeUnit.SECONDS))
      frame.start()
      assert(frameStarted.await(1L, TimeUnit.SECONDS))
      val blockedBy = System.nanoTime() + TimeUnit.SECONDS.toNanos(1L)
      while frame.getState != Thread.State.BLOCKED && frameFinished.getCount > 0 &&
        System.nanoTime() < blockedBy
      do Thread.sleep(2L)
      val publishedEarly = frameFinished.getCount == 0
      releaseInput.countDown()
      input.join(1000L)
      frame.join(1000L)
      assert(!input.isAlive && !frame.isAlive)
      assert(!publishedEarly, "Frame publication overtook the input snapshot")
      assert(contextLocked.get(), "Frame context was captured outside the input lock")
      assertEquals(controller.panel.messages.map(_.text), Vector("Order pending", fill))
    finally
      releaseInput.countDown()
      input.interrupt()
      frame.interrupt()
      input.join(1000L)
      frame.join(1000L)
      controller.close()
  }
