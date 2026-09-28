package desk

import java.time.ZoneOffset
import java.util.concurrent.{CountDownLatch, TimeUnit}

class ExecutionOrderingSpec extends munit.FunSuite:
  test("frame outcomes cannot pass a key thread holding a pending view") {
    val inputLock = new Object
    val snapshot = Snapshot.initial("XMR", 1000L)
    val pending = TradingView("XMR", message = "Sending", pending = true)
    val receipt = "PAPER XMR buy filled 1 at 500\nFee $0.25 | #1 offline"
    val completed = pending.copy(pending = false, outcome = Some(receipt), outcomeId = 1L)
    val controller = new DeskController(
      PanelView(mode = "PAPER"),
      () => 1000L,
      _ => Left("no new order"),
      _ => false,
      _ => Left("no mode switch")
    )
    controller.publish(
      Workspace.draw(
        snapshot,
        pending,
        controller.panel,
        120,
        40,
        1000L,
        ZoneOffset.UTC,
        Style.mono
      )
    )
    val entered = new CountDownLatch(1)
    val finished = new CountDownLatch(1)
    val frame = new Thread(() => {
      try
        WorkspaceApp.collectExecution(
          inputLock,
          controller,
          () => {
            entered.countDown()
            (snapshot, completed)
          }
        )
        ()
      finally finished.countDown()
    })
    try
      val interleaved = inputLock.synchronized {
        frame.start()
        val crossed = entered.await(200L, TimeUnit.MILLISECONDS)
        controller.handle(
          Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Hard)),
          snapshot,
          pending,
          (120, 40),
          false
        )
        crossed
      }
      assert(finished.await(2L, TimeUnit.SECONDS), "frame thread did not finish after key release")
      assert(!interleaved, "frame published a fill while the key still held an older pending view")
      assertEquals(controller.panel.actionNumber, 2L)
      assertEquals(controller.panel.actionNotice, Some(receipt))
    finally frame.join(2000L)
  }
