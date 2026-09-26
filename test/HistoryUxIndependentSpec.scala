package desk

import java.time.ZoneOffset
import java.util.concurrent.{CountDownLatch, CopyOnWriteArrayList, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class HistoryUxIndependentSpec extends munit.FunSuite:
  private val epoch = 1800000000000L
  private val snapshot = Snapshot.initial("XMR", epoch)
  private val view = TradingView("XMR", Some(BigDecimal(5)), "Paper ready", ready = true)
  private def make(save: Vector[DeskMessage] => Unit): DeskController =
    new DeskController(
      PanelView(mode = "PAPER"),
      () => epoch,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      save
    )
  private def waitFor(label: String)(condition: => Boolean): Unit =
    val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while !condition && System.nanoTime() < end do Thread.sleep(2)
    assert(condition, label)
  private def writer(controller: DeskController): Option[Thread] =
    val field = classOf[DeskController].getDeclaredFields.find(_.getName == "historyThread").get
    field.setAccessible(true)
    field.get(controller).asInstanceOf[Option[Thread]]
  private def frame(controller: DeskController): WorkspaceFrame =
    Workspace.draw(snapshot, view, controller.panel, 100, 30, epoch, ZoneOffset.UTC, Style.mono)

  test("closing before start persists the latest snapshot once and leaves no writer alive") {
    val saves = new CopyOnWriteArrayList[Vector[DeskMessage]]()
    val c = make(messages => { saves.add(messages); () })
    try
      c.addMessage("order", "First completed paper order")
      c.addMessage("order", "Second completed paper order")
      assertEquals(saves.size(), 0)
      assertEquals(writer(c), None)
      c.close()
      assertEquals(saves.size(), 1)
      assertEquals(
        saves.get(0).map(_.text),
        Vector("First completed paper order", "Second completed paper order")
      )
      assertEquals(saves.get(0), c.panel.messages)
      assert(writer(c).forall(!_.isAlive))
      c.start()
      c.close()
      assertEquals(saves.size(), 1)
      assert(writer(c).forall(!_.isAlive))
    finally c.close()
  }

  test("closing during a held save drains the newer snapshot without holding keyboard or drawing") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val closed = new CountDownLatch(1)
    val read = new CountDownLatch(1)
    val saves = new CopyOnWriteArrayList[Vector[DeskMessage]]()
    val error = new AtomicReference[Throwable]()
    val c = make(messages => {
      if saves.isEmpty then
        entered.countDown()
        if !release.await(5, TimeUnit.SECONDS) then
          throw new IllegalStateException("synthetic history wait expired")
      saves.add(messages)
      ()
    })
    val closer = new Thread(
      () => {
        try c.close()
        catch case e: Throwable => error.set(e)
        finally closed.countDown()
      },
      "history-ux-close"
    )
    val reader = new Thread(
      () => {
        try
          val f = frame(c)
          c.publish(f)
          c.handle(Command.ToggleMute, snapshot, view, (100, 30), false)
          ()
        catch case e: Throwable => error.set(e)
        finally read.countDown()
      },
      "history-ux-reader"
    )
    try
      c.start()
      c.addMessage("order", "First paper result")
      assert(entered.await(2, TimeUnit.SECONDS))
      c.addMessage("order", "Second paper result")
      c.addMessage("analysis", "Latest synthetic note")
      closer.start()
      assert(!closed.await(50, TimeUnit.MILLISECONDS), "close skipped pending history")
      reader.start()
      assert(read.await(300, TimeUnit.MILLISECONDS), "controller reads held behind history save")
      assertEquals(error.get(), null)
      assert(c.panel.muted)
      release.countDown()
      assert(closed.await(2, TimeUnit.SECONDS), "close did not drain history")
      assertEquals(error.get(), null)
      assertEquals(saves.asScala.toVector.map(_.size), Vector(1, 3))
      assertEquals(saves.get(1), c.panel.messages)
      assert(writer(c).forall(!_.isAlive))
    finally
      release.countDown()
      reader.join(1000L)
      closer.join(1000L)
      c.close()
  }

  test("a held failed save preserves later results, stops retries, and closes its writer") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val calls = new AtomicInteger(0)
    val c = make(_ => {
      calls.incrementAndGet()
      entered.countDown()
      if !release.await(5, TimeUnit.SECONDS) then
        throw new IllegalStateException("synthetic history wait expired")
      throw new java.io.IOException("synthetic history failure")
    })
    try
      c.start()
      c.addMessage("order", "PAPER XMR buy filled 5")
      assert(entered.await(2, TimeUnit.SECONDS))
      c.addMessage("order", "Paper limit: choose a smaller size")
      assertEquals(c.panel.messages.size, 2)
      assert(c.panel.actionNotice.exists(_.contains("Paper limit")))
      release.countDown()
      waitFor("history failure not surfaced")(c.panel.historyFailure.nonEmpty)
      assertEquals(c.panel.messages.count(_.kind == "storage"), 1)
      assert(c.panel.messages.exists(_.text == "PAPER XMR buy filled 5"))
      assert(c.panel.messages.exists(_.text == "Paper limit: choose a smaller size"))
      c.addMessage("order", "PAPER XMR sell filled 1")
      c.close()
      assertEquals(calls.get(), 1)
      assert(writer(c).forall(!_.isAlive))
      val shown = frame(c).lines.mkString("\n")
      assert(shown.contains("!save"))
      assert(c.panel.messages.exists(_.text.contains("sell filled 1")))
      assert(shown.contains("Sold"))
      assert(!shown.toLowerCase.contains("retry"))
    finally
      release.countDown()
      c.close()
  }
