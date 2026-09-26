package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class HistoryProgressSpec extends munit.FunSuite:
  private val now = 1800000000000L
  private def controller(persist: Vector[DeskMessage] => Unit, clock: () => Long = () => now) =
    new DeskController(
      PanelView(mode = "PAPER"),
      clock,
      _ => Left("disabled"),
      _ => false,
      _ => Left("disabled"),
      persist
    )
  private def await(label: String)(condition: => Boolean): Unit =
    val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while !condition && System.nanoTime() < end do Thread.sleep(2)
    assert(condition, label)

  test("slow history coalesces bounded snapshots and shutdown saves the last accepted messages") {
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val saved = new java.util.concurrent.CopyOnWriteArrayList[Vector[DeskMessage]]()
    val clock = new AtomicLong(now)
    val c = controller(
      messages => {
        if saved.isEmpty then
          entered.countDown()
          if !release.await(5, TimeUnit.SECONDS) then
            throw new IllegalStateException("synthetic history save not released")
        saved.add(messages)
        ()
      },
      () => clock.get()
    )
    try
      c.start()
      c.addMessage("order", "PAPER XMR buy filled 5")
      assert(entered.await(2, TimeUnit.SECONDS))
      clock.addAndGet(5000)
      val began = System.nanoTime()
      for i <- 2 to 401 do c.addMessage("analysis", s"Synthetic message $i")
      val panel = c.panel
      val directMs = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began)
      assert(directMs <= 100, s"held history blocked state access for ${directMs}ms")
      assertEquals(panel.messages.size, 300)
      assertEquals(panel.historySavingAt, Some(now))
      val frame = Workspace.draw(
        Snapshot.initial("XMR", now),
        TradingView("XMR", Some(BigDecimal(5)), "Paper ready", ready = true),
        panel,
        100,
        30,
        clock.get(),
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      assertEquals(panel.historySavingAt, Some(now))
      assert(frame.lines.exists(_.contains("Bought")))
      release.countDown()
      c.close()
      assertEquals(saved.asScala.toVector.size, 2)
      assertEquals(saved.get(1), c.panel.messages)
      assertEquals(c.panel.historySavingAt, None)
      println(s"Held history: 400 queued messages and state read ${directMs}ms")
    finally
      release.countDown()
      c.close()
  }

  test("failed history save stays visible alongside the confirmed trade result") {
    val c = controller(_ => throw new java.io.IOException("synthetic history failure"))
    try
      c.start()
      c.addMessage("order", "PAPER XMR buy filled 5")
      await("history failure hidden")(c.panel.historyFailure.nonEmpty)
      val frame = Workspace.draw(
        Snapshot.initial("XMR", now),
        TradingView("XMR", Some(BigDecimal(5)), "Paper ready", ready = true),
        c.panel,
        100,
        30,
        now,
        java.time.ZoneOffset.UTC,
        Style.mono
      )
      assert(frame.lines.head.contains("!save"))
      assert(c.panel.messages.exists(_.text.contains("filled 5")))
      assert(frame.lines.exists(_.contains("Bought")))
      assert(!frame.lines.mkString.contains("retry"))
    finally c.close()
  }

  test("fatal history failure can close from its own uncaught handler without joining itself") {
    val handled = new CountDownLatch(1)
    val c = controller(_ => throw new AssertionError("synthetic fatal history failure"))
    c.start()
    val field = classOf[DeskController].getDeclaredFields.find(_.getName == "historyThread").get
    field.setAccessible(true)
    val writer = field.get(c).asInstanceOf[Option[Thread]].get
    writer.setUncaughtExceptionHandler((_, _) => { c.close(); handled.countDown() })
    try
      c.addMessage("order", "PAPER filled")
      assert(
        handled.await(250, TimeUnit.MILLISECONDS),
        "history writer joined itself during shutdown"
      )
    finally
      writer.interrupt()
      writer.join(1000L)
      c.close()
  }
