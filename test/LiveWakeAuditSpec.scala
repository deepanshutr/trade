package desk

import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class LiveWakeAuditSpec extends munit.FunSuite:
  test("a burst of account wakes coalesces behind one held account read") {
    val key = Array.fill[Byte](32)(1)
    val owner = "0x" + "2" * 40
    val config = TradingConfig(owner = Some(owner), agent = Some(OrderSigning.address(key)))
    val reads = new AtomicInteger()
    val activeReads = new AtomicInteger()
    val maximumActiveReads = new AtomicInteger()
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val second = new CountDownLatch(1)
    val directory = Files.createTempDirectory("desk-live-wake-audit-")
    val exchange = new Exchange:
      def watch(
          coin: String,
          user: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        receive(
          Some(
            ujson.Obj(
              "user" -> owner,
              "coin" -> coin,
              "leverage" -> ujson.Obj("type" -> "cross", "value" -> 5),
              "maxTradeSzs" -> ujson.Arr("100", "100")
            )
          )
        )
      def info(body: ujson.Value): Option[ujson.Value] = body("type").str match
        case "meta" =>
          Some(
            ujson.Obj(
              "universe" -> ujson.Arr(
                ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5)
              )
            )
          )
        case "userRole" => Some(ujson.Obj("role" -> "agent", "data" -> ujson.Obj("user" -> owner)))
        case "userAbstraction"    => Some(ujson.Str("disabled"))
        case "clearinghouseState" =>
          val active = activeReads.incrementAndGet()
          maximumActiveReads.accumulateAndGet(active, Math.max)
          val count = reads.incrementAndGet()
          try
            if count == 1 then
              entered.countDown()
              release.await()
            if count == 2 then second.countDown()
            Some(
              ujson.Obj(
                "marginSummary" -> ujson.Obj("accountValue" -> "1000"),
                "assetPositions" -> ujson.Arr()
              )
            )
          finally activeReads.decrementAndGet()
        case _ => None
      def post(body: ujson.Value): Option[ujson.Value] = fail("wake check must never send an order")
    val worker = new TradingWorker(
      config,
      true,
      "XMR",
      exchange,
      () => key.clone(),
      () => OrderJournal.open(directory, config.identity),
      () => 10000L,
      () => 10000L,
      warmSigning = () => ()
    )
    try
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "XMR",
          Book(
            Vector(Level("99.99", "20", 1)),
            Vector(Level("100", "20", 1)),
            10000L
          ),
          true
        )
      )
      worker.start()
      assert(entered.await(3, TimeUnit.SECONDS))
      val wake = classOf[TradingWorker].getDeclaredMethods.find(_.getName == "wakeAccount").get
      wake.setAccessible(true)
      for _ <- 1 to 100 do wake.invoke(worker)
      assertEquals(reads.get(), 1)
      release.countDown()
      assert(second.await(300, TimeUnit.MILLISECONDS), "the coalesced wake was lost")
      Thread.sleep(100L)
      assertEquals(reads.get(), 2, "a burst of wakes created redundant account reads")
      assertEquals(maximumActiveReads.get(), 1, "account reads overlapped")
    finally
      release.countDown()
      try worker.close()
      finally
        val paths = Files.walk(directory)
        try paths.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists(_))
        finally paths.close()
  }
