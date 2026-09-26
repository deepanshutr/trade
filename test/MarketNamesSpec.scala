package desk

import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

class MarketNamesSpec extends munit.FunSuite:
  private val meta =
    """{"universe":[{"name":"kPEPE","szDecimals":0,"maxLeverage":10}]}"""

  test("market rules accept the exact mixed-case name listed by the venue") {
    assertEquals(Exchange.market(ujson.read(meta), "kPEPE"), MarketRules(0, 0, 10))
  }

  test("market rules refuse a listed name the market prompt would refuse") {
    val listed = ujson.read("""{"universe":[{"name":"../x","szDecimals":0,"maxLeverage":10}]}""")
    intercept[IllegalArgumentException](Exchange.market(listed, "../x"))
  }

  test("paper becomes ready for a listed mixed-case market with fresh prices") {
    val now = 1800000000000L
    val directory = Files.createTempDirectory("desk-market-names")
    val rest = new Rest:
      def post(body: String): Option[String] = Some(meta)
    val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(10000), BigDecimal(0))
    val worker = new PaperWorker(TradingConfig(), "kPEPE", ledger, rest, () => now, () => now)
    try
      worker.start()
      worker.observe(Event.SocketUp)
      worker.observe(
        Event.BookUpdate(
          "kPEPE",
          Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), now),
          true
        )
      )
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !worker.view.ready && System.nanoTime() < deadline do Thread.sleep(5)
      assertEquals(worker.view.message, "Paper ready")
      assert(worker.view.ready)
      assertEquals(worker.view.rules, Some(MarketRules(0, 0, 10)))
    finally
      worker.close()
      val paths = Files.walk(directory)
      try paths.iterator().asScala.toVector.reverse.foreach(Files.delete(_))
      finally paths.close()
  }
