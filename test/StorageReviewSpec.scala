package desk

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class StorageReviewSpec extends TempDirectorySuite:
  private val now = 1_800_000_000_000L
  private val candle = Candle(now - 60000L, 100, 102, 98, 101, 12)
  private def plan(side: Side, quantity: String, price: String): OrderPlan =
    val q = BigDecimal(quantity)
    val p = BigDecimal(price)
    OrderPlan("XMR", 0, side, OrderKind.Hard, q, p, p, q, q * p, Some(p), BigDecimal(0), false)

  private def actualBytes(directory: Path): Long =
    val paths = Files.list(directory)
    try paths.iterator().asScala.filter(Files.isRegularFile(_)).map(Files.size(_)).sum
    finally paths.close()

  test("a refused persistent write leaves the previous account and disables fills") {
    val directory = tempDirectory("desk-prop-write-failure")
    val path = directory.resolve("wallet-mainnet.json")
    val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try
      assert(ledger.fill(plan(Side.Buy, "1", "100"), now).isRight)
      val before = ledger.account
      val bytes = Files.readString(path)
      Platform.setMode(path, "rw-r--r--")
      assert(ledger.fill(plan(Side.Buy, "1", "110"), now + 1).isLeft)
      assertEquals(ledger.account, before)
      assertEquals(Files.readString(path), bytes)
      assert(!ledger.available)
      assert(ledger.fill(plan(Side.Sell, "1", "120"), now + 2).isLeft)
      Platform.setMode(path, "rw-------")
    finally ledger.close()
    val reopened = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    try assertEquals(reopened.account.positions("XMR").quantity, BigDecimal(1))
    finally reopened.close()
  }

  test("unequal adds, partial closes and a new holding retain the correct purchase basis") {
    val ledger = PropLedger.open(
      tempDirectory("desk-prop-weighted"),
      "mainnet",
      BigDecimal(1000),
      BigDecimal("0.001")
    )
    try
      assert(ledger.fill(plan(Side.Buy, "1", "95"), now).isRight)
      assert(ledger.fill(plan(Side.Buy, "3", "103"), now + 1).isRight)
      assertEquals(ledger.account.positions("XMR").entry, BigDecimal(101))
      assert(ledger.fill(plan(Side.Sell, "2", "110"), now + 2).isRight)
      assertEquals(ledger.account.positions("XMR").entry, BigDecimal(101))
      assertEquals(ledger.account.realized, BigDecimal(18))
      val before = ledger.account
      assert(ledger.fill(plan(Side.Sell, "4", "90"), now + 3).isLeft)
      assertEquals(ledger.account, before)
      assert(ledger.fill(plan(Side.Sell, "2", "90"), now + 3).isRight)
      assertEquals(ledger.account.positions, Map.empty[String, PropPosition])
      assertEquals(ledger.account.cash, BigDecimal("995.196"))
      assertEquals(ledger.account.realized, BigDecimal(-4))
      assertEquals(ledger.account.fees, BigDecimal("0.804"))
      assert(ledger.fill(plan(Side.Buy, "3", "85"), now + 4).isRight)
      assertEquals(ledger.account.positions("XMR").entry, BigDecimal(85))
      assertEquals(ledger.account.positions("XMR").quantity, BigDecimal(3))
      assertEquals(ledger.account.realized, BigDecimal(-4))
      assertEquals(ledger.account.cash, BigDecimal("739.941"))
    finally ledger.close()
  }

  test("a prop fill cannot persist an invalid market that prevents reopen") {
    val ledger = PropLedger.open(
      tempDirectory("desk-prop-invalid"),
      "mainnet",
      BigDecimal(1000),
      BigDecimal(0)
    )
    try
      val before = ledger.account
      assert(ledger.fill(plan(Side.Buy, "1", "100").copy(coin = "../XMR"), now).isLeft)
      assertEquals(ledger.account, before)
    finally ledger.close()
  }

  test("stale private atomic files count against the archive disk limit") {
    val directory = tempDirectory("desk-archive-stale")
    val stale = directory.resolve(".desk-12345678901234567890.tmp")
    LocalFiles.write(stale, "x" * 3990)
    val archive = MarketArchive.open(directory, maximumBytes = 4000, chunkBytes = 1000)
    try
      scala.util.Try(archive.record(Event.SocketUp, now))
      assert(
        actualBytes(directory) <= 4000,
        s"actual bytes ${actualBytes(directory)}, reported ${archive.bytes}"
      )
    finally scala.util.Try(archive.close())
  }

  test("an owned event file with changed permissions still counts against the limit") {
    val directory = tempDirectory("desk-archive-unsafe")
    val stale = directory.resolve("events-1800000000000-aaaaaaaa.jsonl")
    LocalFiles.write(stale, "x" * 3990)
    Platform.setMode(stale, "rw-r--r--")
    val archive = MarketArchive.open(directory, maximumBytes = 4000, chunkBytes = 1000)
    try
      scala.util.Try(archive.record(Event.SocketUp, now))
      assert(
        actualBytes(directory) <= 4000,
        s"actual bytes ${actualBytes(directory)}, reported ${archive.bytes}"
      )
      assertEquals(Files.readString(stale), "x" * 3990)
    finally scala.util.Try(archive.close())
  }

  test("an invalid interval from the event parser cannot poison the next checkpoint") {
    val directory = tempDirectory("desk-archive-invalid-event")
    val archive = MarketArchive.open(directory)
    try
      archive.record(Event.CandleHistory("XMR", "1m", Vector(candle)), now)
      archive.checkpoint()
      val event = Events
        .parse(
          """{"channel":"candle","data":{"s":"XMR","i":"bogus","t":1800000000000,"o":"100","h":"102","l":"98","c":"101","v":"12"}}"""
        )
        .get
      scala.util.Try(archive.record(event, now + 1))
      scala.util.Try(archive.checkpoint())
    finally scala.util.Try(archive.close())
    assertEquals(
      MarketArchive.restore(directory, Snapshot.initial("XMR", now + 2)).candles("1m"),
      Vector(candle)
    )
  }

  test("a structurally invalid saved checkpoint is refused by archive open") {
    val directory = tempDirectory("desk-archive-invalid-checkpoint")
    LocalFiles.write(
      directory.resolve("candles.json"),
      """{"at":1800000000000,"markets":{"XMR":{"bogus":[]}}}"""
    )
    intercept[Exception] {
      val archive = MarketArchive.open(directory)
      archive.close()
    }
  }

  test("checkpoint pruning reserves both atomic copies even when the active chunk must close") {
    val directory = tempDirectory("desk-archive-checkpoint-budget")
    val checkpoint = directory.resolve("candles.json")
    val bars = Vector.tabulate(28)(i => candle.copy(t = now - (i + 1) * 60000L))
    val archive = MarketArchive.open(directory, maximumBytes = 4000, chunkBytes = 1000)
    def save(): Unit =
      val previous = if Files.exists(checkpoint) then Files.size(checkpoint) else 0L
      archive.checkpoint()
      // The replaced file and completed files coexisted immediately before the atomic move.
      assert(actualBytes(directory) + previous <= 4000)
    try
      archive.record(Event.CandleHistory("XMR", "1m", bars), now)
      save()
      for i <- 1 to 120 do
        archive.record(Event.SocketUp, now + i)
        save()
    finally scala.util.Try(archive.close())
  }
