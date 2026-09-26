package desk

import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import upickle.default.read

class ArchiveMarketsSpec extends munit.FunSuite:
  import MarketArchive.given

  test("capture evicts the oldest candle market and continues across restarts") {
    val directory = Files.createTempDirectory("desk-archive-markets")
    val at = 1800000000000L
    val archive = MarketArchive.open(directory)
    try
      for i <- 0 until 33 do
        val candle = Candle(at - (i + 1) * 60000L, 100, 102, 98, 101, 12)
        archive.record(Event.CandleUpdate(s"COIN$i", "1m", candle), at + i)
      archive.record(
        Event.BookUpdate("COIN32", Book(Vector.empty, Vector.empty, at), true),
        at + 33
      )
      archive.checkpoint()
      val saved = read[CandleCheckpoint](Files.readString(directory.resolve("candles.json")))
      assertEquals(saved.markets.size, 32)
      assert(!saved.markets.contains("COIN31"))
      assert(saved.markets.contains("COIN0"))
      assert(saved.markets.contains("COIN32"))
      val files = Files.list(directory)
      val records = try
        files
          .iterator()
          .asScala
          .filter(_.getFileName.toString.startsWith("events-"))
          .flatMap(path => Files.readString(path).linesIterator)
          .map(read[PublicRecord](_))
          .toVector
      finally files.close()
      assertEquals(records.size, 34)
      assertEquals(records.last.kind, "book")
    finally archive.close()
    val reopened = MarketArchive.open(directory)
    try
      reopened.record(Event.CandleUpdate("NEW", "1m", Candle(at, 100, 102, 98, 101, 12)), at + 34)
      reopened.checkpoint()
      val saved = read[CandleCheckpoint](Files.readString(directory.resolve("candles.json")))
      assertEquals(saved.markets.size, 32)
      assert(!saved.markets.contains("COIN32"))
      assert(saved.markets.contains("NEW"))
      assertEquals(MarketArchive.lastWrite(directory), at + 34)
    finally reopened.close()
  }

  test("a refused new market keeps every saved market") {
    val directory = Files.createTempDirectory("desk-archive-refused")
    val at = 1800000000000L
    val archive = MarketArchive.open(directory, maximumBytes = 200000L, chunkBytes = 100000L)
    try
      for i <- 0 until 32 do
        val candle = Candle(at - (i + 1) * 60000L, 100, 102, 98, 101, 12)
        archive.record(Event.CandleUpdate(s"COIN$i", "1m", candle), at + i)
      val history = Vector.tabulate(5000)(j => Candle(at - (j + 1) * 60000L, 100, 102, 98, 101, 12))
      intercept[IllegalArgumentException](
        archive.record(Event.CandleHistory("NEW", "1m", history.reverse), at + 40)
      )
      archive.checkpoint()
      val saved = read[CandleCheckpoint](Files.readString(directory.resolve("candles.json")))
      assertEquals(saved.markets.size, 32)
      assert(saved.markets.contains("COIN31"))
      assert(!saved.markets.contains("NEW"))
    finally archive.close()
  }

  test("an update to a saved market evicts nothing") {
    val directory = Files.createTempDirectory("desk-archive-known")
    val at = 1800000000000L
    val archive = MarketArchive.open(directory)
    try
      for i <- 0 until 32 do
        val candle = Candle(at - (i + 1) * 60000L, 100, 102, 98, 101, 12)
        archive.record(Event.CandleUpdate(s"COIN$i", "1m", candle), at + i)
      archive.record(Event.CandleUpdate("COIN0", "1m", Candle(at, 100, 102, 98, 101, 12)), at + 40)
      archive.checkpoint()
      val saved = read[CandleCheckpoint](Files.readString(directory.resolve("candles.json")))
      assertEquals(saved.markets.size, 32)
      assert(saved.markets.contains("COIN31"))
    finally archive.close()
  }
