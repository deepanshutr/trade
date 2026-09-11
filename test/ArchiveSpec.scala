package desk

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class ArchiveSpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private val candle = Candle(now - 60000L, 100, 102, 98, 101, 12)

  test("recorded candles restore after restart while quotes remain disconnected") {
    val directory = Files.createTempDirectory("desk-archive-test")
    val archive = MarketArchive.open(directory)
    try
      archive.record(Event.CandleHistory("XMR", "1m", Vector(candle)), now)
      archive.record(Event.CandleHistory("BTC", "1m", Vector(candle.copy(c = 102))), now)
      archive.record(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("100", "2", 1)), Vector(Level("101", "3", 1)), now),
          true
        ),
        now
      )
      archive.checkpoint()
    finally archive.close()
    val restored = MarketArchive.restore(directory, Snapshot.initial("XMR", now + 10000))
    assertEquals(restored.candles("1m"), Vector(candle))
    assertEquals(restored.refs("BTC").candles("1m").head.c, 102.0)
    assertEquals(restored.book, None)
    assert(restored.feed.isInstanceOf[Health.Down])
    assertEquals(restored.samples, Vector.empty[(Long, Double)])
  }

  test("a partial event write cannot erase the completed candle checkpoint") {
    val directory = Files.createTempDirectory("desk-archive-test")
    val archive = MarketArchive.open(directory)
    try
      archive.record(Event.CandleHistory("XMR", "1m", Vector(candle)), now)
      archive.checkpoint()
    finally archive.close()
    val entries = Files.list(directory)
    val log = try entries.iterator().asScala.find(_.getFileName.toString.endsWith(".jsonl")).get
    finally entries.close()
    Files.writeString(log, "{incomplete", java.nio.file.StandardOpenOption.APPEND)
    assertEquals(
      MarketArchive.restore(directory, Snapshot.initial("XMR", now)).candles("1m"),
      Vector(candle)
    )
  }

  test("retention prunes only owned public chunks and refuses a second writer") {
    val directory = Files.createTempDirectory("desk-archive-test")
    val foreign = directory.resolve("keep.txt")
    Files.writeString(foreign, "keep")
    val archive =
      MarketArchive.open(directory, maximumBytes = 8000, chunkBytes = 1000, retentionMs = 5000)
    try
      intercept[Exception](MarketArchive.open(directory))
      for i <- 0 until 100 do
        archive.record(
          Event.BboUpdate("XMR", Level("100", "2", 1), Level("101", "3", 1), now + i * 1000L),
          now + i * 1000L
        )
      archive.checkpoint()
      assert(archive.bytes <= 8000)
      assertEquals(Files.readString(foreign), "keep")
    finally archive.close()
  }
