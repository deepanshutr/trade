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

  test("close rescans partial writes before enforcing the disk budget") {
    val directory = Files.createTempDirectory("desk-archive-partial")
    val archive = MarketArchive.open(directory, maximumBytes = 8000, chunkBytes = 3000)
    def chunks =
      val entries = Files.list(directory)
      try entries.iterator().asScala.filter(_.getFileName.toString.endsWith(".jsonl")).toVector
      finally entries.close()
    try
      var i = 0
      while (chunks.size < 2 || archive.bytes < 4400) && i < 100 do
        archive.record(
          Event.BboUpdate("XMR", Level("100", "2", 1), Level("101", "3", 1), now + i),
          now + i
        )
        i += 1
      assertEquals(chunks.size, 2)
      val newest = chunks.maxBy(_.getFileName.toString)
      Files.write(newest, Array.fill[Byte](4000)(0), java.nio.file.StandardOpenOption.APPEND)
    finally archive.close()
    val entries = Files.list(directory)
    val total = try entries.iterator().asScala.map(Files.size(_)).sum
    finally entries.close()
    assert(total <= 8000, s"archive uses $total bytes against an 8000-byte budget")
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

  test("an open archive restores from its initial checkpoint without reading the file again") {
    val directory = Files.createTempDirectory("desk-archive-reuse")
    val writer = MarketArchive.open(directory)
    writer.record(Event.CandleHistory("XMR", "1m", Vector(candle)), now)
    writer.close()
    val opened = MarketArchive.open(directory)
    try
      LocalFiles.write(directory.resolve("candles.json"), "invalid after opening")
      assertEquals(opened.restore(Snapshot.initial("XMR", now)).candles("1m"), Vector(candle))
    finally opened.close()
  }

  test("workspace restores candles even when another recorder holds the archive lock") {
    val directory = Files.createTempDirectory("desk-archive-held")
    val owner = MarketArchive.open(directory)
    try
      owner.record(Event.CandleHistory("XMR", "1m", Vector(candle)), now)
      owner.checkpoint()
      val (recovered, recorder) = WorkspaceApp.openArchive(directory, Snapshot.initial("XMR", now))
      assertEquals(recorder, None)
      assertEquals(recovered.get.candles("1m"), Vector(candle))
    finally owner.close()
  }
