package desk

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class ArchiveScalingSpec extends TempDirectorySuite:
  test("regular checkpoints preserve the active chunk while sufficient space remains") {
    val root = tempDirectory("desk-capture-checkpoints")
    val archive = MarketArchive.open(root, maximumBytes = 1000000L, chunkBytes = 100000L)
    val at = 1800000000000L
    val book = Book(Vector(Level("100", "10", 2)), Vector(Level("101", "10", 2)), at)
    try
      for i <- 0 until 30 do
        archive.record(Event.BookUpdate("XMR", book, true), at + i * 10000L)
        archive.checkpoint()
      val files = Files.list(root)
      val chunks = try
        files.iterator().asScala.filter(_.getFileName.toString.startsWith("events-")).toVector
      finally files.close()
      assertEquals(chunks.size, 1)
      assertEquals(Files.readString(chunks.head).linesIterator.size, 30)
    finally archive.close()
  }
