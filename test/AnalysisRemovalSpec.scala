package desk

import java.nio.file.Files

class AnalysisRemovalSpec extends munit.FunSuite:
  test("legacy no-analysis flag is accepted as a no-op and absent from usage") {
    assertEquals(
      Main.parseArgs(Seq("XMR", "--record", "--no-analysis")),
      Main.parseArgs(Seq("XMR", "--record"))
    )
    assert(!Main.usage.contains("--no-analysis"))
  }

  test("saved analysis and question messages still load and survive a new order") {
    val directory = Files.createTempDirectory("desk-legacy-analysis")
    val path = directory.resolve("messages.json")
    try
      LocalFiles.write(
        path,
        """[{"id":1,"at":123,"kind":"analysis","text":"old answer","feedback":false},
          |{"id":2,"at":124,"kind":"question","text":"old question"}]""".stripMargin
      )
      val old = Vector(
        DeskMessage(1, 123, "analysis", "old answer", Some(false)),
        DeskMessage(2, 124, "question", "old question")
      )
      val history = MessageHistory.open(directory)
      val next = old :+ DeskMessage(3, 125, "order", "PAPER XMR buy filled 5")
      try
        assertEquals(history.messages, old)
        history.save(next)
      finally history.close()
      val reopened = MessageHistory.open(directory)
      try assertEquals(reopened.messages, next)
      finally reopened.close()
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory.resolve("messages.lock"))
      Files.deleteIfExists(directory)
  }
