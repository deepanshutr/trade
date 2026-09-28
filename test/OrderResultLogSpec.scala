package desk

import java.nio.file.Files

class OrderResultLogSpec extends munit.FunSuite:
  test("two controllers append all 600 order results without keeping general messages") {
    val directory = Files.createTempDirectory("desk-order-results")
    def controller = new DeskController(
      PanelView(),
      () => 123L,
      _ => Left("unused"),
      _ => false,
      _ => Left("unused"),
      result = text => OrderResultLog.append(directory, 123L, text)
    )
    val first = controller
    val second = controller
    try
      val writers = Vector(first, second).map { c =>
        new Thread(() => {
          for i <- 1 to 300 do
            c.addMessage("analysis", "ignored")
            c.addMessage("order", s"Order pending $i")
        })
      }
      writers.foreach(_.start())
      writers.foreach(_.join())
      val lines = Files.readAllLines(directory.resolve("order-results.jsonl"))
      import scala.jdk.CollectionConverters.*
      val records = lines.asScala.map(ujson.read(_)).toVector
      assertEquals(records.size, 600)
      assertEquals(records.map(_("id").str).distinct.size, 600)
      assert(records.forall(_("kind").str == "order"))
      assertEquals(first.panel.actionNumber, 300L)
      assertEquals(second.panel.actionNumber, 300L)
      assert(!Files.exists(directory.resolve("messages")))
    finally
      val paths = Files.walk(directory)
      try paths.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
      finally paths.close()
  }
