package desk

import java.nio.file.Files

class PublicRequestBudgetSpec extends munit.FunSuite:
  test("public endpoint cost counts response bars conservatively") {
    assertEquals(PublicRequestBudget.cost(Feed.historyBody("XMR", "1m", 1800000000000L)), 104)
    assertEquals(PublicRequestBudget.cost("""{"type":"l2Book","coin":"XMR"}"""), 2)
    assertEquals(PublicRequestBudget.cost("""{"type":"meta"}"""), 20)
  }

  test("an unreadable schedule is preserved and cannot reset the allowance") {
    val root = Files.createTempDirectory("desk-bad-request-budget")
    val now: () => Long = () => 1800000000000L
    val budget = new PublicRequestBudget(root, now)
    budget.reserve(20)
    LocalFiles.write(root.resolve("schedule.txt"), "damaged")
    intercept[NumberFormatException](budget.reserve(20))
    assertEquals(Files.readString(root.resolve("schedule.txt")), "damaged")
  }

  test("a backwards wall-clock step resets the public request schedule") {
    val root = Files.createTempDirectory("desk-clock-step-request-budget")
    var now = 1800000000000L
    val budget = new PublicRequestBudget(root, () => now)
    assertEquals(budget.reserve(2), 172L)
    now -= 600000L
    assertEquals(budget.reserve(2), 172L)
    assertEquals(Files.readString(root.resolve("schedule.txt")), (now + 172L).toString)
    assertEquals(budget.reserve(2), 344L)
  }

  test("two public clients share a paced full-history schedule") {
    val kind = Class.forName("desk.PublicRequestBudget")
    val root = Files.createTempDirectory("desk-request-budget")
    val now: () => Long = () => 1800000000000L
    val constructor = kind.getConstructor(classOf[java.nio.file.Path], classOf[Function0[Long]])
    val first = constructor.newInstance(root, now)
    val second = constructor.newInstance(root, now)
    val reserve = kind.getMethod("reserve", classOf[Int])
    val waits = (0 until 18).map { i =>
      reserve.invoke(if i % 2 == 0 then first else second, Int.box(104)).asInstanceOf[Long]
    }
    assert(waits.sliding(2).forall(pair => pair(1) > pair(0)))
    assert(waits.last >= 18L * 104L * 60000L / 700L)
  }
