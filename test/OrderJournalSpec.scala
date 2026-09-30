package desk

import java.io.IOException
import java.nio.file.Files

class OrderJournalSpec extends TempDirectorySuite:
  private val owner = "0x" + "2" * 40
  private val identity = "mainnet-" + "1" * 40
  private def plan: OrderPlan =
    val b = Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), 1L)
    Trading
      .plan(
        "XMR",
        MarketRules(0, 3, 5),
        b,
        BigDecimal(100),
        BigDecimal(0),
        OrderIntent(Side.Buy, OrderKind.Hard),
        TradeSizes()
      )
      .toOption
      .get

  test("an unresolved press survives restart and blocks replacement") {
    val dir = tempDirectory("desk-journal")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(plan, owner, 1000L)
    assertEquals(saved.nonce, 1000L)
    assertEquals(saved.expiresAfter, 6000L)
    assert(saved.id.matches("0x[0-9a-f]{32}"))
    intercept[IllegalStateException](first.reserve(plan, owner, 1001L))
    first.close()
    val second = OrderJournal.open(dir, identity)
    try
      assertEquals(second.pending, Some(saved))
      assertEquals(second.pending.flatMap(_.owner), Some(owner))
      intercept[IllegalStateException](second.reserve(plan, owner, 1002L))
      intercept[IllegalArgumentException](second.finish("different"))
      second.finish(saved.id)
      val next = second.reserve(plan, owner, 999L)
      assertEquals(next.nonce, 1001L)
      assertNotEquals(next.id, saved.id)
    finally second.close()
  }

  test("a journal written without an owner still loads") {
    val dir = tempDirectory("desk-legacy-owner")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(plan, owner, 1000L)
    first.close()
    val file = dir.resolve(identity + ".json")
    val data = ujson.read(Files.readString(file))
    data("pending").obj.remove("owner")
    Files.writeString(file, ujson.write(data))
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.pending, Some(saved.copy(owner = None)))
    finally reopened.close()
  }

  test("only one process owner can hold a signing state") {
    val dir = tempDirectory("desk-lock")
    val first = OrderJournal.open(dir, identity)
    try intercept[Exception](OrderJournal.open(dir, identity))
    finally first.close()
    OrderJournal.open(dir, identity).close()
  }

  test("a failed reservation writes no request and consumes no nonce") {
    val dir = tempDirectory("desk-write")
    val journal = OrderJournal.open(dir, identity, () => throw new IOException("held"))
    try
      intercept[IOException](journal.reserve(plan, owner, 1000L))
      assertEquals(journal.pending, None)
    finally journal.close()
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.reserve(plan, owner, 1000L).nonce, 1000L)
    finally reopened.close()
  }

  test("a failed completion keeps the previous unresolved record") {
    val dir = tempDirectory("desk-finish")
    var fail = false
    val journal = OrderJournal.open(dir, identity, () => if fail then throw new IOException("held"))
    val saved = journal.reserve(plan, owner, 1000L)
    fail = true
    intercept[IOException](journal.finish(saved.id))
    assertEquals(journal.pending, Some(saved))
    journal.close()
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.pending, Some(saved))
    finally reopened.close()
  }

  test("failure after replacement can leave disk ahead of memory") {
    val dir = tempDirectory("desk-replaced")
    val journal =
      OrderJournal.open(dir, identity, afterReplace = () => throw new IOException("held"))
    try
      intercept[IOException](journal.reserve(plan, owner, 1000L))
      assertEquals(journal.pending, None)
    finally journal.close()
    val reopened = OrderJournal.open(dir, identity)
    try
      assertEquals(reopened.pending.map(_.nonce), Some(1000L))
      intercept[IllegalStateException](reopened.reserve(plan, owner, 1001L))
    finally reopened.close()
  }

  test("leverage nonces persist without replacing an unresolved order") {
    val dir = tempDirectory("desk-nonce")
    val journal = OrderJournal.open(dir, identity)
    assertEquals(journal.nextNonce(1000L), 1000L)
    journal.close()
    val reopened = OrderJournal.open(dir, identity)
    try
      assertEquals(reopened.nextNonce(999L), 1001L)
      val saved = reopened.reserve(plan, owner, 998L)
      assertEquals(saved.nonce, 1002L)
      intercept[IllegalStateException](reopened.nextNonce(1003L))
    finally reopened.close()
  }

  test("malformed saved state cannot be overwritten as a clean start") {
    val dir = tempDirectory("desk-corrupt")
    val file = dir.resolve(identity + ".json")
    Files.writeString(file, "broken")
    Files.setPosixFilePermissions(
      file,
      java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
    )
    intercept[Exception](OrderJournal.open(dir, identity))
    assertEquals(Files.readString(file), "broken")
  }
