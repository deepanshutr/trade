package desk

import java.io.IOException
import java.nio.file.Files

class OrderJournalSpec extends TempDirectorySuite:
  private val owner = "0x" + "2" * 40
  private val identity = "mainnet-" + "1" * 40
  private def planned(side: Side, held: BigDecimal): OrderPlan =
    val b = Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), 1L)
    Trading
      .plan(
        "XMR",
        MarketRules(0, 3, 5),
        b,
        BigDecimal(if side == Side.Buy then 100 else 99),
        held,
        OrderIntent(side, OrderKind.Hard),
        TradeSizes(),
        1,
        Trading.defaultSlippage
      )
      .toOption
      .get
  private def plan: OrderPlan = planned(Side.Buy, BigDecimal(0))
  // A sell of $500 from a long of 10 leaves about 4.95, so it stays a partial reduce.
  private def reduce: OrderPlan = planned(Side.Sell, BigDecimal(10))
  private val legacyFields = Set(
    "id",
    "nonce",
    "coin",
    "asset",
    "side",
    "quantity",
    "limit",
    "reference",
    "expiresAfter",
    "createdAt",
    "owner"
  )

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

  test("a reduce-only order survives restart") {
    assert(reduce.reduceOnly)
    val dir = tempDirectory("desk-reduce")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(reduce, owner, 1000L)
    first.close()
    assert(saved.reduceOnly)
    val written = ujson.read(Files.readString(dir.resolve(identity + ".json")))("pending")
    assertEquals(written.obj.keySet.toSet, legacyFields + "reduceOnly")
    assertEquals(written("reduceOnly"), ujson.Bool(true))
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.pending, Some(saved))
    finally reopened.close()
  }

  test("an opening order is written without the reduce-only field") {
    assert(!plan.reduceOnly)
    val dir = tempDirectory("desk-opening")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(plan, owner, 1000L)
    first.close()
    assert(!saved.reduceOnly)
    val written = ujson.read(Files.readString(dir.resolve(identity + ".json")))
    assertEquals(written("version"), ujson.Num(1))
    assertEquals(written("pending").obj.keySet.toSet, legacyFields)
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.pending, Some(saved))
    finally reopened.close()
  }

  test("a journal written without the reduce-only field loads as an opening order") {
    val dir = tempDirectory("desk-legacy-reduce")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(reduce, owner, 1000L)
    first.close()
    assert(saved.reduceOnly)
    val file = dir.resolve(identity + ".json")
    val data = ujson.read(Files.readString(file))
    data("pending").obj.remove("reduceOnly")
    Files.writeString(file, ujson.write(data))
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.pending, Some(saved.copy(reduceOnly = false)))
    finally reopened.close()
  }

  test("a reduce-only field that is not a boolean cannot load") {
    val dir = tempDirectory("desk-bad-reduce")
    val first = OrderJournal.open(dir, identity)
    first.reserve(reduce, owner, 1000L)
    first.close()
    val file = dir.resolve(identity + ".json")
    Vector[ujson.Value](ujson.Str("true"), ujson.Num(1), ujson.Null).foreach { value =>
      val data = ujson.read(Files.readString(file))
      data("pending")("reduceOnly") = value
      Files.writeString(file, ujson.write(data))
      intercept[Exception](OrderJournal.open(dir, identity))
      assertEquals(ujson.read(Files.readString(file))("pending")("reduceOnly"), value)
    }
  }

  test("a resting order keeps its oid and remaining size through a restart") {
    val dir = tempDirectory("desk-resting")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(plan, owner, 1000L)
    val rested = first.rest(saved.id, 77738308L)
    assertEquals(rested.oid, Some(77738308L))
    assertEquals(rested.remaining, Some(saved.quantity))
    assertEquals(first.pending, None)
    assertEquals(first.resting, Vector(rested))
    first.remaining(saved.id, "0.15")
    first.close()
    val text = ujson.read(Files.readString(dir.resolve(identity + ".json")))
    assertEquals(text("version").num, 2.0)
    assertEquals(text("pending"), ujson.Null)
    assertEquals(text("resting")(0)("oid"), ujson.Str("77738308"))
    assertEquals(text("resting")(0)("remaining"), ujson.Str("0.15"))
    assertEquals(text("resting")(0)("post"), ujson.Bool(false))
    val second = OrderJournal.open(dir, identity)
    try
      assertEquals(second.resting, Vector(rested.copy(remaining = Some("0.15"))))
      second.settle(saved.id)
      assertEquals(second.resting, Vector.empty)
    finally second.close()
    val text1 = Files.readString(dir.resolve(identity + ".json"))
    assert(text1.startsWith("""{"version":1,"""), text1)
  }

  test("a journal with no resting orders is written in the version 1 shape") {
    val dir = tempDirectory("desk-v1-bytes")
    val journal = OrderJournal.open(dir, identity)
    try
      assertEquals(journal.nextNonce(1000L), 1000L)
      assertEquals(
        Files.readString(dir.resolve(identity + ".json")),
        """{"version":1,"lastNonce":"1000","pending":null}"""
      )
    finally journal.close()
  }

  test("a version 1 file loads with no resting orders and version 3 is refused") {
    val dir = tempDirectory("desk-versions")
    val first = OrderJournal.open(dir, identity)
    first.reserve(plan, owner, 1000L)
    first.close()
    val file = dir.resolve(identity + ".json")
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.resting, Vector.empty)
    finally reopened.close()
    val data = ujson.read(Files.readString(file))
    data("version") = 3
    Files.writeString(file, ujson.write(data))
    intercept[Exception](OrderJournal.open(dir, identity))
  }

  test("reserve throws while an order is pending and not while orders rest") {
    val dir = tempDirectory("desk-reserve-rest")
    val journal = OrderJournal.open(dir, identity)
    try
      val saved = journal.reserve(plan, owner, 1000L)
      intercept[IllegalStateException](journal.reserve(plan, owner, 1001L))
      journal.rest(saved.id, 4294967296L)
      val next = journal.reserve(plan, owner, 1002L)
      assertEquals(next.nonce, 1002L)
      assertEquals(journal.resting.map(_.oid), Vector(Some(4294967296L)))
      intercept[IllegalArgumentException](journal.rest("different", 1L))
      intercept[IllegalArgumentException](journal.remaining("different", "1"))
      intercept[IllegalArgumentException](journal.settle("different"))
    finally journal.close()
  }

  test("a cancel nonce persists through nextNonce while an order rests") {
    val dir = tempDirectory("desk-cancel-nonce")
    val first = OrderJournal.open(dir, identity)
    val saved = first.reserve(plan, owner, 1000L)
    first.rest(saved.id, 5L)
    assertEquals(first.nextNonce(900L), 1001L)
    first.close()
    val second = OrderJournal.open(dir, identity)
    try
      assertEquals(second.resting.map(_.id), Vector(saved.id))
      assertEquals(second.nextNonce(0L), 1002L)
    finally second.close()
  }

  private def restingPair(dir: java.nio.file.Path): (OrderJournal, SavedOrder, SavedOrder) =
    val journal = OrderJournal.open(dir, identity)
    val a = journal.reserve(plan, owner, 1000L)
    journal.rest(a.id, 11L)
    val b = journal.reserve(plan, owner, 1001L)
    journal.rest(b.id, 4294967296L)
    (journal, journal.resting(0), journal.resting(1))

  test("finish keeps the resting orders in memory and on disk") {
    val dir = tempDirectory("desk-finish-rest")
    val journal = OrderJournal.open(dir, identity)
    val a = journal.reserve(plan, owner, 1000L)
    val rested = journal.rest(a.id, 11L)
    val b = journal.reserve(plan, owner, 1001L)
    journal.finish(b.id)
    assertEquals(journal.resting, Vector(rested))
    journal.close()
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.resting, Vector(rested))
    finally reopened.close()
  }

  test("settle and remaining act only on the named order") {
    val dir = tempDirectory("desk-two-resting")
    val (journal, a, b) = restingPair(dir)
    journal.remaining(b.id, "0.15")
    assertEquals(journal.resting, Vector(a, b.copy(remaining = Some("0.15"))))
    journal.settle(a.id)
    assertEquals(journal.resting, Vector(b.copy(remaining = Some("0.15"))))
    journal.close()
    val reopened = OrderJournal.open(dir, identity)
    try
      assertEquals(reopened.resting, Vector(b.copy(remaining = Some("0.15"))))
      reopened.remaining(b.id, "0.1")
      assertEquals(reopened.resting.map(_.remaining), Vector(Some("0.1")))
    finally reopened.close()
    val third = tempDirectory("desk-two-resting-first")
    val (other, c, d) = restingPair(third)
    try
      other.remaining(c.id, "0.2")
      assertEquals(other.resting, Vector(c.copy(remaining = Some("0.2")), d))
      other.settle(d.id)
      assertEquals(other.resting, Vector(c.copy(remaining = Some("0.2"))))
    finally other.close()
  }

  test("a write that would pass 32 KiB is refused and the file still opens") {
    val dir = tempDirectory("desk-cap")
    val journal = OrderJournal.open(dir, identity)
    var rested = 0
    var refused = false
    try
      while !refused && rested < 400 do
        try
          val saved = journal.reserve(plan, owner, 1000L + rested)
          journal.rest(saved.id, rested.toLong)
          rested += 1
        catch
          case error: IllegalArgumentException =>
            assert(error.getMessage.contains("saved state is too large"), error.getMessage)
            refused = true
    finally journal.close()
    assert(refused && rested > 10)
    val reopened = OrderJournal.open(dir, identity)
    try assertEquals(reopened.resting.size, rested)
    finally reopened.close()
  }

  test("a saved file over 32 KiB is refused at open") {
    val dir = tempDirectory("desk-read-cap")
    val first = OrderJournal.open(dir, identity)
    first.nextNonce(1000L)
    first.close()
    val file = dir.resolve(identity + ".json")
    val text = Files.readString(file)
    Files.writeString(file, text + " " * (32768 - text.length))
    OrderJournal.open(dir, identity).close()
    Files.writeString(file, text + " " * (32769 - text.length))
    val error = intercept[IllegalArgumentException](OrderJournal.open(dir, identity))
    assert(error.getMessage.contains("too large"), error.getMessage)
  }

  test("a version 2 file with a bad resting list cannot load") {
    val edits: Vector[(String, ujson.Value => Unit)] = Vector(
      "empty list" -> (j => j("resting") = ujson.Arr()),
      "duplicate id" -> (j => j("resting").arr.append(j("resting")(0))),
      "missing oid" -> (j => j("resting")(0).obj.remove("oid")),
      "missing remaining" -> (j => j("resting")(0).obj.remove("remaining")),
      "negative oid" -> (j => j("resting")(0)("oid") = "-1"),
      "oid beyond the nonce" -> (j => j("resting")(0)("nonce") = "999999")
    )
    edits.foreach { case (name, edit) =>
      val dir = tempDirectory("desk-bad-resting")
      val first = OrderJournal.open(dir, identity)
      val saved = first.reserve(plan, owner, 1000L)
      first.rest(saved.id, 11L)
      first.close()
      val file = dir.resolve(identity + ".json")
      val data = ujson.read(Files.readString(file))
      edit(data)
      Files.writeString(file, ujson.write(data))
      intercept[Exception](OrderJournal.open(dir, identity))
      assert(true, name)
    }
    val dir = tempDirectory("desk-negative-oid")
    val journal = OrderJournal.open(dir, identity)
    try
      val saved = journal.reserve(plan, owner, 1000L)
      intercept[IllegalArgumentException](journal.rest(saved.id, -1L))
      assertEquals(journal.pending, Some(saved))
    finally journal.close()
  }

  test("the post flag survives a restart and a rewrite for pending and resting orders") {
    val dir = tempDirectory("desk-post")
    val first = OrderJournal.open(dir, identity)
    val a = first.reserve(plan, owner, 1000L)
    first.rest(a.id, 11L)
    val b = first.reserve(plan, owner, 1001L)
    first.close()
    val file = dir.resolve(identity + ".json")
    val data = ujson.read(Files.readString(file))
    data("pending")("post") = true
    data("resting")(0)("post") = true
    Files.writeString(file, ujson.write(data))
    val reopened = OrderJournal.open(dir, identity)
    try
      assertEquals(reopened.pending.map(_.post), Some(true))
      assertEquals(reopened.resting.map(_.post), Vector(true))
      reopened.remaining(a.id, "0.5")
      assertEquals(reopened.pending.map(_.post), Some(true))
    finally reopened.close()
    val again = OrderJournal.open(dir, identity)
    try
      assertEquals(again.pending.map(_.post), Some(true))
      assertEquals(again.resting.map(_.post), Vector(true))
      assertEquals(again.rest(b.id, 12L).post, true)
      assertEquals(again.resting.map(_.post), Vector(true, true))
    finally again.close()
    val text = ujson.read(Files.readString(file))
    assertEquals(text("resting")(1)("post"), ujson.Bool(true))
  }
