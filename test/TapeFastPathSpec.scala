package desk

import scala.concurrent.duration.*

class TapeFastPathSpec extends munit.FunSuite:
  override val munitTimeout = 180.seconds
  private val now = 1_800_000_000_000L
  private def trade(at: Long, id: Option[Long]) = Trade(at, Side.Buy, "100", "1", id)
  private def original(old: Vector[Trade], batch: Vector[Trade], at: Long): Vector[Trade] =
    val seen = scala.collection.mutable.HashSet.empty[(Long, Long)]
    (old ++ batch)
      .filter(_.time >= at - State.tapeMs)
      .filter(t => t.id.forall(id => seen.add((t.time, id))))
      .sortBy(-_.time)
      .take(State.tapeCap)

  test("new, overlapping, tied, anonymous and expired batches equal the original fold") {
    val random = new scala.util.Random(62)
    var state = Snapshot.initial("XMR", now)
    var expected = Vector.empty[Trade]
    for i <- 0 until 300 do
      val at = now + i * 1000L
      val batch = Vector.fill(random.nextInt(20)) {
        val time = if i % 3 == 0 then at else at - random.nextInt(10000)
        trade(time, if random.nextBoolean() then None else Some(random.nextInt(5).toLong))
      }
      val clock = if i % 17 == 0 then at + State.tapeMs else at
      expected = original(expected, batch, clock)
      state = State.reduce(state, Event.Trades("XMR", batch), clock)
      assertEquals(state.tape, expected)
    val boundary = trade(now - State.tapeMs, Some(1))
    val expired = boundary.copy(time = boundary.time - 1)
    val seeded = State.reduce(
      Snapshot.initial("XMR", now),
      Event.Trades("XMR", Vector(boundary, expired)),
      now - 1
    )
    assertEquals(
      State.reduce(seeded, Event.Trades("XMR", Vector(trade(now, Some(2)))), now).tape,
      Vector(trade(now, Some(2)), boundary)
    )
  }

  test("1000 newer events on a full tape equal the original and avoid its full-tape cost") {
    val seed = Vector.tabulate(State.tapeCap)(i => trade(now - i, Some(i.toLong)))
    val start = State.reduce(Snapshot.initial("XMR", now), Event.Trades("XMR", seed), now)
    val events = Vector.tabulate(1000)(i => Vector(trade(now + i + 1, Some(i.toLong))))
    // Warm both implementations before measuring the same input.
    for batch <- events.take(10) do
      State.reduce(start, Event.Trades("XMR", batch), now)
      original(start.tape, batch, now)
    val began = System.nanoTime()
    val expected = events.foldLeft(start.tape)((tape, batch) => original(tape, batch, now))
    val referenceNanos = System.nanoTime() - began
    val fastBegan = System.nanoTime()
    val actual =
      events.foldLeft(start)((s, batch) => State.reduce(s, Event.Trades("XMR", batch), now))
    val fastNanos = System.nanoTime() - fastBegan
    assertEquals(actual.tape, expected)
    assert(fastNanos * 2 < referenceNanos, "newer events still pay the full-tape cost")
  }
