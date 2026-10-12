package desk

class DataQualitySpec extends munit.FunSuite:
  private val now = 1_800_000_000_000L
  private def candles(n: Int): Vector[Candle] =
    Vector.tabulate(n)(i => Candle(i * 60000L, 100, 105, 95, 100 + i % 7, 1))

  test("replayed venue trades are counted once but distinct equal fills survive") {
    def batch(ids: Vector[Int]): Event = Events
      .parse(
        ujson
          .Obj(
            "channel" -> "trades",
            "data" -> ujson.Arr.from(
              ids.map(id =>
                ujson.Obj(
                  "coin" -> "XMR",
                  "side" -> "B",
                  "px" -> "100",
                  "sz" -> "2",
                  "time" -> ujson.Num(now.toDouble),
                  "tid" -> id
                )
              )
            )
          )
          .render()
      )
      .get
    val initial = Snapshot.initial("XMR", now)
    val first = State.reduce(initial, batch(Vector(11, 12)), now)
    assertEquals(first.tape.size, 2)
    val replay = State.reduce(first, batch(Vector(12, 13)), now)
    assertEquals(replay.tape.size, 3)
  }

  test("backfill adds missing candles and preserves overlapping live observations") {
    val live = candles(10).last.copy(c = 123)
    val s = State.reduce(Snapshot.initial("XMR", now), Event.CandleUpdate("XMR", "1m", live), now)
    val restored = State.reduce(s, Event.CandleHistory("XMR", "1m", candles(10)), now)
    assertEquals(restored.candles("1m").size, 10)
    assertEquals(restored.candles("1m").last, live)
    assertEquals(restored.feed, s.feed)
  }
