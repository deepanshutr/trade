package desk

import java.nio.file.{Files, Paths}

object Golden:
  val now = 1_788_496_084_056L
  def fixture(name: String): String = Files.readString(Paths.get("fixtures", name))
  private def level(px: Double): Level = Level(f"$px%.3f", "1", 1)
  lazy val snapshot: Snapshot =
    val s0 = Snapshot.initial("XMR", now - 310_000)
    val mids =
      ujson.read(fixture("mids_5m.json")).arr.toVector.map(a => (a(0).num.toLong, a(1).num))
    val books =
      ujson.read(fixture("l2Book_5m.json")).arr.toVector.map(m => Events.parse(m.render()).get)
    val timed: Vector[(Long, Event)] =
      mids.map((t, m) => (t, Event.BboUpdate(level(m - 0.005), level(m + 0.005), t))) ++
        books.map { case e @ Event.BookUpdate(b, _) => (b.time, e); case e => (0L, e) }
    val folded = timed.sortBy(_._1).foldLeft(s0)((s, te) => State.reduce(s, te._2, te._1))
    val rest = Vector(
      Events.parse(fixture("trades_5m.json")).get,
      Events.parse(fixture("activeAssetCtx_5m.json")).get,
      Event.CandleHistory(Events.parseRestCandles(fixture("rest_candles.json")).get)
    )
    rest.foldLeft(folded)((s, e) => State.reduce(s, e, now - 300))
