package desk

import java.nio.file.{Files, Paths}

object Golden:
  val now = 1_788_530_348_655L
  def fixture(name: String): String = Files.readString(Paths.get("fixtures", name))
  private def level(px: Double): Level = Level(f"$px%.3f", "1", 1)
  lazy val snapshot: Snapshot =
    val s0 = Snapshot.initial("XMR", now - 150_000)
    val mids = ujson.read(fixture("r2_mids.json")).obj.toVector
    val bbos: Vector[(Long, Event)] = mids.flatMap { (coin, arr) =>
      arr.arr.toVector.map { a =>
        val t = a(0).num.toLong
        (t, Event.BboUpdate(coin, level(a(1).num - 0.005), level(a(1).num + 0.005), t))
      }
    }
    val books: Vector[(Long, Event)] = ujson
      .read(fixture("r2_books.json"))
      .arr
      .toVector
      .map(m => Events.parse(m.render()).get)
      .collect { case e @ Event.BookUpdate(_, b, _) => (b.time, e) }
    val folded = (bbos ++ books).sortBy(_._1).foldLeft(s0)((s, te) => State.reduce(s, te._2, te._1))
    val coarse = ujson.read(fixture("r2_coarse.json")).obj.toVector.map { (k, v) =>
      Event.CoarseBook("XMR", k.toInt, Events.parseRestBook(v.render()).get)
    }
    val candles =
      ujson.read(fixture("r2_candles.json")).obj.toVector.flatMap { (coin, byInterval) =>
        byInterval.obj.toVector.map { (interval, arr) =>
          Event.CandleHistory(coin, interval, Events.parseRestCandles(arr.render()).get)
        }
      }
    val rest = coarse ++ candles ++ Vector(
      Events.parse(fixture("r2_trades.json")).get,
      Events.parse(fixture("r2_ctx.json")).get
    )
    rest.foldLeft(folded)((s, e) => State.reduce(s, e, now - 300))
