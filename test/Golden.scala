package desk

import java.nio.file.{Files, Paths}

object Golden:
  val now = 1_788_345_726_000L
  def fixture(name: String): String = Files.readString(Paths.get("fixtures", name))
  lazy val snapshot: Snapshot =
    val s0 = Snapshot.initial("XMR", "5m", now - 5000)
    val events = Vector(
      Events.parse(fixture("l2Book.json")).get,
      Events.parse(fixture("bbo.json")).get,
      Events.parse(fixture("trades.json")).get,
      Events.parse(fixture("activeAssetCtx.json")).get,
      Event.CandleHistory(Events.parseRestCandles(fixture("rest_candles.json")).get)
    )
    events.foldLeft(s0)((s, e) => State.reduce(s, e, now - 300))
