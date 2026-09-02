package desk

import scala.util.Try

object Events:
  def parse(text: String): Option[Event] =
    Try(parseUnsafe(text)).getOrElse(Some(Event.Bad(Option(text).getOrElse("").take(80))))

  def parseRestBook(text: String): Option[Book] =
    Try(book(ujson.read(text))).toOption

  def parseRestCandles(text: String): Option[Vector[Candle]] =
    Try(ujson.read(text).arr.toVector.map(candle)).toOption

  private def parseUnsafe(text: String): Option[Event] =
    val j = ujson.read(text)
    j("channel").str match
      case "l2Book" => Some(Event.BookUpdate(book(j("data")), fromSocket = true))
      case "bbo"    =>
        val d = j("data")
        val sides = d("bbo").arr
        if sides(0).isNull || sides(1).isNull then None
        else Some(Event.BboUpdate(level(sides(0)), level(sides(1)), d("time").num.toLong))
      case "trades"               => Some(Event.Trades(j("data").arr.toVector.map(trade)))
      case "candle"               => Some(Event.CandleUpdate(candle(j("data"))))
      case "activeAssetCtx"       => Some(Event.CtxUpdate(ctx(j("data")("ctx"))))
      case "pong"                 => Some(Event.Pong)
      case "subscriptionResponse" => None
      case other                  => Some(Event.Bad(s"channel $other"))

  private def level(v: ujson.Value): Level = Level(v("px").str, v("sz").str, v("n").num.toInt)

  private def book(d: ujson.Value): Book =
    val levels = d("levels").arr
    Book(levels(0).arr.toVector.map(level), levels(1).arr.toVector.map(level), d("time").num.toLong)

  private def trade(v: ujson.Value): Trade =
    val side = v("side").str match
      case "B" => Side.Buy
      case "A" => Side.Sell
      case s   => throw new IllegalArgumentException(s"side $s")
    Trade(v("time").num.toLong, side, v("px").str, v("sz").str)

  private def candle(v: ujson.Value): Candle =
    Candle(
      v("t").num.toLong,
      v("o").str.toDouble,
      v("h").str.toDouble,
      v("l").str.toDouble,
      v("c").str.toDouble,
      v("v").str.toDouble
    )

  private def ctx(c: ujson.Value): Ctx =
    val mid = c.obj.get("midPx").filterNot(_.isNull).map(_.str)
    Ctx(
      c("markPx").str,
      c("oraclePx").str,
      c("funding").str,
      c("openInterest").str,
      c("dayNtlVlm").str,
      c("prevDayPx").str,
      mid
    )
