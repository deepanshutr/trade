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
      case "l2Book" =>
        val d = j("data")
        Some(Event.BookUpdate(d("coin").str, book(d), fromSocket = true))
      case "bbo" =>
        val d = j("data")
        val sides = d("bbo").arr
        if sides(0).isNull || sides(1).isNull then None
        else
          Some(
            Event.BboUpdate(
              d("coin").str,
              level(sides(0)),
              level(sides(1)),
              integer(d("time"), Long.MaxValue.toDouble)
            )
          )
      case "trades" =>
        val raw = j("data").arr.toVector
        require(raw.map(_("coin").str).distinct.size <= 1)
        raw.headOption.map(first => Event.Trades(first("coin").str, raw.map(trade)))
      case "candle" =>
        val d = j("data")
        Some(Event.CandleUpdate(d("s").str, d("i").str, candle(d)))
      case "activeAssetCtx" =>
        val d = j("data")
        Some(Event.CtxUpdate(d("coin").str, ctx(d("ctx"))))
      case "pong"                 => Some(Event.Pong)
      case "subscriptionResponse" => None
      case other                  => Some(Event.Bad(s"channel $other"))

  private def number(v: ujson.Value, zero: Boolean = false, signed: Boolean = false): String =
    val text = v.str
    val value = BigDecimal(text)
    val magnitude = text.toDouble.abs
    require(
      (zero && value.signum == 0) ||
        ((signed || value.signum > 0) && magnitude >= 1e-100 && magnitude <= 1e100)
    )
    text

  private def integer(v: ujson.Value, limit: Double): Long =
    val value = v.num
    require(value >= 0 && value < limit && value.isWhole)
    value.toLong

  private def level(v: ujson.Value): Level =
    Level(number(v("px")), number(v("sz"), zero = true), integer(v("n"), 2147483648d).toInt)

  private def book(d: ujson.Value): Book =
    val levels = d("levels").arr
    Book(
      levels(0).arr.toVector.map(level),
      levels(1).arr.toVector.map(level),
      integer(d("time"), Long.MaxValue.toDouble)
    )

  private def trade(v: ujson.Value): Trade =
    val side = v("side").str match
      case "B" => Side.Buy
      case "A" => Side.Sell
      case s   => throw new IllegalArgumentException(s"side $s")
    Trade(
      integer(v("time"), Long.MaxValue.toDouble),
      side,
      number(v("px")),
      number(v("sz"), zero = true),
      v.obj.get("tid").map(integer(_, 9007199254740992d))
    )

  private def candle(v: ujson.Value): Candle =
    Candle(
      integer(v("t"), Long.MaxValue.toDouble),
      number(v("o")).toDouble,
      number(v("h")).toDouble,
      number(v("l")).toDouble,
      number(v("c")).toDouble,
      number(v("v"), zero = true).toDouble
    )

  private def ctx(c: ujson.Value): Ctx =
    val mid = c.obj.get("midPx").filterNot(_.isNull).map(number(_))
    Ctx(
      number(c("markPx")),
      number(c("oraclePx")),
      number(c("funding"), zero = true, signed = true),
      number(c("openInterest"), zero = true),
      number(c("dayNtlVlm"), zero = true),
      number(c("prevDayPx")),
      mid
    )
