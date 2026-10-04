package desk

import scala.util.Try

// Reads of venue replies about resting orders and the one cancel action Desk signs.
// Each reader returns None for a shape it does not know, so the caller never guesses.
object VenueOrders:
  private val WholeLimit = BigInt(2).pow(53) - 1

  // Every order status after which the venue holds nothing more of the order.
  private[desk] val terminal = Set(
    "filled",
    "canceled",
    "rejected",
    "marginCanceled",
    "vaultWithdrawalCanceled",
    "openInterestCapCanceled",
    "selfTradeCanceled",
    "reduceOnlyCanceled",
    "siblingFilledCanceled",
    "delistedCanceled",
    "liquidatedCanceled",
    "scheduledCancel",
    "tickRejected",
    "minTradeNtlRejected",
    "perpMarginRejected",
    "reduceOnlyRejected",
    "badAloPxRejected",
    "iocCancelRejected",
    "badTriggerPxRejected",
    "marketOrderNoLiquidityRejected",
    "positionIncreaseAtOpenInterestCapRejected",
    "positionFlipAtOpenInterestCapRejected",
    "tooAggressiveAtOpenInterestCapRejected",
    "openInterestIncreaseRejected",
    "insufficientSpotBalanceRejected",
    "oracleRejected",
    "perpMaxPositionRejected"
  )

  def cancelAction(asset: Int, cloid: String): ujson.Value =
    ujson.Obj(
      "type" -> "cancelByCloid",
      "cancels" -> ujson.Arr(ujson.Obj("asset" -> asset, "cloid" -> cloid))
    )

  // Order ids exceed Int.MaxValue, so they never go through Exchange.integer.
  // A value of 2^53 or more is refused, because JSON 2^53 + 1 reads as 2^53.
  def wholeLong(value: ujson.Value): Option[Long] = value match
    case ujson.Num(number)
        if java.lang.Double.isFinite(number) && number >= 0 && number == math.floor(number) &&
          number <= WholeLimit.toDouble =>
      Some(number.toLong)
    case ujson.Str(text) if text.matches("[0-9]{1,19}") && BigInt(text) <= WholeLimit =>
      Some(text.toLong)
    case _ => None

  enum Placed:
    case Filled(quantity: BigDecimal, price: BigDecimal, oid: Long)
    case Resting(oid: Long)
    case Rejected(text: String)

  private def statuses(reply: Option[ujson.Value]): Option[Vector[ujson.Value]] =
    reply.flatMap { j =>
      Try {
        require(j("status").str == "ok")
        j("response")("data")("statuses").arr.toVector
      }.toOption
    }

  def placed(reply: Option[ujson.Value]): Option[Placed] =
    statuses(reply).filter(_.size == 1).flatMap { all =>
      val status = all.head
      Try {
        status.obj.keySet match
          case keys if keys == Set("resting") =>
            wholeLong(status("resting")("oid")).map(Placed.Resting(_))
          case keys if keys == Set("filled") =>
            val f = status("filled")
            val quantity = BigDecimal(f("totalSz").str)
            val price = BigDecimal(f("avgPx").str)
            wholeLong(f("oid")).filter(_ => quantity > 0 && price > 0).map { oid =>
              Placed.Filled(quantity, price, oid)
            }
          case keys if keys == Set("error") => Some(Placed.Rejected(status("error").str))
          case _                            => None
      }.toOption.flatten
    }

  def cancelOutcome(reply: Option[ujson.Value]): Option[Either[String, Unit]] =
    statuses(reply).filter(_.size == 1).flatMap { all =>
      all.head match
        case ujson.Str("success")                                     => Some(Right(()))
        case status: ujson.Obj if status.value.keySet == Set("error") =>
          status.value("error").strOpt.map(Left(_))
        case _ => None
    }

  final case class OrderUpdate(
      cloid: String,
      coin: String,
      status: String,
      remaining: BigDecimal,
      oid: Long,
      at: Long
  )

  def orderUpdate(value: ujson.Value): Option[OrderUpdate] =
    Try {
      val order = value("order")
      for
        oid <- wholeLong(order("oid"))
        at <- wholeLong(value("statusTimestamp"))
      yield OrderUpdate(
        order("cloid").str,
        order("coin").str,
        value("status").str,
        BigDecimal(order("sz").str),
        oid,
        at
      )
    }.toOption.flatten

  enum Lookup:
    case Settled(status: String, filledPrice: Option[BigDecimal])
    case Open(oid: Long, remaining: BigDecimal)
    case Unknown

  def lookup(reply: Option[ujson.Value]): Option[Lookup] =
    reply.flatMap { j =>
      Try {
        j("status").str match
          case "unknownOid" => Some(Lookup.Unknown)
          case "order"      =>
            val detail = j("order")
            val order = detail("order")
            detail("status").str match
              case "open" =>
                wholeLong(order("oid")).map(Lookup.Open(_, BigDecimal(order("sz").str)))
              case "filled" =>
                Some(Lookup.Settled("filled", Some(BigDecimal(order("limitPx").str))))
              case status if terminal.contains(status) => Some(Lookup.Settled(status, None))
              case _                                   => None
          case _ => None
      }.toOption.flatten
    }
