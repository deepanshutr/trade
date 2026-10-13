package desk

import scala.util.Try

final case class PortfolioView(
    tokenValue: Option[BigDecimal] = None,
    stables: Option[BigDecimal] = None,
    net: Option[BigDecimal] = None,
    entry: Option[BigDecimal] = None,
    liquidation: Option[BigDecimal] = None,
    openProfit: Option[BigDecimal] = None
)

object PortfolioView:
  // Prop's one cross account at the mark. The value and the open result are signed, net is cash
  // plus the open result less the deposit, and the liquidation price uses the maintenance of the
  // selected market's rules.
  def prop(
      account: PropAccount,
      coin: String,
      mark: Option[BigDecimal],
      maintenance: Option[BigDecimal] = None
  ): PortfolioView =
    val held = account.positions.get(coin)
    val price = mark.filter(_ > 0)
    val exposure = held.fold(Option(BigDecimal(0)))(p =>
      price.map(px => BigDecimal(px.bigDecimal.multiply(p.quantity.bigDecimal)))
    )
    val open = held.fold(Option(BigDecimal(0)))(p =>
      price.map(px =>
        BigDecimal(px.bigDecimal.subtract(p.entry.bigDecimal).multiply(p.quantity.bigDecimal))
      )
    )
    val net = if account.positions.nonEmpty && account.positions.keySet != Set(coin) then None
    else
      open.map(result =>
        BigDecimal(
          account.cash.bigDecimal.add(result.bigDecimal).subtract(account.deposit.bigDecimal)
        )
      )
    PortfolioView(
      exposure,
      Some(account.cash),
      net,
      held.map(_.entry),
      maintenance.flatMap(PropMargin.liquidationPrice(account, coin, _)),
      open
    )

  private def decimal(value: ujson.Value): Option[BigDecimal] =
    Try(BigDecimal(value.str)).toOption.filter(_.abs <= BigDecimal("1e30"))
  private def field(value: ujson.Value, name: String): Option[BigDecimal] =
    value.objOpt.flatMap(_.get(name)).flatMap(decimal)

  def live(account: ujson.Value, coin: String, cash: Option[BigDecimal]): PortfolioView =
    val positions = Try(account("assetPositions").arr.toVector.map(_("position"))).toOption
      .filter(
        _.forall(p => p.objOpt.flatMap(_.get("coin")).flatMap(_.strOpt).exists(_.trim.nonEmpty))
      )
    val selected =
      positions.map(_.filter(p => p.objOpt.flatMap(_.get("coin")).contains(ujson.Str(coin))))
    val held = selected.filter(_.size == 1).flatMap(_.headOption)
    val quantity = held.flatMap(field(_, "szi"))
    val flat = selected.exists(_.isEmpty) || quantity.contains(BigDecimal(0))
    val value = if flat then Some(BigDecimal(0))
    else
      for
        q <- quantity
        v <- held.flatMap(field(_, "positionValue")).filter(_ >= 0)
      yield v * q.signum
    PortfolioView(
      value,
      cash,
      None,
      if flat then None else held.flatMap(field(_, "entryPx")).filter(_ > 0),
      if flat then None else held.flatMap(field(_, "liquidationPx")).filter(_ > 0),
      if flat then Some(BigDecimal(0))
      else quantity.flatMap(_ => held.flatMap(field(_, "unrealizedPnl")))
    )

  def marginCash(account: ujson.Value): Option[BigDecimal] =
    for
      equity <- Try(account("marginSummary")).toOption.flatMap(field(_, "accountValue"))
      positions <- Try(account("assetPositions").arr.toVector.map(_("position"))).toOption
      profits = positions.map(field(_, "unrealizedPnl"))
      if profits.forall(_.nonEmpty)
    yield equity - profits.flatten.sum

  def usdcCash(account: ujson.Value): Option[BigDecimal] =
    val balances = Try(
      account("balances").arr.toVector.filter(b =>
        b("token") == ujson.Num(0) && b("coin") == ujson.Str("USDC")
      )
    ).toOption
    balances.filter(_.size == 1).flatMap(_.headOption).flatMap(field(_, "total"))
