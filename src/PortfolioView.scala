package desk

import scala.util.Try

final case class PortfolioView(
    tokenValue: Option[BigDecimal] = None,
    stables: Option[BigDecimal] = None,
    net: Option[BigDecimal] = None,
    entry: Option[BigDecimal] = None,
    liquidation: Option[BigDecimal] = None
):
  def markerRows(axis: Option[Axis], mode: ChartMode, width: Int, style: Style): Map[Int, String] =
    axis.fold(Map.empty[Int, String]) { a =>
      val marks = Vector("Entry" -> entry, "Liq" -> liquidation).flatMap { (name, value) =>
        value.filter(_ > 0).map { price =>
          val px = price.toDouble
          val row = if px > a.hi then 0
          else if px < a.lo then a.rows - 1
          else if mode == ChartMode.Line then a.dot(px) / 4
          else a.row(px)
          val pointer = if px > a.hi then "↑" else if px < a.lo then "↓" else ""
          (row, name, price, pointer)
        }
      }
      marks.groupBy(_._1).map { (row, here) =>
        val partWidth = (width - 1 - (here.size - 1) * 3) / here.size
        val label = here
          .map { (_, name, price, pointer) =>
            val prefix = pointer + name + " "
            prefix + Trading.display(price, partWidth - prefix.length)
          }
          .mkString(" / ")
        row -> style.wrap(
          if here.exists(_._2 == "Liq") then style.ask else style.bitcoin,
          Chart.fit(label, width - 1) + " "
        )
      }
    }

object PortfolioView:
  def paper(account: PaperAccount, coin: String, mark: Option[BigDecimal]): PortfolioView =
    val held = account.positions.get(coin)
    val price = mark.filter(_ > 0)
    val exposure = held.fold(Option(BigDecimal(0)))(p =>
      price.map(px => BigDecimal(px.bigDecimal.multiply(p.quantity.bigDecimal)))
    )
    val net = if account.positions.nonEmpty && account.positions.keySet != Set(coin) then None
    else
      exposure.map(value =>
        BigDecimal(
          account.cash.bigDecimal.add(value.bigDecimal).subtract(account.deposit.bigDecimal)
        )
      )
    PortfolioView(
      exposure,
      Some(account.cash),
      net,
      held.map(_.entry),
      None
    )

  private def decimal(value: ujson.Value): Option[BigDecimal] =
    Try(BigDecimal(value.str)).toOption.filter(_.abs <= BigDecimal("1e30"))
  private def field(value: ujson.Value, name: String): Option[BigDecimal] =
    value.objOpt.flatMap(_.get(name)).flatMap(decimal)

  def live(account: ujson.Value, coin: String, cash: Option[BigDecimal]): PortfolioView =
    val positions = Try(account("assetPositions").arr.toVector.map(_("position"))).toOption
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
      if flat then None else held.flatMap(field(_, "liquidationPx")).filter(_ > 0)
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
