package desk

object TradingScreen:
  private def number(value: BigDecimal, width: Int = 8): String =
    val exact = Trading.format(value)
    if exact.length <= width then exact
    else
      (7 to 1 by -1).iterator
        .map { precision =>
          val rounded = value.bigDecimal
            .round(new java.math.MathContext(precision, java.math.RoundingMode.DOWN))
            .stripTrailingZeros()
            .toString
          "~" + rounded.replace("E+", "e").replace("E", "e")
        }
        .find(_.length <= width)
        .getOrElse("large")

  private def money(value: BigDecimal): String =
    "$" + number(value.setScale(2, BigDecimal.RoundingMode.FLOOR), 7)

  private def price(value: BigDecimal): String = "$" + number(value, 8)

  private def side(value: Side): String = if value == Side.Buy then "buy" else "sell"

  private def position(value: Option[BigDecimal]): String = value match
    case None              => "unavailable"
    case Some(v) if v == 0 => "flat"
    case Some(v) if v > 0  => "long " + number(v)
    case Some(v)           => "short " + number(-v)

  private def name(value: OrderKind): String = value match
    case OrderKind.Hard   => "Hard"
    case OrderKind.Best   => "Best"
    case OrderKind.Target => "Target"

  def rows(
      view: TradingView,
      target: Option[(Side, BigDecimal)],
      preview: OrderIntent => Either[String, OrderPlan],
      sizes: TradeSizes = TradeSizes()
  ): Vector[String] =
    val actions = Vector(
      OrderIntent(Side.Buy, OrderKind.Hard),
      OrderIntent(Side.Buy, OrderKind.Hard, big = true),
      OrderIntent(Side.Sell, OrderKind.Hard),
      OrderIntent(Side.Sell, OrderKind.Hard, big = true)
    )
    val hardActions = "Hard " + actions.zipWithIndex
      .map { (intent, i) =>
        s"${i + 1} ${side(intent.side)} ${money(if intent.big then sizes.big else sizes.small)}"
      }
      .mkString(" ")
    val bestActions = "Best " + actions.zipWithIndex
      .map { (intent, i) =>
        val amount = preview(intent.copy(kind = OrderKind.Best)).toOption
          .filter(p => p.coin == view.coin && p.estimatedQuantity > 0)
          .map(p => money(p.estimatedValue))
          .getOrElse("--")
        s"${i + 5} ${side(intent.side)} $amount"
      }
      .mkString(" ")
    val selected = target match
      case Some((direction, limit)) =>
        preview(OrderIntent(direction, OrderKind.Target, target = Some(limit))).toOption
          .filter(_.coin == view.coin)
      case None => view.lastPlan.filter(p => p.coin == view.coin && view.pending)
    val heading = target
      .map { (direction, limit) =>
        side(direction) + " " + price(limit)
      }
      .orElse(selected.map(p => name(p.kind) + " " + side(p.side)))
    val estimate = selected.filter(_.estimatedQuantity > 0)
    val amounts = estimate
      .map { p =>
        " value $" + number(p.estimatedValue, 6) + " average " +
          p.average.map(v => "$" + number(v, 8)).getOrElse("unavailable")
      }
      .getOrElse("")
    val detail = heading
      .map { title =>
        val quantities = selected
          .map(p =>
            " request " + number(p.quantity, 6) + " estimate " + number(p.estimatedQuantity, 6)
          )
          .getOrElse(" no estimated fill")
        title + quantities + amounts
      }
      .getOrElse("arrows set price enter sends escape cancels")
    val targetRow = "Target 9 buy 0 sell | " + detail
    val after = if view.position.isEmpty then ""
    else
      estimate
        .map { p =>
          val label = if target.nonEmpty then "target estimate " else "press estimate "
          " | " + label + position(Some(p.positionAfter))
        }
        .getOrElse("")
    val hard = hardActions + " | position " + position(view.position)
    val best = bestActions + after
    val state = if view.pending then "pending" else if view.ready then "ready" else "unavailable"
    val status = state + " | " + view.message
    Vector(hard, best, targetRow, status).map(_.take(100))
