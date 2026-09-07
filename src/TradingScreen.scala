package desk

object TradingScreen:
  private def money(value: BigDecimal): String =
    "$" + Trading.display(value.setScale(2, BigDecimal.RoundingMode.FLOOR), 7)

  private def side(value: Side): String = if value == Side.Buy then "buy" else "sell"

  private def position(value: Option[BigDecimal], width: Int): String = value match
    case None              => "unavailable"
    case Some(v) if v == 0 => "flat"
    case Some(v)           => (if v > 0 then "long " else "short ") + Trading.display(v.abs, width)

  private def fit(groups: Vector[Vector[String]], cols: Int): Vector[String] =
    val normal = groups.map(_.mkString(" "))
    if normal.forall(_.length <= cols) then normal.map(_.padTo(cols, ' '))
    else
      val split = groups.head.indexOf("limit $")
      val initial = Vector.tabulate(Layout.footerRows) { row =>
        if split < 0 then Vector.empty[(Int, String)]
        else if row == 0 then Vector(Int.MaxValue -> "limit $")
        else if row == 1 then Vector(0 -> groups.head(split + 1))
        else Vector.empty[(Int, String)]
      }
      val fields = groups.zipWithIndex
        .flatMap { (group, row) =>
          group.zipWithIndex.collect {
            case (text, index) if row != 0 || split < 0 || (index != split && index != split + 1) =>
              (row, text.stripPrefix("| "))
          }
        }
        .zipWithIndex
        .sortBy { case ((_, text), _) => -text.length }
      val placed = fields.foldLeft(Option(initial)) { case (previous, ((preferred, text), order)) =>
        previous.flatMap { rows =>
          rows.indices
            .map(i => (preferred + i) % rows.size)
            .find { row =>
              rows(row).map(_._2.length).sum + rows(row).size + text.length <= cols
            }
            .map(row => rows.updated(row, rows(row) :+ (order -> text)))
        }
      }
      placed
        .map(_.map(_.sortBy(_._1).map(_._2).mkString(" ").padTo(cols, ' ')))
        .filter(_.forall(_.length <= cols))
        .getOrElse(
          Vector(
            "Order details exceed this width. Order keys disabled. Widen the terminal."
              .padTo(cols, ' ')
          )
        )

  def rows(
      view: TradingView,
      target: Option[(Side, BigDecimal)],
      preview: OrderIntent => Either[String, OrderPlan],
      sizes: TradeSizes = TradeSizes(),
      cols: Int = Layout.minCols
  ): Vector[String] =
    def limitFields(value: BigDecimal): Vector[String] =
      val exact = Trading.displayExact(value)
      if exact.length + 7 <= cols then Vector("limit $" + exact) else Vector("limit $", exact)
    val width = if cols >= 120 then 8 else 6
    val state = if view.pending then "pending" else if view.ready then "ready" else "blocked"
    val current = view.block.getOrElse(state)
    val message = view.outcome.getOrElse(view.message)
    val status =
      Vector(current) ++ Option.when(message != "Ready" && message != current)("| " + message)
    val known =
      "position " + position(view.position, if target.isEmpty && !view.pending then 8 else width)
    val captured = view.lastPlan.filter(p => p.coin == view.coin && view.pending)
    val selected = target.map { (direction, limit) =>
      preview(OrderIntent(direction, OrderKind.Target, target = Some(limit))).flatMap { plan =>
        Either.cond(plan.coin == view.coin, plan, "Waiting for the selected market")
      }
    }
    val plan = selected.flatMap(_.toOption).orElse(if target.isEmpty then captured else None)
    val groups = if target.nonEmpty || view.pending then
      val heading = target
        .map { (direction, limit) =>
          Vector(s"Target ${view.coin} ${side(direction)}") ++ limitFields(limit) ++ Vector(
            "| Enter " + (if view.ready && !view.pending && selected.exists(_.isRight) then "sends"
                          else "disabled"),
            "Escape cancels"
          )
        }
        .orElse(
          captured.map(p => Vector(s"${p.kind} ${p.coin} ${side(p.side)}") ++ limitFields(p.limit))
        )
        .getOrElse(Vector(view.coin + " pending"))
      val details = plan
        .map { p =>
          Vector(
            "request " + Trading.display(p.quantity, width),
            "estimate " + Trading.display(p.estimatedQuantity, width),
            "value $" + Trading.display(p.estimatedValue, width),
            "average " + p.average
              .map(v => "$" + Trading.display(v, width))
              .getOrElse("unavailable")
          )
        }
        .getOrElse(selected.flatMap(_.left.toOption).toVector)
      val after = plan
        .filter(p => view.position.nonEmpty && p.estimatedQuantity > 0)
        .map { p =>
          "| " + (if target.nonEmpty then "target" else "press") + " estimate " + position(
            Some(p.positionAfter),
            width
          )
        }
        .toVector
      val disabled =
        if target.nonEmpty then "| 1-8 and left/right inactive" else "| 1-8 and 9/0 inactive"
      Vector(heading, details, Vector(known) ++ after :+ disabled, status)
    else
      def actions(start: Int): String =
        (start until start + 4)
          .map { index =>
            val intent = Trading.action(index)
            val amount = if start == 0 then money(if intent.big then sizes.big else sizes.small)
            else
              preview(intent).toOption
                .filter(p => p.coin == view.coin && p.estimatedQuantity > 0)
                .map(p => money(p.estimatedValue))
                .getOrElse("--")
            s"${index + 1} ${side(intent.side)} $amount".padTo(16, ' ')
          }
          .mkString
          .trim
      Vector(
        Vector("Hard " + actions(0), "| " + known),
        Vector("Best " + actions(4)),
        Vector("Target 9 buy 0 sell") ++
          Option.when(view.coin.length > Layout.marketLabelWidth)(view.coin),
        status
      )
    val fitted = fit(groups, cols)
    val (row, hint) = if target.nonEmpty then (1, " | up/down price")
    else (2, " | up/down zoom left/right history m market c chart q quit")
    val before = fitted.lift(row).getOrElse("").stripTrailing()
    if fitted.size == Layout.footerRows && before.length + hint.length <= cols then
      fitted.updated(row, (before + hint).padTo(cols, ' '))
    else fitted
