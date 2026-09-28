package desk

object OutlookScreen:
  private def number(value: Option[Double], limit: Int, precision: Int = 6): String =
    value.filter(_.isFinite).fold("--") { n =>
      val rounded = (precision to 1 by -1)
        .map(p => BigDecimal(n).bigDecimal.round(new java.math.MathContext(p)).stripTrailingZeros())
        .filter(r => n == 0 || math.abs(r.doubleValue / n - 1) < 0.01)
      rounded.iterator
        .map(_.toPlainString)
        .find(_.length <= limit)
        .orElse(
          rounded.iterator
            .map { r =>
              Vector(
                r.toString.replace("E+", "e").replace("E", "e"),
                s"${r.unscaledValue}e${-r.scale}"
              ).minBy(_.length)
            }
            .find(_.length <= limit)
        )
        .getOrElse("--")
    }
  private def rate(value: HistoricalRate): String =
    value.percent.fold("--")(p => f"$p%.0f%%")

  def rows(
      view: OutlookView,
      width: Int,
      axis: Option[Axis],
      height: Int,
      mode: ChartMode
  ): Vector[String] =
    val cellWidth = (width - 2) / 3
    val columns = view.columns.reverse.map { column =>
      val minutes = column.horizon.periodMs / 60000L
      val period =
        if minutes % 1440 == 0 then s"${minutes / 1440}d"
        else if minutes % 60 == 0 then s"${minutes / 60}h"
        else s"${minutes}m"
      val direction = column.direction match
        case OutlookDirection.Up    => "↑"
        case OutlookDirection.Down  => "↓"
        case OutlookDirection.Mixed => if column.status == "mixed" then "→" else "--"
      val levels = Vector("S" -> column.support, "T" -> column.target, "R" -> column.resistance)
        .flatMap((label, value) => value.filter(p => p > 0 && p.isFinite).map(label -> _))
      val marks = (0 until height).toVector.map { row =>
        axis match
          case None    => if row == height / 2 then "No prices" else ""
          case Some(a) =>
            val here = levels.filter { (_, price) =>
              val placed = if price > a.hi then 0
              else if price < a.lo then height - 1
              else if mode == ChartMode.Line then a.dot(price) / 4
              else a.row(price)
              placed == row
            }
            def mark(price: Double): String =
              if price > a.hi then "↑"
              else if price < a.lo then "↓"
              else if mode == ChartMode.Line then Vector("⠉", "⠒", "⠤", "⣀")(a.dot(price) % 4)
              else "─"
            val labels = Vector("─", "⠉", "⠒", "⠤", "⣀", "↑", "↓").flatMap { symbol =>
              val names = here.filter((_, price) => mark(price) == symbol).map(_._1).mkString
              Option.when(names.nonEmpty)(symbol + names)
            }
            if here.isEmpty then if levels.isEmpty && row == height / 2 then "No levels" else ""
            else if here.map(_._2).distinct.size == 1 then
              labels.head + " " + number(Some(here.head._2), cellWidth - labels.head.length - 1)
            else labels.mkString(" ") + (if labels.size == 1 then " *" else "")
      }
      val distance = for
        reference <- column.reference; target <- column.target
        if reference > 0
      yield 100 * (target / reference - 1)
      val condition = column.status match
        case "ready" =>
          distance.fold("Test --")(d => "Test " + number(Some(d), cellWidth - 6, 3) + "%")
        case "unavailable"       => "Waiting"
        case "limited history"   => "Thin samples"
        case "gapped history"    => "History gaps"
        case "waiting for price" => "Need price"
        case other               => other.capitalize
      Vector(column.horizon.label + " " + period + " " + direction) ++ marks ++ Vector(
        condition,
        "Base " + rate(column.base),
        "Match " + rate(column.matched),
        s"n ${column.base.samples}/${column.matched.samples}"
      )
    }
    (0 until height + 5).toVector.map { row =>
      columns.map(column => Chart.fit(column(row), cellWidth)).mkString("|")
    }
