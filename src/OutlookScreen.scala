package desk

import java.time.Instant

object OutlookScreen:
  private def exact(value: Option[Double]): String =
    value
      .filter(_.isFinite)
      .fold("--")(n => BigDecimal(n).bigDecimal.stripTrailingZeros().toPlainString)
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
  private def band(value: HistoricalRate): String =
    value.band.fold("--")((lo, hi) => f"$lo%.0f-$hi%.0f%%")

  def rows(
      view: OutlookView,
      width: Int,
      axis: Option[Axis],
      height: Int,
      mode: ChartMode
  ): Vector[String] =
    val cellWidth = (width - 2) / 3
    val columns = view.columns.reverse.map { column =>
      val period = column.horizon.label match
        case "LONG" => "1d"
        case "MID"  => "4h"
        case _      => "1h"
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

  def details(view: OutlookView): String =
    val method = "Base counts past periods that touched the displayed percentage target distance. " +
      "Match uses periods with the same 20/50-average trend. Features use 50 closed candles. " +
      "Up means price above the 20 average above the 50 average. Down is the reverse. " +
      "Support is the lowest low and resistance the highest high. " +
      "S marks support, T target and R resistance on the graph price scale. " +
      "An up or down mark means the level is outside that scale. Combined letters share a tick. " +
      "A star means different prices share one tick, with exact prices below. Levels are rounded for display. " +
      "If a value cannot fit within 1% precision, -- is shown. Exact levels follow. " +
      "Periods do not overlap. At least 100 Base and 50 Match samples and 90% complete history are required. " +
      "Band is a rough 95% historical count interval. Market observations can remain related. " +
      "These are past target touches, not forecast confidence or profitable trades. " +
      "Test is the percentage distance from the last reference close, not the remaining live move. " +
      "A reached target or stale price suppresses the rates."
    method + "\n" + view.columns
      .map { c =>
        val start = c.from.map(t => Instant.ofEpochMilli(t).toString).getOrElse("unknown")
        val end = c.to.map(t => Instant.ofEpochMilli(t).toString).getOrElse("unknown")
        s"${c.horizon.label}: ${c.horizon.interval} candles, ${c.horizon.bars} future candles. " +
          s"Reference close ${exact(c.reference)}. Target ${exact(c.target)}. " +
          s"Support ${exact(c.support)}. Resistance ${exact(c.resistance)}. " +
          s"${c.base.samples}/${c.expected} complete periods. Base ${rate(c.base)} (${c.base.hits}/${c.base.samples}), " +
          s"band ${band(c.base)}. Match ${rate(c.matched)} (${c.matched.hits}/${c.matched.samples}), " +
          s"band ${band(c.matched)}. Observed $start to $end. ${c.status}."
      }
      .mkString("\n")
