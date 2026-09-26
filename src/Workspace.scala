package desk

enum DeskAction:
  case Prepare(intent: OrderIntent)
  case OrderSize(big: Boolean)
  case Cancel
  case Mode(name: String)
  case ConfirmMode(name: String)

final case class OrderReview(
    intent: OrderIntent,
    plan: OrderPlan,
    createdAt: Long,
    expiresAt: Long,
    leverage: Option[Int] = None,
    marginMode: Option[String] = None
)
final case class DeskMessage(
    id: Long,
    at: Long,
    kind: String,
    text: String,
    feedback: Option[Boolean] = None
)
final case class PanelView(
    mode: String = "VIEW",
    account: String = "",
    dataStatus: String = "Public data collecting",
    messages: Vector[DeskMessage] = Vector.empty,
    sizes: TradeSizes = TradeSizes(),
    modeReview: Option[String] = None,
    outlook: OutlookView = Outlook.empty(""),
    muted: Boolean = false,
    actionNotice: Option[String] = None,
    historySavingAt: Option[Long] = None,
    historyFailure: Option[String] = None,
    actionNumber: Long = 0L,
    bigOrder: Boolean = false
)
final case class DeskButton(
    label: String,
    action: DeskAction,
    x: Int,
    y: Int,
    width: Int,
    height: Int = 1
):
  def contains(px: Int, py: Int): Boolean = px >= x && px < x + width && py >= y && py < y + height
final case class WorkspaceFrame(
    lines: Vector[String],
    buttons: Vector[DeskButton],
    coin: String,
    cols: Int,
    rows: Int,
    completeReview: Boolean,
    review: Option[OrderReview] = None,
    mode: String = "VIEW",
    outlook: Option[OutlookView] = None,
    scrollLimit: Int = 0,
    disabledButtons: Vector[DeskButton] = Vector.empty,
    shortcuts: Set[DeskAction] = Set.empty
):
  def controls: Vector[DeskButton] = buttons ++ disabledButtons
  def hit(
      x: Int,
      y: Int,
      actualCols: Int,
      actualRows: Int,
      actualCoin: String
  ): Option[DeskAction] =
    if actualCols != cols || actualRows != rows || actualCoin != coin then None
    else buttons.find(_.contains(x, y)).map(_.action)

object Workspace:
  def safeText(text: String): String =
    text
      .take(30000)
      .replaceAll("\\x1b\\[[0-?]*[ -/]*[@-~]", "")
      .flatMap {
        case '\u2212' | '\u2010' | '\u2011' | '\u2013' | '\u2014' | '\ufe63' | '\uff0d' => "-"
        case '\u2264'                                                                   => "<="
        case '\u2265'                                                                   => ">="
        case '\u2260'                                                                   => "!="
        case '\u00b1'                                                                   => "+/-"
        case '\u00d7'                                                                   => "x"
        case '\u00f7'                                                                   => "/"
        case '\u2018' | '\u2019'                                                        => "'"
        case '\u201c' | '\u201d'                                                        => "\""
        case '\u2026'                                                                   => "..."
        case '\u2191'                                                                   => "↑"
        case '\u2193'                                                                   => "↓"
        case '\u2192'                                                                   => "→"
        case '\u2500'                                                                   => "─"
        case c @ ('⠉' | '⠒' | '⠤' | '⣀')              => c.toString
        case c if c == '\n' || (c >= ' ' && c <= '~') => c.toString
        case _                                        => " "
      }
      .take(30000)

  def wrap(text: String, width: Int): Vector[String] =
    safeText(text).split("\n", -1).toVector.flatMap { line =>
      if line.isEmpty then Vector("") else line.grouped(width.max(1)).toVector
    }

  def indicatorRows(s: Snapshot, now: Long, width: Int, style: Style): Vector[String] =
    val h = ((width - 24) / 2).max(1).min(20)
    val books = Gauges.bookLenses.zipWithIndex.map { case ((label, _), i) =>
      val original = Chart.plain(Gauges.bookRow(s, i, now, h, Style.mono))
      val compact =
        original.replace("BOOK " + Chart.rjust(label, 4), "BOOK " + Chart.rjust(label, 3))
      val fitted = Chart.fit(compact, width)
      val marked = if style.colour && Gauges.bookLenses(i)._2 * 1000 > now - s.since then
        fitted.take(5) + Screen.dim + fitted.slice(5, 8) + Screen.reset + style.book + fitted.drop(
          8
        )
      else fitted
      style.wrap(style.book, marked)
    }
    val tape = Gauges.tapeLenses.indices.map { i =>
      val original = Chart.plain(Gauges.compactTapeRow(s, i, now, h, Style.mono))
      val compact = if original.length > width then original.replace(" n", "n") else original
      val fitted = Chart.fit(compact, width)
      val marked = if style.colour && Gauges.tapeLenses(i)._2 * 1000 > now - s.since then
        fitted.take(5) + Screen.dim + fitted.slice(5, 8) + Screen.reset + style.tape + fitted.drop(
          8
        )
      else fitted
      style.wrap(style.tape, marked)
    }
    val correlations = Vector("BTC", "PAXG").flatMap { coin =>
      Gauges.horizons.map { (label, interval, n) =>
        val r = if coin == s.coin then Comparison(None, 0, n, "same market")
        else
          Quant.compare(
            s.candles.getOrElse(interval, Vector.empty),
            s.refs.getOrElse(coin, Ref.empty).candles.getOrElse(interval, Vector.empty),
            interval,
            n,
            now
          )
        val name = if coin == "PAXG" then "GOLD" else "BTC"
        val count = s"${r.count}/$n"
        val half = ((width - name.length - label.length - count.length - 10) / 2).max(1).min(20)
        val rail = Gauges.track(r.value, half, "")
        val value = r.value.fold("   --")(v => f"$v%+.2f")
        val reading = if r.value.nonEmpty then s"$name $label $rail $value $count"
        else s"$name $label ${r.status} $count"
        style.wrap(if coin == "PAXG" then style.gold else style.bitcoin, Chart.fit(reading, width))
      }
    }
    (books ++ tape).zip(correlations).map((left, right) => left + " " + right).toVector

  private def amount(value: Option[BigDecimal], width: Int): String =
    value.fold("--")(n =>
      "$" + Trading.display(n.setScale(2, BigDecimal.RoundingMode.HALF_UP), width - 1)
    )

  private def orderStatus(view: TradingView, panel: PanelView): String =
    val raw = if view.pending then view.message
    else view.block.orElse(panel.actionNotice).getOrElse("")
    val lower = raw.toLowerCase
    val text =
      if view.pending then
        val stage = "(?i).*\\b(queued|checking|saving) (\\d+s).*".r
        raw match
          case stage(name, age)              => name.capitalize + " " + age
          case _ if lower.contains("result") => "Confirming result"
          case _                             => "Sending"
      else if lower.contains("partial") then "Part filled"
      else if lower.contains(" filled ") then
        if lower.contains(" buy ") then "Bought"
        else if lower.contains(" sell ") then "Sold"
        else "Filled"
      else if lower.contains("no fill") then "No fill"
      else if lower.contains("not enough") || lower.contains("funds") then "Insufficient funds"
      else if lower.contains("storage") || lower.contains("save failed") then "Save failed"
      else if lower.contains("unknown") || lower.contains("unresolved") then "Confirming result"
      else if lower.contains("disconnect") || lower.contains("reconnect") then "Reconnecting"
      else if lower.contains("reject") || lower.contains("not sent") || lower.contains("cancel")
      then "Not sent"
      else if lower.contains("account") || lower.contains("setup") || lower.contains("signer") then
        "Account unavailable"
      else if lower.contains("stale") || lower.contains("price") || lower.contains("depth") || lower
          .contains("quote")
      then "Waiting for prices"
      else if lower.contains("pending") then "Sending"
      else if panel.mode == "VIEW" then "View only"
      else if raw.nonEmpty then safeText(raw).takeWhile(_ != '\n')
      else ""
    if text.nonEmpty && panel.actionNumber > 0 && !view.pending then s"$text #${panel.actionNumber}"
    else text

  def draw(
      s: Snapshot,
      view: TradingView,
      panel: PanelView,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): WorkspaceFrame =
    if view.coin != s.coin then
      return draw(s, view.forMarket(s.coin), panel, cols, rows, now, zone, style)
    if cols < Layout.minCols || rows < Layout.minRows then
      WorkspaceFrame(
        Vector(s"need ${Layout.minCols}x${Layout.minRows}, have ${cols}x$rows"),
        Vector.empty,
        s.coin,
        cols,
        rows,
        false
      )
    else
      val leftWidth = Layout.workspaceMarketWidth(cols)
      val rightWidth = cols - leftWidth - 1
      val chartWidth = Layout.workspaceChartWidth(cols)
      val tradeTop = rows - 15
      val marketFrame = Screen.renderMarket(
        s,
        chartWidth,
        rows,
        now,
        zone,
        style,
        marketOnly = true,
        reservedRows = 12,
        maxPriceRows = (tradeTop - 8).min(
          tradeTop - 3 - Layout.referenceRows * s.referenceCoins.distinct.count(_ != s.coin)
        ),
        portfolio = view.forMarket(s.coin).portfolio
      )
      val gauges = indicatorRows(s, now, (leftWidth - 1) / 2, style).map(Chart.fit(_, leftWidth))
      val market = marketFrame.lines.padTo(tradeTop, " " * chartWidth) ++
        Vector.fill(3)(" " * leftWidth) ++ gauges
      val right = Array.fill(rows)("")
      val buttons = Vector.newBuilder[DeskButton]
      val disabled = Vector.newBuilder[DeskButton]
      val half = rightWidth / 2
      def line(row: Int, text: String): Unit =
        if row >= 0 && row < rows then right(row) = safeText(text)
      def button(
          row: Int,
          column: Int,
          label: String,
          action: DeskAction,
          enabled: Boolean = true
      ): Unit =
        val text = "[" + label + "]"
        if column + text.length <= rightWidth then
          right(row) = right(row).padTo(column, ' ') + text
          val b = DeskButton(label, action, leftWidth + 2 + column, row + 1, text.length)
          if enabled && s.marketPending.isEmpty then buttons += b else disabled += b
      def tile(column: Int, width: Int, label: String, action: DeskAction, enabled: Boolean): Unit =
        if label.length <= width - 2 then
          val border = "+" + "-" * (width - 2) + "+"
          val empty = "|" + " " * (width - 2) + "|"
          val body =
            "|" + (" " * ((width - 2 - label.length) / 2) + label).padTo(width - 2, ' ') + "|"
          Vector(border, empty, body, empty, border).zipWithIndex.foreach { (text, i) =>
            val row = tradeTop + 7 + i
            right(row) = right(row).padTo(column, ' ') + text
          }
          val b = DeskButton(label, action, leftWidth + 2 + column, tradeTop + 8, width, 5)
          if enabled then buttons += b else disabled += b
      val outlook = if panel.outlook.coin.isEmpty then Outlook.empty(s.coin)
      else Outlook.visible(panel.outlook, s, now)
      OutlookScreen
        .rows(outlook, Layout.outlookWidth, marketFrame.axis, marketFrame.priceRows, s.chartMode)
        .zipWithIndex
        .foreach((text, i) => line(i, text))
      val live = panel.mode == "LIVE"
      val leverage = if live then view.leverage.fold("")(n => s" ${n}x") else ""
      val heading = panel.mode + " " + s.coin + leverage
      val direction = if live then
        view.portfolio.tokenValue.fold("")(value =>
          if value > 0 then " LONG" else if value < 0 then " SHORT" else " FLAT"
        )
      else ""
      line(tradeTop, heading)
      line(tradeTop + 1, (s.coin + direction).take(half - 2).padTo(half, ' ') + "Stables")
      line(
        tradeTop + 2,
        amount(view.portfolio.tokenValue, half - 1).padTo(half, ' ') + amount(
          view.portfolio.stables,
          rightWidth - half
        )
      )
      line(
        tradeTop + 3,
        ("Open P&L " + amount(view.portfolio.openProfit, half - 10)).padTo(half, ' ') +
          "Net P&L " + amount(view.portfolio.net, rightWidth - half - 8)
      )
      line(tradeTop + 4, orderStatus(view, panel))
      val choosing =
        !view.pending && s.prompt.isEmpty && s.marketPending.isEmpty && panel.modeReview.isEmpty
      val small = "$" + Trading.format(panel.sizes.small) + (if !panel.bigOrder then " *" else "")
      val big = "$" + Trading.format(panel.sizes.big) + (if panel.bigOrder then " *" else "")
      button(tradeTop + 5, 0, small, DeskAction.OrderSize(false), choosing)
      button(tradeTop + 5, small.length + 3, big, DeskAction.OrderSize(true), choosing)
      val canTrade = choosing && view.ready && view.coin == s.coin && panel.mode != "VIEW" &&
        heading.length <= rightWidth
      Vector(Side.Buy, Side.Sell).zipWithIndex.foreach { (side, i) =>
        val intent = OrderIntent(side, OrderKind.Hard, big = panel.bigOrder)
        val key = (if side == Side.Buy then 1 else 3) + (if panel.bigOrder then 1 else 0)
        val size = if panel.bigOrder then panel.sizes.big else panel.sizes.small
        val label = if live then (if side == Side.Buy then "BUY/LONG" else "SELL/SHORT")
        else side.toString.toUpperCase
        tile(
          i * half,
          if i == 0 then half - 1 else rightWidth - half,
          s"$key $label $$${Trading.format(size)}",
          DeskAction.Prepare(intent),
          canTrade
        )
      }
      panel.modeReview match
        case Some(name) =>
          line(
            tradeTop + 12,
            if name == "LIVE" then "LIVE: one click sends real orders" else "Use simulated trading?"
          )
          button(tradeTop + 13, 0, "Enter " + name, DeskAction.ConfirmMode(name), !view.pending)
          button(tradeTop + 13, 17, "Esc Cancel", DeskAction.Cancel)
        case None => ()
      button(rows - 1, 0, "p Paper", DeskAction.Mode("PAPER"), choosing)
      button(rows - 1, 10, "l Live", DeskAction.Mode("LIVE"), choosing)
      right(rows - 1) =
        right(rows - 1).padTo(20, ' ') + "m Market c Chart v " + (if panel.muted then "Muted"
                                                                  else "Sound")
      val active = buttons.result()
      val inactive = disabled.result()
      val lines = market.zipWithIndex.map { (left, i) =>
        var content = Chart.fit(right(i), if i < tradeTop then Layout.outlookWidth else rightWidth)
        if style.colour then
          if i == 0 then
            content = content
              .replace("↑", style.wrap(style.bid, "↑"))
              .replace("↓", style.wrap(style.ask, "↓"))
          else if i == tradeTop + 2 then content = style.wrap(Screen.bold, content)
          else if i == tradeTop + 3 then
            def profitStyle(value: Option[BigDecimal]): String =
              Screen.bold + value.fold("")(p => if p >= 0 then style.bid else style.ask)
            content = style.wrap(profitStyle(view.portfolio.openProfit), content.take(half)) +
              style.wrap(profitStyle(view.portfolio.net), content.drop(half))
          else if i >= tradeTop + 7 && i <= tradeTop + 11 then
            (active ++ inactive).filter(_.height > 1).sortBy(-_.x).foreach { b =>
              val colour = if inactive.contains(b) then Screen.dim
              else
                b.action match
                  case DeskAction.Prepare(intent) if intent.side == Side.Sell =>
                    style.ask + Screen.bold
                  case _ => style.bid + Screen.bold
              val start = b.x - leftWidth - 2
              content = content.take(start) + style.wrap(
                colour,
                content.slice(start, start + b.width)
              ) + content.drop(start + b.width)
            }
        val warnings = (if panel.dataStatus.contains("Logs unavailable") then " !logs" else "") +
          (if panel.historyFailure.nonEmpty then " !save" else "")
        val publicLine = if i == 0 && warnings.nonEmpty then
          Chart
            .fit(left, Chart.plain(left).length - warnings.length) + style.wrap(style.ask, warnings)
        else left
        (if style.colour then publicLine else Chart.plain(publicLine)) + "|" + content
      }
      WorkspaceFrame(
        lines,
        active,
        s.coin,
        cols,
        rows,
        false,
        None,
        panel.mode,
        Some(outlook),
        disabledButtons = inactive,
        shortcuts = active.collect {
          case b if b.action.isInstanceOf[DeskAction.Prepare] => b.action
        }.toSet
      )
