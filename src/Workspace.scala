package desk

enum DeskAction:
  case Prepare(intent: OrderIntent)
  case Close
  case OrderSize(big: Boolean)
  case Cancel
  case Mode(name: String)
  case ConfirmMode(name: String)
  case OrderType
  case Orders
  case Place

enum OrderMode:
  case Market, Limit

final case class Ticket(
    coin: String,
    intent: OrderIntent,
    price: BigDecimal,
    step: BigDecimal,
    plan: Option[OrderPlan],
    notice: Option[String]
)

final case class OrderReview(
    intent: OrderIntent,
    plan: OrderPlan,
    terms: TradeTerms,
    createdAt: Long,
    expiresAt: Long,
    leverage: Option[Int] = None,
    marginMode: Option[String] = None
)
final case class PanelView(
    mode: String = "VIEW",
    terms: TradeTerms = TradeTerms(TradeSizes(), Trading.defaultSlippage),
    modeReview: Option[String] = None,
    outlook: OutlookView = Outlook.empty(""),
    muted: Boolean = false,
    actionNotice: Option[String] = None,
    actionNumber: Long = 0L,
    bigOrder: Boolean = false,
    // Labels show the key that presses each control.
    bindings: Bindings = Bindings.defaults,
    orderMode: OrderMode = OrderMode.Market,
    ticket: Option[Ticket] = None,
    focus: Option[Int] = None
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
    mode: String = "VIEW",
    outlook: Option[OutlookView] = None,
    disabledButtons: Vector[DeskButton] = Vector.empty,
    shortcuts: Set[DeskAction] = Set.empty,
    // The orders axis exactly as this frame drew it. Keys on the axis act on these rows.
    axis: Option[Placed] = None,
    // The axis cursor this frame drew, from the panel it drew.
    focus: Option[Int] = None,
    // The order type, the ticket, the size and the mode review this frame drew, from the panel
    // it drew.
    orderMode: OrderMode = OrderMode.Market,
    ticket: Option[Ticket] = None,
    bigOrder: Boolean = false,
    modeReview: Option[String] = None
):
  def controls: Vector[DeskButton] = buttons ++ disabledButtons

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

  def indicatorRows(s: Snapshot, now: Long, width: Int, style: Style): Vector[String] =
    val h = ((width - 24) / 2).max(1).min(20)
    def row(compact: String, seconds: Long, ink: String): String =
      val fitted = Chart.fit(compact, width)
      val marked = if style.colour && seconds * 1000 > now - s.since then
        fitted.take(5) + Screen.dim + fitted.slice(5, 8) + Screen.reset + ink + fitted.drop(8)
      else fitted
      style.wrap(ink, marked)
    val books = Gauges.bookLenses.indices.map { i =>
      row(
        Chart.plain(Gauges.bookRow(s, i, now, h, Style.mono, labelWidth = 3)),
        Gauges.bookLenses(i)._2,
        style.book
      )
    }
    val tape = Gauges.tapeLenses.indices.map { i =>
      val original = Chart.plain(Gauges.compactTapeRow(s, i, now, h, Style.mono))
      val compact = if original.length > width then original.replace(" n", "n") else original
      row(compact, Gauges.tapeLenses(i)._2, style.tape)
    }
    val correlations = State.refCoins.flatMap { coin =>
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

  private def mappedOrderStatus(raw: String, panel: PanelView, unsaved: Boolean): String =
    val lower = raw.toLowerCase
    if lower.contains("partial") then "Part filled"
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
    else if unsaved then safeText(raw)
    else if panel.mode == "VIEW" then "View only"
    else if raw.nonEmpty then safeText(raw).takeWhile(_ != '\n')
    else ""

  private def orderStatus(view: TradingView, panel: PanelView): String =
    val raw =
      if view.pending then view.message else view.block.orElse(panel.actionNotice).getOrElse("")
    val unsaved = raw.toLowerCase.matches("order result #[0-9]+ not saved")
    val lower = raw.toLowerCase
    val coded = if view.block.isEmpty || panel.mode == "VIEW" then None
    else
      view.blockCode match
        case BlockCode.Connection                  => Some("Reconnecting")
        case BlockCode.Prices                      => Some("Waiting for prices")
        case BlockCode.Account                     => Some("Account unavailable")
        case BlockCode.Storage                     => Some("Save failed")
        case BlockCode.Market | BlockCode.Leverage => Some(safeText(raw).takeWhile(_ != '\n'))
        // Dynamic action notices and uncoded router blocks retain their existing labels.
        case BlockCode.Blocked => None
    val text =
      if !view.pending && coded.nonEmpty then coded.get
      else if view.pending then
        val stage = "(?i).*\\b(queued|checking|saving) (\\d+s).*".r
        raw match
          case TradingWorker.ownerMismatchMessage | TradingWorker.savedOwnerMismatchMessage => raw
          case stage(name, age)                       => name.capitalize + " " + age
          case _ if lower.contains("result")          => "Confirming result"
          case _ if raw == s"${view.coin} submitting" => "Sending"
          case _                                      => mappedOrderStatus(raw, panel, unsaved)
      else if raw == TradingWorker.setupMessage then raw
      else mappedOrderStatus(raw, panel, unsaved)
    if text.nonEmpty && panel.actionNumber > 0 && !view.pending && !unsaved
    then s"$text #${panel.actionNumber}"
    else text

  def draw(
      s: Snapshot,
      view: TradingView,
      panel: PanelView,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      logsUnavailable: Boolean = false,
      orderResultsUnavailable: Boolean = false
  ): WorkspaceFrame =
    if view.coin != s.coin then
      return draw(
        s,
        view.forMarket(s.coin),
        panel,
        cols,
        rows,
        now,
        zone,
        style,
        logsUnavailable,
        orderResultsUnavailable
      )
    if cols < Layout.minCols || rows < Layout.minRows then
      WorkspaceFrame(
        Vector(s"need ${Layout.minCols}x${Layout.minRows}, have ${cols}x$rows"),
        Vector.empty,
        s.coin,
        cols,
        rows
      )
    else
      val leftWidth = Layout.workspaceMarketWidth(cols)
      val rightWidth = cols - leftWidth - 1
      val chartWidth = Layout.workspaceChartWidth(cols)
      val tradeTop = rows - 15
      val warnings = (if logsUnavailable then " !logs" else "") +
        (if orderResultsUnavailable then " !results" else "")
      val marketFrame = Screen.renderMarket(
        s,
        chartWidth,
        priceRows = (tradeTop - 8).min(
          tradeTop - 3 - Layout.referenceRows * State.refCoins.distinct.count(_ != s.coin)
        ),
        now = now,
        zone = zone,
        style = style,
        portfolio = view.portfolio,
        headerWidth = Some(chartWidth - warnings.length)
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
      def fits(column: Int, label: String): Boolean = column + label.length + 2 <= rightWidth
      def button(
          row: Int,
          column: Int,
          label: String,
          action: DeskAction,
          enabled: Boolean = true
      ): Unit =
        val text = "[" + label + "]"
        if fits(column, label) then
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
      else panel.outlook
      OutlookScreen
        .rows(outlook, Layout.outlookWidth, marketFrame.axis, marketFrame.priceRows, s.chartMode)
        .zipWithIndex
        .foreach((text, i) => line(i, text))
      def key(action: Bind): String = panel.bindings.map.get(action).fold("")(_.short)
      val live = panel.mode == "LIVE"
      val leverage = if live then view.leverage.fold("")(n => s" ${n}x") else ""
      val heading = panel.mode + " " + s.coin + leverage
      val direction = if live then
        view.portfolio.tokenValue.fold("")(value =>
          if value > 0 then " LONG" else if value < 0 then " SHORT" else " FLAT"
        )
      else ""
      val usable = if live then view.usableMargin else None
      val cashWidth = if usable.nonEmpty then (rightWidth - half) / 2 else rightWidth - half
      val cashHeading = "Stables" + usable.fold("")(_ => " " * (cashWidth - 7) + "Usable")
      val cashValues = amount(
        view.portfolio.stables,
        cashWidth - (if usable.nonEmpty then 1 else 0)
      ).padTo(cashWidth, ' ') +
        usable.fold("")(n => amount(Some(n), rightWidth - half - cashWidth))
      line(tradeTop, heading)
      line(tradeTop + 1, (s.coin + direction).take(half - 2).padTo(half, ' ') + cashHeading)
      line(
        tradeTop + 2,
        amount(view.portfolio.tokenValue, half - 1).padTo(half, ' ') + cashValues
      )
      line(
        tradeTop + 3,
        ("Open P&L " + amount(view.portfolio.openProfit, half - 10)).padTo(half, ' ') +
          "Net P&L " + amount(view.portfolio.net, rightWidth - half - 8)
      )
      val status = orderStatus(view, panel)
      line(tradeTop + 4, status)
      // The setup sentence names the key bound to settings the way the buttons name theirs, so
      // the text filter does not blank an Option character.
      if status.startsWith(TradingWorker.setupMessage) then
        right(tradeTop + 4) = TradingWorker.setupFor(key(Bind.Settings)) +
          safeText(status.drop(TradingWorker.setupMessage.length))
      val choosing =
        !view.pending && s.prompt.isEmpty && s.marketPending.isEmpty && panel.modeReview.isEmpty
      val small =
        "$" + Trading.format(panel.terms.sizes.small) + (if !panel.bigOrder then " *" else "")
      val big = "$" + Trading.format(panel.terms.sizes.big) + (if panel.bigOrder then " *" else "")
      button(tradeTop + 5, 0, small, DeskAction.OrderSize(false), choosing)
      button(tradeTop + 5, small.length + 3, big, DeskAction.OrderSize(true), choosing)
      val canTrade = choosing && view.ready && panel.mode != "VIEW" &&
        heading.length <= rightWidth
      // Live labels follow the position. The side facing an open position only reduces it.
      val held = if live then view.position.filter(_ != 0) else None
      val close = held.filter(_ => canTrade)
      val closeColumn = small.length + big.length + 6
      // A label too long for the row drops the coin, then the quantity, rather than the button.
      val closeLabel = close
        .fold(Vector.empty[String]) { p =>
          val quantity = Trading.format(p.abs)
          Vector(
            s"${key(Bind.Close)} Close $quantity ${s.coin}",
            s"${key(Bind.Close)} Close $quantity"
          )
        }
        .find(fits(closeColumn, _))
        .getOrElse(key(Bind.Close) + " Close")
      button(tradeTop + 5, closeColumn, closeLabel, DeskAction.Close, close.nonEmpty)
      val times = if live then view.leverage.fold("")(n => s" x$n") else ""
      Vector(Side.Buy, Side.Sell).zipWithIndex.foreach { (side, i) =>
        val intent = OrderIntent(side, OrderKind.Hard, big = panel.bigOrder)
        val shortcut = (side, panel.bigOrder) match
          case (Side.Buy, false) => key(Bind.LongSmall)
          case (Side.Buy, true)  => key(Bind.LongBig)
          case (_, false)        => key(Bind.ShortSmall)
          case (_, true)         => key(Bind.ShortBig)
        val size = if panel.bigOrder then panel.terms.sizes.big else panel.terms.sizes.small
        val reduces = held.exists(p => p.signum == (if side == Side.Buy then -1 else 1))
        val label = if !live then side.toString.toUpperCase
        else if reduces then "REDUCE"
        else if side == Side.Buy then "LONG"
        else "SHORT"
        val width = if i == 0 then half - 1 else rightWidth - half
        // A long key name gives way before the button does.
        val text = s"$label $$${Trading.format(size)}$times"
        tile(
          i * half,
          width,
          Vector(shortcut + " " + text, text).find(_.length <= width - 2).getOrElse(text),
          DeskAction.Prepare(intent),
          canTrade
        )
      }
      panel.modeReview match
        case Some(name) =>
          line(
            tradeTop + 12,
            if name == "LIVE" then "LIVE: one click sends real orders" else "Use prop trading?"
          )
          button(tradeTop + 13, 0, "Enter " + name, DeskAction.ConfirmMode(name), !view.pending)
          button(tradeTop + 13, 17, "Esc Cancel", DeskAction.Cancel)
        case None => ()
      val prop = key(Bind.Prop) + " Prop"
      val liveLabel = key(Bind.Live) + " Live"
      val liveColumn = (prop.length + 4).max(10)
      button(rows - 1, 0, prop, DeskAction.Mode("PROP"), choosing)
      button(rows - 1, liveColumn, liveLabel, DeskAction.Mode("LIVE"), choosing)
      right(rows - 1) = right(rows - 1).padTo((liveColumn + liveLabel.length + 4).max(20), ' ') +
        s"${key(Bind.Market)} Market ${key(Bind.Chart)} Chart ${key(Bind.Sound)} " +
        (if panel.muted then "Muted" else "Sound")
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
          else
            (active ++ inactive)
              .filter(b =>
                (b.height > 1 || b.action == DeskAction.Close) && i >= b.y - 1 &&
                  i < b.y - 1 + b.height
              )
              .sortBy(-_.x)
              .foreach { b =>
                val colour = if inactive.contains(b) then Screen.dim
                else
                  b.action match
                    case DeskAction.Prepare(intent) if intent.side == Side.Sell =>
                      style.ask + Screen.bold
                    case DeskAction.Close => style.ask + Screen.bold
                    case _                => style.bid + Screen.bold
                val start = b.x - leftWidth - 2
                content = content.take(start) + style.wrap(
                  colour,
                  content.slice(start, start + b.width)
                ) + content.drop(start + b.width)
              }
        val publicLine =
          if i == 0 && warnings.nonEmpty then left + style.wrap(style.ask, warnings)
          else left
        (if style.colour then publicLine else Chart.plain(publicLine)) + "|" + content
      }
      WorkspaceFrame(
        lines,
        active,
        s.coin,
        cols,
        rows,
        panel.mode,
        Some(outlook),
        disabledButtons = inactive,
        shortcuts = active.collect {
          case b if b.action.isInstanceOf[DeskAction.Prepare] || b.action == DeskAction.Close =>
            b.action
        }.toSet,
        // This frame draws the size choice and the mode review from the panel, so it records them.
        bigOrder = panel.bigOrder,
        modeReview = panel.modeReview
      )
