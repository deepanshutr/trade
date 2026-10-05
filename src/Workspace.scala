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

  // Mono keeps no style but the inverse that marks the axis cursor, and the reset that ends it.
  private def mono(line: String): String =
    var inverse = false
    (Screen.esc + "\\[[0-9;]*m").r.replaceAllIn(
      line,
      m =>
        val keep = m.matched == Screen.inverse || inverse && m.matched == Screen.reset
        if m.matched == Screen.inverse then inverse = true
        else if m.matched == Screen.reset then inverse = false
        if keep then scala.util.matching.Regex.quoteReplacement(m.matched) else ""
    )

  // Short words for outcomes whose full text stays in the order result log.
  private[desk] val closeRefused = "Close refused. Press o for orders"
  private[desk] val canceling = "Canceling"
  private[desk] val cancelRefused = "Cancel refused"

  // Refusals that already say what to do. The status row shows them as they are.
  private val unchanged = Vector(
    "Takes liquidity. Move ",
    "Nothing to cancel at that price",
    "A limit order takes a sized press",
    Trading.restingCapText,
    TradingWorker.cancelPendingMessage,
    TradingWorker.closeNextMessage,
    "Read the trading key first. Press r"
  )

  // What a fill of the orders under the axis cursor would do.
  private def effect(text: String): Boolean =
    Set("opens", "reduce", "void").contains(text) || text.matches("flip .* if filled")

  private def mappedOrderStatus(raw: String, panel: PanelView, unsaved: Boolean): String =
    val lower = raw.toLowerCase
    // Desk's own outcomes keep the first dollar figure of their first line.
    def word(name: String): String =
      name + "\\$\\S+".r.findFirstIn(safeText(raw).takeWhile(_ != '\n')).fold("")(" " + _)
    if unchanged.exists(raw.startsWith) then safeText(raw).takeWhile(_ != '\n')
    // A refused CLOSE and a refused cancel name what happened, before the reject rule below.
    else if lower.contains("may block it, press o") then closeRefused
    else if lower.contains("cancel rejected") then cancelRefused
    else if (lower.contains("canceled $") || lower.contains("canceled by self-trade prevention") ||
        lower.contains("canceled by the venue")) && !lower.contains("before sending")
    then word("Canceled")
    else if lower.contains("partial") || lower.contains("part filled") then word("Part filled")
    else if lower.contains(" filled ") then
      if lower.contains(" buy ") then word("Bought")
      else if lower.contains(" sell ") then word("Sold")
      else word("Filled")
    else if lower.contains(" resting $") then word("Resting")
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
    else if panel.mode == "VIEW" then DeskController.viewOnly
    else if raw.nonEmpty then safeText(raw).takeWhile(_ != '\n')
    else ""

  private def orderStatus(view: TradingView, panel: PanelView): String =
    val raw =
      if view.pending then view.message else view.block.orElse(panel.actionNotice).getOrElse("")
    if effect(raw) then return if panel.focus.nonEmpty then raw else ""
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
          case _ if lower.contains(" canceling ")     => canceling
          case _                                      => mappedOrderStatus(raw, panel, unsaved)
      else if raw == TradingWorker.setupMessage then raw
      else mappedOrderStatus(raw, panel, unsaved)
    if text.nonEmpty && panel.actionNumber > 0 && !view.pending && !unsaved &&
      text != DeskController.viewOnly
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
      val regions = Layout.regions(cols, rows)
      val chartWidth = regions.left + regions.plot
      val width = regions.panel
      val tradeTop = rows - 12
      val warnings = (if logsUnavailable then " !logs" else "") +
        (if orderResultsUnavailable then " !results" else "")
      // The chart block fills every row above the band: the header, the price rows, the volume
      // row, three rows for each reference market and the time axis.
      val references = State.refCoins.distinct.count(_ != s.coin)
      val priceRows = tradeTop - 3 - Layout.referenceRows * references
      // View has no account, so its axis and plot carry no entry, order, ticket or fill.
      val trades = panel.mode != "VIEW"
      val held = if trades then view.position.filter(_ != 0) else None
      val entry = held.zip(view.portfolio.entry).map((size, price) => (price, size))
      val resting = if trades then view.resting.filter(_.coin == s.coin) else Vector.empty
      val ticket = panel.ticket.filter(t => trades && t.coin == s.coin)
      val current = s.book.flatMap(_.mid)
      def colour(side: Side): String = if side == Side.Buy then style.bid else style.ask
      // The ticket's size is the plan the controller holds from its preview, and nothing else.
      val plan = ticket.flatMap(_.plan)
      // The liquidation price, in the colour of the side that would close the position.
      val liquidation = for
        size <- held
        price <- view.portfolio.liquidation
      yield (if size > 0 then Side.Sell else Side.Buy, price)
      def place(
          lo: Double,
          hi: Double,
          count: Int,
          liquidated: Option[(Side, BigDecimal)] = None
      ): Placed =
        val ticketMark =
          ticket.map(t => (t.intent.side, t.price, plan.fold(BigDecimal(0))(_.quantity)))
        val placed = OrdersAxis.marks(
          current,
          entry,
          resting,
          ticketMark,
          view.leverage,
          lo,
          hi,
          count,
          liquidated
        )
        if plan.nonEmpty then placed
        else
          placed.copy(marks =
            placed.marks.map(m => if m.kind == AxisKind.Ticket then m.copy(amount = None) else m)
          )
      // With no rows every mark is counted, so this gives the marks above and below the price.
      // The liquidation is left out, so it never widens the range.
      val counted = place(0, 1, 0)
      // The axis exactly as the labels drew it. Keys on the axis act on these rows.
      var drawn = Option.empty[Placed]
      def labels(rules: MarketRules)(a: Axis): Vector[String] =
        val placed = place(a.lo, a.hi, a.rows, liquidation)
        drawn = Some(placed)
        val side = held.map(p => if p > 0 then Side.Buy else Side.Sell)
        OrdersAxis.lines(placed, panel.focus, rules, a.lo, a.hi, a.rows, view.stale, side, style)
      // Own fills at two seconds a cell for each second of zoom on the line, a quarter second on
      // candles, counted back from the right edge.
      val zoom = State.zooms(s.zoom).toLong
      val msPerCell = if s.chartMode == ChartMode.Candles then 250L * zoom else 2000L * zoom
      val fills = if trades then view.fills.filter(_.coin == s.coin) else Vector.empty
      def marks(a: Axis): Vector[PlotMark] =
        Chart.fillMarks(fills, a, now - s.offsetSec * 1000L, msPerCell, regions.plot, style)
      val decor = view.rules.fold(AxisDecor.plain) { rules =>
        AxisDecor(
          OrdersAxis.held(current, entry.map(_._1), resting, ticket.map(_.price)),
          labels(rules),
          marks,
          a => ticket.map(t => (a.trueRow(t.price.toDouble), colour(t.intent.side))),
          counted.above,
          counted.below
        )
      }
      val marketFrame = Screen.renderMarket(
        s,
        now,
        zone,
        style,
        regions.copy(priceRows = priceRows),
        decor,
        headerWidth = Some(chartWidth - warnings.length)
      )
      val gauges = indicatorRows(s, now, (regions.bandRight - 1) / 2, style)
        .map(Chart.fit(_, regions.bandRight))
      val outlook = if panel.outlook.coin.isEmpty then Outlook.empty(s.coin)
      else panel.outlook
      val outlookRows = OutlookScreen
        .rows(outlook, Layout.outlookWidth, marketFrame.axis, marketFrame.priceRows, s.chartMode)
        .map(safeText)
      // The panel: twelve rows of 38 columns, every field in a fixed place.
      val text = Array.fill(12)("")
      val paints = Array.fill(12)(Vector.empty[(Int, Int, String)])
      val buttons = Vector.newBuilder[DeskButton]
      val disabled = Vector.newBuilder[DeskButton]
      def put(row: Int, column: Int, value: String): Unit =
        text(row) = text(row).padTo(column, ' ') + value
      def paint(row: Int, column: Int, value: String, ink: String): Unit =
        if value.trim.nonEmpty then
          paints(row) = paints(row) :+ ((column, column + value.length, ink))
      def fits(column: Int, label: String): Boolean = column + label.length + 2 <= width
      def button(
          row: Int,
          column: Int,
          label: String,
          action: DeskAction,
          enabled: Boolean
      ): Unit =
        if fits(column, label) then
          put(row, column, "[" + label + "]")
          val b = DeskButton(label, action, column + 1, tradeTop + row + 1, label.length + 2)
          if enabled && s.marketPending.isEmpty then buttons += b else disabled += b
      def key(action: Bind): String = panel.bindings.map.get(action).fold("")(_.short)
      def keyed(action: Bind, label: String): String = (key(action) + " " + label).trim
      // Money at two decimals, right aligned in a field, shortened when it does not fit.
      def cash(value: Option[BigDecimal], field: Int = 0): String =
        val shown = value.fold("--") { v =>
          val exact = "$" + v.setScale(2, BigDecimal.RoundingMode.HALF_UP).bigDecimal.toPlainString
          if field == 0 || exact.length <= field then exact
          else "$" + Trading.display(v, field - 1)
        }
        " " * (field - shown.length).max(0) + shown
      val leverage = if trades then view.leverage else None
      // Row 1, the mode, the market, the leverage and the direction.
      val direction =
        if !trades then "--"
        else
          view.portfolio.tokenValue.fold("")(value =>
            if value > 0 then "LONG" else if value < 0 then "SHORT" else "FLAT"
          )
      val heading = Vector(panel.mode, s.coin, leverage.fold("")(n => s"${n}x"), direction)
        .filter(_.nonEmpty)
        .mkString(" ")
      put(0, 0, heading)
      paint(0, 0, panel.mode, Screen.bold)
      if direction == "LONG" || direction == "SHORT" then
        paint(
          0,
          heading.length - direction.length,
          direction,
          colour(
            if direction == "LONG" then Side.Buy else Side.Sell
          )
        )
      // Row 2, the position value, the open profit and the net profit.
      val portfolio = if trades then view.portfolio else PortfolioView()
      put(1, 0, "Val " + cash(portfolio.tokenValue, 8))
      Vector(
        (12, " Open ", portfolio.openProfit),
        (25, " Net ", portfolio.net)
      ).foreach { (column, label, value) =>
        val shown = cash(value, 7)
        put(1, column, label + shown)
        value.foreach(v =>
          paint(
            1,
            column + label.length + shown.indexWhere(_ != ' '),
            shown.trim,
            colour(
              if v >= 0 then Side.Buy else Side.Sell
            )
          )
        )
      }
      // Row 3, the cash and the usable margin, the prop balance in prop.
      val cashLabel = if panel.mode == "PROP" then "Prop" else "Stables"
      put(2, 0, cashLabel + " " + cash(portfolio.stables, 15 - cashLabel.length))
      put(2, 16, "  Usable " + cash(if trades then view.usableMargin else None, 8))
      // Row 4, the status, with the marks the axis left off.
      val status = orderStatus(view, panel)
      val shownStatus =
        if status.startsWith(TradingWorker.setupMessage) then
          TradingWorker.setupFor(key(Bind.Settings)) +
            safeText(status.drop(TradingWorker.setupMessage.length))
        else status
      val more = drawn.toVector
        .flatMap(p =>
          Vector(p.above -> "above", p.below -> "below").collect {
            case (n, side) if n > 0 => s"$n more $side"
          }
        )
        .mkString(" ")
      // A status too long for its row wraps at spaces. Rows 8 to 11 take the rest while no
      // ticket or mode review holds them.
      val first = if more.isEmpty then width else width - more.length - 1
      val wrapped = shownStatus.split(" ").foldLeft(Vector("")) { (lines, word) =>
        val limit = if lines.size == 1 then first else width
        if lines.last.isEmpty then lines.init :+ word
        else if lines.last.length + 1 + word.length <= limit then
          lines.init :+ (lines.last + " " + word)
        else lines :+ word
      }
      put(
        3,
        0,
        if more.isEmpty then wrapped.head
        else wrapped.head.take(first).padTo(width - more.length, ' ') + more
      )
      val choosing =
        !view.pending && s.prompt.isEmpty && s.marketPending.isEmpty && panel.modeReview.isEmpty
      val canTrade = choosing && view.ready && trades && heading.length <= width
      // Row 5, the sizes and CLOSE with the money the position puts up.
      val small =
        "$" + Trading.format(panel.terms.sizes.small) + (if !panel.bigOrder then " *" else "")
      val big = "$" + Trading.format(panel.terms.sizes.big) + (if panel.bigOrder then " *" else "")
      button(4, 0, small, DeskAction.OrderSize(false), choosing && trades)
      button(4, small.length + 3, big, DeskAction.OrderSize(true), choosing && trades)
      val closeColumn = small.length + big.length + 6
      val putUp = entry.flatMap((price, size) => Trading.money(size.abs, price, leverage))
      // An enabled CLOSE names the money the position puts up, shortened or dropped to fit.
      val closes = canTrade && held.nonEmpty
      val closeLabel = Vector(cash(putUp), Trading.dollars(putUp, 0))
        .filter(_ => closes)
        .map(money => keyed(Bind.Close, "Close " + money))
        .find(fits(closeColumn, _))
        .getOrElse(keyed(Bind.Close, "Close"))
      button(4, closeColumn, closeLabel, DeskAction.Close, closes)
      // Row 6, the order type, the axis focus, the sound and the orders on other markets.
      val orderType =
        keyed(Bind.OrderType, if panel.orderMode == OrderMode.Limit then "Limit" else "Market")
      val orders = keyed(Bind.Orders, "Orders")
      // The order type waits while a ticket is open, since the ticket is already a limit order.
      button(5, 0, orderType, DeskAction.OrderType, choosing && trades && ticket.isEmpty)
      button(5, orderType.length + 3, orders, DeskAction.Orders, trades && resting.nonEmpty)
      // A row too wide drops the count first and then the sound.
      val sound = keyed(Bind.Sound, if panel.muted then "Muted" else "Sound")
      val elsewhere = if trades then view.resting.filter(_.coin != s.coin) else Vector.empty
      val count = Option.when(elsewhere.nonEmpty)(
        s"${elsewhere.size} on ${elsewhere.map(_.coin).distinct.sorted.mkString(",")}"
      )
      if text(5).length + 1 + sound.length <= width then
        put(5, text(5).length + 1, sound)
        count
          .filter(text(5).length + 1 + _.length <= width)
          .foreach(c => put(5, width - c.length, c))
      // Row 7, LONG and SHORT or REDUCE. A row too wide drops the leverage, then the amount, then
      // the key.
      val size = if panel.bigOrder then panel.terms.sizes.big else panel.terms.sizes.small
      val times = leverage.fold("")(n => s" x$n")
      val trade = Vector(Side.Buy, Side.Sell).map { side =>
        val intent = OrderIntent(side, OrderKind.Hard, big = panel.bigOrder)
        val shortcut = (side, panel.bigOrder) match
          case (Side.Buy, false) => key(Bind.LongSmall)
          case (Side.Buy, true)  => key(Bind.LongBig)
          case (_, false)        => key(Bind.ShortSmall)
          case (_, true)         => key(Bind.ShortBig)
        val reduces = held.exists(p => p.signum == (if side == Side.Buy then -1 else 1))
        val word = if reduces then "REDUCE" else if side == Side.Buy then "LONG" else "SHORT"
        val amount = "$" + Trading.format(size)
        val labels = Vector(
          s"$shortcut $word $amount$times",
          s"$shortcut $word $amount",
          s"$shortcut $word",
          word
        ).map(_.trim)
        (labels, intent)
      }
      val level = (0 until 4)
        .find(i => trade.map(_._1(i).length + 2).sum + 1 <= width)
        .getOrElse(3)
      val second = trade.head._1(level).length + 3
      trade.zip(Vector(0, second)).foreach { case ((labels, intent), column) =>
        button(6, column, labels(level), DeskAction.Prepare(intent), canTrade && ticket.isEmpty)
      }
      // Rows 8 to 11, the ticket or the mode review.
      ticket match
        case Some(t) =>
          val side = t.intent.side
          val word = if side == Side.Buy then "BUY" else "SELL"
          val facing = held.exists(p => p.signum == (if side == Side.Buy then -1 else 1))
          val reduce = plan.fold(facing)(_.reduceOnly)
          put(7, 0, s"$word LIMIT ${s.coin} ${if reduce then "reduce only" else "post only"}")
          paint(7, 0, word, colour(side) + Screen.bold)
          def price(value: BigDecimal): String =
            view.rules.fold(Trading.format(value))(Trading.priceText(value, _))
          // The touch on the order's own side.
          val touch = if side == Side.Buy then view.bid else view.ask
          val priceField =
            s"Price ${price(t.price)}  ${if side == Side.Buy then "bid" else "ask"} " +
              touch.fold("--")(price)
          val ticks = for rules <- view.rules; at <- touch
          yield f"${Trading.ticks(t.price, at, rules)}%+dt"
          val percent =
            touch.filter(_ > 0).map(at => f"${((t.price / at - 1) * 100).toDouble}%+.2f%%")
          // A row too wide loses its last field first.
          def fitted(fields: Vector[String], gap: String): String =
            fields.inits
              .filter(_.nonEmpty)
              .map(_.mkString(gap))
              .find(_.length <= width)
              .getOrElse(fields.head)
          put(8, 0, fitted(Vector(priceField) ++ ticks ++ percent, " "))
          paint(8, 6, price(t.price), colour(side) + Screen.bold)
          // Row 10 is the plan's money, or the refusal its preview gave in place of a size.
          val refusal = t.notice.map(text => safeText(text).takeWhile(_ != '\n'))
          val sizeRow = plan.fold(refusal.getOrElse("Size --")) { p =>
            val money = Trading.money(p.quantity, t.price, leverage)
            val sizeField = "Size " + cash(money) +
              (if reduce then s" of ${cash(putUp)} held" else "")
            val fee = "fee " + cash(Some(p.quantity * t.price * Trading.baseMakerFee))
            fitted(Vector(sizeField, fee), "  ")
          }
          put(9, 0, sizeRow)
          button(10, 0, "Enter Place", DeskAction.Place, choosing && view.ready)
          button(10, 14, "Esc Cancel", DeskAction.Cancel, enabled = true)
          put(10, 27, "step " + Trading.format(t.step))
        case None =>
          panel.modeReview match
            case Some(name) =>
              put(
                7,
                0,
                if name == "LIVE" then "LIVE: one click sends real orders"
                else "Use prop trading?"
              )
              button(8, 0, "Enter " + name, DeskAction.ConfirmMode(name), !view.pending)
              button(8, name.length + 9, "Esc Cancel", DeskAction.Cancel, enabled = true)
            case None => wrapped.tail.take(4).zipWithIndex.foreach((line, i) => put(7 + i, 0, line))
      // Row 12, the mode keys.
      val prop = keyed(Bind.Prop, "Prop")
      val liveLabel = keyed(Bind.Live, "Live")
      button(11, 0, prop, DeskAction.Mode("PROP"), choosing)
      button(11, prop.length + 3, liveLabel, DeskAction.Mode("LIVE"), choosing)
      put(
        11,
        text(11).length + 1,
        s"${keyed(Bind.Market, "Market")} ${keyed(Bind.Chart, "Chart")}"
      )
      val active = buttons.result()
      val inactive = disabled.result()
      // The colours: bold for the mode word and the entry's and ticket's side, bold side colours
      // on enabled trade buttons and CLOSE, dim for every disabled control and view's trade rows.
      def coloured(row: Int): String =
        val plain = Chart.fit(text(row), width)
        if !style.colour then plain
        else if !trades && row >= 4 && row <= 6 then
          style.wrap(Screen.dim, text(row)) +
            " " * (width - text(row).length).max(0)
        else
          val controls = (active ++ inactive).filter(_.y == tradeTop + row + 1).map { b =>
            val ink = if inactive.contains(b) then Screen.dim
            else
              b.action match
                case DeskAction.Prepare(intent) => colour(intent.side) + Screen.bold
                case DeskAction.Close           => style.ask + Screen.bold
                case _                          => ""
            (b.x - 1, b.x - 1 + b.width, ink)
          }
          (controls ++ paints(row)).filter(_._3.nonEmpty).sortBy(-_._1).foldLeft(plain) {
            case (line, (start, end, ink)) =>
              line.take(start) + style.wrap(ink, line.slice(start, end)) + line.drop(end)
          }
      val lines = (0 until rows).toVector.map { i =>
        if i < tradeTop then
          val chart = marketFrame.lines.lift(i).getOrElse(" " * chartWidth)
          val publicLine =
            if i == 0 && warnings.nonEmpty then chart + style.wrap(style.ask, warnings) else chart
          var content = Chart.fit(outlookRows.lift(i).getOrElse(""), Layout.outlookWidth)
          if style.colour && i == 0 then
            content = content
              .replace("↑", style.wrap(style.bid, "↑"))
              .replace("↓", style.wrap(style.ask, "↓"))
          (if style.colour then publicLine else mono(publicLine)) + "|" + content
        else
          val row = i - tradeTop
          val gauge = gauges.lift(row).getOrElse(" " * regions.bandRight)
          coloured(row) + "|" + (if style.colour then gauge else mono(gauge))
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
          case b
              if b.action.isInstanceOf[DeskAction.Prepare] || b.action == DeskAction.Close ||
                b.action == DeskAction.OrderType || b.action == DeskAction.Orders ||
                b.action == DeskAction.Place =>
            b.action
        }.toSet,
        // This frame records what it drew: the axis, the cursor, the order type, the ticket, the
        // size choice and the mode review.
        axis = drawn,
        focus = panel.focus,
        orderMode = panel.orderMode,
        ticket = ticket,
        bigOrder = panel.bigOrder,
        modeReview = panel.modeReview
      )
