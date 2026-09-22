package desk

enum DeskAction:
  case Prepare(intent: OrderIntent)
  case Target(side: Side)
  case OrderType(kind: OrderKind)
  case OrderSize(big: Boolean)
  case Submit, Refresh, Cancel, Ask, SendQuestion, Status, Details, ToggleMute
  case Feedback(id: Long, accepted: Boolean)
  case Scroll(lines: Int)
  case Mode(name: String)
  case ConfirmMode(name: String)
  case Page(value: DetailPage)

enum DetailPage:
  case Method, Sources, Answers

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
    analysisStatus: String = "Analysis starting",
    review: Option[OrderReview] = None,
    messages: Vector[DeskMessage] = Vector.empty,
    input: Option[String] = None,
    scroll: Int = 0,
    target: Option[(Side, BigDecimal)] = None,
    sizes: TradeSizes = TradeSizes(),
    fee: BigDecimal = BigDecimal("0.0005"),
    modeReview: Option[String] = None,
    details: Boolean = false,
    outlook: OutlookView = Outlook.empty(""),
    alerts: Vector[Announcement] = Vector.empty,
    muted: Boolean = false,
    soundStatus: String = "Sound on",
    newsStatus: String = "News waiting",
    newsDetails: String = "News waiting",
    detailPage: DetailPage = DetailPage.Method,
    actionNotice: Option[String] = None,
    historySavingAt: Option[Long] = None,
    historyFailure: Option[String] = None,
    actionNumber: Long = 0L,
    orderKind: OrderKind = OrderKind.Hard,
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
      val correlationWidth = (leftWidth - 28).min(31)
      val bookWidth = leftWidth - correlationWidth - 1
      val h = ((bookWidth - 27) / 2).max(1).min(20)
      val referenceCoins = Vector("BTC", "PAXG")
      val correlation = referenceCoins.flatMap { coin =>
        Gauges.horizons.zipWithIndex.map { case ((label, interval, n), index) =>
          val r = if coin == s.coin then Comparison(None, 0, n, "same market")
          else
            Quant.compare(
              s.candles.getOrElse(interval, Vector.empty),
              s.refs.getOrElse(coin, Ref.empty).candles.getOrElse(interval, Vector.empty),
              interval,
              n,
              now
            )
          val prefix = if index == 0 then (if coin == "PAXG" then "gold" else "BTC") else ""
          val reading =
            r.value
              .map { v =>
                val rail = Gauges.track(
                  Some(v),
                  ((correlationWidth - 22) / 2).max(1),
                  if v >= 0 then style.bid else style.ask
                )
                rail + f" $v%+.2f ${r.count}/$n"
              }
              .getOrElse(s"${r.status} ${r.count}/$n")
          Chart.fit(s"$prefix $label $reading".trim, correlationWidth)
        }
      }
      val indicators = (0 until 12).toVector.map { i =>
        val reading = if i < 6 then Gauges.bookRow(s, i, now, h, style)
        else if bookWidth < 30 then Gauges.compactTapeRow(s, i - 6, now, h, style)
        else Gauges.tapeRow(s, i - 6, now, h, style)
        Chart.fit(reading, bookWidth) + " " + correlation(i)
      }
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
        )
      )
      val market = marketFrame.lines.padTo(tradeTop, " " * chartWidth) ++ Vector.fill(3)(
        " " * leftWidth
      ) ++ indicators
      val right = Array.fill(rows)("")
      var scrollLimit = 0
      val buttons = Vector.newBuilder[DeskButton]
      val disabledButtons = Vector.newBuilder[DeskButton]
      var shortcuts = Set.empty[DeskAction]
      def line(row: Int, text: String): Unit =
        if row >= 0 && row < rows then right(row) = safeText(text)
      def button(
          row: Int,
          column: Int,
          label: String,
          action: DeskAction,
          enabled: Boolean = true
      ): Unit =
        val text = if enabled then "[" + label + "]" else "(" + label + ")"
        val width = if row < tradeTop then Layout.outlookWidth else rightWidth
        val origin = if row < tradeTop then chartWidth else leftWidth
        if row < rows && column + text.length <= width then
          right(row) = right(row).padTo(column, ' ') + text
          val control = DeskButton(label, action, origin + 2 + column, row + 1, text.length)
          if enabled then buttons += control else disabledButtons += control
      def tradeLine(row: Int, text: String): Unit =
        line(tradeTop + row + (if row >= 3 then 1 else 0), text)
      def tradeButton(
          row: Int,
          column: Int,
          label: String,
          action: DeskAction,
          enabled: Boolean = true
      ): Unit = button(tradeTop + row + (if row >= 3 then 1 else 0), column, label, action, enabled)
      def tile(
          row: Int,
          column: Int,
          width: Int,
          label: String,
          action: DeskAction,
          enabled: Boolean
      ): Boolean =
        val shown = if enabled then label else "(" + label + ")"
        if shown.length > width - 2 || column + width > rightWidth || row + 3 > rows - 3 then false
        else
          val left = (width - 2 - shown.length) / 2
          val border = "+" + "-" * (width - 2) + "+"
          val middle = "|" + (" " * left + shown).padTo(width - 2, ' ') + "|"
          Vector(border, middle, border).zipWithIndex.foreach { (text, i) =>
            right(row + i) = right(row + i).padTo(column, ' ') + text
          }
          val control = DeskButton(label, action, leftWidth + 2 + column, row + 1, width, 3)
          if enabled then buttons += control else disabledButtons += control
          true
      tradeLine(
        0,
        s"${panel.mode} ${s.coin}" + (if panel.mode == "LIVE" then " " + panel.account else "")
      )
      if panel.review.isEmpty then
        tradeButton(1, 0, "p Paper", DeskAction.Mode("PAPER"), !view.pending && panel.input.isEmpty)
        tradeButton(1, 12, "l Live", DeskAction.Mode("LIVE"), !view.pending && panel.input.isEmpty)
      tradeLine(
        2,
        view.position.fold("Position unavailable")(p =>
          "Position " + Trading.display(p, rightWidth - 9)
        )
      )
      val tradeStatus = if view.pending then view.message
      else
        (if panel.review.nonEmpty then panel.actionNotice else None)
          .orElse(view.block)
          .getOrElse(view.message)
      val acknowledgement = if !view.pending && panel.actionNotice.nonEmpty then
        s"Action ${panel.actionNumber}: "
      else ""
      line(tradeTop + 3, acknowledgement + tradeStatus.replace('\n', ' '))
      val canTrade =
        view.ready && !view.pending && panel.input.isEmpty && s.prompt.isEmpty && panel.mode != "VIEW"
      var complete = false
      panel.modeReview match
        case Some(name) =>
          tradeLine(3, "Switch to " + name + "?")
          tradeLine(
            4,
            if name == "PAPER" then "Uses the separate simulated account."
            else "Uses the configured real account."
          )
          tradeLine(5, "Existing positions remain in their account.")
          tradeLine(6, "Each order requires its own review and Send.")
          tradeButton(8, 0, "Enter Select " + name, DeskAction.ConfirmMode(name), !view.pending)
          tradeButton(9, 0, "Esc Cancel", DeskAction.Cancel)
        case None => ()
      (if panel.modeReview.nonEmpty then None else panel.review) match
        case Some(review) =>
          val p = review.plan
          val knownSettings = panel.mode != "LIVE" ||
            (review.leverage.exists(n =>
              n > 0 && view.rules.exists(r => r.maxLeverage > 0 && n <= r.maxLeverage)
            ) &&
              review.marginMode.exists(m => m == "cross" || m == "isolated"))
          val settings = review.leverage.fold(if panel.mode == "PAPER" then "1x paper"
          else "Settings unavailable")(n => s"${n}x ${review.marginMode.getOrElse("unavailable")}")
          val details = Vector(
            s"${panel.mode} ${p.coin} ${p.side.toString.toLowerCase} ${p.kind}",
            "Quantity " + Trading.displayExact(p.quantity),
            "Limit " + Trading.displayExact(p.limit),
            "Max value $" + Trading.displayExact(p.quantity * p.limit),
            if panel.mode == "PAPER" then
              "Estimated fee $" + Trading.display(p.estimatedValue * panel.fee)
            else "Fee unknown. Value excludes fees",
            settings + (if panel.mode == "LIVE" then " " + panel.account
                        else
                          " captured " + java.time.Instant
                            .ofEpochMilli(review.createdAt)
                            .atZone(zone)
                            .toLocalTime
                            .toString
                            .take(8)),
            "Expires in " + ((BigInt(review.expiresAt) - now).max(BigInt(0)) / 1000) + "s",
            view.position.fold("Position unavailable")(p =>
              "Position now " + Trading.displayExact(p)
            )
          )
          complete = knownSettings && details.forall(
            _.length <= rightWidth
          ) && p.coin == s.coin && review.createdAt <= now && now < review.expiresAt
          details.zipWithIndex.foreach((text, i) => line(tradeTop + i, text))
          line(tradeTop + 8, acknowledgement + tradeStatus.replace('\n', ' '))
          val half = rightWidth / 2
          if complete then
            val _ = tile(
              tradeTop + 9,
              0,
              half - 1,
              "Enter Send " + panel.mode,
              DeskAction.Submit,
              canTrade
            )
          else if p.coin != s.coin then
            line(tradeTop + 8, "Market changed. Cancel and review again")
          else if !knownSettings then
            line(tradeTop + 8, "Account settings missing. Review when ready")
          else if review.createdAt > now || now >= review.expiresAt then
            line(tradeTop + 8, "Review expired. Enter reviews again")
            val _ =
              tile(tradeTop + 9, 0, half - 1, "Enter Review again", DeskAction.Refresh, canTrade)
          else line(tradeTop + 8, "Review does not fit. Widen the window")
          val _ = tile(tradeTop + 9, half, rightWidth - half, "Esc Cancel", DeskAction.Cancel, true)
        case None if panel.modeReview.isEmpty =>
          val half = rightWidth / 2
          def targetReady(side: Side): Boolean =
            !view.pending && panel.input.isEmpty && view.rules.exists(r =>
              view.reference(side).exists(Trading.validPrice(_, r))
            )
          val kind = if panel.target.nonEmpty then OrderKind.Target else panel.orderKind
          val choosing = !view.pending && panel.input.isEmpty && s.prompt.isEmpty
          Vector(OrderKind.Hard, OrderKind.Best, OrderKind.Target).zipWithIndex.foreach {
            (value, i) =>
              tradeButton(
                3,
                i * 10,
                value.toString + (if value == kind then " *" else ""),
                DeskAction.OrderType(value),
                choosing
              )
          }
          if kind == OrderKind.Target then
            tradeLine(4, "Size $" + Trading.format(panel.sizes.target))
          else
            val small =
              "$" + Trading.format(panel.sizes.small) + (if !panel.bigOrder then " *" else "")
            tradeButton(4, 0, small, DeskAction.OrderSize(false), choosing)
            tradeButton(
              4,
              small.length + 3,
              "$" + Trading.format(panel.sizes.big) + (if panel.bigOrder then " *" else ""),
              DeskAction.OrderSize(true),
              choosing
            )
          val primary = Vector(Side.Buy, Side.Sell).zipWithIndex.map { (side, i) =>
            val intent = OrderIntent(side, kind, big = panel.bigOrder)
            val index = (if kind == OrderKind.Best then 4
                         else
                           0
            ) + (if side == Side.Sell then 2 else 0) + (if panel.bigOrder then 1 else 0) + 1
            val amount = if panel.bigOrder then panel.sizes.big else panel.sizes.small
            val label = if kind == OrderKind.Target then
              (if side == Side.Buy then "9 BUY target" else "0 SELL target"
            )
            else s"$index ${side.toString.toUpperCase} $$${Trading.format(amount)}"
            val action = if kind == OrderKind.Target then DeskAction.Target(side)
            else DeskAction.Prepare(intent)
            tile(
              tradeTop + 6,
              i * half,
              if i == 0 then half - 1 else rightWidth - half,
              label,
              action,
              if kind == OrderKind.Target then targetReady(side) else canTrade
            )
          }
          if primary.forall(identity) && panel.target.isEmpty && choosing then
            shortcuts = (0 until 8).map(i => DeskAction.Prepare(Trading.action(i))).toSet ++
              Set(DeskAction.Target(Side.Buy), DeskAction.Target(Side.Sell))
          if !primary.forall(identity) then tradeLine(8, "Amount does not fit. Widen the window")
          panel.target.foreach { (side, price) =>
            tradeLine(
              8,
              s"Target ${side.toString.toLowerCase} ${Trading.displayExact(price)} Up/Down"
            )
            tradeButton(
              9,
              0,
              "Enter Review",
              DeskAction.Prepare(OrderIntent(side, OrderKind.Target, target = Some(price))),
              canTrade && Trading.displayExact(price).length + 20 <= rightWidth
            )
            tradeButton(9, 17, "Esc Cancel", DeskAction.Cancel)
          }
          if panel.target.isEmpty then
            panel.actionNotice.foreach { text =>
              val notice = wrap(text, rightWidth)
              notice
                .take(2)
                .zipWithIndex
                .foreach((part, index) =>
                  tradeLine(
                    9 + index,
                    if index == 1 && notice.size > 2 then
                      part.take(rightWidth - 13) + "... h Details"
                    else part
                  )
                )
            }
          else
            panel.actionNotice.foreach { text =>
              val notice = "! " + safeText(text).replace('\n', ' ')
              tradeLine(
                10,
                if notice.length <= rightWidth then notice
                else notice.take(rightWidth - 13) + "... h Details"
              )
            }
        case _ => ()
      val outlook = if panel.outlook.coin.isEmpty then Outlook.empty(s.coin)
      else Outlook.visible(panel.outlook, s, now)
      if panel.details || panel.input.nonEmpty then
        line(0, "DETAILS  " + panel.detailPage)
        button(1, 0, "Method", DeskAction.Page(DetailPage.Method), panel.input.isEmpty)
        button(1, 9, "Sources", DeskAction.Page(DetailPage.Sources), panel.input.isEmpty)
        button(1, 19, "Answers", DeskAction.Page(DetailPage.Answers), panel.input.isEmpty)
        button(2, 0, "Up", DeskAction.Scroll(5))
        button(2, 5, "Down", DeskAction.Scroll(-5))
        line(2, right(2).padTo(13, ' ') + "[ ] scroll | Esc close")
        val alerts = panel.alerts.reverse.zipWithIndex.map { (alert, index) =>
          val dates =
            alert.publishedAt.fold("")(t => "\nPublished " + java.time.Instant.ofEpochMilli(t)) +
              alert.retrievedAt.fold("")(t => "\nRetrieved " + java.time.Instant.ofEpochMilli(t))
          DeskMessage(
            -2L - index,
            alert.at,
            "alert",
            alert.text + dates + alert.url.fold("")(u => "\n" + u)
          )
        }
        val history = panel.detailPage match
          case DetailPage.Method =>
            Vector(DeskMessage(-1L, now, "method", OutlookScreen.details(outlook)))
          case DetailPage.Sources =>
            Vector(DeskMessage(-1L, now, "sources", panel.newsDetails)) ++ alerts
          case DetailPage.Answers => panel.messages
        val messages = history.flatMap { m =>
          val time = java.time.Instant.ofEpochMilli(m.at).atZone(zone).toLocalTime.toString.take(8)
          val label = if m.kind == "analysis" then s"analysis #${m.id}" else m.kind
          wrap(s"$time $label: ${m.text}", Layout.outlookWidth).map(text => (Some(m.id), text)) :+ (
            None,
            ""
          )
        }
        val firstRow = if panel.detailPage == DetailPage.Answers then 5 else 3
        val messageRows = tradeTop - firstRow
        scrollLimit = (messages.size - messageRows).max(0)
        val scroll = panel.scroll.max(0).min(scrollLimit)
        val begin = if panel.detailPage == DetailPage.Answers then
          (messages.size - scroll - messageRows).max(0)
        else scroll
        val visibleMessages = messages.slice(begin, begin + messageRows)
        val visibleIds = visibleMessages.flatMap(_._1).toSet
        val lastAnalysis =
          panel.messages.reverse.find(m => m.kind == "analysis" && visibleIds.contains(m.id))
        if panel.input.isEmpty && lastAnalysis.nonEmpty then
          button(3, 0, "y Accept analysis", DeskAction.Feedback(lastAnalysis.get.id, true))
          button(3, 20, "n Reject", DeskAction.Feedback(lastAnalysis.get.id, false))
        lastAnalysis.foreach { message =>
          val feedback =
            message.feedback.fold("")(accepted => if accepted then " accepted" else " rejected")
          line(4, s"Feedback for analysis #${message.id}$feedback")
        }
        visibleMessages.zipWithIndex.foreach { case ((_, text), i) => line(firstRow + i, text) }
      else
        OutlookScreen
          .rows(outlook, Layout.outlookWidth, marketFrame.axis, marketFrame.priceRows, s.chartMode)
          .zipWithIndex
          .foreach((text, i) => line(i, text))
        val latest = panel.alerts.lastOption
        val headline = latest.fold("Watching for >5% moves in <1h") { alert =>
          val seconds =
            ((BigInt(now) - alert.at).max(BigInt(0)) / 1000).min(BigInt(Long.MaxValue)).toLong
          val age = if now < alert.at then "Clock changed"
          else if seconds < 60 then "Just now"
          else if seconds < 3600 then s"${seconds / 60}m ago"
          else s"${seconds / 3600}h ago"
          age + " " + alert.text
        }
        line(
          marketFrame.priceRows + 5,
          if headline.length <= Layout.outlookWidth then headline
          else headline.take(Layout.outlookWidth - 13) + "... h Details"
        )
        line(marketFrame.priceRows + 6, panel.newsStatus)
        line(marketFrame.priceRows + 7, panel.soundStatus)
      button(rows - 3, 0, "a Ask", DeskAction.Ask)
      button(rows - 3, 8, if panel.details then "h Close" else "h Details", DeskAction.Details)
      button(rows - 3, 20, if panel.muted then "v Sound" else "v Mute", DeskAction.ToggleMute)
      button(rows - 3, 32, "Status", DeskAction.Status, panel.input.isEmpty)
      panel.input match
        case Some(text) =>
          line(rows - 2, "Ask> " + safeText(text).takeRight(rightWidth - 6) + "_")
          button(rows - 1, 0, "Enter Ask", DeskAction.SendQuestion)
          button(rows - 1, 14, "Esc Cancel", DeskAction.Cancel)
        case None =>
          val historyStatus = panel.historyFailure
            .map(_ => "History save failed. h Details")
            .orElse(
              panel.historySavingAt
                .filter(at => BigInt(now) - at >= 1000L)
                .map(at =>
                  "History saving " + ((BigInt(now) - at)
                    .max(BigInt(0)) / 1000) + "s. Trade result above"
                )
            )
          line(rows - 2, historyStatus.getOrElse("q quit  m market  c chart"))
          line(
            rows - 1,
            if panel.details then "1-8 review  9/0 target  Enter send"
            else "S support R resist T target"
          )
      val activeButtons = buttons.result()
      val inactiveButtons = disabledButtons.result()
      val lines = market.zipWithIndex.map { (left, i) =>
        val displayed =
          if style == Style.mono then left.replaceAll("\\x1b\\[[0-9;]*m", "") else left
        val plainPanel = if !panel.details && panel.input.isEmpty && i == 0 then
          right(i)
            .replace("↑", style.wrap(style.bid, "↑"))
            .replace("↓", style.wrap(style.ask, "↓"))
        else right(i)
        val panelLine = if !style.colour || i < tradeTop then plainPanel
        else
          (activeButtons ++ inactiveButtons)
            .filter(b => b.height > 1 && i >= b.y - 1 && i < b.y - 1 + b.height)
            .sortBy(-_.x)
            .foldLeft(plainPanel) { (text, b) =>
              val colour = if inactiveButtons.contains(b) then Screen.dim
              else
                b.action match
                  case DeskAction.Prepare(intent) if intent.side == Side.Sell =>
                    style.ask + Screen.bold
                  case DeskAction.Target(Side.Sell) => style.ask + Screen.bold
                  case DeskAction.Prepare(_) | DeskAction.Target(_) | DeskAction.Submit =>
                    style.bid + Screen.bold
                  case _ => Screen.bold
              val begin = b.x - leftWidth - 2
              text.take(begin) + style.wrap(colour, text.slice(begin, begin + b.width)) + text.drop(
                begin + b.width
              )
            }
        displayed + "|" + Chart.fit(
          panelLine,
          if i < tradeTop then Layout.outlookWidth else rightWidth
        )
      }
      WorkspaceFrame(
        lines,
        activeButtons,
        s.coin,
        cols,
        rows,
        complete,
        panel.review,
        panel.mode,
        Some(outlook),
        scrollLimit,
        inactiveButtons,
        shortcuts
      )
