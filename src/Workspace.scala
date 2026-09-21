package desk

enum DeskAction:
  case Prepare(intent: OrderIntent)
  case Target(side: Side)
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
    actionNumber: Long = 0L
)
final case class DeskButton(label: String, action: DeskAction, x: Int, y: Int, width: Int)
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
    disabledButtons: Vector[DeskButton] = Vector.empty
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
    else buttons.find(b => b.y == y && x >= b.x && x < b.x + b.width).map(_.action)

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
      val bookWidth = leftWidth - 21
      val h = ((bookWidth - 27) / 2).max(1).min(20)
      val referenceCoins = Vector("BTC", "PAXG")
      val correlation = referenceCoins.flatMap { coin =>
        Gauges.horizons.zipWithIndex.map { case ((label, interval, n), index) =>
          val r = Quant.compare(
            s.candles.getOrElse(interval, Vector.empty),
            s.refs.getOrElse(coin, Ref.empty).candles.getOrElse(interval, Vector.empty),
            interval,
            n,
            now
          )
          val prefix = if index == 0 then (if coin == "PAXG" then "gold" else "BTC") else ""
          val reading =
            r.value.map(v => f"$v%+.2f ${r.count}/$n").getOrElse(s"${r.status} ${r.count}/$n")
          Chart.fit(s"$prefix $label $reading".trim, 20)
        }
      }
      val indicators = (0 until 12).toVector.map { i =>
        val reading = if i < 6 then Gauges.bookRow(s, i, now, h, style)
        else Gauges.tapeRow(s, i - 6, now, h, style)
        Chart.fit(reading, bookWidth) + " " + correlation(i)
      }
      val tradeTop = rows - 15
      val marketFrame = Screen.renderMarket(
        s,
        leftWidth,
        rows,
        now,
        zone,
        style,
        marketOnly = true,
        reservedRows = 12,
        maxPriceRows = tradeTop - 7
      )
      val market = marketFrame.lines.padTo(rows - 12, " " * leftWidth) ++ indicators
      val right = Array.fill(rows)("")
      var scrollLimit = 0
      val buttons = Vector.newBuilder[DeskButton]
      val disabledButtons = Vector.newBuilder[DeskButton]
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
        if row < rows && column + text.length <= rightWidth then
          right(row) = right(row).padTo(column, ' ') + text
          val control = DeskButton(label, action, leftWidth + 2 + column, row + 1, text.length)
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
      tradeLine(0, s"${panel.mode} ${s.coin}  ${panel.account}")
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
            settings + " captured " + java.time.Instant
              .ofEpochMilli(review.createdAt)
              .atZone(zone)
              .toLocalTime
              .toString
              .take(8),
            "Expires in " + ((BigInt(review.expiresAt) - now).max(BigInt(0)) / 1000) + "s"
          )
          complete = knownSettings && details.forall(
            _.length <= rightWidth
          ) && p.coin == s.coin && review.createdAt <= now && now < review.expiresAt
          details.zipWithIndex.foreach((text, i) => tradeLine(i + 3, text))
          if complete then
            tradeButton(10, 0, "Enter Send " + panel.mode, DeskAction.Submit, canTrade)
          else if p.coin != s.coin then tradeLine(9, "Market changed. Cancel and review again")
          else if !knownSettings then tradeLine(9, "Account settings missing. Review when ready")
          else if review.createdAt > now || now >= review.expiresAt then
            tradeLine(9, "Review expired. Enter reviews again")
            tradeButton(10, 0, "Enter Review again", DeskAction.Refresh, canTrade)
          else tradeLine(9, "Review does not fit. Widen the window")
          tradeButton(10, 20, "Esc Cancel", DeskAction.Cancel)
        case None if panel.modeReview.isEmpty =>
          val half = rightWidth / 2
          def targetReady(side: Side): Boolean =
            !view.pending && panel.input.isEmpty && view.rules.exists(r =>
              view.reference(side).exists(Trading.validPrice(_, r))
            )
          for index <- 0 until 8 do
            val intent = Trading.action(index)
            val amount = if intent.big then panel.sizes.big else panel.sizes.small
            val label = s"${index + 1} ${intent.kind} ${
                if intent.side == Side.Buy then "buy" else "sell"
              } $$${Trading.display(amount)}"
            tradeButton(
              3 + index / 2,
              (index % 2) * half,
              label,
              DeskAction.Prepare(intent),
              canTrade
            )
          tradeButton(
            7,
            0,
            "9 Target buy",
            DeskAction.Target(Side.Buy),
            targetReady(Side.Buy)
          )
          tradeButton(
            7,
            half,
            "0 Target sell",
            DeskAction.Target(Side.Sell),
            targetReady(Side.Sell)
          )
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
              canTrade
            )
            tradeButton(9, 17, "Esc Cancel", DeskAction.Cancel)
          }
          if panel.target.isEmpty then
            tradeLine(
              8,
              if view.pending then "Waiting for result. Do not send again"
              else if canTrade then "Trade buttons open a review"
              else "( ) unavailable. Select one for the reason"
            )
          if panel.mode == "PAPER" then tradeLine(10, "1x simulation. No funding or liquidation")
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
          wrap(s"$time $label: ${m.text}", rightWidth).map(text => (Some(m.id), text)) :+ (None, "")
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
          button(3, 22, "n Reject analysis", DeskAction.Feedback(lastAnalysis.get.id, false))
        lastAnalysis.foreach { message =>
          val feedback =
            message.feedback.fold("")(accepted => if accepted then " accepted" else " rejected")
          line(4, s"Feedback for analysis #${message.id}$feedback")
        }
        visibleMessages.zipWithIndex.foreach { case ((_, text), i) => line(firstRow + i, text) }
      else
        OutlookScreen
          .rows(outlook, rightWidth, marketFrame.axis, marketFrame.priceRows, s.chartMode)
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
          if headline.length <= rightWidth then headline
          else headline.take(rightWidth - 13) + "... h Details"
        )
        line(marketFrame.priceRows + 6, panel.newsStatus + " | " + panel.soundStatus)
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
            if panel.details then "Answers explain. Trade buttons submit."
            else "S support R resist T target | h Details"
          )
      val lines = market.zipWithIndex.map { (left, i) =>
        val displayed =
          if style == Style.mono then left.replaceAll("\\x1b\\[[0-9;]*m", "") else left
        val panelLine = if !panel.details && panel.input.isEmpty && i == 0 then
          right(i)
            .replace("↑", style.wrap(style.bid, "↑"))
            .replace("↓", style.wrap(style.ask, "↓"))
        else right(i)
        displayed + "|" + Chart.fit(panelLine, rightWidth)
      }
      WorkspaceFrame(
        lines,
        buttons.result(),
        s.coin,
        cols,
        rows,
        complete,
        panel.review,
        panel.mode,
        Some(outlook),
        scrollLimit,
        disabledButtons.result()
      )
