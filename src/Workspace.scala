package desk

enum DeskAction:
  case Prepare(intent: OrderIntent)
  case Target(side: Side)
  case Submit, Cancel, Ask, SendQuestion, Status
  case Feedback(id: Long, accepted: Boolean)
  case Scroll(lines: Int)
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
    analysisStatus: String = "Analysis starting",
    review: Option[OrderReview] = None,
    messages: Vector[DeskMessage] = Vector.empty,
    input: Option[String] = None,
    scroll: Int = 0,
    target: Option[(Side, BigDecimal)] = None,
    sizes: TradeSizes = TradeSizes(),
    fee: BigDecimal = BigDecimal("0.0005"),
    modeReview: Option[String] = None
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
    mode: String = "VIEW"
):
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
      val market = Screen.render(
        s,
        leftWidth,
        rows,
        now,
        zone,
        style,
        marketOnly = true,
        reservedRows = 12
      ) ++ indicators
      val right = Array.fill(rows)("")
      val buttons = Vector.newBuilder[DeskButton]
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
        if row < rows && column + text.length <= rightWidth then
          right(row) = right(row).padTo(column, ' ') + text
          if enabled then
            buttons += DeskButton(label, action, leftWidth + 2 + column, row + 1, text.length)
      line(0, s"${panel.mode} ${s.coin}  ${panel.account}")
      button(1, 0, "p Paper", DeskAction.Mode("PAPER"), !view.pending && panel.input.isEmpty)
      button(1, 12, "l Live", DeskAction.Mode("LIVE"), !view.pending && panel.input.isEmpty)
      line(
        2,
        view.position.fold("Position unavailable")(p =>
          "Position " + Trading.format(p)
        ) + "  " + view.message
      )
      val canTrade =
        view.ready && !view.pending && panel.input.isEmpty && s.prompt.isEmpty && panel.mode != "VIEW"
      var complete = false
      panel.modeReview match
        case Some(name) =>
          line(3, "Switch to " + name + "?")
          line(
            4,
            if name == "PAPER" then "Uses the separate simulated account."
            else "Uses the configured real account."
          )
          line(5, "Existing positions remain in their account.")
          line(6, "Each order requires its own review and Send.")
          button(8, 0, "Enter Select " + name, DeskAction.ConfirmMode(name), !view.pending)
          button(9, 0, "Esc Cancel", DeskAction.Cancel)
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
          details.zipWithIndex.foreach((text, i) => line(i + 3, text))
          if complete then button(10, 0, "Enter Send " + panel.mode, DeskAction.Submit, canTrade)
          else line(9, "Review unavailable. Refresh before send")
          button(10, 20, "Esc Cancel", DeskAction.Cancel)
        case None if panel.modeReview.isEmpty =>
          val half = rightWidth / 2
          for index <- 0 until 8 do
            val intent = Trading.action(index)
            val amount = if intent.big then panel.sizes.big else panel.sizes.small
            val label = s"${index + 1} ${intent.kind} ${
                if intent.side == Side.Buy then "buy" else "sell"
              } $$${Trading.display(amount)}"
            button(3 + index / 2, (index % 2) * half, label, DeskAction.Prepare(intent), canTrade)
          button(
            7,
            0,
            "9 Target buy",
            DeskAction.Target(Side.Buy),
            !view.pending && panel.input.isEmpty
          )
          button(
            7,
            half,
            "0 Target sell",
            DeskAction.Target(Side.Sell),
            !view.pending && panel.input.isEmpty
          )
          panel.target.foreach { (side, price) =>
            line(8, s"Target ${side.toString.toLowerCase} ${Trading.displayExact(price)} Up/Down")
            button(
              9,
              0,
              "Enter Review",
              DeskAction.Prepare(OrderIntent(side, OrderKind.Target, target = Some(price))),
              canTrade
            )
            button(9, 17, "Esc Cancel", DeskAction.Cancel)
          }
          if panel.target.isEmpty then line(8, "Trade buttons open a review")
          if panel.mode == "PAPER" then line(10, "1x simulation. No funding or liquidation")
        case _ => ()
      line(11, panel.dataStatus)
      line(12, panel.analysisStatus)
      button(13, 0, "a Ask", DeskAction.Ask)
      button(13, 9, "[ Older", DeskAction.Scroll(5))
      button(13, 20, "] Newer", DeskAction.Scroll(-5))
      button(13, 33, "Status", DeskAction.Status, panel.input.isEmpty)
      val messageRows = rows - 19
      val messages = panel.messages.flatMap { m =>
        val time = java.time.Instant.ofEpochMilli(m.at).atZone(zone).toLocalTime.toString.take(8)
        val label = if m.kind == "analysis" then s"analysis #${m.id}" else m.kind
        wrap(s"$time $label: ${m.text}", rightWidth).map(text => (Some(m.id), text)) :+ (None, "")
      }
      val end = (messages.size - panel.scroll.max(0)).max(0)
      val visibleMessages = messages.slice((end - messageRows).max(0), end)
      val visibleIds = visibleMessages.flatMap(_._1).toSet
      val lastAnalysis =
        panel.messages.reverse.find(m => m.kind == "analysis" && visibleIds.contains(m.id))
      if panel.input.isEmpty && lastAnalysis.nonEmpty then
        button(14, 0, "y Accept analysis", DeskAction.Feedback(lastAnalysis.get.id, true))
        button(14, 22, "n Reject analysis", DeskAction.Feedback(lastAnalysis.get.id, false))
      lastAnalysis.foreach { message =>
        val feedback =
          message.feedback.fold("")(accepted => if accepted then " accepted" else " rejected")
        line(15, s"Feedback for analysis #${message.id}$feedback")
      }
      visibleMessages.zipWithIndex
        .foreach { case ((_, text), i) => line(16 + i, text) }
      line(rows - 3, "Messages " + end + "/" + messages.size + "  q quit  m market")
      panel.input match
        case Some(text) =>
          line(rows - 2, "Ask> " + safeText(text).takeRight(rightWidth - 6) + "_")
          button(rows - 1, 0, "Enter Ask", DeskAction.SendQuestion)
          button(rows - 1, 14, "Esc Cancel", DeskAction.Cancel)
        case None =>
          line(rows - 2, "Assistant explains. Trade buttons submit.")
          line(rows - 1, "PAXG (token tied to gold)")
      val lines = market.zipWithIndex.map { (left, i) =>
        val displayed =
          if style == Style.mono then left.replaceAll("\\x1b\\[[0-9;]*m", "") else left
        displayed + "|" + Chart.fit(right(i), rightWidth)
      }
      WorkspaceFrame(
        lines,
        buttons.result(),
        s.coin,
        cols,
        rows,
        complete,
        panel.review,
        panel.mode
      )
