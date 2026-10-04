package desk

object DeskController:
  val viewOnly: String = "View only: p Prop or l Live to trade"

final class DeskController(
    initial: PanelView,
    wall: () => Long,
    preview: (OrderIntent, TradeTerms) => Either[String, OrderPlan],
    submit: OrderReview => Boolean,
    switchMode: String => Either[String, Unit],
    mute: Boolean => Unit = _ => (),
    result: (Long, String) => Unit = (_, _) => (),
    cancel: Vector[String] => Boolean = _ => false
):
  private var current = initial
  private var shown = Option.empty[WorkspaceFrame]
  private var pressed = Option.empty[(WorkspaceFrame, DeskButton, Boolean)]
  // The axis row under the cursor, and current.focus its index among the rows of the frame that
  // drew it. Keys act on that row only through a frame drawn with this focus, and stale holds them
  // when the orders change after the frame was drawn.
  private var cursor = Option.empty[AxisMark]
  private var stale = false
  // The orders and entry the axis rows are made of, as last seen.
  private var made = (Vector.empty[(String, BigDecimal)], Option.empty[BigDecimal])
  private val orderUpdates = new OrderUpdates
  def panel: PanelView = synchronized(current)
  // The render pass calls this before it draws, so a frame shows the orders seen here. Orders that
  // change after a frame was drawn hold the axis keys until the next frame.
  def orders(view: TradingView): Unit = synchronized {
    orderUpdates.collect(view)(addMessage("order", _))
    val now = (
      view.resting.filter(_.coin == view.coin).map(o => (o.id, o.price)),
      view.position.filter(_ != 0).flatMap(_ => view.portfolio.entry)
    )
    if now != made then stale = true
    made = now
  }
  private def sameControl(
      before: WorkspaceFrame,
      button: DeskButton,
      after: WorkspaceFrame
  ): Boolean =
    before.cols == after.cols && before.rows == after.rows && before.coin == after.coin &&
      before.mode == after.mode &&
      before.buttons.contains(button) == after.buttons.contains(button) && after.controls.contains(
        button
      )
  def publish(frame: WorkspaceFrame): Unit = synchronized {
    pressed = pressed.map((before, button, valid) =>
      (before, button, valid && sameControl(before, button, frame))
    )
    shown = Some(frame)
    stale = false
    if current.focus.nonEmpty then refocus(frame)
  }
  def invalidate(): Unit = synchronized { shown = None }
  // A ticket or a cursor never carries one market's price or row to another.
  def resetMarket(): Unit = synchronized {
    current = current.copy(
      modeReview = None,
      outlook = Outlook.empty(""),
      actionNotice = None,
      ticket = None
    )
    point(Vector.empty, None)
    shown = None
    pressed = None
  }
  // New sizes and slippage apply from the next press. The frame drawn with the old sizes is
  // dropped, so no press is sent at a size its button did not show. A ticket drawn on it closes.
  def setTerms(terms: TradeTerms): Unit = synchronized {
    current = current.copy(terms = terms, ticket = None)
    point(Vector.empty, None)
    shown = None
  }
  // New bindings change the labels, so the frame drawn with the old ones is dropped.
  def setBindings(bindings: Bindings): Unit = synchronized {
    current = current.copy(bindings = bindings, ticket = None)
    point(Vector.empty, None)
    shown = None
  }
  def outlook(value: OutlookView): Unit = synchronized {
    current = current.copy(outlook = value)
  }
  def orderResultNotSaved(number: Long): Unit = synchronized {
    current = current.copy(actionNotice = Some(s"Order result #$number not saved"))
  }
  def addMessage(kind: String, text: String): Unit = synchronized {
    val number = current.actionNumber + 1
    val safe = Workspace.safeText(text)
    current = current.copy(
      actionNotice = if kind == "order" || kind == "mode" then Some(safe)
      else current.actionNotice,
      actionNumber = if kind == "order" || kind == "mode" then number
      else current.actionNumber
    )
    if kind == "order" then result(number, safe)
  }

  // Esc closes the ticket, then the mode review, then the axis focus.
  private def dismiss(): Unit =
    if current.ticket.nonEmpty then current = current.copy(ticket = None)
    else if current.modeReview.nonEmpty then current = current.copy(modeReview = None)
    else point(Vector.empty, None)
    shown = None

  private def isOrder(row: AxisMark): Boolean =
    row.kind == AxisKind.Buy || row.kind == AxisKind.Sell

  // The entry is one row whatever its price. An order row is its side and price.
  private def same(a: AxisMark, b: AxisMark): Boolean =
    a.kind == b.kind && (a.kind == AxisKind.Entry || a.price == b.price)

  private def point(rows: Vector[AxisMark], at: Option[AxisMark]): Unit =
    cursor = at
    current = current.copy(focus = at.map(row => rows.indexWhere(same(_, row))))

  // The cursor keeps its row in the frame's rows. When its row is gone it moves to the nearest
  // order row, the one above on a tie and never by itself onto the entry row. Focus drops when the
  // frame draws no order row. A focus that changes here waits for the frame that draws it.
  private def refocus(frame: WorkspaceFrame): Unit =
    val rows = frame.axis.fold(Vector.empty[AxisMark])(OrdersAxis.selectable)
    val orders = rows.filter(isOrder)
    val next = cursor
      .flatMap(was =>
        rows
          .find(same(_, was))
          .orElse(orders.minByOption(row => ((row.price - was.price).abs, row.row)))
      )
      .filter(_ => orders.nonEmpty)
    point(rows, next)

  // What a fill of the row's orders would do to the position. A flip names the money that
  // would open the other side.
  private def effect(view: TradingView, row: AxisMark): Option[String] =
    view.position.filter(_ => isOrder(row)).map { position =>
      val effects = Trading.effects(position, view.resting.filter(_.coin == view.coin))
      val facing = effects.filter((_, e) => e == Effect.Reduces || e == Effect.Flips)
      val ahead = facing.scanLeft(BigDecimal(0))(_ + _._1.remaining)
      val opening = facing
        .zip(ahead)
        .collect {
          case ((o, Effect.Flips), before) if row.ids.contains(o.id) =>
            o.remaining - (position.abs - before).max(0)
        }
        .sum
      val here = effects.collect { case (o, e) if row.ids.contains(o.id) => e }
      if here.contains(Effect.Flips) then
        val money = Trading.money(opening, row.price, view.leverage)
        s"flip ${Trading.dollars(money, 0)} ${view.coin} if filled"
      else if here.contains(Effect.Reduces) then "reduce"
      else if here.contains(Effect.Opens) then "opens"
      else "void"
    }

  private val small = BigDecimal("0.01")
  private val large = BigDecimal("0.1")

  // A ticket holds a sized press at a chosen price. Its step starts as the move Up would make.
  private def open(coin: String, intent: OrderIntent, price: BigDecimal, view: TradingView): Unit =
    val step = view.rules
      .flatMap(Trading.movePrice(price, _, small, up = true))
      .map(_ - price)
      .getOrElse(small)
    current = current.copy(
      ticket = Some(Ticket(coin, intent.copy(price = Some(price)), price, step, None, None))
    )
    point(Vector.empty, None)

  def handle(
      command: Command,
      snapshot: Snapshot,
      view: TradingView,
      size: (Int, Int),
      promptOpen: Boolean
  ): Boolean = synchronized {
    orders(view)
    val axis = view.forMarket(snapshot.coin)
    val frame = shown.filter(f =>
      f.cols == size._1 && f.rows == size._2 &&
        f.coin == snapshot.coin && f.mode == current.mode
    )
    val available = frame.filter(_ =>
      !snapshot.quit && !promptOpen && snapshot.prompt.isEmpty &&
        snapshot.marketPending.isEmpty && view.coin == snapshot.coin
    )
    def refuse(): Unit =
      addMessage(
        "order",
        if view.pending then "Order pending"
        else view.block.getOrElse("Not sent: price or account changed")
      )
    // The axis as the shown frame drew it. Without it the axis keys do nothing, as a press does
    // without a frame.
    val drawn = available.flatMap(_.axis)
    val rows = drawn.fold(Vector.empty[AxisMark])(OrdersAxis.selectable)
    // The row the shown frame drew under the cursor, when it drew the cursor where it is now and
    // the orders have not changed since.
    val chosen =
      if stale || !available.exists(_.focus == current.focus) then None
      else current.focus.flatMap(rows.lift)
    def toggle(): Unit =
      current = current.copy(orderMode =
        if current.orderMode == OrderMode.Market then OrderMode.Limit else OrderMode.Market
      )
    // o focuses the order row nearest the current price, or drops focus. A ticket keeps it out.
    def focus(): Unit =
      if current.ticket.isEmpty then
        if current.focus.nonEmpty then point(Vector.empty, None)
        else
          drawn.foreach { placed =>
            OrdersAxis.start(placed).flatMap(rows.lift) match
              case Some(row) =>
                point(rows, Some(row))
                current = current.copy(actionNotice = effect(axis, row))
              case None => addMessage("order", s"No orders on ${snapshot.coin}")
          }
    def step(by: Int): Unit =
      current.focus.filter(_ => rows.nonEmpty).foreach { i =>
        val at = rows.lift((i + by).max(0).min(rows.size - 1))
        point(rows, at)
        current = current.copy(actionNotice = at.flatMap(effect(axis, _)))
      }
    def move(ticket: Ticket, delta: BigDecimal, up: Boolean): Unit =
      axis.rules.flatMap(Trading.movePrice(ticket.price, _, delta, up)) match
        case Some(price) =>
          val next = ticket.copy(
            intent = ticket.intent.copy(price = Some(price)),
            price = price,
            step = (price - ticket.price).abs,
            notice = None
          )
          current = current.copy(ticket = Some(next))
        case None => addMessage("order", "no further step")
    def act(action: DeskAction, shortcut: Boolean = false): Unit =
      // A press acts only through a frame drawn with the order type as it is now, and Place only
      // through a frame drawn with the ticket as it is now. Otherwise they wait for that frame.
      val seen = available.filter(f =>
        action match
          case DeskAction.Prepare(_) => f.orderMode == current.orderMode
          case DeskAction.Place      => f.ticket == current.ticket
          case _                     => true
      )
      val visible = seen.exists(_.buttons.exists(_.action == action))
      val fromKey = shortcut && seen.exists(_.shortcuts.contains(action))
      if seen.exists(_.disabledButtons.exists(_.action == action)) then
        if view.ready && !view.pending && view.block.isEmpty then
          addMessage("order", "Not sent: controls changed")
        else refuse()
      else if visible || fromKey then
        def send(intent: OrderIntent, fits: OrderPlan => Boolean = _ => true): Boolean =
          val settings = current.mode != "LIVE" || (view.leverage
            .exists(n => n > 0 && view.rules.exists(r => n <= r.maxLeverage)) &&
            view.marginMode.exists(m => m == "cross" || m == "isolated"))
          if !settings then
            refuse()
            false
          else
            val terms = current.terms
            preview(intent, terms) match
              case Right(plan) if plan.coin == snapshot.coin && fits(plan) =>
                val at = wall()
                val order = OrderReview(
                  intent,
                  plan,
                  terms,
                  at,
                  Math.addExact(at, TradingWorker.maximumReviewLifetimeMs),
                  view.leverage,
                  view.marginMode
                )
                val sent = submit(order)
                if sent then current = current.copy(actionNotice = Some("Sending"))
                else refuse()
                sent
              case Left(reason) =>
                addMessage("order", reason)
                false
              case _ =>
                refuse()
                false
        // A ticket sends one post only order at its own price, or stays open with the reason.
        def place(ticket: Ticket): Unit =
          val crossing = view.bid
            .zip(view.ask)
            .map((bid, ask) => Trading.crossing(ticket.intent.side, ticket.price, bid, ask))
          val sent =
            if ticket.coin != snapshot.coin || !view.ready || view.pending || crossing.isEmpty then
              refuse()
              false
            else
              crossing.flatten match
                case Some(text) =>
                  addMessage("order", text)
                  false
                case None =>
                  send(
                    ticket.intent.copy(price = Some(ticket.price)),
                    plan => plan.post && plan.limit == ticket.price
                  )
          current = current.copy(ticket =
            if sent then None else Some(ticket.copy(notice = current.actionNotice))
          )
        action match
          case DeskAction.Prepare(intent)
              if view.ready && !view.pending && current.mode != "VIEW" &&
                current.modeReview.isEmpty && current.ticket.isEmpty &&
                intent.kind == OrderKind.Hard && intent.big == current.bigOrder =>
            if current.orderMode == OrderMode.Market then
              val _ = send(intent)
            else
              // A limit order starts at the touch on its own side.
              val touch: Option[BigDecimal] = if intent.side == Side.Buy then view.bid else view.ask
              touch match
                case Some(price) => open(snapshot.coin, intent, price, view)
                case None        => refuse()
          case DeskAction.Place if current.mode != "VIEW" => current.ticket.foreach(place)
          // A Close carries no amount. It sells a long and buys a short. On a flat or unknown
          // position it asks for a buy, which the plan or the worker refuses. Prop is long only,
          // so its close always sells.
          case DeskAction.Close
              if view.ready && !view.pending && current.mode != "VIEW" &&
                current.modeReview.isEmpty =>
            val sells = current.mode == "PROP" || view.position.exists(_ > 0)
            val _ = send(OrderIntent(if sells then Side.Sell else Side.Buy, OrderKind.Close))
          case DeskAction.OrderType if current.mode != "VIEW"                           => toggle()
          case DeskAction.Orders if current.mode != "VIEW"                              => focus()
          case DeskAction.OrderSize(big) if !view.pending && current.modeReview.isEmpty =>
            current = current.copy(bigOrder = big, actionNotice = None)
            shown = None
          // A mode review takes Enter and Esc, so a ticket or a cursor does not stay behind it.
          case DeskAction.Mode(name) if !view.pending && name != current.mode =>
            current = current.copy(modeReview = Some(name), ticket = None)
            point(Vector.empty, None)
            shown = None
          case DeskAction.ConfirmMode(name) if current.modeReview.contains(name) && !view.pending =>
            switchMode(name) match
              case Right(()) =>
                current = current.copy(
                  mode = name,
                  modeReview = None,
                  actionNotice = None
                )
              case Left(reason) =>
                current = current.copy(modeReview = None)
                addMessage("mode", reason)
            shown = None
          case DeskAction.Cancel                                           => dismiss()
          case DeskAction.Prepare(_) | DeskAction.Close | DeskAction.Place => refuse()
          case _                                                           => ()
    // Enter on the row the frame drew under the cursor. The entry row is key x. An order row
    // cancels exactly the ids the frame drew there. A row whose orders have all gone answers so.
    def choose(): Unit =
      val resting = axis.resting.map(_.id).toSet
      if cursor.exists(row => isOrder(row) && !row.ids.exists(resting)) then
        addMessage("order", "Nothing to cancel at that price")
      else
        chosen.foreach { row =>
          if row.kind == AxisKind.Entry then act(DeskAction.Close, shortcut = true)
          else if !cancel(row.ids) then refuse()
        }
    command match
      case Command.Cancel
          if snapshot.marketPending.nonEmpty || promptOpen || snapshot.prompt.nonEmpty =>
        false
      case Command.PointerPress(x, y, button) =>
        pressed = if button != 0 then None
        else
          available.flatMap { f =>
            f.controls.find(_.contains(x, y)).map(b => (f, b, true))
          }
        true
      case Command.Pointer(x, y, 0) =>
        val click = pressed
        pressed = None
        click.filter((_, button, _) => button.contains(x, y)).foreach { (before, button, valid) =>
          val order = button.action == DeskAction.Close || button.action == DeskAction.Place ||
            button.action.isInstanceOf[DeskAction.Prepare]
          if valid && available.exists(sameControl(before, button, _)) then act(button.action)
          else if order then addMessage("order", "Not sent: controls changed")
        }
        true
      case other =>
        pressed = None
        other match
          // View trades nothing. Its keys answer how to trade and reach no worker.
          case Command.OrderType | Command.Orders | Command.Place | Command.PlaceOrder(_) |
              Command.Close if current.mode == "VIEW" =>
            addMessage("order", DeskController.viewOnly)
            true
          case Command.Confirm if current.mode == "VIEW" && current.modeReview.isEmpty =>
            addMessage("order", DeskController.viewOnly)
            true
          case Command.PlaceOrder(intent) =>
            act(DeskAction.Prepare(intent), shortcut = true)
            true
          case Command.Close =>
            act(DeskAction.Close, shortcut = true)
            true
          case Command.OrderType =>
            toggle()
            true
          case Command.Orders =>
            focus()
            true
          // a opens a ticket at the side and price of the order row the frame drew under the cursor.
          case Command.Place =>
            chosen
              .filter(row => isOrder(row) && current.ticket.isEmpty)
              .foreach { row =>
                val side = if row.kind == AxisKind.Buy then Side.Buy else Side.Sell
                val intent = OrderIntent(side, OrderKind.Hard, big = current.bigOrder)
                open(snapshot.coin, intent, row.price, axis)
              }
            true
          case Command.Confirm =>
            if current.ticket.nonEmpty then act(DeskAction.Place, shortcut = true)
            else
              current.modeReview match
                case Some(name) => act(DeskAction.ConfirmMode(name))
                case None       => choose()
            true
          case Command.Cancel =>
            dismiss()
            true
          // Up and Down move a ticket's price by 0.01 or the cursor by a row. Left and Right move
          // a ticket's price by 0.1. Otherwise the arrows move the chart.
          case Command.ZoomIn | Command.ZoomOut | Command.ScrubForward | Command.ScrubBack =>
            val up = other == Command.ZoomIn || other == Command.ScrubForward
            val fine = other == Command.ZoomIn || other == Command.ZoomOut
            current.ticket match
              case Some(ticket) =>
                move(ticket, if fine then small else large, up)
                true
              case None if fine && current.focus.nonEmpty =>
                step(if up then -1 else 1)
                true
              case None => false
          case Command.Mode(name) =>
            act(DeskAction.Mode(name))
            true
          case Command.StartMarketPrompt | Command.Quit =>
            resetMarket()
            false
          case Command.ToggleMute =>
            val muted = !current.muted
            current = current.copy(muted = muted)
            mute(muted)
            true
          case Command.SelectTarget(_) | Command.Pointer(_, _, _) =>
            true
          // The keys thread opens settings after this returns. No ticket, mode review or cursor
          // stays open behind them.
          case Command.Settings =>
            current = current.copy(modeReview = None, ticket = None)
            point(Vector.empty, None)
            shown = None
            true
          case _ => false
  }
