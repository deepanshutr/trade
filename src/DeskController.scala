package desk

final class DeskController(
    initial: PanelView,
    wall: () => Long,
    preview: OrderIntent => Either[String, OrderPlan],
    submit: OrderReview => Boolean,
    switchMode: String => Either[String, Unit],
    mute: Boolean => Unit = _ => (),
    result: (Long, String) => Unit = (_, _) => ()
):
  private var current = initial
  private var shown = Option.empty[WorkspaceFrame]
  private var pressed = Option.empty[(WorkspaceFrame, DeskButton, Boolean)]
  private val orderUpdates = new OrderUpdates
  def panel: PanelView = synchronized(current)
  def orders(view: TradingView): Unit = synchronized {
    orderUpdates.collect(view)(addMessage("order", _))
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
  }
  def invalidate(): Unit = synchronized { shown = None }
  def resetMarket(): Unit = synchronized {
    current = current.copy(
      modeReview = None,
      outlook = Outlook.empty(""),
      actionNotice = None
    )
    shown = None
    pressed = None
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

  private def cancel(): Unit =
    current = current.copy(modeReview = None)
    shown = None

  def handle(
      command: Command,
      snapshot: Snapshot,
      view: TradingView,
      size: (Int, Int),
      promptOpen: Boolean
  ): Boolean = synchronized {
    orders(view)
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
    def act(action: DeskAction, shortcut: Boolean = false): Unit =
      val visible = available.exists(_.buttons.exists(_.action == action))
      val fromKey = shortcut && available.exists(_.shortcuts.contains(action))
      if available.exists(_.disabledButtons.exists(_.action == action)) then
        if view.ready && !view.pending && view.block.isEmpty then
          addMessage("order", "Not sent: controls changed")
        else refuse()
      else if visible || fromKey then
        def send(intent: OrderIntent): Unit =
          val settings = current.mode != "LIVE" || (view.leverage
            .exists(n => n > 0 && view.rules.exists(r => n <= r.maxLeverage)) &&
            view.marginMode.exists(m => m == "cross" || m == "isolated"))
          if !settings then refuse()
          else
            preview(intent) match
              case Right(plan) if plan.coin == snapshot.coin =>
                val at = wall()
                val order = OrderReview(
                  intent,
                  plan,
                  at,
                  Math.addExact(at, TradingWorker.maximumReviewLifetimeMs),
                  view.leverage,
                  view.marginMode
                )
                if submit(order) then current = current.copy(actionNotice = Some("Sending"))
                else refuse()
              case Left(reason) => addMessage("order", reason)
              case _            => refuse()
        action match
          case DeskAction.Prepare(intent)
              if view.ready && !view.pending && current.mode != "VIEW" &&
                current.modeReview.isEmpty && intent.kind == OrderKind.Hard && intent.big == current.bigOrder =>
            send(intent)
          // A Close carries no amount. It sells a long and buys a short. On a flat or unknown
          // position it asks for a buy, which the plan or the worker refuses.
          case DeskAction.Close if view.ready && !view.pending && current.modeReview.isEmpty =>
            send(
              OrderIntent(
                if view.position.exists(_ > 0) then Side.Sell else Side.Buy,
                OrderKind.Close
              )
            )
          case DeskAction.OrderSize(big) if !view.pending && current.modeReview.isEmpty =>
            current = current.copy(bigOrder = big, actionNotice = None)
            shown = None
          case DeskAction.Mode(name) if !view.pending && name != current.mode =>
            current = current.copy(modeReview = Some(name))
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
          case DeskAction.Cancel                        => cancel()
          case DeskAction.Prepare(_) | DeskAction.Close => refuse()
          case _                                        => ()
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
          val order =
            button.action == DeskAction.Close || button.action.isInstanceOf[DeskAction.Prepare]
          if valid && available.exists(sameControl(before, button, _)) then act(button.action)
          else if order then addMessage("order", "Not sent: controls changed")
        }
        true
      case other =>
        pressed = None
        other match
          case Command.PlaceOrder(intent) =>
            act(DeskAction.Prepare(intent), shortcut = true)
            true
          // CLOSE is live only until prop margin exists. In prop or view mode x does nothing.
          case Command.Close =>
            if current.mode == "LIVE" then act(DeskAction.Close, shortcut = true)
            true
          case Command.Confirm =>
            current.modeReview.foreach(name => act(DeskAction.ConfirmMode(name)))
            true
          case Command.Cancel =>
            cancel()
            true
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
          case _ => false
  }
