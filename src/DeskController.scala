package desk

final class DeskController(
    initial: PanelView,
    wall: () => Long,
    preview: OrderIntent => Either[String, OrderPlan],
    submit: OrderReview => Boolean,
    switchMode: String => Either[String, String],
    persist: Vector[DeskMessage] => Unit = _ => (),
    mute: Boolean => Unit = _ => ()
) extends AutoCloseable:
  private var current = initial
  private var shown = Option.empty[WorkspaceFrame]
  private var pressed = Option.empty[(WorkspaceFrame, DeskButton, Boolean)]
  private var serial = initial.messages.map(_.id).maxOption.getOrElse(0L)
  private var storageFailed = false
  private var messagesToSave = Option.empty[Vector[DeskMessage]]
  private var stopping = false
  private var historyThread = Option.empty[Thread]
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
      before.mode == after.mode && before.review == after.review &&
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
  def status(data: String, account: String): Unit = synchronized {
    current = current.copy(dataStatus = data, account = account)
  }
  def outlook(value: OutlookView): Unit = synchronized {
    current = current.copy(outlook = value)
  }
  private def saveMessages(): Unit =
    if !storageFailed && !stopping then
      messagesToSave = Some(current.messages)
      current = current.copy(historySavingAt = current.historySavingAt.orElse(Some(wall())))
      notifyAll()

  def start(): Unit = synchronized {
    if historyThread.isEmpty && !stopping then
      val thread = new Thread(() => writeMessages(), "message-history")
      thread.setDaemon(true)
      historyThread = Some(thread)
      thread.start()
  }

  private def writeMessages(): Unit =
    try
      var done = false
      while !done do
        val next = synchronized {
          while messagesToSave.isEmpty && !stopping && !storageFailed do wait()
          val value = messagesToSave
          messagesToSave = None
          value
        }
        next match
          case None           => done = true
          case Some(messages) =>
            persist(messages)
            synchronized {
              if messagesToSave.isEmpty then current = current.copy(historySavingAt = None)
            }
    catch
      case _: Exception =>
        synchronized {
          storageFailed = true
          messagesToSave = None
          serial += 1
          val reason = "Message history save failed. Trade results remain visible for this session"
          current = current.copy(
            historySavingAt = None,
            historyFailure = Some(reason),
            messages =
              (current.messages :+ DeskMessage(serial, wall(), "storage", reason)).takeRight(300)
          )
        }

  def close(): Unit =
    start()
    val thread = synchronized {
      stopping = true
      notifyAll()
      historyThread
    }
    thread.filterNot(_ == Thread.currentThread()).foreach(_.join())
  def addMessage(kind: String, text: String): Unit = synchronized {
    serial += 1
    current = current.copy(
      messages = (current.messages :+ DeskMessage(serial, wall(), kind, Workspace.safeText(text)))
        .takeRight(300),
      actionNotice = if kind == "order" || kind == "mode" then Some(Workspace.safeText(text))
      else current.actionNotice,
      actionNumber = if kind == "order" || kind == "mode" then current.actionNumber + 1
      else current.actionNumber
    )
    saveMessages()
  }

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
        action match
          case DeskAction.Prepare(intent)
              if view.ready && !view.pending && current.mode != "VIEW" &&
                current.modeReview.isEmpty && intent.kind == OrderKind.Hard && intent.big == current.bigOrder =>
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
          case DeskAction.OrderSize(big) if !view.pending && current.modeReview.isEmpty =>
            current = current.copy(bigOrder = big, actionNotice = None)
            shown = None
          case DeskAction.Mode(name) if !view.pending && name != current.mode =>
            current = current.copy(modeReview = Some(name))
            shown = None
          case DeskAction.ConfirmMode(name) if current.modeReview.contains(name) && !view.pending =>
            switchMode(name) match
              case Right(account) =>
                current = current.copy(
                  mode = name,
                  account = account,
                  modeReview = None,
                  actionNotice = None
                )
              case Left(reason) =>
                current = current.copy(modeReview = None)
                addMessage("mode", reason)
            shown = None
          case DeskAction.Cancel =>
            current = current.copy(modeReview = None)
            shown = None
          case DeskAction.Prepare(_) => refuse()
          case _                     => ()
    command match
      case Command.CancelTarget
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
          if valid && available.exists(sameControl(before, button, _)) then act(button.action)
          else if button.action.isInstanceOf[DeskAction.Prepare] then
            addMessage("order", "Not sent: controls changed")
        }
        true
      case other =>
        pressed = None
        other match
          case Command.PlaceOrder(intent) =>
            act(DeskAction.Prepare(intent), shortcut = true)
            true
          case Command.SubmitTarget =>
            current.modeReview.foreach(name => act(DeskAction.ConfirmMode(name)))
            true
          case Command.CancelTarget =>
            current = current.copy(modeReview = None)
            shown = None
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
