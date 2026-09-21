package desk

final class DeskController(
    initial: PanelView,
    wall: () => Long,
    preview: OrderIntent => Either[String, OrderPlan],
    submit: OrderReview => Boolean,
    ask: String => Boolean,
    switchMode: String => Either[String, String],
    persist: Vector[DeskMessage] => Unit = _ => (),
    mute: Boolean => Unit = _ => ()
) extends AutoCloseable:
  private var current = initial
  private var shown = Option.empty[WorkspaceFrame]
  private var pressed = Option.empty[(WorkspaceFrame, DeskButton)]
  private var serial = initial.messages.map(_.id).maxOption.getOrElse(0L)
  private var storageFailed = false
  private var messagesToSave = Option.empty[Vector[DeskMessage]]
  private var stopping = false
  private var historyThread = Option.empty[Thread]
  def panel: PanelView = synchronized(current)
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
    pressed = pressed.filter((before, button) => sameControl(before, button, frame))
    shown = Some(frame)
  }
  def invalidate(): Unit = synchronized { shown = None }
  def resetMarket(): Unit = synchronized {
    current =
      current.copy(review = None, target = None, modeReview = None, outlook = Outlook.empty(""))
    shown = None
    pressed = None
  }
  def status(data: String, analysis: String, account: String): Unit = synchronized {
    current = current.copy(dataStatus = data, analysisStatus = analysis, account = account)
  }
  def outlook(value: OutlookView): Unit = synchronized {
    current = current.copy(outlook = value)
  }
  def announcement(value: Announcement): Unit = synchronized {
    current = current.copy(alerts = (current.alerts :+ value).takeRight(30))
  }
  def alertStatus(sound: String, news: String, details: String = "News waiting"): Unit =
    synchronized {
      current = current.copy(soundStatus = sound, newsStatus = news, newsDetails = details)
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
      scroll = if current.detailPage == DetailPage.Answers then 0 else current.scroll,
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
    val frame = shown.filter(f =>
      f.cols == size._1 && f.rows == size._2 && f.coin == snapshot.coin && f.mode == current.mode
    )
    val available = frame.filter(_ =>
      !snapshot.quit && !promptOpen && snapshot.prompt.isEmpty && view.coin == snapshot.coin
    )
    def visible(action: DeskAction): Boolean =
      available.exists(_.buttons.exists(_.action == action))
    def refuse(): Unit =
      val reason = if view.pending then "Order pending. Wait for its result before another order"
      else if current.mode == "VIEW" then "Select Paper or Live before reviewing a trade"
      else
        view.block.getOrElse(if !view.ready then view.message
        else "Review the order again before Send")
      addMessage("order", "Not sent: " + reason)
    def prepare(intent: OrderIntent): Unit =
      preview(intent) match
        case Right(plan) if plan.coin == snapshot.coin =>
          val at = wall()
          current = current.copy(
            review = Some(
              OrderReview(
                intent,
                plan,
                at,
                Math.addExact(at, TradingWorker.maximumReviewLifetimeMs),
                view.leverage,
                view.marginMode
              )
            ),
            modeReview = None,
            actionNotice = None
          )
          shown = None
        case Left(reason) =>
          current = current.copy(review = None)
          addMessage("order", reason)
          shown = None
        case _ => ()
    def act(action: DeskAction): Unit =
      if current.input.isEmpty && available.exists(_.disabledButtons.exists(_.action == action))
      then refuse()
      else if visible(action) then
        action match
          case DeskAction.Prepare(intent)
              if current.input.isEmpty && current.review.isEmpty && view.ready && !view.pending =>
            prepare(intent)
          case DeskAction.Refresh if current.input.isEmpty && view.ready && !view.pending =>
            current.review.foreach(review => prepare(review.intent))
          case DeskAction.Submit if current.input.isEmpty && view.ready && !view.pending =>
            val valid = current.review
              .filter(r =>
                r.createdAt <= wall() && wall() < r.expiresAt && available
                  .exists(f => f.completeReview && f.review.contains(r))
              )
            valid match
              case Some(review) =>
                shown = None
                current = current.copy(review = None, target = None)
                if !submit(review) then
                  addMessage(
                    "order",
                    "Price, account or readiness changed. Review the order again."
                  )
              case None => addMessage("order", "Review expired. Press Enter to review again")
          case DeskAction.Target(side) if current.input.isEmpty && !view.pending =>
            val selected = for
              rules <- view.rules
              price <- view.reference(side)
              if Trading.validPrice(price, rules)
            yield (side, price)
            selected match
              case Some(target) =>
                current = current.copy(target = Some(target), review = None, actionNotice = None)
                shown = None
              case None =>
                addMessage("order", view.block.getOrElse("Waiting for a current market quote"))
          case DeskAction.Cancel =>
            current = current.copy(
              review = None,
              target = None,
              input = None,
              modeReview = None,
              details = false
            )
            shown = None
          case DeskAction.Ask if current.input.isEmpty =>
            current = current.copy(
              input = Some(""),
              review = None,
              target = None,
              modeReview = None,
              details = true,
              detailPage = DetailPage.Answers,
              scroll = 0
            )
            shown = None
          case DeskAction.SendQuestion => sendQuestion()
          case DeskAction.Details      =>
            current = current.copy(
              details = !current.details,
              input = None,
              detailPage = if current.actionNotice.nonEmpty || current.historyFailure.nonEmpty then
                DetailPage.Answers
              else DetailPage.Method,
              scroll = 0
            )
            shown = None
          case DeskAction.Page(page) if current.input.isEmpty =>
            current = current.copy(details = true, detailPage = page, scroll = 0)
            shown = None
          case DeskAction.ToggleMute =>
            val next = !current.muted
            mute(next)
            current = current.copy(muted = next, soundStatus = if next then "Muted" else "Sound on")
            shown = None
          case DeskAction.Status if current.input.isEmpty =>
            current = current.copy(details = true, detailPage = DetailPage.Answers)
            addMessage(
              "data",
              s"${current.mode} ${snapshot.coin}\n${current.account}\n${view.message}\n${current.dataStatus}\n${current.analysisStatus}"
            )
          case DeskAction.Feedback(id, accepted) =>
            current = current.copy(messages =
              current.messages.map(m =>
                if m.id == id && m.kind == "analysis" then m.copy(feedback = Some(accepted)) else m
              )
            )
            saveMessages()
          case DeskAction.Scroll(lines) =>
            val delta = if current.detailPage == DetailPage.Answers then lines else -lines
            current = current.copy(scroll =
              (current.scroll + delta).max(0).min(available.fold(0)(_.scrollLimit))
            )
          case DeskAction.Mode(name)
              if current.input.isEmpty && !view.pending && name != current.mode =>
            current = current.copy(modeReview = Some(name), review = None, target = None)
            shown = None
          case DeskAction.ConfirmMode(name) if current.modeReview.contains(name) && !view.pending =>
            switchMode(name) match
              case Right(account) =>
                current = current.copy(
                  mode = name,
                  account = account,
                  modeReview = None,
                  review = None,
                  target = None
                )
                addMessage("mode", name + " selected. Each trade still needs review and Send.")
              case Left(reason) =>
                current = current.copy(modeReview = None)
                addMessage("mode", reason)
            shown = None
          case DeskAction.Prepare(_) | DeskAction.Submit | DeskAction.Refresh |
              DeskAction.Target(_) =>
            refuse()
          case _ => ()
    def sendQuestion(): Unit =
      current.input.map(_.trim).filter(_.nonEmpty).foreach { question =>
        if ask(question) then
          addMessage("question", question)
          current = current.copy(input = None)
          shown = None
        else addMessage("analysis", "Question queue is full. Try again after the pending answer.")
      }

    def regular(): Boolean = if current.input.nonEmpty then
      command match
        case Command.PromptChar(c) =>
          current = current.copy(input = current.input.map(t => (t + c).take(4000)))
          true
        case Command.PromptText(text) =>
          current = current.copy(input =
            current.input.map(t => (t + Workspace.safeText(text).replace('\n', ' ')).take(4000))
          )
          true
        case Command.PromptBackspace =>
          current = current.copy(input = current.input.map(_.dropRight(1)))
          true
        case Command.PromptCancel | Command.CancelTarget =>
          current = current.copy(input = None)
          shown = None
          true
        case Command.PromptSubmit =>
          sendQuestion()
          true
        case Command.Quit => false
        case _            => true
    else
      command match
        case Command.PlaceOrder(intent) =>
          act(DeskAction.Prepare(intent))
          true
        case Command.SelectTarget(side) =>
          act(DeskAction.Target(side))
          true
        case Command.SubmitTarget =>
          if current.review.nonEmpty then
            if available.exists(_.controls.exists(_.action == DeskAction.Refresh)) then
              act(DeskAction.Refresh)
            else act(DeskAction.Submit)
          else if current.modeReview.nonEmpty then
            act(DeskAction.ConfirmMode(current.modeReview.get))
          else
            current.target.foreach((side, price) =>
              act(DeskAction.Prepare(OrderIntent(side, OrderKind.Target, target = Some(price))))
            )
          true
        case Command.CancelTarget =>
          current = current.copy(review = None, target = None, modeReview = None, details = false)
          shown = None
          true
        case Command.ZoomIn | Command.ZoomOut if current.target.nonEmpty =>
          current.target.foreach { (side, price) =>
            view.rules
              .flatMap(r => Trading.nextPrice(price, r, command == Command.ZoomIn))
              .foreach { next =>
                current = current.copy(target = Some((side, next)), review = None)
                shown = None
              }
          }
          true
        case Command.Ask =>
          act(DeskAction.Ask)
          true
        case Command.Details =>
          act(DeskAction.Details)
          true
        case Command.ToggleMute =>
          act(DeskAction.ToggleMute)
          true
        case Command.Mode(name) =>
          act(DeskAction.Mode(name))
          true
        case Command.Feedback(accepted) =>
          available.toVector
            .flatMap(_.buttons)
            .map(_.action)
            .collectFirst {
              case action @ DeskAction.Feedback(_, value) if value == accepted => action
            }
            .foreach(act)
          true
        case Command.ScrollMessages(lines) =>
          act(DeskAction.Scroll(lines))
          true
        case Command.Pointer(x, y, button) =>
          if (button == 64 || button == 65) && available.exists(f => x > f.cols * 3 / 5) then
            act(DeskAction.Scroll(if button == 64 then 5 else -5))
          true
        case Command.StartMarketPrompt | Command.Quit =>
          resetMarket()
          false
        case _ => false

    command match
      case Command.PointerPress(x, y, button) =>
        pressed = if button != 0 then None
        else
          available.flatMap { f =>
            f.controls.find(b => b.y == y && x >= b.x && x < b.x + b.width).map(b => (f, b))
          }
        true
      case Command.Pointer(x, y, 0) =>
        val click = pressed
        pressed = None
        for
          (before, button) <- click
          after <- available
          if sameControl(before, button, after)
          if button.y == y && x >= button.x && x < button.x + button.width
        do act(button.action)
        true
      case _ =>
        pressed = None
        regular()
  }
