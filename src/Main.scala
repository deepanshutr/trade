package desk

import java.nio.file.{Files, LinkOption, Path}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

object Main:
  final case class Args(
      coin: String,
      mono: Boolean,
      trade: Boolean = false,
      config: Option[String] = None,
      paper: Boolean = false,
      record: Boolean = false,
      stateDirectory: Option[String] = None,
      audio: String = "voice"
  )
  val usage =
    "usage: desk [COIN] [--mono] [--paper | --trade | --record] [--config PATH] [--state PATH] [--chime | --mute]"
  val escapeWaitNanos = 40_000_000L

  private[desk] def announcements(args: Args, output: AnnouncementOutput): Announcements =
    new Announcements(output, voice = args.audio != "chime", mutedInitially = args.audio == "mute")

  def parseArgs(args: Seq[String]): Either[String, Args] =
    def loop(rest: List[String], chosen: Args, hasCoin: Boolean): Either[String, Args] =
      rest match
        case Nil =>
          if Vector(chosen.trade, chosen.paper, chosen.record).count(identity) > 1 then
            Left("choose one of paper, live or public recording")
          else Right(chosen)
        case "--no-analysis" :: tail => loop(tail, chosen, hasCoin)
        case "--mono" :: tail        => loop(tail, chosen.copy(mono = true), hasCoin)
        case "--trade" :: tail       => loop(tail, chosen.copy(trade = true), hasCoin)
        case "--paper" :: tail       => loop(tail, chosen.copy(paper = true), hasCoin)
        case "--record" :: tail      => loop(tail, chosen.copy(record = true), hasCoin)
        case "--chime" :: tail if chosen.audio == "voice" =>
          loop(tail, chosen.copy(audio = "chime"), hasCoin)
        case "--mute" :: tail if chosen.audio == "voice" =>
          loop(tail, chosen.copy(audio = "mute"), hasCoin)
        case "--config" :: path :: tail if !path.startsWith("--") && chosen.config.isEmpty =>
          loop(tail, chosen.copy(config = Some(path)), hasCoin)
        case "--state" :: path :: tail if !path.startsWith("--") && chosen.stateDirectory.isEmpty =>
          loop(tail, chosen.copy(stateDirectory = Some(path)), hasCoin)
        case "--config" :: _ => Left("give one settings path after --config")
        case "--state" :: _  => Left("give one local state directory after --state")
        case flag :: _ if flag.startsWith("--")                  => Left(s"unknown flag $flag")
        case coin :: tail if !hasCoin && State.validMarket(coin) =>
          loop(tail, chosen.copy(coin = coin), true)
        case extra :: _ => Left(s"unexpected argument $extra")
    loop(args.toList, Args("XMR", false), false)

  def style(args: Args, env: Map[String, String]): Style =
    if args.mono || env.contains("NO_COLOR") then Style.mono else Style.colour

  def configPath(args: Args, home: Path): Option[Path] =
    args.config.map(Path.of(_)).orElse {
      val path = home.resolve(".config/desk/config.json")
      Option.when(Files.exists(path, LinkOption.NOFOLLOW_LINKS))(path)
    }

  def publicSources(config: TradingConfig): (SocketConnector, EndpointRest) =
    (new SocketConnector(config.socketUrl), new EndpointRest(config.baseUrl + "/info"))

  def nextPromptOpen(promptOpen: Boolean, command: Command): Boolean = command match
    case Command.StartMarketPrompt                   => true
    case Command.PromptSubmit | Command.PromptCancel => false
    case _                                           => promptOpen

  def sizeEvent(last: (Int, Int), size: (Int, Int)): Option[Event] =
    if size == last then None else Some(Event.Resize(size._1, size._2))

  def queueInput(
      command: Command,
      queue: LinkedBlockingQueue[Event],
      pendingPrompts: AtomicInteger
  ): Unit =
    command match
      case Command.PromptSubmit | Command.PromptCancel =>
        val _ = pendingPrompts.incrementAndGet()
      case _ => ()
    queue.put(Event.Key(command))

  def handleTrade(
      command: Command,
      snapshot: Snapshot,
      promptOpen: Boolean,
      size: (Int, Int),
      view: TradingView,
      shownCoin: Option[String],
      pendingPrompts: Int,
      target: Option[(Side, BigDecimal)],
      submit: (OrderIntent, String) => Boolean
  ): (Boolean, Option[(Side, BigDecimal)]) =
    val visible = size._1 >= Layout.minCols && size._2 >= Layout.minRows
    val outsidePrompt = !promptOpen && snapshot.prompt.isEmpty &&
      snapshot.marketPending.isEmpty && pendingPrompts == 0
    val matching = shownCoin.contains(snapshot.coin) && view.coin == snapshot.coin
    val permitted = visible && outsidePrompt && matching && !snapshot.quit &&
      view.ready && !view.pending
    command match
      case Command.StartMarketPrompt | Command.PromptCancel | Command.PromptSubmit =>
        (false, None)
      case Command.CancelTarget       => (true, None)
      case Command.PlaceOrder(intent) =>
        if permitted && target.isEmpty then
          val _ = submit(intent, snapshot.coin)
        (true, target)
      case Command.SelectTarget(side) =>
        if visible && outsidePrompt && matching && !snapshot.quit && !view.pending then
          val selected = for
            rules <- view.rules
            price <- view.reference(side)
            if Trading.validPrice(price, rules)
          yield (side, price)
          (true, selected.orElse(target))
        else (true, target)
      case Command.SubmitTarget =>
        val accepted = target.exists { (side, price) =>
          permitted && submit(
            OrderIntent(side, OrderKind.Target, target = Some(price)),
            snapshot.coin
          )
        }
        (true, if accepted then None else target)
      case Command.ZoomIn | Command.ZoomOut if target.nonEmpty =>
        val moved = for
          selected <- target
          rules <- view.rules
          price <- Trading.nextPrice(selected._2, rules, command == Command.ZoomIn)
        yield (selected._1, price)
        (true, moved.orElse(target))
      case Command.ScrubBack | Command.ScrubForward if target.nonEmpty => (true, target)
      case _                                                           => (false, target)

  def main(argv: Array[String]): Unit =
    parseArgs(argv.toSeq) match
      case Left(message) =>
        System.err.println(message)
        System.err.println(usage)
        sys.exit(64)
      case Right(args) => run(args)

  private def daemon(name: String)(body: => Unit): Thread =
    val thread = new Thread(
      () => {
        try body
        catch case _: InterruptedException => Thread.currentThread().interrupt()
      },
      name
    )
    thread.setDaemon(true)
    thread.start()
    thread

  def startFold(
      queue: LinkedBlockingQueue[Event],
      state: AtomicReference[Snapshot],
      feed: MarketFeed,
      now: () => Long,
      onMarket: String => Unit = _ => (),
      onPromptDone: () => Unit = () => (),
      onFold: (Snapshot, Event, Snapshot) => Unit = (_, _, _) => ()
  ): Vector[Thread] =
    val switches = new LinkedBlockingQueue[Either[(Long, String), String]]()
    val market = daemon("market") {
      while true do
        switches.take() match
          case Right(coin)   => feed.setMarket(coin)
          case Left(request) =>
            if state.get().marketPending.contains(request) && !state.get().quit then
              queue.put(Event.MarketChecked(request, feed.checkMarket(request._2)))
    }
    val fold = daemon("fold") {
      var requestId = 0L
      while true do
        val event = queue.take()
        val before = state.get()
        val after = event match
          case Event.Key(Command.PromptSubmit) =>
            before.prompt match
              case Some(name) if State.validMarket(name) && name != before.coin =>
                requestId += 1
                before.copy(
                  prompt = None,
                  marketPending = Some((requestId, name)),
                  marketNotice = None
                )
              case name =>
                State
                  .reduce(before, event, now())
                  .copy(
                    marketPending = None,
                    marketNotice = Option.when(
                      name.exists(n => n.nonEmpty && !State.validMarket(n))
                    )("Invalid market name. Press m to retry")
                  )
          case Event.MarketChecked(request, result) =>
            if before.marketPending.contains(request) && !before.quit then
              result match
                case Left(reason) => before.copy(marketPending = None, marketNotice = Some(reason))
                case Right(_)     =>
                  State
                    .reduce(
                      before.copy(prompt = Some(request._2)),
                      Event.Key(Command.PromptSubmit),
                      now()
                    )
                    .copy(marketPending = None, marketNotice = None)
            else before
          case Event.Key(Command.StartMarketPrompt | Command.PromptCancel | Command.Quit) =>
            State.reduce(before, event, now()).copy(marketPending = None, marketNotice = None)
          case Event.Key(Command.CancelTarget) if before.marketPending.nonEmpty =>
            before.copy(marketPending = None, marketNotice = None)
          case _ => State.reduce(before, event, now())
        if after.coin != before.coin then onMarket(after.coin)
        state.set(after)
        onFold(before, event, after)
        event match
          case Event.Key(Command.PromptSubmit | Command.PromptCancel) => onPromptDone()
          case _                                                      => ()
        if after.coin != before.coin then
          val _ = switches.offer(Right(after.coin))
        if after.marketPending != before.marketPending then
          after.marketPending.foreach(request => { val _ = switches.offer(Left(request)) })
    }
    Vector(market, fold)

  def startTradingFold(
      queue: LinkedBlockingQueue[Event],
      state: AtomicReference[Snapshot],
      feed: MarketFeed,
      now: () => Long,
      pendingPrompts: AtomicInteger,
      targetLock: Object,
      target: AtomicReference[Option[(Side, BigDecimal)]],
      select: String => Unit
  ): Vector[Thread] =
    startFold(
      queue,
      state,
      feed,
      now,
      coin =>
        targetLock.synchronized {
          target.set(None)
          select(coin)
        },
      () => {
        val _ = pendingPrompts.decrementAndGet()
      }
    )

  def writeMarketFrame(
      coin: String,
      size: (Int, Int),
      shownCoin: AtomicReference[Option[String]],
      output: () => Unit,
      complete: Boolean = true
  ): Unit =
    val visible = size._1 >= Layout.minCols && size._2 >= Layout.minRows && complete
    shownCoin.set(None)
    output()
    shownCoin.set(Option.when(visible)(coin))

  def run(
      args: Args,
      sources: TradingConfig => (Connector, Rest) = publicSources,
      createWorker: (TradingConfig, Boolean, String, Path) => TradingWorker = TradingWorker.create,
      queue: LinkedBlockingQueue[Event] = new LinkedBlockingQueue[Event]()
  ): Unit =
    WorkspaceApp.run(args, sources, createWorker, queue)
