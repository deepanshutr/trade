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

  private def marketRefusal(name: String): Option[String] =
    Option.when(name.contains(":"))("Colon markets are not supported.")

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
        case ("--chime" | "--mute") :: _ => Left("choose one of chime or mute")
        case "--config" :: path :: tail if !path.startsWith("--") && chosen.config.isEmpty =>
          loop(tail, chosen.copy(config = Some(path)), hasCoin)
        case "--state" :: path :: tail if !path.startsWith("--") && chosen.stateDirectory.isEmpty =>
          loop(tail, chosen.copy(stateDirectory = Some(path)), hasCoin)
        case "--config" :: _ => Left("give one settings path after --config")
        case "--state" :: _  => Left("give one local state directory after --state")
        case flag :: _ if flag.startsWith("--")                  => Left(s"unknown flag $flag")
        case coin :: tail if !hasCoin && State.validMarket(coin) =>
          marketRefusal(coin) match
            case Some(message) => Left(message)
            case None          => loop(tail, chosen.copy(coin = coin), true)
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

  def main(argv: Array[String]): Unit =
    parseArgs(argv.toSeq) match
      case Left(message) =>
        System.err.println(message)
        System.err.println(usage)
        sys.exit(64)
      case Right(args) => run(args)

  private def daemon(name: String)(body: => Unit): Thread =
    val thread = Threads.daemon(name)(body)
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
            val refusal = before.prompt.flatMap(marketRefusal)
            before.prompt match
              case _ if refusal.nonEmpty =>
                before.copy(
                  prompt = None,
                  marketPending = None,
                  marketNotice = refusal.map(_ + " Press m to retry")
                )
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
          case Event.Key(Command.Cancel) if before.marketPending.nonEmpty =>
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

  def run(
      args: Args,
      sources: TradingConfig => (Connector, Rest) = publicSources,
      createWorker: (TradingConfig, Boolean, String, Path) => TradingWorker = TradingWorker.create,
      queue: LinkedBlockingQueue[Event] = new LinkedBlockingQueue[Event]()
  ): Unit =
    WorkspaceApp.run(args, sources, createWorker, queue)
