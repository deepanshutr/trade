package desk

import java.nio.file.{Files, LinkOption, Path}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

object Main:
  final case class Args(
      coin: String,
      mono: Boolean,
      trade: Boolean = false,
      config: Option[String] = None
  )
  val usage = "usage: desk [COIN] [--mono] [--trade] [--config PATH]"
  val escapeWaitNanos = 40_000_000L

  def parseArgs(args: Seq[String]): Either[String, Args] =
    def loop(
        rest: List[String],
        coin: Option[String],
        mono: Boolean,
        trade: Boolean,
        config: Option[String]
    ): Either[String, Args] = rest match
      case Nil               => Right(Args(coin.getOrElse("XMR"), mono, trade, config))
      case "--mono" :: tail  => loop(tail, coin, true, trade, config)
      case "--trade" :: tail => loop(tail, coin, mono, true, config)
      case "--config" :: path :: tail if !path.startsWith("--") && config.isEmpty =>
        loop(tail, coin, mono, trade, Some(path))
      case "--config" :: _                    => Left("give one settings path after --config")
      case flag :: _ if flag.startsWith("--") => Left(s"unknown flag $flag")
      case c :: tail if coin.isEmpty && c.matches("[A-Za-z0-9][A-Za-z0-9:._-]{0,39}") =>
        loop(tail, Some(c), mono, trade, config)
      case extra :: _ => Left(s"unexpected argument $extra")
    loop(args.toList, None, false, false, None)

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
    val outsidePrompt = !promptOpen && snapshot.prompt.isEmpty && pendingPrompts == 0
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
      feed: Feed,
      now: () => Long,
      onMarket: String => Unit = _ => (),
      onPromptDone: () => Unit = () => ()
  ): Vector[Thread] =
    val switches = new LinkedBlockingQueue[String]()
    val market = daemon("market") {
      while true do feed.setMarket(switches.take())
    }
    val fold = daemon("fold") {
      while true do
        val event = queue.take()
        val before = state.get()
        val after = State.reduce(before, event, now())
        if after.coin != before.coin then onMarket(after.coin)
        state.set(after)
        event match
          case Event.Key(Command.PromptSubmit | Command.PromptCancel) => onPromptDone()
          case _                                                      => ()
        if after.coin != before.coin then
          val _ = switches.offer(after.coin)
    }
    Vector(market, fold)

  def startTradingFold(
      queue: LinkedBlockingQueue[Event],
      state: AtomicReference[Snapshot],
      feed: Feed,
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
    val home = Path.of(System.getProperty("user.home"))
    val config = TradingConfig.load(configPath(args, home)) match
      case Right(value)  => value
      case Left(message) =>
        System.err.println(message)
        sys.exit(64)
    val st = style(args, sys.env)
    val now = () => System.currentTimeMillis()
    val initial = Snapshot.initial(args.coin, now(), config.references)
    val state = new AtomicReference(initial)
    val shownCoin = new AtomicReference(Option.empty[String])
    val pendingPrompts = new AtomicInteger(0)
    val target = new AtomicReference(Option.empty[(Side, BigDecimal)])
    val targetLock = new Object
    val (connector, rest) = sources(config)
    val worker =
      createWorker(config, args.trade, args.coin, home.resolve(".local/state/desk"))
    val feed = new Feed(connector, rest, queue, now, args.coin, config.references, worker.observe)
    val zone = java.time.ZoneId.systemDefault()
    val saved = scala.util.Try(Term.enterRaw()).getOrElse {
      worker.close()
      System.err.println("desk needs a terminal")
      sys.exit(1)
    }
    val restored = new AtomicBoolean(false)
    def restore(): Unit = if restored.compareAndSet(false, true) then
      try worker.close()
      finally Term.restore(saved)
    Runtime.getRuntime.addShutdownHook(new Thread(() => restore()))
    Thread.setDefaultUncaughtExceptionHandler { (_, error) =>
      restore()
      System.err.println(error)
      sys.exit(1)
    }
    val actualSize = new AtomicReference(Term.size())
    worker.start()
    daemon("feed")(feed.runForever(ms => Thread.sleep(ms)))
    daemon("heartbeat") {
      while true do
        Thread.sleep(50_000)
        feed.heartbeat()
    }
    daemon("poll") {
      while true do
        feed.poll()
        Thread.sleep(500)
    }
    daemon("coarse") {
      while true do
        feed.pollCoarse()
        Thread.sleep(2000)
    }
    daemon("keys") {
      var pending = Vector.empty[Int]
      var promptOpen = false
      var escapeAt = 0L
      var quitting = false
      def emit(command: Command): Unit = targetLock.synchronized {
        val (handled, selected) = handleTrade(
          command,
          state.get(),
          promptOpen,
          actualSize.get(),
          worker.view,
          shownCoin.get(),
          pendingPrompts.get(),
          target.get(),
          worker.submit
        )
        target.set(selected)
        promptOpen = nextPromptOpen(promptOpen, command)
        if command == Command.Quit then quitting = true
        if !handled then queueInput(command, queue, pendingPrompts)
      }
      while !quitting && !state.get().quit do
        if System.in.available() > 0 then
          val byte = System.in.read()
          if byte < 0 then emit(Command.Quit)
          else
            val (next, command) = Keys.step(pending, byte, promptOpen)
            pending = next
            if pending == Vector(27) then escapeAt = System.nanoTime()
            command.foreach(emit)
        else
          if pending == Vector(27) && System.nanoTime() - escapeAt >= escapeWaitNanos then
            val (next, command) = Keys.expire(pending, promptOpen)
            pending = next
            command.foreach(emit)
          Thread.sleep(10)
    }
    val _ = startTradingFold(
      queue,
      state,
      feed,
      now,
      pendingPrompts,
      targetLock,
      target,
      worker.select
    )
    var last: Snapshot = null
    var lastView: TradingView = null
    var lastTarget = Option.empty[(Side, BigDecimal)]
    var lastFrameAt = 0L
    var size = actualSize.get()
    if size != (0, 0) then queue.put(Event.Resize(size._1, size._2))
    var sizeAt = now()
    try
      while !state.get().quit do
        val time = now()
        if time - sizeAt >= 1000 then
          val polled = Term.size()
          actualSize.set(polled)
          if polled != (0, 0) then
            sizeEvent(size, polled).foreach(queue.put)
            size = polled
          sizeAt = time
        val snapshot = state.get()
        val view = worker.view
        val selected = target.get()
        if (snapshot ne last) || view != lastView || selected != lastTarget || time - lastFrameAt >= 1000
        then
          val matching = view.coin == snapshot.coin
          val shown = view.forMarket(snapshot.coin)
          val preview: OrderIntent => Either[String, OrderPlan] =
            if matching then worker.preview else _ => Left("changing market")
          val footer = TradingScreen.rows(shown, selected, preview, config.sizes, size._1)
          writeMarketFrame(
            snapshot.coin,
            size,
            shownCoin,
            () => Term.write(Screen.render(snapshot, size._1, size._2, time, zone, st, footer)),
            complete = footer.size == Layout.footerRows && footer.forall(_.length <= size._1)
          )
          last = snapshot
          lastView = view
          lastTarget = selected
          lastFrameAt = time
        Thread.sleep(100)
    finally restore()
    sys.exit(0)
