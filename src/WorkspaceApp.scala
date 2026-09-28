package desk

import java.nio.file.Path
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.util.Try

object WorkspaceApp:
  private[desk] final class Restoration(closeAll: () => Unit, terminal: () => Unit):
    @volatile private var restored = false
    def started: Boolean = restored
    def restore(): Boolean = synchronized {
      if restored then false
      else
        restored = true
        try closeAll()
        finally terminal()
        true
    }

  private[desk] def failureHandler(
      restoration: Restoration,
      mainThread: Thread,
      record: (Thread, Throwable) => Unit,
      report: String => Unit,
      exit: Int => Unit
  ): Thread.UncaughtExceptionHandler = (thread, error) =>
    record(thread, error)
    def stop(): Unit =
      report("Desk stopped: " + error.getClass.getSimpleName)
      exit(1)
    if thread eq mainThread then
      var restored = false
      try restored = !restoration.started && restoration.restore()
      finally if restored || !restoration.started then stop()
    else if !restoration.started && restoration.restore() then stop()

  private[desk] def closeAll(
      outlook: AutoCloseable,
      announcements: AutoCloseable,
      recorder: Option[AutoCloseable],
      worker: AutoCloseable,
      diagnostics: AutoCloseable
  ): Unit =
    outlook.close()
    announcements.close()
    recorder.foreach(_.close())
    worker.close()
    diagnostics.close()

  private[desk] def enterTerminal(
      enter: () => String,
      closeAll: () => Unit
  ): scala.util.Try[String] =
    Try(enter()).recoverWith { case error =>
      closeAll()
      scala.util.Failure(error)
    }

  private[desk] def openArchive(
      directory: Path,
      seed: Snapshot
  ): (Try[Snapshot], Option[MarketArchive]) =
    val archive = Try(MarketArchive.open(directory)).toOption
    val recovered = Try(archive.fold(MarketArchive.restore(directory, seed))(_.restore(seed)))
    (recovered, archive)

  private[desk] def collectExecution(
      inputLock: Object,
      controller: DeskController,
      context: () => (Snapshot, TradingView)
  ): (Snapshot, TradingView) = inputLock.synchronized {
    val result = context()
    controller.orders(result._2)
    result
  }

  private[desk] def drawFrame(
      market: Snapshot,
      view: TradingView,
      panel: PanelView,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      outlook: OutlookWorker,
      logsUnavailable: Boolean = false
  ): WorkspaceFrame =
    val visible = outlook.forDisplay(panel.outlook, market, now)
    Workspace.draw(
      market,
      view,
      panel.copy(outlook = visible),
      cols,
      rows,
      now,
      zone,
      style,
      logsUnavailable
    )

  private[desk] def announce(
      alert: Announcement,
      sound: Announcements
  ): Unit =
    val _ = sound.offer(alert)

  private[desk] def enqueueInput(
      command: Command,
      size: (Int, Int),
      queue: LinkedBlockingQueue[Event],
      pendingPrompts: AtomicInteger
  ): Unit =
    queue.put(Event.Resize(size._1, size._2))
    Main.queueInput(command, queue, pendingPrompts)

  private[desk] def dispatchInput(
      command: Command,
      controller: DeskController,
      context: () => (Snapshot, TradingView),
      size: (Int, Int),
      promptOpen: Boolean
  ): Boolean =
    val (snapshot, view) = context()
    controller.handle(command, snapshot, view, size, promptOpen)

  private[desk] def measureInput(command: Command, measure: () => (Int, Int)): Unit =
    command match
      case Command.PromptChar(_) | Command.PromptBackspace | Command.PointerPress(_, _, _) |
          Command.ToggleMute | Command.ToggleCandles =>
        ()
      case Command.Pointer(_, _, button) if button != 0 => ()
      case _                                            => { val _ = measure() }

  private def background(name: String)(body: => Unit): Thread =
    val thread = new Thread(
      () => {
        try body
        catch case _: InterruptedException => ()
      },
      name
    )
    thread.setDaemon(true)
    thread.start()
    thread

  private def publicLoops(feed: Feed): Vector[Thread] = Vector(
    background("feed")(feed.runForever(ms => Thread.sleep(ms))),
    background("heartbeat") {
      while true do
        Thread.sleep(Feed.heartbeatEveryMs)
        feed.heartbeat()
    },
    background("poll") {
      while true do
        feed.poll()
        Thread.sleep(500L)
    },
    background("coarse") {
      while true do
        feed.pollCoarse()
        Thread.sleep(2000L)
    },
    background("history-repair") {
      while true do feed.awaitHistoryRepair()
    }
  )

  private def record(
      args: Main.Args,
      config: TradingConfig,
      directory: Path,
      connector: Connector,
      rest: Rest
  ): Unit =
    val now = () => System.currentTimeMillis()
    val recorder = new PublicRecorder(MarketArchive.open(directory.resolve("public")), now)
    // Capture-only queue discards display events. Recording has its own bounded
    // queue and never constructs a trading worker or accesses an account.
    val sink = new java.util.AbstractQueue[Event]:
      def iterator(): java.util.Iterator[Event] = java.util.Collections.emptyIterator()
      def size(): Int = 0
      def peek(): Event = null
      def poll(): Event = null
      def offer(event: Event): Boolean = true
    val feed = new Feed(connector, rest, sink, now, args.coin, State.refCoins, recorder.observe)
    recorder.start()
    val threads = publicLoops(feed)
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      threads.foreach(_.interrupt())
      recorder.close()
    }))
    val never = new CountDownLatch(1)
    while !never.await(10L, TimeUnit.SECONDS) do
      if recorder.status.contains("failed") then
        throw new IllegalStateException("Public capture stopped. Saved files were preserved")

  def run(
      args: Main.Args,
      sources: TradingConfig => (Connector, Rest),
      createWorker: (TradingConfig, Boolean, String, Path) => TradingWorker,
      queue: LinkedBlockingQueue[Event]
  ): Unit =
    val home = Path.of(System.getProperty("user.home"))
    val config = TradingConfig.load(Main.configPath(args, home)) match
      case Right(value)  => value
      case Left(message) =>
        System.err.println(message)
        sys.exit(64)
    val directory = args.stateDirectory
      .map(Path.of(_))
      .getOrElse(home.resolve(".local/state/desk"))
      .toAbsolutePath
      .normalize()
    val (connector, rest) = sources(config)
    if args.record then record(args, config, directory, connector, rest)
    else
      // Keep construction before local runtime work for the injected offline
      // runner. Its factory rejects any attempt to enable real trading.
      val initialLive = createWorker(config, args.trade, args.coin, directory)
      val now = () => System.currentTimeMillis()
      val diagnostics = Diagnostics.open(directory, now, config.quoteAgeMs)
      val monotonic = () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime())
      val mode = if args.trade then "LIVE" else if args.paper then "PAPER" else "VIEW"
      val worker = new ExecutionRouter(
        config,
        mode,
        args.coin,
        initialLive,
        coin => createWorker(config, true, coin, directory),
        rest,
        directory,
        now,
        monotonic
      )
      val publicDirectory = directory.resolve("public")
      val seed = Snapshot.initial(args.coin, now(), State.refCoins)
      val (recovered, archive) = openArchive(publicDirectory, seed)
      val state = new AtomicReference(recovered.getOrElse(seed))
      val recorder = archive.map(new PublicRecorder(_, now))
      val observe: Event => Unit = event => {
        worker.observe(event)
        recorder.foreach(_.observe(event))
      }
      // The venue feed always runs, for the references. When the chosen market is
      // on-chain it follows the first reference so nothing is asked of the venue for
      // a name it does not list.
      val venueCoin =
        if config.onchainMarket(args.coin).nonEmpty then State.refCoins.head
        else args.coin
      val feed = new Feed(connector, rest, queue, now, venueCoin, State.refCoins, observe)
      val clients = scala.collection.concurrent.TrieMap.empty[String, HttpRpc]
      // The trading workers follow the venue only, so on-chain events reach the recorder alone.
      val onchain = Option.when(config.onchain.nonEmpty)(
        new OnchainFeed(
          config.onchain,
          url => clients.getOrElseUpdate(url, new HttpRpc(url)),
          queue,
          now,
          args.coin,
          event => recorder.foreach(_.observe(event))
        )
      )
      val routed: MarketFeed =
        onchain.fold[MarketFeed](feed)(o => new CompositeFeed(feed, o, () => state.get().coin))
      val announcements = Main.announcements(args, NativeAnnouncementOutput.create())
      val controller = new DeskController(
        PanelView(
          mode = mode,
          sizes = config.sizes,
          muted = args.audio == "mute"
        ),
        now,
        worker.preview,
        worker.submitReviewed,
        worker.switchMode,
        announcements.setMuted,
        text =>
          Try(OrderResultLog.append(directory, now(), text)).failed.foreach(error =>
            diagnostics.failure(DiagnosticPart.OrderResults, error)
          )
      )
      val outlook = new OutlookWorker(() => state.get(), now, controller.outlook)
      val alerts = new AlertPolicy
      def closeAll(): Unit = WorkspaceApp.closeAll(
        outlook,
        announcements,
        recorder,
        worker,
        diagnostics
      )
      val saved = enterTerminal(() => Term.enterRaw(), () => closeAll()).getOrElse {
        System.err.println("desk needs a terminal")
        sys.exit(1)
      }
      print(Screen.esc + "[?1000h" + Screen.esc + "[?1006h")
      Console.flush()
      val restoration = new Restoration(
        () => closeAll(),
        () => {
          print(Screen.esc + "[?1006l" + Screen.esc + "[?1000l")
          Term.restore(saved)
        }
      )
      def restore(): Unit = { restoration.restore(); () }
      Runtime.getRuntime.addShutdownHook(new Thread(() => restore()))
      Thread.setDefaultUncaughtExceptionHandler(
        failureHandler(
          restoration,
          Thread.currentThread(),
          (thread, error) => diagnostics.failure(Diagnostics.component(thread.getName), error),
          message => System.err.println(message),
          status => sys.exit(status)
        )
      )
      val actualSize = new AtomicReference(Term.size())
      val pendingPrompts = new AtomicInteger(0)
      val inputLock = new Object
      val dimensions = new TerminalSize(
        () => Term.size(),
        measured =>
          inputLock.synchronized {
            if measured != actualSize.get() then controller.invalidate()
            actualSize.set(measured)
          }
      )
      worker.start()
      diagnostics.start()
      recorder.foreach(_.start())
      outlook.start()
      announcements.start()
      val alertThread = background("market-alerts") {
        while !state.get().quit do
          val time = now()
          val found = alerts.market(state.get(), time)
          found.foreach(announce(_, announcements))
          Thread.sleep(250L)
      }
      val publicThreads = publicLoops(feed) ++ onchain.toVector.map(o =>
        background("onchain") {
          while true do
            o.poll()
            Thread.sleep(Onchain.pollMs)
        }
      )
      val foldThreads = Main.startFold(
        queue,
        state,
        routed,
        now,
        coin =>
          inputLock.synchronized {
            controller.resetMarket()
            worker.select(coin)
          },
        () => { val _ = pendingPrompts.decrementAndGet() },
        diagnostics.folded
      )
      val keys = background("keys") {
        var pending = Vector.empty[Int]
        var marketPrompt = false
        var escapeAt = 0L
        var quitting = false
        def emit(command: Command): Unit =
          val began = System.nanoTime()
          measureInput(command, () => dimensions.read())
          inputLock.synchronized {
            val measured = actualSize.get()
            val handled = dispatchInput(
              command,
              controller,
              () => (state.get(), worker.view),
              measured,
              marketPrompt || pendingPrompts.get() > 0
            )
            if !handled then
              marketPrompt = Main.nextPromptOpen(marketPrompt, command)
              enqueueInput(command, measured, queue, pendingPrompts)
            if command == Command.Quit then quitting = true
          }
          diagnostics.slow(DiagnosticPart.Input, System.nanoTime() - began)
        while !quitting && !state.get().quit do
          if System.in.available() > 0 then
            val byte = System.in.read()
            if byte < 0 then emit(Command.Quit)
            else
              val (next, command) = Keys.step(
                pending,
                byte,
                marketPrompt,
                mouseEnabled = true
              )
              pending = next
              if pending == Vector(27) then escapeAt = System.nanoTime()
              command.foreach(emit)
          else
            if pending == Vector(27) && System.nanoTime() - escapeAt >= Main.escapeWaitNanos then
              val (next, command) = Keys.expire(pending, marketPrompt)
              pending = next
              command.foreach(emit)
            Thread.sleep(10L)
      }
      val terminal = new Term.Writer()
      val zone = java.time.ZoneId.systemDefault()
      val style = Main.style(args, sys.env)
      var size = inputLock.synchronized {
        val measured = actualSize.get()
        if measured != (0, 0) then queue.put(Event.Resize(measured._1, measured._2))
        measured
      }
      var sizeAt = 0L
      try
        while !state.get().quit do
          val time = now()
          if time - sizeAt >= 500L then
            val _ = dimensions.read()
            sizeAt = time
          inputLock.synchronized {
            val measured = actualSize.get()
            if measured != size then
              size = measured
              Main.sizeEvent((state.get().cols, state.get().rows), size).foreach(queue.put)
          }
          val (market, execution) =
            collectExecution(inputLock, controller, () => (state.get(), worker.view))
          val view = execution.forMarket(market.coin)
          diagnostics.execution(controller.panel.mode, view)
          val building = System.nanoTime()
          val frame =
            drawFrame(
              market,
              view,
              controller.panel,
              size._1,
              size._2,
              time,
              zone,
              style,
              outlook,
              diagnostics.status.nonEmpty
            )
          diagnostics.slow(DiagnosticPart.Build, System.nanoTime() - building)
          val waiting = System.nanoTime()
          var lockWait = 0L
          var terminalTime = 0L
          var published = false
          inputLock.synchronized {
            lockWait = System.nanoTime() - waiting
            controller.invalidate()
            val writing = System.nanoTime()
            terminal.write(frame.lines, (frame.cols, frame.rows))
            terminalTime = System.nanoTime() - writing
            if state.get().coin == frame.coin && actualSize.get() == (frame.cols, frame.rows) then
              controller.publish(frame)
              published = true
          }
          diagnostics.slow(DiagnosticPart.FrameLock, lockWait)
          diagnostics.slow(DiagnosticPart.Terminal, terminalTime)
          if published then diagnostics.presented(market, frame.cols, frame.rows)
          Thread.sleep(25L)
      catch
        case error: Exception =>
          diagnostics.failure(DiagnosticPart.Render, error)
          throw error
      finally
        (publicThreads ++ foldThreads ++ Vector(keys, alertThread)).foreach(_.interrupt())
        restore()
      sys.exit(0)
