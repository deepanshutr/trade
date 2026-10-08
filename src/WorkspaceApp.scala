package desk

import java.io.InputStream
import java.nio.file.Path
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*
import scala.util.Try

object WorkspaceApp:
  private[desk] final case class OrderResultHandling(
      handle: (Long, String) => Unit,
      unavailable: () => Boolean
  )

  private[desk] final class Restoration(closeAll: () => Unit, terminal: () => Unit):
    @volatile private var restored = false
    @volatile private var restoreOwner: Thread = null
    def started: Boolean = restored
    def owner: Thread = restoreOwner
    def restore(): Boolean = synchronized {
      if restored then false
      else
        restoreOwner = Thread.currentThread()
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
      try if !restoration.started then restoration.restore()
      finally if restoration.owner eq thread then stop()
    else if !restoration.started && restoration.restore() then stop()

  private[desk] def orderResultFailureHandler(
      diagnostics: Diagnostics,
      reportNotice: Long => Unit,
      markUnavailable: () => Unit
  ): (Long, Throwable) => Unit = (number, error) =>
    diagnostics.failure(DiagnosticPart.OrderResults, error)
    if !error.isInstanceOf[OrderResultLog.SavedRecordFailure] then
      markUnavailable()
      reportNotice(number)

  private[desk] def orderResultHandler(
      writer: OrderResultLog.Writer,
      diagnostics: Diagnostics,
      reportNotice: Long => Unit
  ): OrderResultHandling =
    val unavailable = new java.util.concurrent.atomic.AtomicBoolean(false)
    val failed = orderResultFailureHandler(diagnostics, reportNotice, () => unavailable.set(true))
    OrderResultHandling(
      (number, text) => writer.append(text, error => failed(number, error)),
      () => unavailable.get()
    )

  private[desk] def closeAll(
      outlook: AutoCloseable,
      announcements: AutoCloseable,
      recorder: Option[AutoCloseable],
      worker: AutoCloseable,
      diagnostics: AutoCloseable,
      session: Option[AutoCloseable] = None
  ): Unit =
    outlook.close()
    announcements.close()
    recorder.foreach(_.close())
    worker.close()
    diagnostics.close()
    // The workers hold their own copies and have closed by now.
    session.foreach(_.close())

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

  // The workspace's controller. Every order action it takes goes to the router, which sends it
  // to the worker of the selected mode.
  private[desk] def deskController(
      panel: PanelView,
      now: () => Long,
      worker: ExecutionRouter,
      mute: Boolean => Unit,
      result: (Long, String) => Unit
  ): DeskController =
    new DeskController(
      panel,
      now,
      worker.preview,
      worker.submitReviewed,
      worker.switchMode,
      mute,
      result,
      worker.cancel,
      worker.closeFirst,
      worker.withdrawClose
    )

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
      logsUnavailable: Boolean = false,
      orderResultsUnavailable: Boolean = false
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
      logsUnavailable,
      orderResultsUnavailable
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

  // The workspace's reader of the terminal. The time of every byte is kept, so a lone escape or
  // a held first key expires by the time since the last byte, whatever state it is in.
  private[desk] def readKeys(
      input: InputStream,
      running: () => Boolean,
      promptOpen: () => Boolean,
      bindings: () => Bindings,
      emit: Command => Unit,
      nanoTime: () => Long = () => System.nanoTime(),
      idle: () => Unit = () => Thread.sleep(10L)
  ): Unit =
    var pending = Vector.empty[Int]
    var lastByte = nanoTime()
    while running() do
      if input.available() > 0 then
        val byte = input.read()
        lastByte = nanoTime()
        if byte < 0 then emit(Command.Quit)
        else
          val (next, command) =
            Keys.step(pending, byte, promptOpen(), mouseEnabled = true, bindings = bindings())
          pending = next
          command.foreach(emit)
      else
        if pending.nonEmpty then
          val (next, command) = Keys.expire(pending, promptOpen(), nanoTime() - lastByte)
          pending = next
          command.foreach(emit)
        idle()

  // The --config words the approval step takes. The vault's approval works only with a settings
  // file inside ~/.config/desk.
  private[desk] def approveConfig(args: Main.Args, home: Path): Either[String, Seq[String]] =
    args.config.map(Path.of(_).toAbsolutePath.normalize()) match
      case None => Right(Nil)
      case Some(path) if path.startsWith(home.resolve(".config/desk").toAbsolutePath.normalize()) =>
        Right(Seq("--config", path.toString))
      case Some(_) => Left("Approval needs the settings file in ~/.config/desk")

  // Writes the bindings. The folder rules of LocalFiles name the folder. Nothing else is shown.
  private[desk] def saveBindings(home: Path, chosen: Bindings): Either[String, Unit] =
    val path = Bindings.path(home)
    Try(Bindings.save(path, chosen)).toEither.left.map { error =>
      Option(error.getMessage)
        .map(_.stripPrefix("requirement failed: "))
        .filter(_.startsWith("local storage"))
        .fold("The key bindings could not be written")(reason => s"$reason (${path.getParent})")
    }

  private def mouse(on: Boolean): String =
    if on then Screen.esc + "[?1000h" + Screen.esc + "[?1006h"
    else Screen.esc + "[?1006l" + Screen.esc + "[?1000l"

  private def background(name: String)(body: => Unit): Thread =
    val thread = Threads.daemon(name)(body)
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
    val feed = new Feed(connector, rest, sink, now, args.coin, recorder.observe)
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
      queue: LinkedBlockingQueue[Event],
      keyHooks: KeyStartup.Hooks
  ): Unit =
    val home = keyHooks.home
    val loaded = TradingConfig.load(Main.configPath(args, home)) match
      case Right(value)  => value
      case Left(message) =>
        System.err.println(message)
        sys.exit(64)
    val directory = args.stateDirectory
      .map(Path.of(_))
      .getOrElse(home.resolve(".local/state/desk"))
      .toAbsolutePath
      .normalize()
    // A refused bindings file stops the start like a refused settings file, before any Keychain
    // read. The key screen child and the recorder read no keys from the workspace.
    val bindings =
      if args.keys || args.record then Bindings.defaults
      else Bindings.load(Bindings.path(home)).fold(m => keyHooks.stop(64, Some(m)), identity)
    // The key step comes first. The key screen never touches the feed, the event queue or
    // diagnostics, and the single Keychain read happens before the workspace starts.
    val prepared = KeyStartup.prepare(args, loaded, directory, keyHooks)
    val config = prepared.config
    val session = prepared.session
    val bound = new AtomicReference(bindings)
    val (connector, rest) = sources(config)
    if args.record then record(args, config, directory, connector, rest)
    else
      // With a session every worker takes its own copy of the one key read. Without one the
      // injected factory builds the workers, which is how the offline runner stays offline.
      val live: (TradingConfig, Boolean, String, Path) => TradingWorker = session match
        case Some(held) =>
          (c, enabled, coin, dir) => keyHooks.worker(c, enabled, coin, dir, () => held.copy())
        case None => createWorker
      // Keep construction before local runtime work for the injected offline
      // runner. Its factory rejects any attempt to enable real trading.
      val initialLive = live(config, args.trade, args.coin, directory)
      val now = () => System.currentTimeMillis()
      val diagnostics = Diagnostics.open(directory, now, config.quoteAgeMs)
      val monotonic = () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime())
      val mode = args.mode
      val worker = new ExecutionRouter(
        config,
        mode,
        args.coin,
        initialLive,
        coin => live(config, true, coin, directory),
        rest,
        directory,
        now,
        monotonic
      )
      val publicDirectory = directory.resolve("public")
      val seed = Snapshot.initial(args.coin, now())
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
      val feed = new Feed(connector, rest, queue, now, venueCoin, observe)
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
      val results: OrderResultLog.Writer = new OrderResultLog.Writer(
        text => OrderResultLog.append(directory, now(), text),
        error => diagnostics.failure(DiagnosticPart.OrderResults, error)
      )
      lazy val orderResults = orderResultHandler(
        results,
        diagnostics,
        number => controller.orderResultNotSaved(number)
      )
      lazy val controller: DeskController = deskController(
        PanelView(
          mode = mode,
          terms = TradeTerms(config.sizes, config.slippage),
          muted = args.audio == "mute",
          bindings = bindings
        ),
        now,
        worker,
        announcements.setMuted,
        orderResults.handle
      )
      // The reload prints the Keychain prompt line over the settings screen, which then repaints.
      val keyReload = new KeyReload(
        args,
        config,
        session,
        directory,
        keyHooks.copy(say = text => {
          print(Screen.esc + "[2J" + Screen.esc + "[H" + text)
          Console.flush()
        }),
        createWorker,
        (next, factory) => worker.replaceLive(next, factory),
        terms => controller.setTerms(terms)
      )
      val outlook = new OutlookWorker(() => state.get(), now, controller.outlook)
      val alerts = new AlertPolicy
      def closeAll(): Unit =
        results.close()
        WorkspaceApp.closeAll(
          outlook,
          announcements,
          recorder,
          worker,
          diagnostics,
          keyReload.session
        )
      val saved = enterTerminal(() => Term.enterRaw(), () => closeAll()).getOrElse {
        System.err.println("desk needs a terminal")
        sys.exit(1)
      }
      print(mouse(true))
      Console.flush()
      val children =
        new SettingsWiring.Children(words => new ProcessBuilder(words.asJava).inheritIO().start())
      val restoration = new Restoration(
        () => closeAll(),
        SettingsWiring.restoring(
          children,
          () => {
            print(mouse(false))
            Term.restore(saved)
          }
        )
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
      // While settings are open nothing is drawn. The fold, the feed and the workers go on.
      val paused = new AtomicBoolean(false)
      val terminal = new Term.Writer()
      val ownerReading = new SettingsScreen.ReadGate
      val javaCommand =
        ProcessHandle
          .current()
          .info()
          .command()
          .orElse(System.getProperty("java.home") + "/bin/java")
      val keyChild =
        SettingsWiring.keyChild(args, javaCommand, System.getProperty("java.class.path"))
      def handOff(command: Seq[String]): Either[String, Int] =
        SettingsWiring.handOff(
          command,
          worker,
          () => keyReload.reload(),
          restoration,
          saved,
          Term.input,
          System.out,
          SettingsWiring.terminal,
          children.start
        )
      // Runs the settings screen on the keys thread. True when it asks Desk to quit.
      def openSettings(): Boolean =
        val context = SettingsWiring.context(
          network = config.network,
          config = args.config.map(Path.of(_)),
          home = home,
          bound = bound,
          controller = controller,
          keyRow = () => keyReload.keyRow(),
          reload = () => keyReload.reload(),
          ownerRead = () =>
            OwnerStatus.read(
              config.network,
              sys.env,
              (command, input) => ProcessRun.systemWithin(command, input, 20L)
            ),
          reading = ownerReading,
          handOff = handOff,
          keyChild = keyChild,
          env = sys.env,
          copy = SettingsWiring.copyAddress,
          livePending = () => worker.livePending,
          size = () => Term.size(),
          now = now,
          approve = approveConfig(args, home),
          activeKey = () => keyReload.activeKey()
        )
        SettingsWiring.show(paused, inputLock, terminal, () => controller.invalidate(), System.out)(
          SettingsScreen.run(context, Term.input, System.out)
        )
      val keys = background("keys") {
        var marketPrompt = false
        var quitting = false
        // True when the controller took the command.
        def handle(command: Command): Boolean =
          val began = System.nanoTime()
          measureInput(command, () => dimensions.read())
          val handled = inputLock.synchronized {
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
            handled
          }
          diagnostics.slow(DiagnosticPart.Input, System.nanoTime() - began)
          handled
        SettingsWiring.keys(
          Term.input,
          () => !quitting && !state.get().quit,
          () => marketPrompt,
          bound,
          handle,
          () => openSettings()
        )
      }
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
          var market = state.get()
          var lockWait = 0L
          var terminalTime = 0L
          var presented = Option.empty[(Int, Int)]
          SettingsWiring.renderPass(paused, () => !state.get().quit, inputLock, Thread.sleep(_))(
            () => {
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
              val (snapshot, execution) =
                collectExecution(inputLock, controller, () => (state.get(), worker.view))
              market = snapshot
              val view = execution.forMarket(snapshot.coin)
              diagnostics.execution(controller.panel.mode, view)
              val building = System.nanoTime()
              val frame =
                drawFrame(
                  snapshot,
                  view,
                  controller.panel,
                  size._1,
                  size._2,
                  time,
                  zone,
                  style,
                  outlook,
                  diagnostics.status.nonEmpty,
                  orderResults.unavailable()
                )
              diagnostics.slow(DiagnosticPart.Build, System.nanoTime() - building)
              frame
            }
          )((frame, waited) => {
            lockWait = waited
            controller.invalidate()
            val writing = System.nanoTime()
            terminal.write(frame.lines, (frame.cols, frame.rows))
            terminalTime = System.nanoTime() - writing
            if state.get().coin == frame.coin && actualSize.get() == (frame.cols, frame.rows) then
              controller.publish(frame)
              presented = Some((frame.cols, frame.rows))
          })
          diagnostics.slow(DiagnosticPart.FrameLock, lockWait)
          diagnostics.slow(DiagnosticPart.Terminal, terminalTime)
          presented.foreach((cols, rows) => diagnostics.presented(market, cols, rows))
          Thread.sleep(25L)
      catch
        case error: Exception =>
          diagnostics.failure(DiagnosticPart.Render, error)
          throw error
      finally
        (publicThreads ++ foldThreads ++ Vector(keys, alertThread)).foreach(_.interrupt())
        restore()
      sys.exit(0)
