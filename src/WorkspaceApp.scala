package desk

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.util.Try

object WorkspaceApp:
  private[desk] def drawFrame(
      market: Snapshot,
      view: TradingView,
      panel: PanelView,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style,
      outlook: OutlookWorker
  ): WorkspaceFrame =
    val visible = outlook.forDisplay(panel.outlook, market, now)
    Workspace.draw(market, view, panel.copy(outlook = visible), cols, rows, now, zone, style)

  private[desk] def announce(
      alert: Announcement,
      controller: DeskController,
      sound: Announcements
  ): Unit =
    controller.announcement(alert)
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
      promptOpen: Boolean,
      measure: () => (Int, Int)
  ): (Boolean, (Int, Int)) =
    val measured = command match
      case Command.PromptChar(_) | Command.PromptText(_) | Command.PromptBackspace => size
      case _                                                                       => measure()
    if measured != size then controller.invalidate()
    val (snapshot, view) = context()
    (controller.handle(command, snapshot, view, measured, promptOpen), measured)

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
    val feed = new Feed(connector, rest, sink, now, args.coin, config.references, recorder.observe)
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
      val history = Try(MessageHistory.open(directory.resolve("messages"))).getOrElse {
        worker.close()
        System.err.println(
          "Desk message storage is unavailable or another desk owns it: " + directory.resolve(
            "messages"
          )
        )
        sys.exit(1)
      }
      val publicDirectory = directory.resolve("public")
      val seed = Snapshot.initial(args.coin, now(), config.references).copy(workspace = true)
      val recovered = Try(MarketArchive.restore(publicDirectory, seed))
      val state = new AtomicReference(recovered.getOrElse(seed))
      val recorder = Try(new PublicRecorder(MarketArchive.open(publicDirectory), now)).toOption
      val observe: Event => Unit = event => {
        worker.observe(event)
        recorder.foreach(_.observe(event))
      }
      // The venue feed always runs, for the references. When the chosen market is
      // on-chain it follows the first reference so nothing is asked of the venue for
      // a name it does not list.
      val venueCoin =
        if config.onchainMarket(args.coin).nonEmpty then
          config.references.headOption.getOrElse("BTC")
        else args.coin
      val feed = new Feed(connector, rest, queue, now, venueCoin, config.references, observe)
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
      val connectedNews = Option.when(args.analysis)(new News.Worker(now))
      val connection =
        Option.when(args.analysis)(CodexConnection.create(directory.resolve("analysis-workspace")))
      val analysisRef = new AtomicReference(Option.empty[AnalysisWorker])
      val announcements = Main.announcements(args, NativeAnnouncementOutput.create())
      val controller = new DeskController(
        PanelView(
          mode = mode,
          sizes = config.sizes,
          fee = config.paperFee,
          messages = history.messages,
          muted = args.audio == "mute",
          soundStatus = if args.audio == "mute" then "Muted" else "Sound on",
          newsStatus = if args.analysis then "News waiting" else "News disabled"
        ),
        now,
        worker.preview,
        worker.submitReviewed,
        question => analysisRef.get().exists(_.ask(question)),
        worker.switchMode,
        history.save,
        announcements.setMuted
      )
      controller.start()
      val outlook = new OutlookWorker(() => state.get(), now, controller.outlook)
      val alerts = new AlertPolicy
      val analysis = new AnalysisWorker(
        () => state.get(),
        () =>
          connectedNews.fold("Connected news disabled. Calendar availability is unknown")(
            _.context
          ),
        (context, canceled, study) =>
          connection.fold[Either[String, ModelReply]](Left("Connected analysis disabled"))(
            _.reply(context, canceled, study)
          ),
        controller.addMessage,
        now,
        automaticEnabled = args.analysis
      )
      analysisRef.set(Some(analysis))
      if recovered.isFailure then
        controller.addMessage(
          "data",
          "Saved public candles could not be loaded. Available exchange history will be requested"
        )
      val saved = Try(Term.enterRaw()).getOrElse {
        analysis.close()
        outlook.close()
        announcements.close()
        connectedNews.foreach(_.close())
        recorder.foreach(_.close())
        controller.close()
        history.close()
        worker.close()
        System.err.println("desk needs a terminal")
        sys.exit(1)
      }
      print(Screen.esc + "[?1000h" + Screen.esc + "[?1006h")
      Console.flush()
      val restored = new AtomicBoolean(false)
      def restore(): Unit =
        if restored.compareAndSet(false, true) then
          try
            analysis.close()
            outlook.close()
            announcements.close()
            connectedNews.foreach(_.close())
            recorder.foreach(_.close())
            worker.close()
            controller.close()
            history.close()
          finally
            print(Screen.esc + "[?1006l" + Screen.esc + "[?1000l")
            Term.restore(saved)
      Runtime.getRuntime.addShutdownHook(new Thread(() => restore()))
      Thread.setDefaultUncaughtExceptionHandler { (_, error) =>
        restore()
        System.err.println("Desk stopped: " + error.getClass.getSimpleName)
        sys.exit(1)
      }
      val actualSize = new AtomicReference(Term.size())
      val pendingPrompts = new AtomicInteger(0)
      val inputLock = new Object
      worker.start()
      recorder.foreach(_.start())
      connectedNews.foreach(_.start())
      analysis.start()
      outlook.start()
      announcements.start()
      val alertThread = background("market-alerts") {
        while !state.get().quit do
          val time = now()
          val found = alerts.market(state.get(), time) ++
            connectedNews.toVector.flatMap(n => alerts.news(n.batches, time))
          found.foreach(announce(_, controller, announcements))
          controller.alertStatus(
            announcements.status,
            connectedNews.fold("News disabled")(_.summary),
            connectedNews.fold("News disabled by --no-analysis")(_.context)
          )
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
        () => { val _ = pendingPrompts.decrementAndGet() }
      )
      val keys = background("keys") {
        var pending = Vector.empty[Int]
        var marketPrompt = false
        var escapeAt = 0L
        var quitting = false
        def emit(command: Command): Unit = inputLock.synchronized {
          val (handled, measured) = dispatchInput(
            command,
            controller,
            () => (state.get(), worker.view),
            actualSize.get(),
            marketPrompt || pendingPrompts.get() > 0,
            () => Term.size()
          )
          actualSize.set(measured)
          if !handled then
            marketPrompt = Main.nextPromptOpen(marketPrompt, command)
            enqueueInput(command, measured, queue, pendingPrompts)
          if command == Command.Quit then quitting = true
        }
        while !quitting && !state.get().quit do
          val focused = marketPrompt || controller.panel.input.nonEmpty
          if System.in.available() > 0 then
            val byte = System.in.read()
            if byte < 0 then emit(Command.Quit)
            else
              val (next, command) = Keys.step(
                pending,
                byte,
                focused,
                mouseEnabled = true,
                textInput = controller.panel.input.nonEmpty
              )
              pending = next
              if pending == Vector(27) then escapeAt = System.nanoTime()
              command.foreach(emit)
          else
            if pending == Vector(27) && System.nanoTime() - escapeAt >= Main.escapeWaitNanos then
              val (next, command) = Keys.expire(pending, focused)
              pending = next
              command.foreach(emit)
            Thread.sleep(10L)
      }
      val zone = java.time.ZoneId.systemDefault()
      val style = Main.style(args, sys.env)
      var size = inputLock.synchronized {
        val measured = actualSize.get()
        if measured != (0, 0) then queue.put(Event.Resize(measured._1, measured._2))
        measured
      }
      var sizeAt = 0L
      var statusAt = 0L
      val orderUpdates = new OrderUpdates
      var captureStatus = "Public capture starting"
      try
        while !state.get().quit do
          val time = now()
          if time - sizeAt >= 500L then
            val measured = Term.size()
            inputLock.synchronized {
              if measured != actualSize.get() then controller.invalidate()
              actualSize.set(measured)
            }
            sizeAt = time
          inputLock.synchronized {
            val measured = actualSize.get()
            if measured != size then
              size = measured
              Main.sizeEvent((state.get().cols, state.get().rows), size).foreach(queue.put)
          }
          val market = state.get()
          val view = worker.view.forMarket(market.coin)
          orderUpdates.collect(view)(controller.addMessage("order", _))
          if time - statusAt >= 2000L then
            captureStatus = recorder.map(_.status).getOrElse {
              val at = Try(
                Files.getLastModifiedTime(publicDirectory.resolve("candles.json")).toMillis
              ).getOrElse(0L)
              if at > 0 && time >= at && time - at <= 30000L then
                "Background capture saved " + (time - at) / 1000L + "s ago"
              else "Public capture unavailable or stale"
            }
            statusAt = time
          val usage =
            connection.flatMap(_.totalTokens).fold("")(n => " last request " + n + " tokens")
          controller.status(
            "Prices: " + routed.status + ". " + captureStatus,
            analysis.status + usage + connectedNews.fold("")(n => ". " + n.status),
            worker.accountLabel
          )
          val frame =
            drawFrame(market, view, controller.panel, size._1, size._2, time, zone, style, outlook)
          inputLock.synchronized {
            controller.invalidate()
            Term.write(frame.lines)
            if state.get().coin == frame.coin && actualSize.get() == (frame.cols, frame.rows) then
              controller.publish(frame)
          }
          Thread.sleep(100L)
      finally
        (publicThreads ++ foldThreads ++ Vector(keys, alertThread)).foreach(_.interrupt())
        restore()
      sys.exit(0)
