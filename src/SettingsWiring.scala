package desk

import java.io.{InputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

// How the running Desk wires settings in. WorkspaceApp.run calls these with the real terminal,
// router, controller and streams. Every step that live trading and Control C rest on lives here,
// so a test can drive it with fakes and record the order.
object SettingsWiring:
  // Leaving raw mode with the saved terminal settings, and entering it again.
  final case class TermOps(restore: String => Unit, enterRaw: () => Unit)

  // Keys still waiting in the terminal are dropped, so none answers the next prompt.
  private def drain(input: InputStream): Unit =
    while input.available() > 0 do
      val _ = input.read()

  // One child on the terminal.
  // - Live stops first, so the child never meets a worker that holds the journal lock or a key.
  // - Control C is ignored from before the terminal leaves raw mode until after it is back.
  // - Keys pressed while live stopped are dropped before the child starts, and lines left after
  //   it exits are dropped before "Press Enter".
  // - When Desk is already stopping, restoring the terminal is left to the failure handler.
  // - Live resumes whatever happened.
  def handOff(
      command: Seq[String],
      router: ExecutionRouter,
      reload: () => Either[String, Unit],
      restoration: WorkspaceApp.Restoration,
      saved: String,
      input: InputStream,
      output: PrintStream,
      term: TermOps,
      start: Seq[String] => Process
  ): Either[String, Int] =
    SettingsScreen
      .HandOff(
        suspend = () => router.suspendLive(),
        restore = () => {
          drain(input)
          term.restore(saved)
        },
        start = start,
        guard = Child.ignoreSignals,
        enter = () => {
          drain(input)
          Child.awaitEnter(input, output)
        },
        enterRaw = () =>
          restoration.synchronized {
            if !restoration.started then term.enterRaw()
          },
        reload = reload,
        resume = () => router.resumeLive()
      )
      .run(command)

  private def mouse(on: Boolean): String =
    if on then Screen.esc + "[?1000h" + Screen.esc + "[?1006h"
    else Screen.esc + "[?1006l" + Screen.esc + "[?1000l"

  // Runs the screen over the paused workspace. The pause and the forgotten frame change under the
  // input lock, where the render loop writes. Returns true when the screen asks Desk to quit.
  def show(
      paused: AtomicBoolean,
      lock: Object,
      terminal: Term.Writer,
      invalidate: () => Unit,
      output: PrintStream
  )(screen: => Boolean): Boolean =
    SettingsScreen.around(
      on => lock.synchronized(paused.set(on)),
      on => {
        output.print(mouse(on))
        output.flush()
      },
      () => {
        output.print(Screen.esc + "[2J")
        output.flush()
      },
      () => lock.synchronized(terminal.reset()),
      invalidate
    )(screen)

  // One pass of the render loop. While settings are open it only waits: no size read, no build,
  // no write and no publish. A frame built just as settings opened is dropped. write gets the
  // time spent waiting for the lock.
  def renderPass[A](
      paused: AtomicBoolean,
      running: () => Boolean,
      lock: Object,
      sleep: Long => Unit
  )(build: () => A)(write: (A, Long) => Unit): Unit =
    while paused.get() && running() do sleep(50L)
    val frame = build()
    val waiting = System.nanoTime()
    lock.synchronized {
      if !paused.get() then write(frame, System.nanoTime() - waiting)
    }

  // The keys thread. Keys are read through the bindings in force at each byte. When the
  // controller took Settings, the screen runs here, after the command and outside the input lock.
  def keys(
      input: InputStream,
      running: () => Boolean,
      promptOpen: () => Boolean,
      bound: AtomicReference[Bindings],
      handle: Command => Boolean,
      settings: () => Boolean,
      nanoTime: () => Long = () => System.nanoTime(),
      idle: () => Unit = () => Thread.sleep(10L)
  ): Unit =
    def emit(command: Command): Unit =
      if handle(command) && command == Command.Settings && settings() then emit(Command.Quit)
    WorkspaceApp.readKeys(input, running, promptOpen, () => bound.get(), emit, nanoTime, idle)

  // The screen's view of the running Desk for one opening. Bindings and terms change in the keys
  // reader and the controller together. The owner status is read on its own daemon thread.
  def context(
      network: String,
      config: Option[Path],
      home: Path,
      bound: AtomicReference[Bindings],
      controller: DeskController,
      keyRow: () => Either[String, String],
      reload: () => Either[String, Unit],
      ownerRead: () => Either[String, OwnerStatus],
      reading: SettingsScreen.ReadGate,
      handOff: Seq[String] => Either[String, Int],
      keyChild: Either[String, Seq[String]],
      env: Map[String, String],
      copy: String => Either[String, Unit],
      livePending: () => Boolean,
      size: () => (Int, Int),
      now: () => Long,
      approve: Either[String, Seq[String]],
      activeKey: () => Option[String] = () => None
  ): SettingsScreen.Context =
    val reads = new SettingsScreen.OwnerReads(
      ownerRead,
      now,
      task => Threads.daemon("owner-status")(task.run()).start(),
      reading
    )
    SettingsScreen.Context(
      network = network,
      settings = () => SettingsFile.read(config, home),
      writeSettings = (snapshot, terms) => SettingsFile.write(snapshot, terms),
      applyTerms = terms => controller.setTerms(terms),
      bindings = () => bound.get(),
      writeBindings = chosen => WorkspaceApp.saveBindings(home, chosen),
      applyBindings = chosen => {
        bound.set(chosen)
        controller.setBindings(chosen)
      },
      keyRow = keyRow,
      ownerStatus = () => reads.status,
      refreshOwner = () => reads.refresh(),
      runKeyScreen = () => keyChild.flatMap(handOff),
      runOwner = words => Child.vault(env, words).flatMap(handOff),
      copyAddress = address => copy(address),
      reload = reload,
      livePending = livePending,
      now = now,
      size = size,
      approveConfig = approve,
      activeKey = activeKey
    )

  // The real terminal operations.
  val terminal: TermOps = TermOps(saved => Term.restore(saved), () => { val _ = Term.enterRaw() })

  // The clipboard copy of a public address.
  def copyAddress(address: String): Either[String, Unit] =
    Clipboard.copySecret(address.getBytes(UTF_8), ProcessRun.system)

  // The key child is Desk's own jar run again. Under a development run the class path is a list
  // of folders and jars, and no child can start from that.
  def keyChild(args: Main.Args, java: String, classPath: String): Either[String, Seq[String]] =
    Either.cond(
      classPath.endsWith(".jar") && Files.isRegularFile(Path.of(classPath)),
      Child.keyScreen(args, java, classPath),
      "Settings children need the installed desk.jar"
    )

  // Starts children and keeps the latest, so Desk can end it when Desk stops.
  final class Children(launch: Seq[String] => Process):
    private val current = new AtomicReference(Option.empty[Process])
    def start(command: Seq[String]): Process =
      val process = launch(command)
      current.set(Some(process))
      process
    def stop(): Unit =
      current.getAndSet(None).filter(_.isAlive).foreach(_.destroy())

  // What restoring the terminal does when Desk stops: a child still on the terminal ends first,
  // so the shell gets the terminal back with nothing else reading it.
  def restoring(children: Children, restore: () => Unit): () => Unit = () =>
    children.stop()
    restore()
