package desk

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

object Main:
  final case class Args(coin: String)
  val usage = "usage: desk [COIN]"

  def parseArgs(args: Seq[String]): Either[String, Args] =
    def loop(rest: List[String], coin: Option[String]): Either[String, Args] =
      rest match
        case Nil                                => Right(Args(coin.getOrElse("XMR")))
        case flag :: _ if flag.startsWith("--") => Left(s"unknown flag $flag")
        case c :: tail if coin.isEmpty          => loop(tail, Some(c))
        case extra :: _                         => Left(s"unexpected argument $extra")
    loop(args.toList, None)

  def nextPromptOpen(promptOpen: Boolean, command: Command): Boolean = command match
    case Command.StartMarketPrompt                   => true
    case Command.PromptSubmit | Command.PromptCancel => false
    case _                                           => promptOpen

  def sizeEvent(last: (Int, Int), size: (Int, Int)): Option[Event] =
    if size == last then None else Some(Event.Resize(size._1, size._2))

  def main(argv: Array[String]): Unit =
    parseArgs(argv.toSeq) match
      case Left(message) =>
        System.err.println(message)
        System.err.println(usage)
        sys.exit(64)
      case Right(args) => run(args)

  private def daemon(name: String)(body: => Unit): Unit =
    val t = new Thread(() => body, name)
    t.setDaemon(true)
    t.start()

  def run(args: Args): Unit =
    val now = () => System.currentTimeMillis()
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial(args.coin, now()))
    val feed = new Feed(JdkConnector, JdkRest, queue, now, args.coin)
    val zone = java.time.ZoneId.systemDefault()
    val saved = scala.util.Try(Term.enterRaw()).getOrElse {
      System.err.println("desk needs a terminal")
      sys.exit(1)
    }
    val restored = new AtomicBoolean(false)
    def restore(): Unit = if restored.compareAndSet(false, true) then Term.restore(saved)
    Runtime.getRuntime.addShutdownHook(new Thread(() => restore()))
    Thread.setDefaultUncaughtExceptionHandler { (_, error) =>
      restore()
      System.err.println(error)
      sys.exit(1)
    }
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
    daemon("keys") {
      var pending = Vector.empty[Int]
      var promptOpen = false
      var b = System.in.read()
      while b >= 0 do
        val (p, cmd) = Keys.step(pending, b, promptOpen)
        pending = p
        cmd.foreach { c =>
          promptOpen = nextPromptOpen(promptOpen, c)
          queue.put(Event.Key(c))
        }
        b = System.in.read()
    }
    daemon("fold") {
      while true do
        val event = queue.take()
        val before = state.get()
        val after = State.reduce(before, event, now())
        state.set(after)
        if after.coin != before.coin then feed.setMarket(after.coin)
    }
    var last: Snapshot = null
    var lastFrameAt = 0L
    var size = Term.size()
    if size != (0, 0) then queue.put(Event.Resize(size._1, size._2))
    var sizeAt = now()
    try
      while !state.get().quit do
        val t = now()
        if t - sizeAt >= 1000 then
          val polled = Term.size()
          if polled != (0, 0) then
            sizeEvent(size, polled).foreach(e => queue.put(e))
            size = polled
          sizeAt = t
        val s = state.get()
        if (s ne last) || t - lastFrameAt >= 1000 then
          Term.write(Screen.render(s, size._1, size._2, t, zone))
          last = s
          lastFrameAt = t
        Thread.sleep(100)
    finally restore()
    sys.exit(0)
