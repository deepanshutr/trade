package desk

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

object Main:
  final case class Args(coin: String, interval: String)
  val usage = "usage: desk [COIN] [--interval I]"
  private val intervals =
    Set("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "8h", "12h", "1d", "3d", "1w", "1M")

  def parseArgs(args: Seq[String]): Either[String, Args] =
    def loop(rest: List[String], coin: Option[String], interval: String): Either[String, Args] =
      rest match
        case Nil => Right(Args(coin.getOrElse("XMR"), interval))
        case "--interval" :: i :: tail if intervals(i) => loop(tail, coin, i)
        case "--interval" :: i :: _                    => Left(s"unknown interval $i")
        case "--interval" :: Nil                       => Left("--interval needs a value")
        case flag :: _ if flag.startsWith("--")        => Left(s"unknown flag $flag")
        case c :: tail if coin.isEmpty                 => loop(tail, Some(c), interval)
        case extra :: _                                => Left(s"unexpected argument $extra")
    loop(args.toList, None, "5m")

  // byte codes: 3 control C, 8 backspace, 10 newline, 13 return, 27 escape, 127 delete,
  // 113 q, 105 i, 109 m
  def keyToCommand(byte: Int, promptOpen: Boolean): Option[Command] =
    if promptOpen then
      byte match
        case 13 | 10                 => Some(Command.PromptSubmit)
        case 27                      => Some(Command.PromptCancel)
        case 127 | 8                 => Some(Command.PromptBackspace)
        case 3                       => Some(Command.Quit)
        case c if c >= 32 && c < 127 => Some(Command.PromptChar(c.toChar))
        case _                       => None
    else
      byte match
        case 113 | 3 => Some(Command.Quit)
        case 105     => Some(Command.NextInterval)
        case 109     => Some(Command.StartMarketPrompt)
        case _       => None

  def nextPromptOpen(promptOpen: Boolean, command: Command): Boolean = command match
    case Command.StartMarketPrompt                   => true
    case Command.PromptSubmit | Command.PromptCancel => false
    case _                                           => promptOpen

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
    val state = new AtomicReference(Snapshot.initial(args.coin, args.interval, now()))
    val feed = new Feed(JdkConnector, JdkRest, queue, now, args.coin, args.interval)
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
      var promptOpen = false
      var byte = System.in.read()
      while byte >= 0 do
        keyToCommand(byte, promptOpen).foreach { c =>
          queue.add(Event.Key(c))
          promptOpen = nextPromptOpen(promptOpen, c)
        }
        byte = System.in.read()
    }
    daemon("fold") {
      while true do
        val event = queue.take()
        val before = state.get()
        val after = State.reduce(before, event, now())
        state.set(after)
        if after.coin != before.coin || after.interval != before.interval then
          feed.setMarket(after.coin, after.interval)
    }
    var last: Snapshot = null
    var lastFrameAt = 0L
    var size = Term.size()
    var sizeAt = now()
    try
      while !state.get().quit do
        val t = now()
        if t - sizeAt >= 1000 then
          size = Term.size()
          sizeAt = t
        val s = state.get()
        if (s ne last) || t - lastFrameAt >= 1000 then
          Term.write(Screen.render(s, size._1, size._2, t, zone))
          last = s
          lastFrameAt = t
        Thread.sleep(100)
    finally restore()
    sys.exit(0)
