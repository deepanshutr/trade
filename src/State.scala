package desk

enum Side:
  case Buy, Sell

final case class Level(px: String, sz: String, n: Int)
final case class Book(bids: Vector[Level], asks: Vector[Level], time: Long)
final case class Trade(time: Long, side: Side, px: String, sz: String)
final case class Candle(t: Long, o: Double, h: Double, l: Double, c: Double, v: Double)
final case class Ctx(
    mark: String,
    oracle: String,
    funding: String,
    openInterest: String,
    dayVolume: String,
    prevDayPx: String,
    mid: Option[String]
)

enum Health:
  case Live(lastMsgAt: Long)
  case Down(since: Long, lastMsgAt: Long)

enum Command:
  case Quit, NextInterval, StartMarketPrompt, PromptBackspace, PromptCancel, PromptSubmit
  case PromptChar(c: Char)
  case ZoomIn, ZoomOut, ScrubBack, ScrubForward

enum Event:
  case BookUpdate(book: Book, fromSocket: Boolean)
  case BboUpdate(bid: Level, ask: Level, time: Long)
  case Trades(trades: Vector[Trade])
  case CandleUpdate(candle: Candle)
  case CandleHistory(candles: Vector[Candle])
  case CandleHistoryFailed
  case CtxUpdate(ctx: Ctx)
  case Pong
  case Bad(text: String)
  case SocketUp
  case SocketDown
  case Key(command: Command)
  case Resize(cols: Int, rows: Int)

final case class Snapshot(
    coin: String,
    interval: String,
    book: Option[Book],
    tape: Vector[Trade],
    candles: Vector[Candle],
    ctx: Option[Ctx],
    feed: Health,
    bad: Int,
    prompt: Option[String],
    quit: Boolean,
    samples: Vector[(Long, Double)],
    books: Vector[(Long, Book)],
    since: Long,
    zoom: Int,
    offsetSec: Long,
    cols: Int,
    rows: Int
)

object Snapshot:
  def initial(coin: String, interval: String, now: Long): Snapshot =
    Snapshot(
      coin,
      interval,
      None,
      Vector.empty,
      Vector.empty,
      None,
      Health.Down(now, now - 6000),
      0,
      None,
      false,
      Vector.empty,
      Vector.empty,
      now,
      0,
      0L,
      Layout.minCols,
      Layout.minRows
    )

object State:
  val intervals: Vector[String] = Vector("1m", "5m", "15m", "1h", "4h", "1d")
  val tapeCap: Int = 200_000
  val candleCap = 1200
  val zooms: Vector[Int] = Vector(1, 2, 5, 10, 30, 60, 120, 300)
  val samplesMs: Long = 6L * 3600_000L
  val booksMs: Long = 900_000L
  val tapeMs: Long = 4L * 3600_000L

  def lastMsgAt(h: Health): Long = h match
    case Health.Live(t)    => t
    case Health.Down(_, t) => t

  def mid(b: Book): Option[Double] =
    for bid <- b.bids.headOption; ask <- b.asks.headOption
    yield (bid.px.toDouble + ask.px.toDouble) / 2

  private def keep[A](ring: Vector[(Long, A)], sec: Long, ms: Long): Vector[(Long, A)] =
    val kept = ring.filter(_._1 >= sec - ms / 1000)
    if kept.lastOption.exists(_._1 == sec) then kept.init else kept

  private def sample(s: Snapshot, now: Long): Snapshot =
    s.book.fold(s) { b =>
      val sec = now / 1000
      val samples = mid(b) match
        case Some(m) => keep(s.samples, sec, samplesMs) :+ (sec, m)
        case None    => s.samples.filter(_._1 >= sec - samplesMs / 1000)
      s.copy(samples = samples, books = keep(s.books, sec, booksMs) :+ (sec, b))
    }

  private def step(s: Snapshot): Long = Layout.dotCols(s.cols) / 4L * zooms(s.zoom)

  def reduce(s: Snapshot, e: Event, now: Long): Snapshot = e match
    case Event.BookUpdate(book, fromSocket) =>
      val fresh = if fromSocket then s.copy(feed = Health.Live(now)) else s
      if s.book.exists(_.time >= book.time) then fresh
      else sample(fresh.copy(book = Some(book)), now)
    case Event.BboUpdate(bid, ask, time) =>
      val fresh = s.copy(feed = Health.Live(now))
      s.book match
        case Some(b) if b.time >= time => fresh
        case Some(b)                   =>
          val bids = bid +: b.bids.drop(1).filterNot(_.px.toDouble >= bid.px.toDouble)
          val asks = ask +: b.asks.drop(1).filterNot(_.px.toDouble <= ask.px.toDouble)
          sample(fresh.copy(book = Some(Book(bids, asks, time))), now)
        case None => sample(fresh.copy(book = Some(Book(Vector(bid), Vector(ask), time))), now)
    case Event.Trades(ts) =>
      s.copy(
        feed = Health.Live(now),
        tape = (ts.sortBy(-_.time) ++ s.tape).filter(_.time >= now - tapeMs).take(tapeCap)
      )
    case Event.CandleUpdate(c) =>
      val kept = (s.candles.filterNot(_.t == c.t) :+ c).sortBy(_.t)
      s.copy(feed = Health.Live(now), candles = kept.takeRight(candleCap))
    case Event.CandleHistory(cs)   => s.copy(candles = cs.sortBy(_.t).takeRight(candleCap))
    case Event.CandleHistoryFailed => s.copy(bad = s.bad + 1)
    case Event.CtxUpdate(c)        => s.copy(feed = Health.Live(now), ctx = Some(c))
    case Event.Pong                => s.copy(feed = Health.Live(now))
    case Event.Bad(_)              => s.copy(feed = Health.Live(now), bad = s.bad + 1)
    case Event.SocketDown          => s.copy(feed = Health.Down(now, lastMsgAt(s.feed)))
    case Event.SocketUp            => s.copy(feed = Health.Live(now))
    case Event.Key(cmd)            => key(s, cmd, now)
    case Event.Resize(c, r) => s.copy(cols = c.max(Layout.minCols), rows = r.max(Layout.minRows))

  private def key(s: Snapshot, cmd: Command, now: Long): Snapshot = cmd match
    case Command.Quit         => s.copy(quit = true)
    case Command.NextInterval =>
      val i = intervals.indexOf(s.interval)
      val next = if i < 0 then intervals.head else intervals((i + 1) % intervals.size)
      s.copy(interval = next, candles = Vector.empty)
    case Command.StartMarketPrompt => s.copy(prompt = Some(""))
    case Command.PromptChar(c)     => s.copy(prompt = s.prompt.map(_ + c))
    case Command.PromptBackspace   => s.copy(prompt = s.prompt.map(_.dropRight(1)))
    case Command.PromptCancel      => s.copy(prompt = None)
    case Command.PromptSubmit      =>
      s.prompt match
        case Some(name) if name.nonEmpty && name != s.coin =>
          s.copy(
            coin = name,
            book = None,
            tape = Vector.empty,
            candles = Vector.empty,
            ctx = None,
            prompt = None,
            samples = Vector.empty,
            books = Vector.empty,
            since = now,
            offsetSec = 0L
          )
        case _ => s.copy(prompt = None)
    case Command.ZoomOut      => s.copy(zoom = (s.zoom + 1).min(zooms.size - 1))
    case Command.ZoomIn       => s.copy(zoom = (s.zoom - 1).max(0))
    case Command.ScrubBack    => s.copy(offsetSec = s.offsetSec + step(s))
    case Command.ScrubForward => s.copy(offsetSec = (s.offsetSec - step(s)).max(0L))
