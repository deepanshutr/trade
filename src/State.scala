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

final case class Snapshot(
    coin: String,
    interval: String,
    book: Option[Book],
    tape: Vector[Trade],
    candles: Vector[Candle],
    ctx: Option[Ctx],
    feed: Health,
    bad: Int,
    sizes: Vector[(Long, Vector[Double])],
    prompt: Option[String],
    quit: Boolean
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
      Health.Down(now, now),
      0,
      Vector.empty,
      None,
      false
    )

object State:
  val intervals: Vector[String] = Vector("1m", "5m", "15m", "1h", "4h", "1d")
  val tapeCap = 200
  val candleCap = 200
  val windowMs = 60_000L

  def lastMsgAt(h: Health): Long = h match
    case Health.Live(t)    => t
    case Health.Down(_, t) => t

  def reduce(s: Snapshot, e: Event, now: Long): Snapshot = e match
    case Event.BookUpdate(book, fromSocket) =>
      val fresh = if fromSocket then s.copy(feed = Health.Live(now)) else s
      if s.book.exists(_.time >= book.time) then fresh
      else fresh.copy(book = Some(book), sizes = addSizes(fresh.sizes, now, book))
    case Event.BboUpdate(bid, ask, time) =>
      val fresh = s.copy(feed = Health.Live(now))
      s.book match
        case Some(b) if b.time >= time => fresh
        case Some(b)                   =>
          val bids = bid +: b.bids.tail.filterNot(_.px.toDouble >= bid.px.toDouble)
          val asks = ask +: b.asks.tail.filterNot(_.px.toDouble <= ask.px.toDouble)
          fresh.copy(book = Some(Book(bids, asks, time)))
        case None => fresh.copy(book = Some(Book(Vector(bid), Vector(ask), time)))
    case Event.Trades(ts) =>
      s.copy(feed = Health.Live(now), tape = (ts.sortBy(-_.time) ++ s.tape).take(tapeCap))
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
    case Event.Key(cmd)            => key(s, cmd)

  private def addSizes(
      w: Vector[(Long, Vector[Double])],
      now: Long,
      b: Book
  ): Vector[(Long, Vector[Double])] =
    val pruned = w.filter(_._1 >= now - windowMs)
    if pruned.exists(_._1 / 1000 == now / 1000) then pruned
    else pruned :+ (now, (b.bids ++ b.asks).map(_.sz.toDouble))

  private def key(s: Snapshot, cmd: Command): Snapshot = cmd match
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
        case Some(name) if name.nonEmpty =>
          s.copy(
            coin = name,
            book = None,
            tape = Vector.empty,
            candles = Vector.empty,
            ctx = None,
            sizes = Vector.empty,
            prompt = None
          )
        case _ => s.copy(prompt = None)
