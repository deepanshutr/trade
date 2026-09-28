package desk

enum Side:
  case Buy, Sell

final case class Level(px: String, sz: String, n: Int)
final case class Book(bids: Vector[Level], asks: Vector[Level], time: Long)
final case class Trade(time: Long, side: Side, px: String, sz: String, id: Option[Long] = None):
  val displaySize: Double = sz.toDouble
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

final case class Ref(samples: Vector[(Long, Double)], candles: Map[String, Vector[Candle]])

object Ref:
  val empty: Ref = Ref(Vector.empty, Map.empty)

enum Health:
  case Live(lastMsgAt: Long)
  case Down(since: Long, lastMsgAt: Long)

enum Command:
  case Quit, StartMarketPrompt, PromptBackspace, PromptCancel, PromptSubmit
  case PromptChar(c: Char)
  case Pointer(x: Int, y: Int, button: Int)
  case PointerPress(x: Int, y: Int, button: Int)
  case ToggleMute
  case Mode(name: String)
  case ZoomIn, ZoomOut, ScrubBack, ScrubForward
  case PlaceOrder(intent: OrderIntent)
  case SelectTarget(side: Side)
  case Confirm, Cancel, ToggleCandles

enum Event:
  case BookUpdate(coin: String, book: Book, fromSocket: Boolean)
  case CoarseBook(coin: String, sigFigs: Int, book: Book)
  case BboUpdate(coin: String, bid: Level, ask: Level, time: Long)
  case Trades(coin: String, trades: Vector[Trade])
  case CandleUpdate(coin: String, interval: String, candle: Candle)
  case CandleHistory(
      coin: String,
      interval: String,
      candles: Vector[Candle],
      requestedAt: Long = 0L
  )
  case CandleHistoryFailed
  case CtxUpdate(coin: String, ctx: Ctx)
  case Pong
  case Bad(text: String)
  case SocketUp
  case SocketDown
  case Key(command: Command)
  case Resize(cols: Int, rows: Int)
  case MarketChecked(request: (Long, String), result: Either[String, Unit])
  case MarketUnavailable(coin: String, reason: String)

enum ChartMode:
  case Line, Candles

final case class Snapshot(
    coin: String,
    book: Option[Book] = None,
    tape: Vector[Trade] = Vector.empty,
    candles: Map[String, Vector[Candle]] = Map.empty,
    ctx: Option[Ctx] = None,
    feed: Health,
    bad: Int = 0,
    prompt: Option[String] = None,
    quit: Boolean = false,
    samples: Vector[(Long, Double)] = Vector.empty,
    books: Vector[(Long, Book)] = Vector.empty,
    since: Long,
    zoom: Int = 0,
    offsetSec: Long = 0L,
    cols: Int = Layout.minCols,
    rows: Int = Layout.minRows,
    coarse: Map[Int, Book] = Map.empty,
    refs: Map[String, Ref] = Map.empty,
    chartMode: ChartMode = ChartMode.Line,
    lineZoom: Int = 0,
    referenceCoins: Vector[String] = State.refCoins,
    candleReceipts: Map[(String, String), Map[Long, Long]] = Map.empty,
    workspace: Boolean = false,
    marketPending: Option[(Long, String)] = None,
    marketNotice: Option[String] = None,
    marketHistory: Vector[(String, Ref)] = Vector.empty
)

object Snapshot:
  def initial(
      coin: String,
      now: Long,
      references: Vector[String] = State.refCoins
  ): Snapshot =
    val chosen = references.distinct
    require(chosen.size <= 3, "choose at most three reference markets")
    Snapshot(
      coin = coin,
      feed = Health.Down(now, now - 6000),
      since = now,
      refs = chosen.map(_ -> Ref.empty).toMap,
      referenceCoins = chosen
    )

object State:
  val marketHistoryLimit = 32
  private def mergeHistory(older: Ref, newer: Ref, now: Long): Ref =
    val samples = (older.samples ++ newer.samples).toMap.toVector
      .filter(_._1 >= now / 1000 - samplesMs / 1000)
      .sortBy(_._1)
    val candles =
      newer.candles.filter(_._2.nonEmpty).foldLeft(older.candles) { case (all, (interval, bars)) =>
        val joined = (all.getOrElse(interval, Vector.empty) ++ bars)
          .map(c => c.t -> c)
          .toMap
          .values
          .toVector
          .sortBy(_.t)
          .takeRight(seriesCap)
        all.updated(interval, joined)
      }
    Ref(samples, candles)
  def validMarket(name: String): Boolean = name.matches("[A-Za-z0-9][A-Za-z0-9:._-]{0,39}")
  val tapeCap: Int = 200_000
  val refCoins: Vector[String] = Vector("BTC", "PAXG")
  val intervals: Vector[String] = Vector("1m", "5m", "15m", "1h", "4h", "1d")
  def intervalMs(interval: String): Long = interval match
    case "1m"  => 60_000L
    case "5m"  => 300_000L
    case "15m" => 900_000L
    case "1h"  => 3_600_000L
    case "4h"  => 14_400_000L
    case _     => 86_400_000L
  val seriesCap = 5200
  val zooms: Vector[Int] =
    Vector(1, 2, 5, 10, 30, 60, 120, 300, 900, 1800, 3600, 14400, 43200, 86400, 259200, 604800)

  val candleMinZoom: Int = zooms.indexOf(60)

  def seriesFor(zoom: Int): Option[String] =
    if zoom < 60 then None
    else if zoom < 300 then Some("1m")
    else if zoom < 900 then Some("5m")
    else if zoom < 3600 then Some("15m")
    else if zoom < 14400 then Some("1h")
    else if zoom < 86400 then Some("4h")
    else Some("1d")

  def zoomLabel(zoom: Int): String =
    if zoom < 60 then s"${zoom}s"
    else if zoom < 3600 then s"${zoom / 60}m"
    else if zoom < 86400 then s"${zoom / 3600}h"
    else if zoom < 604800 then s"${zoom / 86400}d"
    else "1w"

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

  private def addCandle(
      map: Map[String, Vector[Candle]],
      interval: String,
      c: Candle
  ): Map[String, Vector[Candle]] =
    val kept = (map.getOrElse(interval, Vector.empty).filterNot(_.t == c.t) :+ c).sortBy(_.t)
    map.updated(interval, kept.takeRight(seriesCap))

  private def setHistory(
      map: Map[String, Vector[Candle]],
      interval: String,
      cs: Vector[Candle],
      receipts: Map[Long, Long],
      requestedAt: Long
  ): Map[String, Vector[Candle]] =
    val previous = map.getOrElse(interval, Vector.empty)
    val protectedValues = previous.filter(c => receipts.get(c.t).exists(_ >= requestedAt))
    val merged = (previous ++ cs ++ protectedValues).map(c => c.t -> c).toMap
    map.updated(interval, merged.values.toVector.sortBy(_.t).takeRight(seriesCap))

  private def sampleRef(r: Ref, mid: Double, sec: Long): Ref =
    r.copy(samples = keep(r.samples, sec, samplesMs) :+ (sec, mid))

  private def step(s: Snapshot): Long =
    val width = Layout.workspaceChartWidth(s.cols)
    (Layout.plotColumns(width, s.chartMode) / 4L).max(1L) * zooms(s.zoom)

  def reduce(s: Snapshot, e: Event, now: Long): Snapshot = e match
    case Event.BookUpdate(coin, book, fromSocket) =>
      val fresh = if fromSocket then s.copy(feed = Health.Live(now)) else s
      if coin != s.coin || s.book.exists(_.time >= book.time) then fresh
      else sample(fresh.copy(book = Some(book)), now)
    case Event.CoarseBook(coin, k, book) =>
      if coin == s.coin then s.copy(coarse = s.coarse.updated(k, book)) else s
    case Event.BboUpdate(coin, bid, ask, time) =>
      val fresh = s.copy(feed = Health.Live(now))
      if coin == s.coin then
        s.book match
          case Some(b) if b.time >= time => fresh
          case Some(b)                   =>
            val bids = bid +: b.bids.drop(1).filterNot(_.px.toDouble >= bid.px.toDouble)
            val asks = ask +: b.asks.drop(1).filterNot(_.px.toDouble <= ask.px.toDouble)
            sample(fresh.copy(book = Some(Book(bids, asks, time))), now)
          case None => sample(fresh.copy(book = Some(Book(Vector(bid), Vector(ask), time))), now)
      else
        s.refs.get(coin).fold(fresh) { r =>
          val refMid = (bid.px.toDouble + ask.px.toDouble) / 2
          fresh.copy(refs = s.refs.updated(coin, sampleRef(r, refMid, now / 1000)))
        }
    case Event.Trades(coin, ts) =>
      val fresh = s.copy(feed = Health.Live(now))
      if coin != s.coin then fresh
      else
        val seen = scala.collection.mutable.HashSet.empty[(Long, Long)]
        def sorted(batch: Vector[Trade]): Vector[Trade] = batch
          .filter(_.time >= now - tapeMs)
          .filter(t => t.id.forall(id => seen.add((t.time, id))))
          .sortBy(-_.time)
          .take(tapeCap)
        val tape =
          if s.tape.headOption.forall(head => ts.forall(_.time > head.time)) then
            val expired = s.tape.reverseIterator.takeWhile(_.time < now - tapeMs).size
            (sorted(ts) ++ s.tape.dropRight(expired)).take(tapeCap)
          else sorted(s.tape ++ ts)
        fresh.copy(tape = tape)
    case Event.CandleUpdate(coin, interval, c) =>
      val key = (coin, interval)
      val updated = s.candleReceipts.getOrElse(key, Map.empty) + (c.t -> now)
      val times = if updated.size <= seriesCap then updated else updated - updated.keys.min
      val fresh =
        s.copy(feed = Health.Live(now), candleReceipts = s.candleReceipts.updated(key, times))
      if coin == s.coin then fresh.copy(candles = addCandle(s.candles, interval, c))
      else
        s.refs
          .get(coin)
          .fold(fresh)(r =>
            fresh.copy(refs =
              s.refs.updated(coin, r.copy(candles = addCandle(r.candles, interval, c)))
            )
          )
    case Event.CandleHistory(coin, interval, cs, requestedAt) =>
      val receipts = s.candleReceipts.getOrElse((coin, interval), Map.empty)
      if coin == s.coin then
        s.copy(candles = setHistory(s.candles, interval, cs, receipts, requestedAt))
      else
        s.refs
          .get(coin)
          .fold(s)(r =>
            s.copy(refs =
              s.refs.updated(
                coin,
                r.copy(candles = setHistory(r.candles, interval, cs, receipts, requestedAt))
              )
            )
          )
    case Event.CandleHistoryFailed => s.copy(bad = s.bad + 1)
    case Event.CtxUpdate(coin, c)  =>
      val fresh = s.copy(feed = Health.Live(now))
      if coin == s.coin then fresh.copy(ctx = Some(c)) else fresh
    case Event.Pong         => s.copy(feed = Health.Live(now))
    case Event.Bad(_)       => s.copy(feed = Health.Live(now), bad = s.bad + 1)
    case Event.SocketDown   => s.copy(feed = Health.Down(now, lastMsgAt(s.feed)))
    case Event.SocketUp     => s.copy(feed = Health.Live(now), marketNotice = None)
    case Event.Key(cmd)     => key(s, cmd, now)
    case Event.Resize(c, r) => s.copy(cols = c.max(Layout.minCols), rows = r.max(Layout.minRows))
    // The runtime accepts only the result matching its current selection request.
    case Event.MarketChecked(_, _)             => s
    case Event.MarketUnavailable(coin, reason) =>
      if coin == s.coin then s.copy(marketNotice = Some(reason)) else s

  private def key(s: Snapshot, cmd: Command, now: Long): Snapshot = cmd match
    case Command.PlaceOrder(_) | Command.SelectTarget(_) | Command.Confirm | Command.Cancel |
        Command.Pointer(_, _, _) | Command.PointerPress(_, _, _) | Command.ToggleMute |
        Command.Mode(_) =>
      s
    case Command.Quit              => s.copy(quit = true)
    case Command.StartMarketPrompt => s.copy(prompt = Some(""))
    case Command.PromptChar(c)     => s.copy(prompt = s.prompt.map(_ + c))
    case Command.PromptBackspace   => s.copy(prompt = s.prompt.map(_.dropRight(1)))
    case Command.PromptCancel      => s.copy(prompt = None)
    case Command.PromptSubmit      =>
      s.prompt match
        case Some(name) if validMarket(name) && name != s.coin =>
          val current = Ref(s.samples, s.candles)
          val refs = s.refs
            .get(s.coin)
            .fold(s.refs)(old => s.refs.updated(s.coin, mergeHistory(old, current, now)))
          val history = (s.marketHistory.filterNot(_._1 == s.coin) :+ (s.coin -> current))
            .takeRight(marketHistoryLimit)
          val remembered = s.marketHistory.find(_._1 == name).map(_._2).getOrElse(Ref.empty)
          val next = mergeHistory(remembered, refs.getOrElse(name, Ref.empty), now)
          s.copy(
            coin = name,
            refs = refs,
            book = None,
            tape = Vector.empty,
            candles = next.candles,
            coarse = Map.empty,
            ctx = None,
            prompt = None,
            samples = next.samples,
            books = Vector.empty,
            since = now,
            offsetSec = 0L,
            marketHistory = history
          )
        case _ => s.copy(prompt = None)
    case Command.ZoomOut => s.copy(zoom = (s.zoom + 1).min(zooms.size - 1))
    case Command.ZoomIn  =>
      val minimum = if s.chartMode == ChartMode.Candles then candleMinZoom else 0
      s.copy(zoom = (s.zoom - 1).max(minimum))
    case Command.ToggleCandles =>
      s.chartMode match
        case ChartMode.Line =>
          s.copy(chartMode = ChartMode.Candles, lineZoom = s.zoom, zoom = s.zoom.max(candleMinZoom))
        case ChartMode.Candles =>
          s.copy(chartMode = ChartMode.Line, zoom = s.lineZoom)
    case Command.ScrubBack    => s.copy(offsetSec = s.offsetSec + step(s))
    case Command.ScrubForward => s.copy(offsetSec = (s.offsetSec - step(s)).max(0L))
