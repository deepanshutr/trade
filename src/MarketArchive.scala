package desk

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import scala.jdk.CollectionConverters.*
import upickle.default.{ReadWriter, macroRW, read, write}

final case class PublicRecord(
    at: Long,
    kind: String,
    source: String = "socket",
    coin: String = "",
    interval: String = "",
    book: Option[Book] = None,
    trades: Vector[Trade] = Vector.empty,
    candles: Vector[Candle] = Vector.empty,
    context: Option[Ctx] = None,
    requestedAt: Long = 0L
)

final case class CandleCheckpoint(at: Long, markets: Map[String, Map[String, Vector[Candle]]])

object MarketArchive:
  private[desk] given ReadWriter[Side] =
    upickle.default.readwriter[String].bimap[Side](_.toString, Side.valueOf)
  private[desk] given ReadWriter[Level] = macroRW
  private[desk] given ReadWriter[Book] = macroRW
  private[desk] given ReadWriter[Trade] = macroRW
  private[desk] given ReadWriter[Candle] = macroRW
  private[desk] given ReadWriter[Ctx] = macroRW
  private[desk] given ReadWriter[PublicRecord] = macroRW
  private[desk] given ReadWriter[CandleCheckpoint] = macroRW
  private val checkpointName = "candles.json"
  private val chunkName = "events-([0-9]{13,17})-[a-f0-9]{8}\\.jsonl".r

  private def capture(event: Event, at: Long): Option[PublicRecord] = event match
    case Event.BookUpdate(c, b, socket) =>
      Some(PublicRecord(at, "book", if socket then "socket" else "poll", c, book = Some(b)))
    case Event.CoarseBook(c, k, b) =>
      Some(PublicRecord(at, s"coarse-$k", "poll", c, book = Some(b)))
    case Event.BboUpdate(c, b, a, t) =>
      Some(PublicRecord(at, "quote", coin = c, book = Some(Book(Vector(b), Vector(a), t))))
    case Event.Trades(c, ts)           => Some(PublicRecord(at, "trades", coin = c, trades = ts))
    case Event.CandleUpdate(c, i, bar) =>
      Some(PublicRecord(at, "candle", coin = c, interval = i, candles = Vector(bar)))
    case Event.CandleHistory(c, i, bars, requested) =>
      Some(PublicRecord(at, "history", "history", c, i, candles = bars, requestedAt = requested))
    case Event.CtxUpdate(c, ctx) => Some(PublicRecord(at, "context", coin = c, context = Some(ctx)))
    case Event.SocketUp          => Some(PublicRecord(at, "connected"))
    case Event.SocketDown        => Some(PublicRecord(at, "disconnected"))
    case Event.CandleHistoryFailed => Some(PublicRecord(at, "history-unavailable", "history"))
    case Event.Bad(_)              => Some(PublicRecord(at, "invalid-or-dropped-event"))
    case _                         => None

  private def chunks(directory: Path): Vector[(Path, Long)] =
    val listed = Files.list(directory)
    try
      listed
        .iterator()
        .asScala
        .toVector
        .flatMap { path =>
          path.getFileName.toString match
            case chunkName(time) if TradingConfig.privateFile(path) => Some(path -> time.toLong)
            case _                                                  => None
        }
        .sortBy(_._2)
    finally listed.close()

  private def checked(saved: CandleCheckpoint): CandleCheckpoint =
    require(saved.markets.size <= 32 && saved.markets.values.forall(_.forall { (i, cs) =>
      State.intervals.contains(i) && cs.size <= State.seriesCap &&
      cs.groupBy(_.t).values.forall(_.distinct.size == 1) && cs.forall(c =>
        c.t >= 0 && Vector(c.o, c.h, c.l, c.c).forall(v =>
          v > 0 && v.isFinite
        ) && c.v >= 0 && c.v.isFinite && c.l <= c.o && c.o <= c.h &&
          c.l <= c.c && c.c <= c.h
      )
    }))
    saved

  private def load(directory: Path): CandleCheckpoint =
    LocalFiles
      .read(directory.resolve(checkpointName), 128_000_000L)
      .map(text => checked(read[CandleCheckpoint](text)))
      .getOrElse(CandleCheckpoint(0L, Map.empty))

  def restore(directory: Path, initial: Snapshot): Snapshot =
    restore(load(directory), initial)

  private def restore(saved: CandleCheckpoint, initial: Snapshot): Snapshot =
    initial.copy(
      candles = saved.markets.getOrElse(initial.coin, Map.empty),
      refs = initial.refs.map { (coin, ref) =>
        coin -> ref.copy(candles = saved.markets.getOrElse(coin, Map.empty))
      },
      marketHistory = saved.markets.toVector
        .sortBy(_._1)
        .takeRight(State.marketHistoryLimit)
        .map((coin, candles) => coin -> Ref(Vector.empty, candles))
    )

  def lastWrite(directory: Path): Long = load(directory).at

  def open(
      directory: Path,
      maximumBytes: Long = 1_000_000_000L,
      chunkBytes: Long = 16_000_000L,
      retentionMs: Long = 7L * 86400000L
  ): MarketArchive =
    require(
      maximumBytes >= 4000 && chunkBytes > 0 && chunkBytes <= maximumBytes / 2 && retentionMs > 0
    )
    val lease = LocalFiles.lock(directory.resolve("capture.lock"))
    try new MarketArchive(directory, maximumBytes, chunkBytes, retentionMs, load(directory), lease)
    catch
      case error: Exception =>
        lease.close()
        throw error

final class MarketArchive private (
    directory: Path,
    maximumBytes: Long,
    chunkBytes: Long,
    retentionMs: Long,
    initial: CandleCheckpoint,
    lease: AutoCloseable
) extends AutoCloseable:
  import MarketArchive.{given, *}
  private var states = initial.markets.map { (coin, candles) =>
    coin -> Snapshot.initial(coin, initial.at, Vector.empty).copy(candles = candles)
  }
  private var output = Option.empty[(Path, FileChannel, Long)]
  private var lastAt = initial.at
  private var closed = false
  private var total = bytes

  private[desk] def restore(snapshot: Snapshot): Snapshot = MarketArchive.restore(initial, snapshot)

  def bytes: Long = synchronized {
    val listed = Files.list(directory)
    try
      listed
        .iterator()
        .asScala
        .map { path =>
          require(Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS), "unexpected archive entry")
          Files.size(path)
        }
        .foldLeft(0L)((total, size) => Math.addExact(total, size))
    finally listed.close()
  }

  private def closeChunk(): Unit =
    output.foreach { (_, channel, _) =>
      try channel.force(true)
      finally channel.close()
    }
    output = None

  private def prune(at: Long, reserve: Long): Unit =
    chunks(directory).foreach { (path, created) =>
      if !output.exists(
          _._1 == path
        ) && (created < at - retentionMs || total + reserve > maximumBytes)
      then
        val size = Files.size(path)
        Files.delete(path)
        total -= size
    }
    require(total + reserve <= maximumBytes, "public history disk budget exhausted")

  def record(event: Event, at: Long): Unit = synchronized {
    require(!closed && at >= 0)
    capture(event, at).foreach { item =>
      event match
        case _: Event.CandleUpdate | _: Event.CandleHistory =>
          checked(CandleCheckpoint(at, Map(item.coin -> Map(item.interval -> item.candles))))
        case _ => ()
      val encoded = (write(item) + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
      require(encoded.length <= maximumBytes / 2, "public event exceeds disk budget")
      if output.exists { (_, channel, opened) =>
          channel.size() + encoded.length > chunkBytes || at - opened >= retentionMs.min(3600000L)
        }
      then closeChunk()
      if output.isEmpty || total + encoded.length > maximumBytes then prune(at, encoded.length)
      if output.isEmpty then
        val name = f"events-$at%013d-" + java.util.UUID.randomUUID().toString.take(8) + ".jsonl"
        val path = directory.resolve(name)
        val channel = FileChannel.open(
          path,
          java.util.Set.of[java.nio.file.OpenOption](
            StandardOpenOption.CREATE_NEW,
            StandardOpenOption.WRITE,
            LinkOption.NOFOLLOW_LINKS
          ),
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
        )
        output = Some((path, channel, at))
      val buffer = ByteBuffer.wrap(encoded)
      while buffer.hasRemaining do output.get._2.write(buffer)
      total += encoded.length
      event match
        case _: Event.CandleUpdate | _: Event.CandleHistory =>
          val kept =
            if states.contains(item.coin) || states.size < 32 then states
            else
              states - states
                .minBy(_._2.candles.valuesIterator.flatten.map(_.t).maxOption.getOrElse(0L))
                ._1
          val before = kept.getOrElse(item.coin, Snapshot.initial(item.coin, at, Vector.empty))
          states = kept.updated(item.coin, State.reduce(before, event, at))
        case _ => ()
      lastAt = at
    }
  }

  def checkpoint(): Unit = synchronized {
    require(!closed)
    val text = write(CandleCheckpoint(lastAt, states.map((coin, s) => coin -> s.candles)))
    val size = text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length.toLong
    require(size <= maximumBytes / 2, "candle checkpoint exceeds disk budget")
    total = bytes
    if BigInt(total) + size > maximumBytes then closeChunk()
    else output.foreach { (_, channel, _) => channel.force(true) }
    prune(lastAt, size)
    LocalFiles.write(directory.resolve(checkpointName), text)
    total = bytes
  }

  def close(): Unit = synchronized {
    if !closed then
      try checkpoint()
      finally
        closed = true
        try closeChunk()
        finally lease.close()
  }
