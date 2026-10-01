package desk

import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, OpenOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit
import scala.util.Try
import scala.util.control.NonFatal

final case class KeyEntry(address: String, name: String, network: String, created: Long)

// Names for the trading keys, kept in one append only file. Desk never rewrites or repairs it.
object KeyNames:
  private val maximumBytes = 1048576L
  private val maximumLine = 512
  private val maximumTime = 9007199254740991L
  private val unreadable = "key names file must be a private regular file under 1 MiB"

  private enum Record:
    case Create(entry: KeyEntry)
    case Rename(address: String, name: String, time: Long)

  def path(home: Path): Path = home.resolve(".config/desk/key-names.jsonl")

  private[desk] def validAddress(address: String): Boolean = address.matches("0x[0-9a-f]{40}")
  private[desk] def validNetwork(network: String): Boolean =
    network == "mainnet" || network == "testnet"
  private def validTime(time: Long): Boolean = time >= 0 && time <= maximumTime

  def validName(name: String): Boolean =
    name.nonEmpty && name.length <= 32 && name.forall(c => c >= ' ' && c <= '~') &&
      name.head != ' ' && name.last != ' '

  private def render(record: Record): String = record match
    case Record.Create(e) =>
      ujson.write(
        ujson.Obj(
          "kind" -> "create",
          "address" -> e.address,
          "name" -> e.name,
          "network" -> e.network,
          "time" -> e.created.toDouble
        )
      )
    case Record.Rename(address, name, time) =>
      ujson.write(
        ujson.Obj(
          "kind" -> "rename",
          "address" -> address,
          "name" -> name,
          "time" -> time.toDouble
        )
      )

  private def valid(record: Record): Boolean = record match
    case Record.Create(e) =>
      validAddress(e.address) && validNetwork(e.network) && validName(e.name) &&
      validTime(e.created)
    case Record.Rename(address, name, time) =>
      validAddress(address) && validName(name) && validTime(time)

  // A line is accepted only when it is exactly what Desk would have written for its values.
  // That refuses unknown fields, repeated keys, other key orders and extra spaces.
  private def parse(line: String): Option[Record] =
    Try {
      val o = ujson.read(line).obj
      val time = o("time").num
      require(time == Math.floor(time) && time >= 0 && time <= maximumTime.toDouble)
      o("kind").str match
        case "create" =>
          Record.Create(KeyEntry(o("address").str, o("name").str, o("network").str, time.toLong))
        case "rename" => Record.Rename(o("address").str, o("name").str, time.toLong)
    }.toOption.filter(r => valid(r) && render(r) == line)

  private def apply(entries: Vector[KeyEntry], line: String): Option[Vector[KeyEntry]] =
    if line.getBytes(StandardCharsets.UTF_8).length > maximumLine then None
    else
      parse(line).flatMap {
        case Record.Create(e) => Option.when(!entries.exists(_.address == e.address))(entries :+ e)
        case Record.Rename(address, name, _) =>
          val at = entries.indexWhere(_.address == address)
          Option.when(at >= 0)(entries.updated(at, entries(at).copy(name = name)))
      }

  def load(path: Path): Either[String, Vector[KeyEntry]] =
    Try(LocalFiles.read(path, maximumBytes)).toEither.left.map(_ => unreadable).flatMap {
      case None       => Right(Vector.empty)
      case Some(text) =>
        val all = text.split("\n", -1)
        // The last piece is empty when the file ends with a newline. Otherwise it is torn.
        if all.last.nonEmpty then Left(invalid(all.length))
        else
          all.init.iterator.zipWithIndex.foldLeft[Either[String, Vector[KeyEntry]]](
            Right(Vector.empty)
          ) {
            case (Right(entries), (line, index)) =>
              apply(entries, line).toRight(invalid(index + 1))
            case (refused, _) => refused
          }
    }

  private def invalid(line: Int): String = s"key names file is invalid at line $line"

  private def current(path: Path): Vector[KeyEntry] = load(path) match
    case Right(entries) => entries
    case Left(message)  => throw new IllegalStateException(message)

  private def append(path: Path, record: Record): Unit =
    LocalFiles.directory(path.getParent)
    val created = !Files.exists(path, LinkOption.NOFOLLOW_LINKS)
    val channel = FileChannel.open(
      path,
      java.util.Set.of[OpenOption](
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND,
        LinkOption.NOFOLLOW_LINKS
      ),
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )
    try
      val bytes = ByteBuffer.wrap((render(record) + "\n").getBytes(StandardCharsets.UTF_8))
      while bytes.hasRemaining do channel.write(bytes)
      channel.force(true)
    finally channel.close()
    if created then
      val parent = FileChannel.open(path.getParent, StandardOpenOption.READ)
      try parent.force(true)
      finally parent.close()

  def create(path: Path, entry: KeyEntry): Unit =
    val record = Record.Create(entry)
    require(valid(record), "invalid key entry")
    require(!current(path).exists(_.address == entry.address), "key is already named")
    append(path, record)

  def rename(path: Path, address: String, name: String, time: Long): Unit =
    val record = Record.Rename(address, name, time)
    require(valid(record), "invalid key rename")
    require(current(path).exists(_.address == address), "key is not named")
    append(path, record)

// The chosen key for each network, replaced whole in one atomic write.
object ActiveKey:
  private val maximumBytes = 1024L

  def path(home: Path): Path = home.resolve(".config/desk/active-key.json")

  def load(path: Path): Either[String, Map[String, String]] =
    Try(LocalFiles.read(path, maximumBytes)).toEither.left
      .map(_ => "active key file must be a private regular file under 1 KiB")
      .flatMap {
        case None       => Right(Map.empty)
        case Some(text) =>
          Try(
            ujson.read(text).obj.iterator.map((network, value) => network -> value.str).toMap
          ).toOption
            .filter(
              _.forall((network, address) =>
                KeyNames.validNetwork(network) && KeyNames.validAddress(address)
              )
            )
            .toRight("active key file is invalid")
      }

  def select(path: Path, network: String, address: String): Unit =
    require(KeyNames.validNetwork(network) && KeyNames.validAddress(address), "invalid active key")
    val chosen = load(path) match
      case Right(map)    => map.updated(network, address)
      case Left(message) => throw new IllegalStateException(message)
    val value = ujson.Obj.from(chosen.toSeq.sortBy(_._1).map((n, a) => n -> ujson.Str(a)))
    LocalFiles.write(path, ujson.write(value) + "\n")

// Looks for a Keychain item by its attributes only. The secret is never read.
object KeychainItems:
  private val missing = 44
  private val timedOut = 124

  def present(service: String, address: String, run: Seq[String] => Int): Either[String, Boolean] =
    Try(run(Seq("find-generic-password", "-s", service, "-a", address))).toOption match
      case Some(0)         => Right(true)
      case Some(`missing`) => Right(false)
      case _               => Left("Keychain lookup failed")

  private[desk] def securityRunner(run: (Seq[String], Long) => Int): Seq[String] => Int =
    arguments => run("/usr/bin/security" +: arguments, 8000L)

  val security: Seq[String] => Int = securityRunner(runProcess)

  // Output is discarded and standard input is empty. A process past the limit is destroyed.
  private[desk] def runProcess(command: Seq[String], limitMillis: Long): Int =
    val process = new ProcessBuilder(command*)
      .redirectInput(ProcessBuilder.Redirect.from(new File("/dev/null")))
      .redirectOutput(ProcessBuilder.Redirect.DISCARD)
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    try
      if process.waitFor(limitMillis, TimeUnit.MILLISECONDS) then process.exitValue()
      else timedOut
    finally
      if process.isAlive then
        process.destroyForcibly()
        process.waitFor(2, TimeUnit.SECONDS)

object KeySetup:
  private val inUse = "Another Desk is using the current key"

  def usable(
      network: String,
      names: Vector[KeyEntry],
      active: Map[String, String],
      present: String => Either[String, Boolean]
  ): Either[String, String] =
    for
      address <- active.get(network).toRight(s"No active key for $network. Run desk --keys")
      entry <- names.find(_.address == address).toRight("The active key is not in key names")
      _ <- Either.cond(
        entry.network == network,
        (),
        s"The active key belongs to ${entry.network}"
      )
      found <- present(address).left.map(_ => "Keychain lookup failed")
      _ <- Either.cond(found, (), "The active key is not in the Keychain")
    yield address

  // Opening the journal takes its lock, so a second Desk that trades with the key is seen.
  def switchAllowed(
      stateDirectory: Path,
      network: String,
      current: Option[String]
  ): Either[String, Unit] = current match
    case None          => Right(())
    case Some(address) =>
      try
        val journal =
          OrderJournal.open(stateDirectory, network + "-" + address.stripPrefix("0x"))
        try
          Either.cond(
            journal.pending.isEmpty,
            (),
            "A saved order for the current key must finish first"
          )
        finally journal.close()
      catch
        case _: OverlappingFileLockException => Left(inUse)
        case error: IllegalArgumentException
            if Option(error.getMessage).exists(_.contains("another process owns")) =>
          Left(inUse)
        case NonFatal(_) => Left("The saved state for the current key cannot be read")
