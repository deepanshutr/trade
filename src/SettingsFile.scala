package desk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, LinkOption, Path}
import java.nio.file.attribute.PosixFilePermissions
import scala.util.Try

// The bytes are the file as read or last written. They are empty when no file exists.
final case class SettingsSnapshot(path: Path, bytes: Array[Byte], config: TradingConfig)

// Desk owns small, big and slippage in the settings file and keeps every other field as it is.
object SettingsFile:
  val changed = "Settings changed on disk. Leave and open settings again"
  val slippageRefusal = "Slippage must be above 0% and at most 1%, with at most four decimals"

  def read(path: Option[Path], home: Path): Either[String, SettingsSnapshot] =
    read(path, home, () => ())

  // A test writes the file through afterLoad to stand in for another program.
  private[desk] def read(
      path: Option[Path],
      home: Path,
      afterLoad: () => Unit
  ): Either[String, SettingsSnapshot] =
    val target = path.getOrElse(home.resolve(".config/desk/config.json"))
    if path.isEmpty && !Files.exists(target, LinkOption.NOFOLLOW_LINKS) then
      Right(SettingsSnapshot(target, Array.emptyByteArray, TradingConfig()))
    else
      // The bytes are read before and after the load and must match, so they are the bytes the
      // load checked. A file that fails the load keeps the load's reason.
      val before = privateBytes(target)
      TradingConfig.load(Some(target)).flatMap { config =>
        afterLoad()
        before
          .filter(bytes => privateBytes(target).exists(_.sameElements(bytes)))
          .map(SettingsSnapshot(target, _, config))
          .toRight(changed)
      }

  def write(snapshot: SettingsSnapshot, terms: TradeTerms): Either[String, SettingsSnapshot] =
    write(snapshot, terms, () => ())

  // A test writes the file through beforeReplace to stand in for another program.
  private[desk] def write(
      snapshot: SettingsSnapshot,
      terms: TradeTerms,
      beforeReplace: () => Unit
  ): Either[String, SettingsSnapshot] =
    val path = snapshot.path.toAbsolutePath
    val folder = path.getParent
    // LocalFiles.write calls this just before its atomic move, so only a write by another program
    // in the instant between the compare and the move could be lost.
    val compareBeforeReplace = () =>
      beforeReplace()
      require(unchanged(path, snapshot.bytes), changed)
    for
      text <- attempt(render(snapshot.bytes, terms))
      _ <- attempt(LocalFiles.directory(folder)).left.map(reason => s"$reason ($folder)")
      config <- attempt(proof(folder, text)).flatten
      _ <- attempt(LocalFiles.write(path, text, compareBeforeReplace))
    yield SettingsSnapshot(path, text.getBytes(UTF_8), config)

  // Percent with at least two decimals: 0.001 shows as 0.10.
  def percent(fraction: BigDecimal): String =
    val value = fraction.bigDecimal.movePointRight(2).stripTrailingZeros
    value.setScale(math.max(2, value.scale)).toPlainString

  def fraction(percent: String): Either[String, BigDecimal] =
    Some(percent)
      .filter(_.matches("[0-9]+(\\.[0-9]{0,4})?|\\.[0-9]{1,4}"))
      .map(text => BigDecimal(new java.math.BigDecimal(text).movePointLeft(2).stripTrailingZeros))
      .filter(value => value > 0 && value <= BigDecimal("0.01"))
      .toRight(slippageRefusal)

  private def render(bytes: Array[Byte], terms: TradeTerms): String =
    val json = if bytes.isEmpty then ujson.Obj() else ujson.read(bytes)
    val fields = json.obj
    fields("small") = terms.sizes.small.bigDecimal.toPlainString
    fields("big") = terms.sizes.big.bigDecimal.toPlainString
    fields("slippage") = terms.slippage.bigDecimal.toPlainString
    ujson.write(json, indent = 2) + "\n"

  // The rendered text must load under the start-up rules before it replaces the file.
  private def proof(folder: Path, text: String): Either[String, TradingConfig] =
    val temporary = Files.createTempFile(
      folder,
      ".desk-",
      ".tmp",
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )
    try
      Files.writeString(temporary, text)
      TradingConfig.load(Some(temporary))
    finally Files.deleteIfExists(temporary)

  private def unchanged(path: Path, bytes: Array[Byte]): Boolean =
    if !Files.exists(path, LinkOption.NOFOLLOW_LINKS) then bytes.isEmpty
    else bytes.nonEmpty && privateBytes(path).exists(_.sameElements(bytes))

  // The file's bytes, only from a private regular file within the 16 KiB settings limit.
  private def privateBytes(path: Path): Option[Array[Byte]] =
    Try {
      require(TradingConfig.privateFile(path) && Files.size(path) <= 16384)
      Files.readAllBytes(path)
    }.toOption

  private def attempt[A](body: => A): Either[String, A] =
    Try(body).toEither.left.map { error =>
      Option(error.getMessage) match
        case Some(message) if message.startsWith("requirement failed: ") =>
          message.stripPrefix("requirement failed: ")
        case _ => "The settings file could not be read or written"
    }
