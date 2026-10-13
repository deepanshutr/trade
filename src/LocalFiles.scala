package desk

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{FileSystems, Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}
import java.nio.file.attribute.{
  AclEntryType,
  AclFileAttributeView,
  BasicFileAttributes,
  FileAttribute,
  PosixFilePermissions,
  UserPrincipal
}
import scala.jdk.CollectionConverters.*
import scala.util.Try

object LocalFiles:
  // Windows has no permission bits. There a path is private when its access list allows only
  // the user, SYSTEM and the Administrators group, which a folder in the user's profile passes
  // on to what Desk creates inside it. New paths take no attribute there and are checked instead.
  val posix: Boolean = FileSystems.getDefault.supportedFileAttributeViews.contains("posix")

  def privateFile: Seq[FileAttribute[?]] =
    if posix then
      Seq(PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")))
    else Nil

  def privateFolder: Seq[FileAttribute[?]] =
    if posix then
      Seq(PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------")))
    else Nil

  // The user, and the accounts that may also reach a private path. A failed lookup leaves none,
  // so every check fails closed.
  private[desk] final case class Accounts(user: UserPrincipal, others: Set[UserPrincipal])

  private lazy val accounts: Option[Accounts] =
    Try {
      val lookup = FileSystems.getDefault.getUserPrincipalLookupService
      Accounts(
        lookup.lookupPrincipalByName(System.getProperty("user.name")),
        Set("NT AUTHORITY\\SYSTEM", "BUILTIN\\Administrators").map(lookup.lookupPrincipalByName)
      )
    }.toOption

  // Private on Windows: an allowed owner, an allow entry for the user, and no allow entry for anyone
  // else. The JDK reads a missing access list, which lets everyone in, as an empty one.
  private[desk] def privateAccess(
      owner: UserPrincipal,
      entries: Seq[(AclEntryType, UserPrincipal)],
      accounts: Option[Accounts]
  ): Boolean =
    accounts.exists { known =>
      val allowed = known.others + known.user
      allowed(owner) &&
      entries.exists((kind, who) => kind == AclEntryType.ALLOW && who == known.user) &&
      entries.forall((kind, who) => kind != AclEntryType.ALLOW || allowed(who))
    }

  // No group or other permission bits, or the access list rule above on Windows.
  def isPrivate(path: Path): Boolean =
    if posix then
      PosixFilePermissions
        .toString(Files.getPosixFilePermissions(path, LinkOption.NOFOLLOW_LINKS))
        .drop(3) == "------"
    else
      Option(
        Files.getFileAttributeView(path, classOf[AclFileAttributeView], LinkOption.NOFOLLOW_LINKS)
      ).exists(view =>
        privateAccess(
          view.getOwner,
          view.getAcl.asScala.toSeq.map(e => (e.`type`, e.principal)),
          accounts
        )
      )

  // Writes a folder's entries to disk after a rename. Windows cannot open a folder this way, so
  // there the rename is left to the file system's own journal.
  def flushFolder(path: Path): Unit =
    if posix then
      val folder = FileChannel.open(path, StandardOpenOption.READ)
      try folder.force(true)
      finally folder.close()

  // A symbolic link, or a Windows junction, which the JDK reports as neither a link nor a file
  // but as other when the link is not followed. A missing path is not linked. Any other failure to
  // read the path throws, so the check fails closed.
  def linked(path: Path): Boolean =
    Files.isSymbolicLink(path) ||
      (try
        Files
          .readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
          .isOther
      catch case _: java.nio.file.NoSuchFileException => false)

  def directory(path: Path): Unit =
    require(!linked(path), "local storage cannot be a symbolic link")
    Files.createDirectories(path, privateFolder*)
    require(Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS), "local storage is not a directory")
    require(isPrivate(path), "local storage must be private")

  def read(path: Path, maximum: Long): Option[String] =
    if !Files.exists(path, LinkOption.NOFOLLOW_LINKS) then None
    else
      require(TradingConfig.privateFile(path) && Files.size(path) <= maximum, "invalid local file")
      Some(Files.readString(path))

  def write(path: Path, text: String, beforeReplace: () => Unit = () => ()): Unit =
    directory(path.getParent)
    require(
      !Files.exists(path, LinkOption.NOFOLLOW_LINKS) || TradingConfig.privateFile(path),
      "invalid local file"
    )
    val temporary = Files.createTempFile(
      path.getParent,
      ".desk-",
      ".tmp",
      privateFile*
    )
    try
      val channel = FileChannel.open(temporary, StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS)
      try
        val bytes = ByteBuffer.wrap(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        while bytes.hasRemaining do channel.write(bytes)
        channel.force(true)
      finally channel.close()
      beforeReplace()
      Files.move(
        temporary,
        path,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      flushFolder(path.getParent)
    finally Files.deleteIfExists(temporary)

  final class Lease private[LocalFiles] (channel: FileChannel, held: FileLock)
      extends AutoCloseable:
    def close(): Unit =
      try held.release()
      finally channel.close()

  def lock(path: Path): Lease =
    directory(path.getParent)
    require(
      !Files.exists(path, LinkOption.NOFOLLOW_LINKS) || TradingConfig.privateFile(path),
      "invalid storage lock"
    )
    val channel = FileChannel.open(
      path,
      java.util.Set.of[java.nio.file.OpenOption](
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        LinkOption.NOFOLLOW_LINKS
      ),
      privateFile*
    )
    try
      val held = channel.tryLock()
      require(held != null, "another process owns this local store")
      new Lease(channel, held)
    catch
      case error: Exception =>
        channel.close()
        throw error
