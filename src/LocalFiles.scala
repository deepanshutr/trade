package desk

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions

object LocalFiles:
  def directory(path: Path): Unit =
    require(!Files.isSymbolicLink(path), "local storage cannot be a symbolic link")
    Files.createDirectories(
      path,
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))
    )
    require(Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS), "local storage is not a directory")
    val permissions = PosixFilePermissions.toString(Files.getPosixFilePermissions(path))
    require(permissions.drop(3) == "------", "local storage must be private")

  def read(path: Path, maximum: Long): Option[String] =
    if !Files.exists(path, LinkOption.NOFOLLOW_LINKS) then None
    else
      require(TradingConfig.privateFile(path) && Files.size(path) <= maximum, "invalid local file")
      Some(Files.readString(path))

  def write(path: Path, text: String): Unit =
    directory(path.getParent)
    require(
      !Files.exists(path, LinkOption.NOFOLLOW_LINKS) || TradingConfig.privateFile(path),
      "invalid local file"
    )
    val temporary = Files.createTempFile(
      path.getParent,
      ".desk-",
      ".tmp",
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )
    try
      val channel = FileChannel.open(temporary, StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS)
      try
        val bytes = ByteBuffer.wrap(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        while bytes.hasRemaining do channel.write(bytes)
        channel.force(true)
      finally channel.close()
      Files.move(
        temporary,
        path,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      val parent = FileChannel.open(path.getParent, StandardOpenOption.READ)
      try parent.force(true)
      finally parent.close()
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
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )
    try
      val held = channel.tryLock()
      require(held != null, "another process owns this local store")
      new Lease(channel, held)
    catch
      case error: Exception =>
        channel.close()
        throw error
