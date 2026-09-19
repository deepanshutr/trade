package desk

import java.nio.channels.FileChannel
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.ConcurrentHashMap
import scala.util.Try

object AccountGate:
  private val active = ConcurrentHashMap.newKeySet[Path]()
  def directory: Path = Path.of(System.getProperty("user.home"), ".local/state/desk/accounts")

  private def identity(network: String, owner: String): String =
    require(Set("mainnet", "testnet").contains(network))
    require(owner.matches("0x[0-9a-f]{40}"))
    network + "-" + owner.drop(2)

  def paused(root: Path, network: String, owner: String): Boolean =
    Try {
      val marker = root.resolve(identity(network, owner) + ".paused")
      require(!Files.isSymbolicLink(root))
      !Files.notExists(marker, LinkOption.NOFOLLOW_LINKS)
    }.getOrElse(true)

  def withOrder(root: Path, network: String, owner: String)(send: => Unit): Unit =
    val name = identity(network, owner)
    require(!Files.isSymbolicLink(root), "account state is symbolic")
    Files.createDirectories(
      root,
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))
    )
    require(
      Files.getPosixFilePermissions(root) == PosixFilePermissions.fromString("rwx------"),
      "account state must be private"
    )
    val lockPath = root.resolve(name + ".lock")
    val key = root.toRealPath().resolve(name + ".lock")
    require(active.add(key), "an order already owns this account")
    try
      require(!Files.isSymbolicLink(lockPath), "account lock is symbolic")
      val channel = FileChannel.open(
        lockPath,
        java.util.Set.of[java.nio.file.OpenOption](
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE,
          LinkOption.NOFOLLOW_LINKS
        ),
        PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
      )
      try
        val lock = Option(channel.tryLock())
        require(lock.nonEmpty, "wallet change owns this account")
        try
          require(!paused(root, network, owner), "wallet change paused this account")
          send
        finally lock.get.release()
      finally channel.close()
    finally active.remove(key)
