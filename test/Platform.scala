package desk

import java.nio.file.{FileSystems, Files, LinkOption, Path}
import java.nio.file.attribute.{
  AclEntry,
  AclEntryPermission,
  AclEntryType,
  AclFileAttributeView,
  PosixFilePermissions
}
import scala.jdk.CollectionConverters.*
import scala.util.Try

// Test helpers that work where the file system has POSIX permission bits and on Windows.
object Platform:
  private def acl(path: Path): AclFileAttributeView =
    Files.getFileAttributeView(path, classOf[AclFileAttributeView], LinkOption.NOFOLLOW_LINKS)

  private lazy val everyone =
    FileSystems.getDefault.getUserPrincipalLookupService.lookupPrincipalByName("Everyone")

  // Sets the mode bits, or on Windows opens the path to Everyone when the mode gives the group
  // or others any right and closes it again when it gives them none. Owner bits stay as they
  // are on Windows, so a test about the owner's own rights runs only where LocalFiles.posix.
  def setMode(path: Path, mode: String): Unit =
    if LocalFiles.posix then
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(mode))
    else
      val view = acl(path)
      val kept = view.getAcl.asScala.filterNot(_.principal == everyone)
      if mode.drop(3).exists(_ != '-') then
        val open = AclEntry
          .newBuilder()
          .setType(AclEntryType.ALLOW)
          .setPrincipal(everyone)
          .setPermissions(AclEntryPermission.READ_DATA, AclEntryPermission.READ_ATTRIBUTES)
          .build()
        view.setAcl((open +: kept).asJava)
      else view.setAcl(kept.asJava)

  // True when the path has exactly this mode, or on Windows when the mode is private and the
  // access list keeps the path private.
  def hasMode(path: Path, mode: String): Boolean =
    if LocalFiles.posix then
      val bits = Files.getPosixFilePermissions(path, LinkOption.NOFOLLOW_LINKS)
      PosixFilePermissions.toString(bits) == mode
    else LocalFiles.isPrivate(path) == !mode.drop(3).exists(_ != '-')

  // Windows lets only an administrator or developer mode make a symbolic link.
  lazy val links: Boolean =
    Try {
      val folder = Files.createTempDirectory("desk-link-probe")
      try
        val link = Files.createSymbolicLink(folder.resolve("link"), folder.resolve("target"))
        Files.delete(link)
      finally Files.delete(folder)
    }.isSuccess

  // A second name for a folder: a symbolic link, or a junction where Windows refuses the link.
  def alias(link: Path, folder: Path): Path =
    if links then Files.createSymbolicLink(link, folder)
    else
      val made = new ProcessBuilder("cmd", "/c", "mklink", "/J", link.toString, folder.toString)
        .redirectErrorStream(true)
        .start()
      val said = new String(made.getInputStream.readAllBytes())
      require(made.waitFor() == 0 && Files.isDirectory(link), said)
      link

  // A Unix absolute path for a program the test fakes, made absolute on Windows too.
  def absolute(unix: String): String =
    if Term.windows then "C:" + unix.replace('/', '\\') else unix

  // A second process that tests or holds a lock the way the Python vault does with lockf. Windows
  // has no lockf, so there a second JVM takes the same lock through FileChannel instead.
  def lockChild(unix: => ProcessBuilder, words: String*): ProcessBuilder =
    if Term.windows then
      val java = Path.of(System.getProperty("java.home"), "bin", "java").toString
      val command = Seq(java, "-cp", System.getProperty("java.class.path"), "desk.LockChild")
      new ProcessBuilder((command ++ words)*)
    else unix

// probe PATH exits 73 when another process holds the lock and 0 when it can take it.
// hold PATH [SIGNAL] takes the lock, prints locked, touches SIGNAL and keeps the lock until its
// input ends.
object LockChild:
  def main(args: Array[String]): Unit =
    val channel = java.nio.channels.FileChannel.open(
      Path.of(args(1)),
      java.nio.file.StandardOpenOption.READ,
      java.nio.file.StandardOpenOption.WRITE
    )
    args(0) match
      case "probe" => System.exit(if channel.tryLock() == null then 73 else 0)
      case _       =>
        channel.lock()
        println("locked")
        System.out.flush()
        args.lift(2).foreach(signal => Files.createFile(Path.of(signal)))
        while System.in.read() >= 0 do ()
        System.exit(0)
