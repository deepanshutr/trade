package desk

import java.lang.reflect.{InvocationTargetException, Proxy}
import java.net.URI
import java.nio.channels.SeekableByteChannel
import java.nio.file.*
import java.nio.file.attribute.*
import java.nio.file.spi.FileSystemProvider

class ArchiveListingSpec extends TempDirectorySuite:
  // Only the directory is wrapped. Resolved files use the real filesystem, so locking,
  // writes and permissions retain their normal behavior. Unexpected probe use fails.
  private class CountingDirectory(directory: Path):
    var listings = 0
    private def unused: Nothing = throw new UnsupportedOperationException("listing probe only")
    private val listingProvider = new FileSystemProvider:
      def getScheme(): String = unused
      def newFileSystem(uri: URI, env: java.util.Map[String, ?]): FileSystem = unused
      def getFileSystem(uri: URI): FileSystem = unused
      def getPath(uri: URI): Path = unused
      def newByteChannel(
          path: Path,
          options: java.util.Set[? <: OpenOption],
          attrs: FileAttribute[?]*
      ): SeekableByteChannel = unused
      def newDirectoryStream(
          path: Path,
          filter: DirectoryStream.Filter[? >: Path]
      ): DirectoryStream[Path] =
        listings += 1
        Files.newDirectoryStream(directory, filter)
      def createDirectory(path: Path, attrs: FileAttribute[?]*): Unit = unused
      def delete(path: Path): Unit = unused
      def copy(source: Path, target: Path, options: CopyOption*): Unit = unused
      def move(source: Path, target: Path, options: CopyOption*): Unit = unused
      def isSameFile(a: Path, b: Path): Boolean = unused
      def isHidden(path: Path): Boolean = unused
      def getFileStore(path: Path): FileStore = unused
      def checkAccess(path: Path, modes: AccessMode*): Unit = unused
      def getFileAttributeView[V <: FileAttributeView](
          path: Path,
          kind: Class[V],
          options: LinkOption*
      ): V = unused
      def readAttributes[A <: BasicFileAttributes](
          path: Path,
          kind: Class[A],
          options: LinkOption*
      ): A = unused
      def readAttributes(
          path: Path,
          attributes: String,
          options: LinkOption*
      ): java.util.Map[String, Object] = unused
      def setAttribute(path: Path, attribute: String, value: Object, options: LinkOption*): Unit =
        unused
    private val filesystem = new FileSystem:
      def provider(): FileSystemProvider = listingProvider
      def close(): Unit = unused
      def isOpen(): Boolean = unused
      def isReadOnly(): Boolean = unused
      def getSeparator(): String = unused
      def getRootDirectories(): java.lang.Iterable[Path] = unused
      def getFileStores(): java.lang.Iterable[FileStore] = unused
      def supportedFileAttributeViews(): java.util.Set[String] = unused
      def getPath(first: String, more: String*): Path = unused
      def getPathMatcher(pattern: String): PathMatcher = unused
      def getUserPrincipalLookupService(): UserPrincipalLookupService = unused
      def newWatchService(): WatchService = unused
    val path: Path = Proxy
      .newProxyInstance(
        classOf[Path].getClassLoader,
        Array(classOf[Path]),
        (_, method, args) =>
          if method.getName == "getFileSystem" then filesystem
          else
            try method.invoke(directory, Option(args).getOrElse(Array.empty[Object])*)
            catch case error: InvocationTargetException => throw error.getCause
      )
      .asInstanceOf[Path]

  test("directory listings scale with chunk rotation rather than event count") {
    val probe = new CountingDirectory(tempDirectory("desk-archive-listings"))
    Files.list(probe.path).close()
    assertEquals(probe.listings, 1)
    probe.listings = 0
    val archive = MarketArchive.open(probe.path, maximumBytes = 10000000L, chunkBytes = 1000000L)
    val at = 1800000000000L
    try
      for i <- 0 until 1000 do archive.record(Event.SocketUp, at + i)
      assert(probe.listings <= 2, s"1000 events required ${probe.listings} directory listings")
      val beforeCheckpoint = probe.listings
      archive.checkpoint()
      assert(probe.listings > beforeCheckpoint, "checkpoint must rescan disk usage")
      val afterCheckpoint = probe.listings
      for i <- 1000 until 2000 do archive.record(Event.SocketUp, at + i)
      assertEquals(probe.listings, afterCheckpoint)
      archive.record(Event.SocketUp, at + 3600000L)
      assertEquals(probe.listings, afterCheckpoint + 1)
    finally archive.close()
  }

  private def withinBudget(checkpointFirst: Boolean): Unit =
    val archive = MarketArchive.open(
      tempDirectory("desk-archive-budget"),
      maximumBytes = 4000L,
      chunkBytes = 500L
    )
    val at = 1800000000000L
    try
      if checkpointFirst then
        archive.record(
          Event.CandleUpdate("XMR", "1m", Candle(at - 60000L, 100, 102, 98, 101, 12)),
          at
        )
        archive.checkpoint()
      for i <- 1 to 300 do
        archive.record(Event.SocketUp, at + i)
        assert(archive.bytes <= 4000L, s"event $i left ${archive.bytes} bytes on disk")
    finally archive.close()

  test("recording between checkpoints stays within the disk budget") {
    withinBudget(checkpointFirst = false)
  }

  test("the disk budget counts the checkpoint file written before later events") {
    withinBudget(checkpointFirst = true)
  }
