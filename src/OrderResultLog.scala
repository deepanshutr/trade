package desk

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import scala.util.control.NonFatal

/** Replay receipts only. Each append owns the lock, so viewing processes may coexist. */
object OrderResultLog:
  final class Writer(write: String => Unit, failure: Throwable => Unit) extends AutoCloseable:
    private val executor = java.util.concurrent.Executors.newSingleThreadExecutor()
    private val retryWindowNanos = TimeUnit.SECONDS.toNanos(5)

    private def writeWithRetry(text: String): Unit =
      val began = System.nanoTime()
      var saved = false
      var failed = Option.empty[Throwable]
      while !saved && failed.isEmpty do
        try
          write(text)
          saved = true
        catch
          case error: java.io.IOException if error.getMessage == "order result lock timed out" =>
            if System.nanoTime() - began >= retryWindowNanos then failed = Some(error)
            else
              try Thread.sleep(10L)
              catch
                case interrupted: InterruptedException =>
                  Thread.currentThread().interrupt()
                  failed = Some(interrupted)
          case NonFatal(error) => failed = Some(error)
      failed.foreach(failure)

    def append(text: String): Unit =
      try executor.execute(() => writeWithRetry(text))
      catch case error: java.util.concurrent.RejectedExecutionException => failure(error)
    def close(): Unit = executor.close()

  private[desk] def trim(path: Path, beforeReplace: () => Unit = () => ()): Unit =
    val lines = new String(
      Files.readAllBytes(path),
      java.nio.charset.StandardCharsets.UTF_8
    ).linesIterator.toVector
    if lines.size > 1000 then
      val records = lines.filter(line => scala.util.Try(ujson.read(line)).isSuccess).takeRight(1000)
      LocalFiles.write(path, records.mkString("", "\n", "\n"), beforeReplace)

  private val writing = new ReentrantLock(true)
  private def open(path: Path, append: Boolean): FileChannel =
    require(
      !Files.exists(path, LinkOption.NOFOLLOW_LINKS) || TradingConfig.privateFile(path),
      "invalid order result file"
    )
    val options = java.util.HashSet[java.nio.file.OpenOption]()
    options.add(StandardOpenOption.CREATE)
    options.add(StandardOpenOption.WRITE)
    options.add(LinkOption.NOFOLLOW_LINKS)
    if append then options.add(StandardOpenOption.READ)
    FileChannel.open(
      path,
      options,
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )

  private def lock(channel: FileChannel, began: Long): FileLock =
    var lease: FileLock = null
    while lease == null do
      try lease = channel.tryLock()
      catch case _: OverlappingFileLockException => ()
      if lease == null then
        if System.nanoTime() - began >= 50000000L then
          throw new java.io.IOException("order result lock timed out")
        Thread.sleep(1L)
    lease

  def append(directory: Path, at: Long, text: String): Unit =
    val began = System.nanoTime()
    if !writing.tryLock(50L, TimeUnit.MILLISECONDS) then
      throw new java.io.IOException("order result lock timed out")
    try appendLocked(directory, at, text, began)
    finally writing.unlock()

  private def appendLocked(directory: Path, at: Long, text: String, began: Long): Unit = {
    LocalFiles.directory(directory)
    val channel = open(directory.resolve("order-results.lock"), false)
    try
      val lease = lock(channel, began)
      try
        val log = open(directory.resolve("order-results.jsonl"), true)
        try
          val record = ujson.Obj(
            "id" -> java.util.UUID.randomUUID().toString,
            "at" -> at,
            "kind" -> "order",
            "text" -> text
          )
          val end = log.size()
          val last = ByteBuffer.allocate(1)
          val separator = if end > 0 then
            log.read(last, end - 1)
            if last.get(0) == '\n'.toByte then "" else "\n"
          else ""
          log.position(end)
          val bytes = ByteBuffer.wrap(
            (separator + record.toString + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
          )
          while bytes.hasRemaining do log.write(bytes)
          log.force(true)
        finally log.close()
        trim(directory.resolve("order-results.jsonl"))
      finally lease.release()
    finally channel.close()
  }
