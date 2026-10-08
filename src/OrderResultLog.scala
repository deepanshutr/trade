package desk

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import scala.util.control.NonFatal

/** Replay receipts only. Each append owns the lock, so viewing processes may coexist. */
object OrderResultLog:
  final class LockTimeout extends java.io.IOException("order result lock timed out")
  private[desk] final class SavedRecordFailure(cause: Throwable)
      extends java.io.IOException("order result was saved but later work failed", cause)

  final class Writer(
      write: String => Unit,
      failure: Throwable => Unit,
      retryWindowNanos: Long = TimeUnit.SECONDS.toNanos(5)
  ) extends AutoCloseable:
    private val executor = java.util.concurrent.Executors.newSingleThreadExecutor()
    // After close a queued record keeps retrying while this writer still saves records, and gives
    // up once nothing was saved for 500 ms. A busy log drains, and a stuck lock still lets Desk quit.
    private val closing = new java.util.concurrent.atomic.AtomicBoolean(false)
    private val lastActivity = new java.util.concurrent.atomic.AtomicLong(0L)
    private val quietNanos = TimeUnit.MILLISECONDS.toNanos(500)

    private def writeWithRetry(text: String, onFailure: Throwable => Unit): Unit =
      val began = System.nanoTime()
      var saved = false
      var failed = Option.empty[Throwable]
      while !saved && failed.isEmpty do
        try
          write(text)
          saved = true
          lastActivity.set(System.nanoTime())
        catch
          case error: LockTimeout =>
            val now = System.nanoTime()
            if (closing.get() && now - lastActivity.get() >= quietNanos) ||
              now - began >= retryWindowNanos
            then failed = Some(error)
            else
              try Thread.sleep(10L)
              catch
                case interrupted: InterruptedException =>
                  Thread.currentThread().interrupt()
                  failed = Some(interrupted)
          case NonFatal(error) => failed = Some(error)
      failed.foreach(onFailure)

    def append(text: String): Unit =
      append(text, failure)

    def append(text: String, onFailure: Throwable => Unit): Unit =
      try executor.execute(() => writeWithRetry(text, onFailure))
      catch case error: java.util.concurrent.RejectedExecutionException => onFailure(error)
    def close(): Unit =
      lastActivity.set(System.nanoTime())
      closing.set(true)
      executor.close()

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
      LocalFiles.privateFile*
    )

  private def lock(channel: FileChannel, began: Long): FileLock =
    var lease: FileLock = null
    while lease == null do
      try lease = channel.tryLock()
      catch case _: OverlappingFileLockException => ()
      if lease == null then
        if System.nanoTime() - began >= 50000000L then throw new LockTimeout
        Thread.sleep(1L)
    lease

  def append(directory: Path, at: Long, text: String): Unit =
    appendAfterForce(directory, at, text, () => ())

  private[desk] def appendAfterForce(
      directory: Path,
      at: Long,
      text: String,
      afterForce: () => Unit
  ): Unit =
    val began = System.nanoTime()
    if !writing.tryLock(50L, TimeUnit.MILLISECONDS) then throw new LockTimeout
    try appendLocked(directory, at, text, began, afterForce)
    finally writing.unlock()

  private def appendLocked(
      directory: Path,
      at: Long,
      text: String,
      began: Long,
      afterForce: () => Unit
  ): Unit =
    var forced = false
    try
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
            forced = true
            afterForce()
          finally log.close()
          trim(directory.resolve("order-results.jsonl"))
        finally lease.release()
      finally channel.close()
    catch case NonFatal(error) if forced => throw new SavedRecordFailure(error)
