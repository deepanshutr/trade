package desk

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, LinkOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions

/** Replay receipts only. Each append owns the lock, so viewing processes may coexist. */
object OrderResultLog:
  private def open(path: Path, append: Boolean): FileChannel =
    require(
      !Files.exists(path, LinkOption.NOFOLLOW_LINKS) || TradingConfig.privateFile(path),
      "invalid order result file"
    )
    val options = java.util.HashSet[java.nio.file.OpenOption]()
    options.add(StandardOpenOption.CREATE)
    options.add(StandardOpenOption.WRITE)
    options.add(LinkOption.NOFOLLOW_LINKS)
    if append then options.add(StandardOpenOption.APPEND)
    FileChannel.open(
      path,
      options,
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )

  def append(directory: Path, at: Long, text: String): Unit = synchronized {
    LocalFiles.directory(directory)
    val channel = open(directory.resolve("order-results.lock"), false)
    try
      val lease = channel.lock()
      try
        val log = open(directory.resolve("order-results.jsonl"), true)
        try
          val record = ujson.Obj(
            "id" -> java.util.UUID.randomUUID().toString,
            "at" -> at,
            "kind" -> "order",
            "text" -> text
          )
          val bytes = ByteBuffer.wrap(
            (record.toString + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)
          )
          while bytes.hasRemaining do log.write(bytes)
          log.force(true)
        finally log.close()
      finally lease.release()
    finally channel.close()
  }
