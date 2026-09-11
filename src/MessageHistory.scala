package desk

import java.nio.file.Path
import upickle.default.{ReadWriter, macroRW, read, write}

object MessageHistory:
  private given ReadWriter[DeskMessage] = macroRW
  private val maximumBytes = 10_000_000L
  private def validate(messages: Vector[DeskMessage]): Unit =
    require(
      messages.size <= 300 && messages.map(_.id).distinct.size == messages.size && messages.forall(
        m =>
          m.id >= 0 && m.at >= 0 && m.text.length <= 30000 && Set(
            "analysis",
            "question",
            "order",
            "mode",
            "storage",
            "data",
            "news"
          ).contains(m.kind)
      )
    )
  private def encode(messages: Vector[DeskMessage]): String =
    val text = write(messages)
    require(
      text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length <= maximumBytes,
      "message history exceeds storage limit"
    )
    text
  def open(directory: Path): MessageHistory =
    val lease = LocalFiles.lock(directory.resolve("messages.lock"))
    try
      val path = directory.resolve("messages.json")
      val saved = LocalFiles
        .read(path, maximumBytes)
        .map(read[Vector[DeskMessage]](_))
        .getOrElse(Vector.empty)
      validate(saved)
      new MessageHistory(saved, value => LocalFiles.write(path, encode(value)), lease)
    catch
      case error: Exception =>
        lease.close()
        throw error

final class MessageHistory private (
    initial: Vector[DeskMessage],
    persist: Vector[DeskMessage] => Unit,
    lease: AutoCloseable
) extends AutoCloseable:
  private var saved = initial
  private var closed = false
  def messages: Vector[DeskMessage] = synchronized(saved)
  def save(next: Vector[DeskMessage]): Unit = synchronized {
    require(!closed, "message storage is closed")
    MessageHistory.validate(next)
    persist(next)
    saved = next
  }
  def close(): Unit = synchronized {
    if !closed then
      closed = true
      lease.close()
  }
