package desk

import scala.util.Try

// Holds the trading key for the life of the process. The key is read once, each
// worker takes its own copy, and close zeroes the held key. The reason text is
// fixed so that nothing from the read can reach a message.
object SessionKey:
  // Workers show this as their block, so it fits the panel's 38 columns.
  val unavailable: String = "Signing key is unavailable or differs"

final class SessionKey(read: () => Array[Byte]) extends AutoCloseable:
  private val reading = new Object
  private var first = Option.empty[Either[String, Unit]]
  private var held = Option.empty[Array[Byte]]
  private var closed = false
  private val unavailable = SessionKey.unavailable

  // The first caller reads while later callers wait, then all get that result.
  // Close does not wait for the read. A key that arrives after close is zeroed,
  // and a load after close does not read at all.
  def load(): Either[String, Unit] = reading.synchronized {
    first.getOrElse {
      val result =
        if synchronized(closed) then Left(unavailable)
        else
          Try(read()).fold[Either[String, Unit]](
            _ => Left(unavailable),
            key =>
              val kept = synchronized {
                if closed then false
                else
                  held = Some(key)
                  true
              }
              if kept then Right(())
              else
                java.util.Arrays.fill(key, 0.toByte)
                Left(unavailable)
          )
      first = Some(result)
      result
    }
  }

  def copy(): Array[Byte] = synchronized {
    held.getOrElse(throw new IllegalStateException("no key is held")).clone()
  }

  def close(): Unit = synchronized {
    closed = true
    held.foreach(java.util.Arrays.fill(_, 0.toByte))
    held = None
  }
