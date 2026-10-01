package desk

// A fixed buffer for one secret typed on the key screen. Bytes go in and out one at a time and
// the content is never copied into a String. toString shows the length and nothing else.
final class SecretField(capacity: Int):
  require(capacity > 0, "a field needs a positive capacity")
  private val buffer = new Array[Byte](capacity)
  private var used = 0

  def length: Int = used

  def append(byte: Byte): Boolean =
    if used >= capacity then false
    else
      buffer(used) = byte
      used += 1
      true

  def backspace(): Unit =
    if used > 0 then
      used -= 1
      buffer(used) = 0

  // The live buffer. Only the first length bytes are in use.
  def bytes: Array[Byte] = buffer

  def wipe(): Unit =
    java.util.Arrays.fill(buffer, 0.toByte)
    used = 0

  override def toString: String = s"SecretField($used)"
