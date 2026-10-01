package desk

class SecretFieldSpec extends munit.FunSuite:
  private val zeros = (n: Int) => new Array[Byte](n)

  private def fill(field: SecretField, text: String): Unit =
    text.foreach(c => assert(field.append(c.toByte)))

  test("a new field is empty and its buffer is zero") {
    val field = new SecretField(8)
    assertEquals(field.length, 0)
    assert(java.util.Arrays.equals(field.bytes, zeros(8)))
  }

  test("append adds bytes in order and bytes is the live buffer") {
    val field = new SecretField(8)
    val buffer = field.bytes
    fill(field, "abc")
    assertEquals(field.length, 3)
    assert(field.bytes eq buffer, "bytes must not copy the content")
    assertEquals(buffer.take(3).toSeq, "abc".getBytes("US-ASCII").toSeq)
  }

  test("a full field refuses more bytes and keeps its content") {
    val field = new SecretField(3)
    fill(field, "abc")
    assert(!field.append('d'.toByte))
    assertEquals(field.length, 3)
    assertEquals(field.bytes.toSeq, "abc".getBytes("US-ASCII").toSeq)
  }

  test("backspace removes the last byte and zeroes the place it held") {
    val field = new SecretField(8)
    fill(field, "abc")
    field.backspace()
    assertEquals(field.length, 2)
    assertEquals(field.bytes.take(3).toSeq, Seq[Byte]('a'.toByte, 'b'.toByte, 0))
    field.backspace()
    field.backspace()
    field.backspace()
    assertEquals(field.length, 0)
    assert(java.util.Arrays.equals(field.bytes, zeros(8)))
  }

  test("wipe zeroes the whole buffer and the field can be used again") {
    val field = new SecretField(8)
    val buffer = field.bytes
    fill(field, "abcdefgh")
    field.wipe()
    assertEquals(field.length, 0)
    assert(java.util.Arrays.equals(buffer, zeros(8)))
    assert(field.append('z'.toByte))
    assertEquals(field.length, 1)
    assertEquals(buffer(0), 'z'.toByte)
  }

  test("toString never shows the content") {
    val field = new SecretField(16)
    fill(field, "hunter2-secret")
    val text = field.toString
    assert(!text.contains("hunter2"))
    assert(!text.contains("secret"))
    assertEquals(text, "SecretField(14)")
  }

  test("a field needs a positive capacity") {
    intercept[IllegalArgumentException](new SecretField(0))
    intercept[IllegalArgumentException](new SecretField(-1))
  }
