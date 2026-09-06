package desk

import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

class OrderSigningSpec extends munit.FunSuite:
  private val fixtures =
    ujson.read(Files.readString(Paths.get("fixtures", "order_signing.json")))
  private val secret = java.util.HexFormat
    .of()
    .parseHex(
      "0123456789012345678901234567890123456789012345678901234567890123"
    )
  private val curveOrder =
    new BigInteger("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
  private def hex(bytes: Array[Byte]): String = java.util.HexFormat.of().formatHex(bytes)
  private def copy(value: ujson.Value): ujson.Value = ujson.read(ujson.write(value))
  private def sample: ujson.Value = copy(fixtures("vectors")(1)("action"))
  private def sign(action: ujson.Value): ujson.Value =
    OrderSigning.sign(secret, action, 1788684358292L, true, Some(1788684359292L))

  fixtures("vectors").arr.foreach { fixture =>
    test("matches recorded signing bytes for " + fixture("name").str) {
      val action = fixture("action")
      val nonce = fixture("nonce").num.toLong
      val expiry = fixture("expiresAfter") match
        case ujson.Null       => None
        case ujson.Num(value) => Some(value.toLong)
        case _                => fail("Invalid signing fixture")
      assertEquals(hex(OrderSigning.packed(action)), fixture("packed").str)
      assertEquals(hex(OrderSigning.actionHash(action, nonce, expiry)), fixture("actionHash").str)
      assertEquals(
        hex(OrderSigning.digest(action, nonce, fixture("mainnet").bool, expiry)),
        fixture("digest").str
      )
      val signature =
        OrderSigning.sign(secret, action, nonce, fixture("mainnet").bool, expiry)
      assertEquals(signature, fixture("signature"))
      assertEquals(OrderSigning.address(secret), fixture("address").str)
      val s = new BigInteger(signature("s").str.drop(2), 16)
      assert(s.signum() > 0 && s.compareTo(curveOrder.shiftRight(1)) <= 0)
    }
  }

  test("positive integer encodings match the reference at each boundary") {
    fixtures("integerBytes").obj.foreach { case (number, expected) =>
      assertEquals(hex(OrderSigning.packed(ujson.Num(number.toDouble))), expected.str)
    }
    assertEquals(hex(OrderSigning.packed(ujson.Num(100000000000L.toDouble))), "cf000000174876e800")
  }

  test("the nonce uses exactly eight big endian bytes and expiry has its own marker") {
    val action = sample
    val packed = java.util.HexFormat.of().parseHex(fixtures("vectors")(1)("packed").str)
    Vector(0L, 1L, 255L, 256L, 4294967296L, 1788684358292L).foreach { nonce =>
      val suffix = ByteBuffer.allocate(9).putLong(nonce).put(0.toByte).array()
      assertEquals(
        hex(OrderSigning.actionHash(action, nonce, None)),
        hex(OrderSigning.hash(packed ++ suffix))
      )
      val expiry = nonce + 1000
      val withExpiry =
        ByteBuffer.allocate(18).putLong(nonce).put(0.toByte).put(0.toByte).putLong(expiry).array()
      assertEquals(
        hex(OrderSigning.actionHash(action, nonce, Some(expiry))),
        hex(OrderSigning.hash(packed ++ withExpiry))
      )
    }
  }

  test("strings use their byte length at compact boundaries") {
    val short = "a" * 31
    val longer = "a" * 32
    assertEquals(hex(OrderSigning.packed(ujson.Str(short))), "bf" + "61" * 31)
    assertEquals(hex(OrderSigning.packed(ujson.Str(longer))), "d920" + "61" * 32)
    val unicode = "é" * 16
    assertEquals(
      hex(OrderSigning.packed(ujson.Str(unicode))),
      "d920" + hex(unicode.getBytes(UTF_8))
    )
  }

  test("signing is deterministic and does not alter the secret or action") {
    val action = sample
    val originalAction = ujson.write(action)
    val originalSecret = secret.clone()
    assertEquals(sign(action), sign(action))
    assertEquals(ujson.write(action), originalAction)
    assert(secret.sameElements(originalSecret))
  }

  test("the signature changes with either side or a valid limit change") {
    val original = sample
    val otherSide = sample
    otherSide("orders")(0)("b") = true
    val otherLimit = sample
    otherLimit("orders")(0)("p") = "546.06"
    assertNotEquals(sign(original), sign(otherSide))
    assertNotEquals(sign(original), sign(otherLimit))
  }

  test("unsupported actions, extra fields and resting orders cannot be signed") {
    val withdrawn = ujson.Obj("type" -> "withdraw3")
    val extra = sample
    extra("vaultAddress") = "0x0000000000000000000000000000000000000000"
    val resting = sample
    resting("orders")(0)("t")("limit")("tif") = "Gtc"
    val grouped = sample
    grouped("grouping") = "positionTpsl"
    val priority = sample
    priority("grouping") = ujson.Obj("p" -> 100)
    Vector(withdrawn, extra, resting, grouped, priority).foreach { action =>
      intercept[IllegalArgumentException](sign(action))
    }
  }

  test("action field order is explicit") {
    val action = sample
    val changed = ujson.Obj(
      "orders" -> action("orders"),
      "type" -> action("type"),
      "grouping" -> action("grouping")
    )
    assertNotEquals(hex(OrderSigning.packed(action)), hex(OrderSigning.packed(changed)))
    intercept[IllegalArgumentException](sign(changed))
  }

  test("one press signs exactly one order") {
    val empty = sample
    empty("orders") = ujson.Arr()
    val multiple = sample
    multiple("orders").arr += copy(multiple("orders")(0))
    Vector(empty, multiple).foreach { action =>
      intercept[IllegalArgumentException](sign(action))
    }
  }

  test("prices and sizes must be positive canonical decimal strings") {
    val invalid = Vector[ujson.Value](
      ujson.Num(546.05),
      ujson.Str("0"),
      ujson.Str("-1"),
      ujson.Str("1.0"),
      ujson.Str("01"),
      ujson.Str("1e2"),
      ujson.Str(" 1"),
      ujson.Str("1."),
      ujson.Null
    )
    Vector("p", "s").foreach { field =>
      invalid.foreach { value =>
        val action = sample
        action("orders")(0)(field) = value
        intercept[IllegalArgumentException](sign(action))
      }
    }
  }

  test("asset numbers are whole, nonnegative and fit the supported range") {
    Vector(-1.0, 1.5, 2147483648.0, Double.NaN, Double.PositiveInfinity).foreach { value =>
      val action = sample
      action("orders")(0)("a") = ujson.Num(value)
      intercept[IllegalArgumentException](sign(action))
    }
  }

  test("client identifiers have exactly sixteen lowercase hexadecimal bytes") {
    Vector("0x01", "0x" + "a" * 33, "0x" + "A" * 32, "a" * 32).foreach { value =>
      val action = sample
      action("orders")(0)("c") = value
      intercept[IllegalArgumentException](sign(action))
    }
  }

  test("leverage is five and the margin mode is explicit") {
    val action = copy(fixtures("vectors")(3)("action"))
    val cross = copy(action)
    cross("isCross") = true
    assertNotEquals(sign(action), sign(cross))
    action("leverage") = 6
    intercept[IllegalArgumentException](sign(action))
    val missing = ujson.Obj("type" -> "updateLeverage", "asset" -> 1, "leverage" -> 5)
    intercept[IllegalArgumentException](sign(missing))
  }

  test("nonce and expiry reject impossible local deadlines") {
    intercept[IllegalArgumentException] {
      OrderSigning.sign(secret, sample, -1, true, None)
    }
    Vector(0L, 9L, 10L).foreach { expiry =>
      intercept[IllegalArgumentException] {
        OrderSigning.sign(secret, sample, 10, true, Some(expiry))
      }
    }
  }

  test("invalid private scalars fail before signing without echoing their value") {
    val invalid = Vector(
      Array.emptyByteArray,
      Array.fill[Byte](31)(1),
      Array.fill[Byte](32)(0),
      java.util.HexFormat.of().parseHex(curveOrder.toString(16)),
      Array.fill[Byte](32)(255.toByte)
    )
    invalid.foreach { key =>
      val error = intercept[IllegalArgumentException](OrderSigning.address(key))
      assertEquals(error.getMessage, "Unsupported order signing input")
      intercept[IllegalArgumentException] {
        OrderSigning.sign(key, sample, 10, true, Some(20))
      }
    }
  }
