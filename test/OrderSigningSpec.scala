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

  fixtures("vectors").arr.filter(_("action")("type").str == "order").foreach { fixture =>
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

  test("fund-transfer actions cannot be signed through Desk") {
    val internalTransfer = ujson.Obj(
      "type" -> "agentSendAsset",
      "destination" -> fixtures("vectors")(1)("address").str,
      "sourceDex" -> "",
      "destinationDex" -> "spot",
      "token" -> "USDC:0x6d1e7cde53ba9467b783cb7c530ce054",
      "amount" -> "0.01",
      "fromSubAccount" -> "",
      "nonce" -> ujson.Num(1788684358292L.toDouble)
    )
    val transfers = Vector(
      internalTransfer,
      ujson.Obj("type" -> "usdClassTransfer", "amount" -> "0.01", "toPerp" -> true),
      ujson.Obj("type" -> "subAccountTransfer", "isDeposit" -> true, "usd" -> 10000),
      ujson.Obj("type" -> "vaultTransfer", "isDeposit" -> true, "usd" -> 10000),
      ujson.Obj("type" -> "sendAsset"),
      ujson.Obj("type" -> "usdSend"),
      ujson.Obj("type" -> "spotSend"),
      ujson.Obj("type" -> "withdraw3")
    )
    assertEquals(sign(sample).obj.keySet, Set("r", "s", "v"))
    transfers.foreach { action =>
      val error = intercept[IllegalArgumentException](sign(action))
      assertEquals(error.getMessage, "Unsupported order signing input")
    }
  }

  test("an order cannot carry transfer or builder-fee fields") {
    Vector("destination", "amount", "builder").foreach { name =>
      val action = sample
      action(name) = if name == "builder" then
        ujson.Obj("b" -> fixtures("vectors")(1)("address").str, "f" -> 10)
      else ujson.Str("0.01")
      intercept[IllegalArgumentException](sign(action))
    }
  }

  test("extra fields are rejected at every nested order level") {
    Vector("order", "type", "limit").foreach { level =>
      val action = sample
      val order = action("orders")(0)
      val target = level match
        case "order" => order
        case "type"  => order("t")
        case _       => order("t")("limit")
      target("unexpected") = true
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

  test("the signer signs either reduce-only value of a saved order and nothing else as the flag") {
    val order = SavedOrder(
      "0x0123456789abcdef0123456789abcdef",
      1788684358292L,
      "XMR",
      32767,
      Side.Sell,
      "1.83",
      "546.05",
      "546.05",
      1788684359292L,
      1788684358292L
    )
    // The recorded reference packs "r" as a172 and false as c2. True is c3, as "b" shows in
    // the first vector.
    val reference = fixtures("vectors")(1)("packed").str
    val flag = reference.indexOf("a172c2")
    assert(flag > 0 && flag == reference.lastIndexOf("a172c2"))
    assert(fixtures("vectors")(0)("packed").str.contains("a162c3"))
    assertEquals(hex(OrderSigning.packed(Exchange.action(order))), reference)
    val reducing = Exchange.action(order.copy(reduceOnly = true))
    assertEquals(hex(OrderSigning.packed(reducing)), reference.patch(flag, "a172c3", 6))
    assertNotEquals(sign(reducing), sign(sample))
    Vector[ujson.Value](ujson.Str("true"), ujson.Num(1), ujson.Null).foreach { value =>
      val changed = copy(reducing)
      changed("orders")(0)("r") = value
      intercept[IllegalArgumentException](sign(changed))
    }
    val missing = copy(reducing)
    missing("orders")(0).obj.remove("r")
    intercept[IllegalArgumentException](sign(missing))
  }

  test("leverage and account changes are outside local order signing") {
    val action = copy(fixtures("vectors")(3)("action"))
    val cross = copy(action)
    cross("isCross") = true
    Vector(
      action,
      cross,
      ujson.Obj("type" -> "updateIsolatedMargin", "asset" -> 1, "isBuy" -> true, "ntli" -> 1),
      ujson.Obj("type" -> "approveAgent"),
      ujson.Obj("type" -> "approveBuilderFee"),
      ujson.Obj("type" -> "cancel", "cancels" -> ujson.Arr()),
      ujson.Obj("type" -> "futureAction")
    ).foreach { change =>
      intercept[IllegalArgumentException](sign(change))
    }
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

  private val deskCloid = "0x0123456789abcdef0123456789abcdef"
  private def cancel(asset: ujson.Value = ujson.Num(32767), cloid: ujson.Value = deskCloid) =
    ujson.Obj(
      "type" -> "cancelByCloid",
      "cancels" -> ujson.Arr(ujson.Obj("asset" -> asset, "cloid" -> cloid))
    )

  test("post-only and immediate orders sign and a resting good-till-cancel order does not") {
    Vector("Alo", "Ioc").foreach { tif =>
      val action = sample
      action("orders")(0)("t")("limit")("tif") = tif
      sign(action)
    }
    val resting = sample
    resting("orders")(0)("t")("limit")("tif") = "Gtc"
    intercept[IllegalArgumentException](sign(resting))
  }

  test("exactly Alo and Ioc pass the time in force check") {
    val others: Vector[ujson.Value] = Vector(
      "Gtc",
      "FrontendMarket",
      "LiquidationMarket",
      "alo",
      "ioc",
      "ALO",
      " Ioc",
      "Alo ",
      ""
    ).map(ujson.Str(_)) ++ Vector(ujson.Num(1), ujson.Null, ujson.Bool(true))
    others.foreach { tif =>
      val action = sample
      action("orders")(0)("t")("limit")("tif") = tif
      intercept[IllegalArgumentException](sign(action))
    }
  }

  test("a cancel by client id matches the recorded reference bytes") {
    val fixture = fixtures("vectors").arr.find(_("name").str == "cancelByCloidExpiryMain").get
    val action = fixture("action")
    val nonce = fixture("nonce").num.toLong
    val expiry = Some(fixture("expiresAfter").num.toLong)
    assertEquals(hex(OrderSigning.packed(action)), fixture("packed").str)
    assertEquals(hex(OrderSigning.actionHash(action, nonce, expiry)), fixture("actionHash").str)
    assertEquals(hex(OrderSigning.digest(action, nonce, true, expiry)), fixture("digest").str)
    assertEquals(OrderSigning.sign(secret, action, nonce, true, expiry), fixture("signature"))
  }

  test("only one well formed cancel by client id can be signed") {
    sign(cancel())
    val two = cancel()
    two("cancels").arr.append(ujson.Obj("asset" -> 1, "cloid" -> deskCloid))
    val none = cancel()
    none("cancels").arr.clear()
    val flagged = cancel()
    flagged("f") = 1
    val inner = cancel()
    inner("cancels")(0)("f") = 1
    val swapped = ujson.Obj(
      "type" -> "cancelByCloid",
      "cancels" -> ujson.Arr(ujson.Obj("cloid" -> deskCloid, "asset" -> 1))
    )
    val reordered = ujson.Obj(
      "cancels" -> cancel()("cancels"),
      "type" -> "cancelByCloid"
    )
    Vector[ujson.Value](
      two,
      none,
      flagged,
      inner,
      swapped,
      reordered,
      cancel(asset = ujson.Num(-1)),
      cancel(asset = ujson.Num(1.5)),
      cancel(asset = ujson.Str("1")),
      cancel(cloid = ujson.Str("0x0123456789ABCDEF0123456789abcdef")),
      cancel(cloid = ujson.Str("0x0123")),
      cancel(cloid = ujson.Str("0123456789abcdef0123456789abcdef")),
      cancel(cloid = ujson.Num(5)),
      cancel(cloid = ujson.Str(deskCloid + "0")),
      cancel(cloid = ujson.Str("0x" + "a" * 33)),
      cancel(cloid = ujson.Str(" " + deskCloid)),
      cancel(cloid = ujson.Str(deskCloid + " ")),
      ujson.Obj("type" -> "cancel", "cancels" -> cancel()("cancels")),
      ujson.Obj("type" -> "cancel", "cancels" -> ujson.Arr(ujson.Obj("a" -> 1, "o" -> 77738308))),
      ujson.Obj("type" -> "modify", "oid" -> 1, "order" -> sample("orders")(0)),
      ujson.Obj("type" -> "batchModify", "modifies" -> ujson.Arr()),
      ujson.Obj("type" -> "scheduleCancel", "time" -> 1788684359292L)
    ).foreach(action => intercept[IllegalArgumentException](sign(action)))
  }
