package desk

import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import org.bouncycastle.crypto.digests.{KeccakDigest, SHA256Digest}
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.ECAlgorithms

object OrderSigning:
  private val curve = CustomNamedCurves.getByName("secp256k1")
  private val domain =
    new ECDomainParameters(curve.getCurve, curve.getG, curve.getN, curve.getH)
  private val order = curve.getN
  private val halfOrder = order.shiftRight(1)

  private def invalid(): Nothing =
    throw new IllegalArgumentException("Unsupported order signing input")

  private def scalar(secret: Array[Byte]): BigInteger =
    if secret == null || secret.length != 32 then invalid()
    val value = new BigInteger(1, secret)
    if value.signum() <= 0 || value.compareTo(order) >= 0 then invalid()
    value

  private[desk] def hash(bytes: Array[Byte]): Array[Byte] =
    val digest = new KeccakDigest(256)
    digest.update(bytes, 0, bytes.length)
    val result = new Array[Byte](32)
    digest.doFinal(result, 0)
    result

  private def hashText(value: String): Array[Byte] = hash(value.getBytes(UTF_8))

  private def word(value: BigInteger): Array[Byte] =
    val bytes = value.toByteArray
    if value.signum() < 0 || value.bitLength() > 256 then invalid()
    val unsigned = if bytes.length > 32 then bytes.drop(1) else bytes
    Array.fill[Byte](32 - unsigned.length)(0) ++ unsigned

  def address(secret: Array[Byte]): String =
    val point = curve.getG.multiply(scalar(secret)).normalize().getEncoded(false)
    "0x" + java.util.HexFormat.of().formatHex(hash(point.drop(1)).takeRight(20))

  private def objectValue(value: ujson.Value): ujson.Obj = value match
    case obj: ujson.Obj => obj
    case _              => invalid()

  private def fields(value: ujson.Value, expected: Vector[String]): ujson.Obj =
    val obj = objectValue(value)
    if obj.value.keysIterator.toVector != expected then invalid()
    obj

  private def text(value: ujson.Value): String = value match
    case ujson.Str(value) => value
    case _                => invalid()

  private def bool(value: ujson.Value): Boolean = value match
    case ujson.Bool(value) => value
    case _                 => invalid()

  private def integer(value: ujson.Value): Int = value match
    case ujson.Num(value)
        if java.lang.Double.isFinite(value) && value >= 0 &&
          value <= Int.MaxValue && value == math.floor(value) =>
      value.toInt
    case _ => invalid()

  private def decimal(value: ujson.Value): Unit =
    val raw = text(value)
    if raw.length > 64 || !raw.matches("(0|[1-9][0-9]*)(\\.[0-9]+)?") then invalid()
    val amount = BigDecimal(raw)
    if amount <= 0 || amount.bigDecimal.stripTrailingZeros().toPlainString != raw then invalid()

  private def validate(action: ujson.Value): Unit =
    val root = objectValue(action)
    text(root.value.getOrElse("type", invalid())) match
      case "order" =>
        fields(root, Vector("type", "orders", "grouping"))
        if text(root("grouping")) != "na" then invalid()
        val values = root("orders") match
          case array: ujson.Arr if array.value.size == 1 => array.value
          case _                                         => invalid()
        val value = objectValue(values.head)
        val names = Vector("a", "b", "p", "s", "r", "t") ++
          (if value.value.contains("c") then Vector("c") else Vector.empty)
        fields(value, names)
        integer(value("a"))
        bool(value("b"))
        decimal(value("p"))
        decimal(value("s"))
        bool(value("r"))
        val orderType = fields(value("t"), Vector("limit"))
        val limitType = fields(orderType("limit"), Vector("tif"))
        if text(limitType("tif")) != "Ioc" then invalid()
        value.value.get("c").foreach { identifier =>
          if !text(identifier).matches("0x[0-9a-f]{32}") then invalid()
        }
      case _ => invalid()

  private def writeUnsigned(out: ByteArrayOutputStream, value: Long, width: Int): Unit =
    (0 until width).reverse.foreach { index =>
      out.write(((value >>> (index * 8)) & 255L).toInt)
    }

  private[desk] def packed(value: ujson.Value): Array[Byte] =
    val out = new ByteArrayOutputStream()
    def header(size: Int, fixed: Int, wide16: Int, wide32: Int): Unit =
      if size <= 15 then out.write(fixed | size)
      else if size <= 65535 then
        out.write(wide16)
        writeUnsigned(out, size.toLong, 2)
      else
        out.write(wide32)
        writeUnsigned(out, size.toLong, 4)
    def string(raw: String): Unit =
      val bytes = raw.getBytes(UTF_8)
      if bytes.length <= 31 then out.write(160 | bytes.length)
      else if bytes.length <= 255 then
        out.write(217)
        writeUnsigned(out, bytes.length.toLong, 1)
      else if bytes.length <= 65535 then
        out.write(218)
        writeUnsigned(out, bytes.length.toLong, 2)
      else
        out.write(219)
        writeUnsigned(out, bytes.length.toLong, 4)
      out.write(bytes)
    def number(raw: Double): Unit =
      if !java.lang.Double.isFinite(raw) || raw < 0 ||
        raw > 9007199254740991L.toDouble || raw != math.floor(raw)
      then invalid()
      val value = raw.toLong
      if value <= 127 then out.write(value.toInt)
      else if value <= 255 then
        out.write(204)
        writeUnsigned(out, value, 1)
      else if value <= 65535 then
        out.write(205)
        writeUnsigned(out, value, 2)
      else if value <= 4294967295L then
        out.write(206)
        writeUnsigned(out, value, 4)
      else
        out.write(207)
        writeUnsigned(out, value, 8)
    def loop(current: ujson.Value): Unit = current match
      case obj: ujson.Obj =>
        header(obj.value.size, 128, 222, 223)
        obj.value.foreach { case (key, child) =>
          string(key)
          loop(child)
        }
      case array: ujson.Arr =>
        header(array.value.size, 144, 220, 221)
        array.value.foreach(loop)
      case ujson.Str(raw)  => string(raw)
      case ujson.Bool(raw) => out.write(if raw then 195 else 194)
      case ujson.Num(raw)  => number(raw)
      case _               => invalid()
    loop(value)
    out.toByteArray

  private[desk] def actionHash(
      action: ujson.Value,
      nonce: Long,
      expiresAfter: Option[Long]
  ): Array[Byte] =
    validate(action)
    if nonce < 0 || expiresAfter.exists(_ <= nonce) then invalid()
    val bytes = new ByteArrayOutputStream()
    bytes.write(packed(action))
    writeUnsigned(bytes, nonce, 8)
    bytes.write(0)
    expiresAfter.foreach { expiry =>
      bytes.write(0)
      writeUnsigned(bytes, expiry, 8)
    }
    hash(bytes.toByteArray)

  private val domainHash = hash(
    hashText(
      "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"
    ) ++ hashText("Exchange") ++ hashText("1") ++ word(BigInteger.valueOf(1337)) ++
      Array.fill[Byte](32)(0)
  )

  private[desk] def digest(
      action: ujson.Value,
      nonce: Long,
      isMainnet: Boolean,
      expiresAfter: Option[Long]
  ): Array[Byte] =
    val message = hash(
      hashText("Agent(string source,bytes32 connectionId)") ++
        hashText(if isMainnet then "a" else "b") ++ actionHash(action, nonce, expiresAfter)
    )
    hash(Array[Byte](25, 1) ++ domainHash ++ message)

  private[desk] def warm(
      signOrder: (Array[Byte], ujson.Value, Long, Boolean, Option[Long]) => ujson.Value = sign
  ): Unit =
    val key = java.util.HexFormat
      .of()
      .parseHex(
        "0123456789012345678901234567890123456789012345678901234567890123"
      )
    try
      val action = ujson.Obj(
        "type" -> "order",
        "orders" -> ujson.Arr(
          ujson.Obj(
            "a" -> 32767,
            "b" -> false,
            "p" -> "546.05",
            "s" -> "1.83",
            "r" -> false,
            "t" -> ujson.Obj("limit" -> ujson.Obj("tif" -> "Ioc")),
            "c" -> "0x0123456789abcdef0123456789abcdef"
          )
        ),
        "grouping" -> "na"
      )
      val _ = signOrder(key, action, 1788684358292L, false, Some(1788684359292L))
    finally java.util.Arrays.fill(key, 0.toByte)

  def sign(
      secret: Array[Byte],
      action: ujson.Value,
      nonce: Long,
      isMainnet: Boolean,
      expiresAfter: Option[Long]
  ): ujson.Value =
    val privateScalar = scalar(secret)
    val bytes = digest(action, nonce, isMainnet, expiresAfter)
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    signer.init(true, new ECPrivateKeyParameters(privateScalar, domain))
    val values = signer.generateSignature(bytes)
    val r = values(0)
    val s = if values(1).compareTo(halfOrder) > 0 then order.subtract(values(1)) else values(1)
    val expected = curve.getG.multiply(privateScalar).normalize()
    val inverse = r.modInverse(order)
    val e = new BigInteger(1, bytes).mod(order)
    val recovery = (0 to 1)
      .find { parity =>
        try
          val point = curve.getCurve.decodePoint(Array((2 + parity).toByte) ++ word(r))
          point.multiply(order).isInfinity &&
          ECAlgorithms
            .sumOfTwoMultiplies(
              curve.getG,
              order.subtract(e).multiply(inverse).mod(order),
              point,
              s.multiply(inverse).mod(order)
            )
            .normalize() == expected
        catch case _: IllegalArgumentException => false
      }
      .getOrElse(invalid())
    ujson.Obj(
      "r" -> ("0x" + r.toString(16)),
      "s" -> ("0x" + s.toString(16)),
      "v" -> (27 + recovery)
    )
