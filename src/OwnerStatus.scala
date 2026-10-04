package desk

import scala.collection.mutable
import scala.util.Try
import upickle.core.{ObjVisitor, Visitor}

// The vault's public owner status for one network. The reply's network is checked and dropped.
final case class OwnerStatus(
    owner: Option[String],
    phase: Option[String],
    candidates: Int,
    key: Option[String],
    approved: Option[Boolean],
    balance: Option[BigDecimal],
    balanceAt: Option[Long],
    due: Vector[String],
    message: String
)

object OwnerStatus:
  val unavailable = "Owner status unavailable"
  // The runner keeps at most 4096 bytes, so a reply that fills them may have been cut.
  private val limit = 4096
  private val names = Set(
    "network",
    "owner",
    "phase",
    "candidates",
    "key",
    "approved",
    "balance",
    "balanceAt",
    "due",
    "message"
  )

  // Runs the vault's status command with no input. The caller's runner sets the time limit.
  def read(
      network: String,
      env: Map[String, String],
      run: (Seq[String], Array[Byte]) => ProcessRun
  ): Either[String, OwnerStatus] =
    Child.vault(env, Seq("--json", "owner", "status", "--network", network)).flatMap { command =>
      Try(run(command, Array.emptyByteArray)).toOption
        .filter(reply => reply.exit == 0 && reply.stdout.length < limit)
        .flatMap(reply => Try(parse(network, reply.stdout)).toOption)
        .toRight(unavailable)
    }

  private def invalid(): Nothing = throw new IllegalArgumentException(unavailable)

  // Builds ujson values as ujson.read does, but refuses a key an object already has and a number
  // written with a point or an exponent. It reaches the reply's own fields, where the only numbers
  // are counts and times. Inside an array it does not reach, and due must hold strings only. The
  // byte parser reports every number through visitFloat64ByteParts.
  private object Strict extends Visitor.Delegate[ujson.Value, ujson.Value](ujson.Value):
    override def visitFloat64ByteParts(
        bytes: Array[Byte],
        offset: Int,
        length: Int,
        decIndex: Int,
        expIndex: Int,
        index: Int
    ): ujson.Value =
      if decIndex != -1 || expIndex != -1 then invalid()
      ujson.Value.visitFloat64ByteParts(bytes, offset, length, decIndex, expIndex, index)

    override def visitObject(
        length: Int,
        jsonableKeys: Boolean,
        index: Int
    ): ObjVisitor[ujson.Value, ujson.Value] =
      val inner = ujson.Value.visitObject(length, jsonableKeys, index)
      val seen = mutable.Set.empty[String]
      new ObjVisitor[ujson.Value, ujson.Value]:
        def visitKey(index: Int): Visitor[?, ?] = inner.visitKey(index)
        def visitKeyValue(key: Any): Unit =
          if !seen.add(key.toString) then invalid()
          inner.visitKeyValue(key)
        def subVisitor: Visitor[?, ?] = Strict
        def visitValue(value: ujson.Value, index: Int): Unit = inner.visitValue(value, index)
        def visitEnd(index: Int): ujson.Value = inner.visitEnd(index)

  private def text(value: ujson.Value): String = value match
    case ujson.Str(value) => value
    case _                => invalid()

  private def address(value: ujson.Value): String =
    val raw = text(value)
    if !raw.matches("0x[0-9a-fA-F]{40}") then invalid()
    raw

  private def whole(value: ujson.Value, most: Double): Long = value match
    case ujson.Num(n) if n >= 0 && n <= most => n.toLong
    case _                                   => invalid()

  // The vault's own decimal form: an optional minus sign, at most 24 digits with no leading zero,
  // and at most 18 decimals.
  private def decimal(value: ujson.Value): BigDecimal =
    val raw = text(value)
    if !raw.matches("-?(0|[1-9][0-9]{0,23})(\\.[0-9]{1,18})?") then invalid()
    BigDecimal(raw)

  private def bool(value: ujson.Value): Boolean = value match
    case ujson.Bool(value) => value
    case _                 => invalid()

  private def texts(value: ujson.Value): Vector[String] = value match
    case ujson.Arr(items) => items.map(text).toVector
    case _                => invalid()

  private def nullable[A](value: ujson.Value, read: ujson.Value => A): Option[A] =
    if value.isNull then None else Some(read(value))

  private def parse(network: String, bytes: Array[Byte]): OwnerStatus =
    val fields = ujson.ByteArrayParser.transform(bytes, Strict) match
      case obj: ujson.Obj if obj.value.keys.toSet == names => obj.value
      case _                                               => invalid()
    if text(fields("network")) != network then invalid()
    OwnerStatus(
      owner = nullable(fields("owner"), address),
      phase = nullable(fields("phase"), text),
      candidates = whole(fields("candidates"), Int.MaxValue).toInt,
      key = nullable(fields("key"), address),
      approved = nullable(fields("approved"), bool),
      balance = nullable(fields("balance"), decimal),
      balanceAt = nullable(fields("balanceAt"), whole(_, 9_007_199_254_740_991d)),
      due = texts(fields("due")),
      message = text(fields("message"))
    )

  private def short(address: String): String = address.take(6) + "..." + address.takeRight(4)

  def ownerText(status: OwnerStatus): String =
    (status.message +: status.owner.map(short).toSeq).mkString("   ")

  def approvalText(status: OwnerStatus): String = status.approved match
    case Some(true)  => "approved"
    case Some(false) => "not approved"
    case None        => "not checked"

  // The balance in cents, rounded toward minus infinity so funding is never overstated, and its
  // age in seconds.
  def fundingText(status: OwnerStatus, nowSeconds: Long): String =
    (status.owner, status.balance) match
      case (None, _)      => "no owner yet"
      case (_, None)      => "balance not read"
      case (_, Some(sum)) =>
        val cents = sum.setScale(2, BigDecimal.RoundingMode.FLOOR)
        val shown = (if cents < 0 then "-$" else "$") + cents.abs.bigDecimal.toPlainString
        val age = status.balanceAt.fold("")(at => s", read ${math.max(0L, nowSeconds - at)} s ago")
        shown + " at the venue" + age
