package desk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import scala.util.Try

sealed trait StoreResult
object StoreResult:
  case object Stored extends StoreResult
  case object Exists extends StoreResult
  final case class Failed(reason: String) extends StoreResult

// Hands the derived key to the Keychain helper on standard input as 64 lowercase
// hex characters and a newline. The hex lives in one byte array that is zeroed
// when the helper returns. The helper reply is public data and is parsed as text.
object KeyStore:
  private val missing = "key store helper is not installed. Run make install"
  private val digits = "0123456789abcdef".getBytes(UTF_8)

  private def absolute(env: Map[String, String], name: String): Option[String] =
    env.get(name).filter(value => Try(Path.of(value).isAbsolute).getOrElse(false))

  private def hexLine(key: Array[Byte]): Array[Byte] =
    val line = new Array[Byte](65)
    for i <- key.indices do
      line(i * 2) = digits((key(i) >> 4) & 15)
      line(i * 2 + 1) = digits(key(i) & 15)
    line(64) = 10
    line

  private def reply(run: ProcessRun): Option[(String, String)] =
    Try {
      val body = ujson.read(new String(run.stdout, UTF_8)).obj
      (body("state").str, body("account").str)
    }.toOption

  def store(
      key: Array[Byte],
      expected: String,
      service: String,
      env: Map[String, String],
      run: (Seq[String], Array[Byte]) => ProcessRun
  ): StoreResult =
    (absolute(env, "DESK_PYTHON"), absolute(env, "DESK_KEY_HELPER")) match
      case (Some(python), Some(helper)) if key.length == 32 =>
        val line = hexLine(key)
        val outcome =
          try Try(run(Seq(python, helper, "--store", "agent", "--service", service), line))
          finally java.util.Arrays.fill(line, 0.toByte)
        outcome.toOption.fold[StoreResult](StoreResult.Failed("key store helper failed"))(
          verdict(_, expected)
        )
      case (Some(_), Some(_)) => StoreResult.Failed("key store needs a 32 byte key")
      case _                  => StoreResult.Failed(missing)

  private def verdict(run: ProcessRun, expected: String): StoreResult =
    reply(run) match
      case Some(("stored", account)) if run.exit == 0 && account == expected =>
        StoreResult.Stored
      case Some(("exists", account)) if run.exit == 3 && account == expected =>
        StoreResult.Exists
      case Some((_, account)) if account != expected && (run.exit == 0 || run.exit == 3) =>
        StoreResult.Failed("key store account differs")
      case _ => StoreResult.Failed("key store helper failed")
