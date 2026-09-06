package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermission
import scala.jdk.CollectionConverters.*
import scala.util.Try

final case class TradingConfig(
    sizes: TradeSizes = TradeSizes(),
    network: String = "mainnet",
    owner: Option[String] = None,
    agent: Option[String] = None,
    keyService: String = "desk",
    references: Vector[String] = State.refCoins,
    quoteAgeMs: Long = 1000L,
    positionAgeMs: Long = 2000L
):
  def configured: Boolean = owner.nonEmpty && agent.nonEmpty
  def baseUrl: String =
    if network == "mainnet" then "https://api.hyperliquid.xyz"
    else "https://api.hyperliquid-testnet.xyz"
  def socketUrl: String = baseUrl.replace("https://", "wss://") + "/ws"
  def identity: String = network + "-" + agent.getOrElse("").stripPrefix("0x")

object TradingConfig:
  private val fields = Set(
    "small",
    "big",
    "target",
    "network",
    "owner",
    "agent",
    "keyService",
    "references",
    "quoteAgeMs",
    "positionAgeMs"
  )
  private val forbidden = Set(
    PosixFilePermission.GROUP_READ,
    PosixFilePermission.GROUP_WRITE,
    PosixFilePermission.GROUP_EXECUTE,
    PosixFilePermission.OTHERS_READ,
    PosixFilePermission.OTHERS_WRITE,
    PosixFilePermission.OTHERS_EXECUTE
  )

  def privateFile(path: Path): Boolean =
    !Files.isSymbolicLink(path) && Files.isRegularFile(path) &&
      Files.getPosixFilePermissions(path).asScala.intersect(forbidden).isEmpty

  def load(path: Option[Path]): Either[String, TradingConfig] = path match
    case None    => Right(TradingConfig())
    case Some(p) =>
      Try {
        require(privateFile(p), "settings must be a private regular file")
        require(Files.size(p) <= 16384, "settings are too large")
        val j = ujson.read(Files.readString(p))
        require(j.obj.keySet.forall(fields.contains), "unknown setting")
        def decimal(name: String, fallback: BigDecimal): BigDecimal =
          j.obj.get(name).map(v => BigDecimal(v.str)).getOrElse(fallback)
        def string(name: String, fallback: String): String =
          j.obj.get(name).map(_.str).getOrElse(fallback)
        def address(name: String): Option[String] = j.obj.get(name).map { v =>
          val a = v.str.toLowerCase(java.util.Locale.ROOT)
          require(a.matches("0x[0-9a-f]{40}"), "invalid address")
          a
        }
        def millis(name: String, fallback: Long): Long =
          val n = decimal(name, BigDecimal(fallback))
          require(n.isWhole && n >= 100 && n <= 5000, "invalid age limit")
          n.toLongExact
        val defaults = TradeSizes()
        val sizes = TradeSizes(
          decimal("small", defaults.small),
          decimal("big", defaults.big),
          decimal("target", defaults.target)
        )
        require(
          Vector(sizes.small, sizes.big, sizes.target).forall(n =>
            n > 0 && n <= BigDecimal("1000000000")
          ),
          "invalid dollar size"
        )
        require(sizes.small <= sizes.big, "small exceeds big")
        val network = string("network", "mainnet")
        require(Set("mainnet", "testnet").contains(network), "invalid network")
        val refs = j.obj.get("references").map(_.arr.toVector.map(_.str)).getOrElse(State.refCoins)
        require(
          refs.size <= 3 && refs.distinct == refs && refs.forall(
            _.matches("[A-Z0-9][A-Z0-9._-]{0,19}")
          ),
          "invalid references"
        )
        val service = string("keyService", "desk")
        require(service.matches("[A-Za-z0-9._-]{1,80}"), "invalid key service")
        TradingConfig(
          sizes,
          network,
          address("owner"),
          address("agent"),
          service,
          refs,
          millis("quoteAgeMs", 1000L),
          millis("positionAgeMs", 2000L)
        )
      }.toEither.left.map(_ => "invalid or unreadable trading settings")
