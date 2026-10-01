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
    quoteAgeMs: Long = 1000L,
    positionAgeMs: Long = 2000L,
    paperBalance: BigDecimal = BigDecimal("10000"),
    paperFee: BigDecimal = BigDecimal("0.0005"),
    onchain: Vector[OnchainMarket] = Vector.empty
):
  def configured: Boolean = owner.nonEmpty && agent.nonEmpty
  def onchainMarket(name: String): Option[OnchainMarket] = onchain.find(_.name == name)
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
    "keyService",
    "references", // Accepted for saved settings compatibility; its value is ignored.
    "quoteAgeMs",
    "positionAgeMs",
    "paperBalance",
    "paperFee",
    "onchain"
  )
  private val marketName = "[A-Z0-9][A-Z0-9._-]{0,19}"
  private def validAddress(value: String): Boolean = value.matches("0x[0-9a-f]{40}")
  private val endpoint = "https://[A-Za-z0-9.-]+(:[0-9]+)?(/[A-Za-z0-9._~/-]*)?"

  private[desk] def onchainMarkets(j: ujson.Value): Vector[OnchainMarket] =
    require(j.obj.get("onchain").forall(_.arrOpt.isDefined), "onchain must be a list")
    val markets = j.obj.get("onchain").map(_.arr.toVector).getOrElse(Vector.empty).map { m =>
      require(
        m.objOpt.exists(o =>
          Vector("name", "pair", "quotePool").forall(n => o.get(n).exists(_.strOpt.isDefined)) &&
            o.get("rpc").exists(_.arrOpt.exists(_.forall(_.strOpt.isDefined)))
        ),
        "onchain market name, pair and quotePool must be strings and rpc must be a list of strings"
      )
      val name = m("name").str
      require(name.matches(marketName) && State.validMarket(name), "invalid on-chain market name")
      def address(field: String): String =
        val a = m(field).str.toLowerCase(java.util.Locale.ROOT)
        require(validAddress(a), "invalid on-chain address")
        a
      val rpc = m("rpc").arr.toVector.map(_.str)
      require(
        rpc.nonEmpty && rpc.size <= 8 && rpc.forall(_.matches(endpoint)),
        "invalid on-chain endpoint"
      )
      OnchainMarket(name, address("pair"), address("quotePool"), rpc)
    }
    val names = markets.map(_.name)
    require(markets.size <= 8 && names.distinct == names, "duplicate on-chain markets")
    require(
      names.forall(n => !State.refCoins.contains(n)),
      "an on-chain market cannot be a reference"
    )
    markets
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

  private def shallowJson(text: String): Boolean =
    var depth = 0
    var quoted = false
    var escaped = false
    var index = 0
    while index < text.length do
      val c = text.charAt(index)
      if quoted then
        if escaped then escaped = false
        else if c == '\\' then escaped = true
        else if c == '"' then quoted = false
      else if c == '"' then quoted = true
      else if c == '[' || c == '{' then
        depth += 1
        if depth > 32 then return false
      else if c == ']' || c == '}' then depth -= 1
      index += 1
    true

  def load(path: Option[Path]): Either[String, TradingConfig] = path match
    case None    => Right(TradingConfig())
    case Some(p) =>
      Try {
        require(privateFile(p), "settings must be a private regular file")
        require(Files.size(p) <= 16384, "settings are too large")
        val text = Files.readString(p)
        require(shallowJson(text))
        val j = ujson.read(text)
        require(
          !j.obj.contains("agent"),
          "agent is now chosen on the key screen (desk --keys)"
        )
        require(j.obj.keySet.forall(fields.contains), "unknown setting")
        def stringFields(names: String*): Boolean =
          names.forall(name => j.obj.get(name).forall(_.strOpt.isDefined))
        require(stringFields("owner"), "owner must be a string")
        require(stringFields("network", "keyService"), "network and keyService must be strings")
        require(
          stringFields("paperBalance", "paperFee"),
          "paperBalance and paperFee must be strings"
        )
        require(stringFields("small", "big", "target"), "small, big and target must be strings")
        require(
          stringFields("quoteAgeMs", "positionAgeMs"),
          "quoteAgeMs and positionAgeMs must be strings"
        )
        def decimal(name: String, fallback: BigDecimal): BigDecimal =
          j.obj.get(name).map(v => BigDecimal(v.str)).getOrElse(fallback)
        def string(name: String, fallback: String): String =
          j.obj.get(name).map(_.str).getOrElse(fallback)
        def address(name: String): Option[String] = j.obj.get(name).map { v =>
          val a = v.str.toLowerCase(java.util.Locale.ROOT)
          require(validAddress(a), "invalid owner address")
          a
        }
        def millis(name: String, fallback: Long): Long =
          val n = decimal(name, BigDecimal(fallback))
          require(
            n.isWhole && n >= 100 && n <= 5000,
            "invalid quoteAgeMs or positionAgeMs age limit"
          )
          n.toLongExact
        val d = TradingConfig()
        val defaults = d.sizes
        val sizes = TradeSizes(
          decimal("small", defaults.small),
          decimal("big", defaults.big),
          decimal("target", defaults.target)
        )
        require(
          Vector(sizes.small, sizes.big, sizes.target).forall(n =>
            n > 0 && n <= BigDecimal("1000000000")
          ),
          "invalid small, big or target dollar size"
        )
        require(sizes.small <= sizes.big, "small exceeds big")
        val network = string("network", d.network)
        require(Set("mainnet", "testnet").contains(network), "invalid network")
        val service = string("keyService", d.keyService)
        require(service.matches("[A-Za-z0-9._-]{1,80}"), "invalid key service")
        val balance = decimal("paperBalance", d.paperBalance)
        val fee = decimal("paperFee", d.paperFee)
        require(balance > 0 && balance <= BigDecimal("1000000000"), "invalid paper balance")
        require(fee >= 0 && fee <= BigDecimal("0.01"), "invalid paper fee")
        TradingConfig(
          sizes,
          network,
          address("owner"),
          None,
          service,
          millis("quoteAgeMs", d.quoteAgeMs),
          millis("positionAgeMs", d.positionAgeMs),
          balance,
          fee,
          onchainMarkets(j)
        )
      }.toEither.left.map { error =>
        Option(error.getMessage) match
          case Some(message) if message.startsWith("requirement failed: ") =>
            "invalid trading settings: " + message.stripPrefix("requirement failed: ")
          case _ => "invalid or unreadable trading settings"
      }
