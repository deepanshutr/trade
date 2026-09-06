package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

class TradingConfigSpec extends munit.FunSuite:
  private def setting(text: String): Path =
    val p = Files.createTempFile("desk-settings", ".json")
    Files.setPosixFilePermissions(p, PosixFilePermissions.fromString("rw-------"))
    Files.writeString(p, text)
    p

  test("defaults preserve dollar sizes and bounded reference count") {
    val c = TradingConfig.load(None).toOption.get
    assertEquals(c.sizes, TradeSizes())
    assertEquals(c.network, "mainnet")
    assertEquals(c.references, Vector("BTC", "PAXG"))
    assertEquals(c.quoteAgeMs, 1000L)
    assertEquals(c.positionAgeMs, 2000L)
    assert(!c.configured)
  }

  test("settings are exact and do not contain a signing key") {
    val p = setting(
      """{"small":"501.01","big":"1000","target":"800","network":"testnet","references":["BTC","ETH"]}"""
    )
    try
      val c = TradingConfig.load(Some(p)).toOption.get
      assertEquals(c.sizes.small, BigDecimal("501.01"))
      assertEquals(c.sizes.target, BigDecimal(800))
      assertEquals(c.baseUrl, "https://api.hyperliquid-testnet.xyz")
      assertEquals(c.references, Vector("BTC", "ETH"))
    finally Files.deleteIfExists(p)
  }

  test("decimal settings preserve their text and reject rounded numeric values") {
    val exact = setting("""{"small":"499.99999999999999"}""")
    val rounded = setting("""{"small":499.99999999999999}""")
    try
      assertEquals(
        TradingConfig.load(Some(exact)).toOption.get.sizes.small,
        BigDecimal("499.99999999999999")
      )
      assert(TradingConfig.load(Some(rounded)).isLeft)
    finally
      Files.deleteIfExists(exact)
      Files.deleteIfExists(rounded)
  }

  test("invalid and unknown settings fail closed") {
    val cases = Vector(
      """{"small":"0"}""",
      """{"small":"1001","big":"1000"}""",
      """{"target":"-1"}""",
      """{"network":"elsewhere"}""",
      """{"references":["BTC","BTC"]}""",
      """{"references":["BTC","ETH","SOL","PAXG"]}""",
      """{"quoteAgeMs":"999999"}""",
      """{"owner":"not an address"}""",
      """{"privateKey":"never read this"}""",
      "[]",
      "broken"
    )
    cases.foreach { text =>
      val p = setting(text)
      try assert(TradingConfig.load(Some(p)).isLeft)
      finally Files.deleteIfExists(p)
    }
  }

  test("an explicit missing path does not silently use defaults") {
    assert(TradingConfig.load(Some(Path.of("/missing/desk/settings.json"))).isLeft)
  }

  test("readable-by-others and symbolic settings are refused") {
    val p = setting("{}")
    val link = p.resolveSibling(p.getFileName.toString + ".link")
    try
      Files.setPosixFilePermissions(p, PosixFilePermissions.fromString("rw-r--r--"))
      assert(TradingConfig.load(Some(p)).isLeft)
      Files.setPosixFilePermissions(p, PosixFilePermissions.fromString("rw-------"))
      Files.createSymbolicLink(link, p)
      assert(TradingConfig.load(Some(link)).isLeft)
    finally
      Files.deleteIfExists(link)
      Files.deleteIfExists(p)
  }
