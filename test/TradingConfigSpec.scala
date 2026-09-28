package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

class TradingConfigSpec extends munit.FunSuite:
  private def setting(text: String): Path =
    val p = Files.createTempFile("desk-settings", ".json")
    Files.setPosixFilePermissions(p, PosixFilePermissions.fromString("rw-------"))
    Files.writeString(p, text)
    p

  test("an empty private settings file loads every default") {
    val p = setting("{}")
    try assertEquals(TradingConfig.load(Some(p)), Right(TradingConfig()))
    finally Files.deleteIfExists(p)
  }

  test("defaults preserve dollar sizes and age limits") {
    val c = TradingConfig.load(None).toOption.get
    assertEquals(c.sizes, TradeSizes())
    assertEquals(c.network, "mainnet")
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

  test("legacy references settings load and their values are ignored") {
    Vector("[\"ETH\"]", "[]", "[\"BTC\",\"BTC\"]", "null", "42").foreach { value =>
      val p = setting(s"""{"references":$value}""")
      try assertEquals(TradingConfig.load(Some(p)), TradingConfig.load(None))
      finally Files.deleteIfExists(p)
    }
  }

  test("invalid and unknown settings fail closed") {
    val cases = Vector(
      """{"small":"0"}""",
      """{"small":"1001","big":"1000"}""",
      """{"target":"-1"}""",
      """{"network":"elsewhere"}""",
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

  test("invalid settings report fixed validation messages") {
    val p = setting("""{"small":"2000","big":"1000"}""")
    try
      assertEquals(
        TradingConfig.load(Some(p)),
        Left("invalid trading settings: small exceeds big")
      )
    finally Files.deleteIfExists(p)
  }

  test("parse errors retain the generic settings error") {
    Vector("broken", """{"small":"secret"}""").foreach { text =>
      val p = setting(text)
      try
        assertEquals(
          TradingConfig.load(Some(p)),
          Left("invalid or unreadable trading settings")
        )
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

  test("a failure without a message keeps the generic settings error") {
    val p = setting("""{"small":""}""")
    try
      assertEquals(TradingConfig.load(Some(p)), Left("invalid or unreadable trading settings"))
    finally Files.deleteIfExists(p)
  }

  test("deep settings return the generic error before parsing or rendering values") {
    val nested = "[" * 3000 + "0" + "]" * 3000
    for text <- Vector(nested, "{\"references\":" + nested + "}") do
      val p = setting(text)
      try assertEquals(TradingConfig.load(Some(p)), Left("invalid or unreadable trading settings"))
      finally Files.deleteIfExists(p)
  }

  test("nesting checks ignore escaped quotes and brackets inside strings") {
    val bracketText = "\"\\" + "[{}]" * 1000
    val p = setting(ujson.Obj("references" -> bracketText).render())
    try assertEquals(TradingConfig.load(Some(p)), TradingConfig.load(None))
    finally Files.deleteIfExists(p)
  }

  test("shared validation errors name their fields without including supplied values") {
    val cases = Vector(
      "owner" -> "private-address-canary",
      "agent" -> "private-address-canary",
      "small" -> "0",
      "big" -> "0",
      "target" -> "0",
      "quoteAgeMs" -> "99999",
      "positionAgeMs" -> "99999"
    )
    for (field, value) <- cases do
      val p = setting(ujson.Obj(field -> value).render())
      try
        val error = TradingConfig.load(Some(p)).swap.toOption.get
        assert(error.contains(field), error)
        assert(!error.contains("private-address-canary"))
      finally Files.deleteIfExists(p)
  }

  test("settings validation source rejects interpolated messages") {
    val source = Files.readString(Path.of("src/TradingConfig.scala"))
    val interpolation = """(?<![A-Za-z0-9_])(?:s|f|raw)["]""".r
    def safe(text: String): Boolean = interpolation.findFirstIn(text).isEmpty
    assert(safe(source), "settings errors must use fixed messages")
    val mutant = source.replace("\"invalid network\"", "s\"invalid $network\"")
    assertNotEquals(mutant, source)
    assert(!safe(mutant), "the check must reject an interpolated require message")
  }

  test("settings nesting accepts depth thirty-two and refuses thirty-three") {
    for (depth, accepted) <- Vector(31 -> true, 32 -> false) do
      val p = setting("{\"references\":" + "[" * depth + "0" + "]" * depth + "}")
      try assertEquals(TradingConfig.load(Some(p)).isRight, accepted)
      finally Files.deleteIfExists(p)
  }
