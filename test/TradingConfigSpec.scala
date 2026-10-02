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

  test("missing slippage keeps one tenth of a percent") {
    val p = setting("{}")
    try
      assertEquals(TradingConfig.load(Some(p)).map(_.slippage), Right(BigDecimal("0.001")))
      assertEquals(TradingConfig.load(None).map(_.slippage), Right(BigDecimal("0.001")))
    finally Files.deleteIfExists(p)
  }

  test("a quoted slippage above zero and at most one percent is read exactly") {
    for value <- Vector("0.0001", "0.005", "0.01") do
      val p = setting(ujson.Obj("slippage" -> value).render())
      try assertEquals(TradingConfig.load(Some(p)).map(_.slippage), Right(BigDecimal(value)))
      finally Files.deleteIfExists(p)
  }

  test("slippage that is not a quoted fraction above zero and at most one percent is refused") {
    val values = Vector(
      ujson.Str("0"),
      ujson.Str("-0.001"),
      ujson.Str("0.0100001"),
      ujson.Str("0.02"),
      ujson.Num(0.001),
      ujson.Str("abc"),
      ujson.Str(""),
      ujson.Str("NaN"),
      ujson.Bool(true),
      ujson.Null
    )
    for value <- values do
      val p = setting(ujson.Obj("slippage" -> value).render())
      try
        assertEquals(
          TradingConfig.load(Some(p)),
          Left("invalid trading settings: invalid slippage"),
          value.render()
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

  test("settings with an agent field are refused before any value is read") {
    val message =
      "invalid trading settings: remove agent from the settings. " +
        "The trading key is now chosen with desk --keys"
    val values = Vector(
      ujson.Str("0x" + "2" * 40),
      ujson.Str("private-address-canary"),
      ujson.Str(""),
      ujson.Num(5),
      ujson.Bool(true),
      ujson.Null,
      ujson.Arr("private-address-canary"),
      ujson.Obj("private-address-canary" -> "private-address-canary")
    )
    for value <- values do
      val p = setting(ujson.Obj("agent" -> value).render())
      try assertEquals(TradingConfig.load(Some(p)), Left(message))
      finally Files.deleteIfExists(p)
    val withOwner = setting(
      ujson
        .Obj("owner" -> ("0x" + "1" * 40), "agent" -> ("0x" + "2" * 40), "network" -> "testnet")
        .render()
    )
    try assertEquals(TradingConfig.load(Some(withOwner)), Left(message))
    finally Files.deleteIfExists(withOwner)
  }

  test("settings without an agent load as before and keep the key service") {
    val owner = "0x" + "1" * 40
    val p = setting(
      ujson.Obj("owner" -> owner, "network" -> "testnet", "keyService" -> "desk-test").render()
    )
    try
      val c = TradingConfig.load(Some(p)).toOption.get
      assertEquals(c.owner, Some(owner))
      assertEquals(c.agent, None)
      assertEquals(c.keyService, "desk-test")
      assertEquals(c.network, "testnet")
      assert(!c.configured)
    finally Files.deleteIfExists(p)
  }

  test("a settings owner that is not an address names the owner") {
    val p = setting("""{"owner":"private-address-canary"}""")
    try
      val error = TradingConfig.load(Some(p)).swap.toOption.get
      assertEquals(error, "invalid trading settings: invalid owner address")
    finally Files.deleteIfExists(p)
  }

  test("the settings case class still carries the agent chosen elsewhere") {
    val agent = "0x" + "2" * 40
    val c = TradingConfig(owner = Some("0x" + "1" * 40), agent = Some(agent), network = "testnet")
    assert(c.configured)
    assertEquals(c.identity, "testnet-" + "2" * 40)
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

  for field <- Vector(
      "owner",
      "small",
      "big",
      "target",
      "quoteAgeMs",
      "positionAgeMs",
      "network",
      "keyService",
      "propBalance",
      "propFee",
      "slippage",
      "onchain"
    )
  do
    test(s"wrong JSON types name $field without including supplied values") {
      val values = Vector(
        ujson.Num(5),
        ujson.Bool(true),
        ujson.Null,
        ujson.Arr("private-type-canary"),
        ujson.Obj("private-type-canary" -> "private-type-canary")
      )
      for value <- values do
        val p = setting(ujson.Obj(field -> value).render())
        try
          val error = TradingConfig.load(Some(p)).swap.toOption.get
          assert(error.contains(field), error)
          assert(!error.contains("private-type-canary"), error)
          assert(!error.contains("5"), error)
          assert(!error.contains("true"), error)
        finally Files.deleteIfExists(p)
    }

  test("wrong onchain market types name onchain without including supplied values") {
    val values = Vector(
      ujson.Num(5),
      ujson.Bool(true),
      ujson.Null,
      ujson.Arr("private-type-canary"),
      ujson.Obj("private-type-canary" -> "private-type-canary")
    )
    val markets = values ++ Vector(ujson.Obj("name" -> 5)) ++
      (for
        field <- Vector("name", "pair", "quotePool", "rpc")
        value <- values
      yield
        val market = ujson.Obj(
          "name" -> "DEMO",
          "pair" -> ("0x" + "1" * 40),
          "quotePool" -> ("0x" + "2" * 40),
          "rpc" -> ujson.Arr("https://rpc.example.com")
        )
        market(field) =
          if field == "rpc" && value.arrOpt.isDefined then
            ujson.Arr(ujson.Obj("private-type-canary" -> true))
          else value
        market)
      ++ Vector(
        ujson.Obj(
          "name" -> "DEMO",
          "pair" -> ("0x" + "1" * 40),
          "quotePool" -> ("0x" + "2" * 40),
          "rpc" -> ujson.Arr(5)
        )
      )
    for market <- markets do
      val p = setting(ujson.Obj("onchain" -> ujson.Arr(market)).render())
      try
        val error = TradingConfig.load(Some(p)).swap.toOption.get
        assert(error.contains("onchain"), error)
        assert(!error.contains("private-type-canary"), error)
        assert(!error.contains("5"), error)
        assert(!error.contains("true"), error)
      finally Files.deleteIfExists(p)
  }

  // Tokenize strings and comments before looking for require calls. Delimiters inside
  // conditions or quoted text must not be mistaken for the message argument.
  private def fixedRequireMessages(source: String): Boolean =
    val displayMapping =
      """case Some(message) if message.startsWith("requirement failed: ") =>
        |            "invalid trading settings: " + message.stripPrefix("requirement failed: ")""".stripMargin
    if source
        .replaceFirst(java.util.regex.Pattern.quote(displayMapping), "")
        .contains("requirement failed")
    then return false
    val plain = """"(?:\\.|[^"\\])*"""".r
    val token =
      """(?s)//[^\r\n]*|/\*.*?\*/|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|[A-Za-z_][A-Za-z0-9_]*|[^\s]""".r
    val tokens =
      token.findAllIn(source).filterNot(t => t.startsWith("//") || t.startsWith("/*")).toVector
    val opening = Map(")" -> "(", "]" -> "[", "}" -> "{")
    var seen = false
    var i = 0
    while i < tokens.size do
      if tokens(i) == "require" then
        if i + 1 >= tokens.size || tokens(i + 1) != "(" then return false
        seen = true
        var stack = List("(")
        var message = -1
        var end = i + 2
        while end < tokens.size && stack.nonEmpty do
          tokens(end) match
            case "(" | "[" | "{" => stack = tokens(end) :: stack
            case ")" | "]" | "}" =>
              if stack.head != opening(tokens(end)) then return false
              stack = stack.tail
            case "," if stack.size == 1 =>
              if message >= 0 then return false
              message = end + 1
            case _ => ()
          end += 1
        if stack.nonEmpty then return false
        // A require without a message uses the generic settings error.
        if message >= 0 && (end - message != 2 || !plain.matches(tokens(message))) then return false
      i += 1
    seen

  test("every settings require message is a single plain string literal") {
    val source = Files.readString(Path.of("src/TradingConfig.scala"))
    assert(fixedRequireMessages(source), "settings errors must use fixed messages")
    val messages = Vector(
      "\"invalid key service\"" -> "\"invalid key service \" + service",
      "\"unknown setting\"" -> "\"unknown setting \" + j.obj.keySet.mkString",
      "\"invalid key service\"" -> "message",
      "\"invalid key service\"" -> "\"invalid %s setting\".format(service)",
      "\"invalid network\"" -> "s\"invalid $network\"",
      "\"invalid network\"" -> "f\"invalid $network\"",
      "\"invalid network\"" -> "raw\"invalid $network\"",
      "\"invalid key service\"" -> "custom\"invalid $service\""
    )
    for (literal, expression) <- messages do
      val changed = source
        .replace(literal, expression)
        .replace("val service =", "val message = \"private setting value\"\n        val service =")
      assertNotEquals(changed, source)
      assert(!fixedRequireMessages(changed), expression)
    assert(
      fixedRequireMessages("""require(check(Vector(1, 2), { x => x }), "fixed (, require)")""")
    )
    assert(
      fixedRequireMessages("""require(true, "escaped \"quote\"") // require(false, variable)""")
    )
    assert(fixedRequireMessages("""/* require(false, variable) */ require(check(')', ','))"""))
    assert(!fixedRequireMessages("""require(true, "fixed", "extra")"""))
    assert(!fixedRequireMessages("""require(true, "unfinished""""))
  }

  test("settings require aliases cannot bypass the fixed message guard") {
    val source = Files.readString(Path.of("src/TradingConfig.scala"))
    val changed = source
      .replace(
        "object TradingConfig:",
        "object TradingConfig:\n  private val check: (Boolean, Any) => Unit = Predef.require"
      )
      .replace(
        "require(service.matches(\"[A-Za-z0-9._-]{1,80}\"), \"invalid key service\")",
        "check(service.matches(\"[A-Za-z0-9._-]{1,80}\"), \"invalid key service \" + service)"
      )
    assertNotEquals(changed, source)
    assert(!fixedRequireMessages(changed), "require alias must fail the guard")
    assert(!fixedRequireMessages(source + "\nprivate val check = Predef.require"))
    assert(fixedRequireMessages(source))
  }

  test("hand-written settings throws cannot bypass the fixed message guard") {
    val source = Files.readString(Path.of("src/TradingConfig.scala"))
    val changed = source.replace(
      "require(service.matches(\"[A-Za-z0-9._-]{1,80}\"), \"invalid key service\")",
      "if !service.matches(\"[A-Za-z0-9._-]{1,80}\") then\n" +
        "          throw IllegalArgumentException(\"requirement failed: invalid \" + service)"
    )
    assertNotEquals(changed, source)
    assert(!fixedRequireMessages(changed), "hand-written requirement failure must fail the guard")
    assert(!fixedRequireMessages(source + "\n// requirement failed"))
    assert(fixedRequireMessages(source))
  }

  test("settings nesting accepts depth thirty-two and refuses thirty-three") {
    for (depth, accepted) <- Vector(31 -> true, 32 -> false) do
      val p = setting("{\"references\":" + "[" * depth + "0" + "]" * depth + "}")
      try assertEquals(TradingConfig.load(Some(p)).isRight, accepted)
      finally Files.deleteIfExists(p)
  }
