package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

class BindingsSpec extends TempDirectorySuite:
  private def key(text: String): KeyGroup =
    KeyGroup(text.getBytes(java.nio.charset.StandardCharsets.UTF_8).toVector.map(_ & 0xff))
  private def bytes(values: Int*): KeyGroup = KeyGroup(values.toVector)
  private def one(text: String): Binding = Binding(Vector(key(text)))
  private def two(first: String, second: String): Binding = Binding(Vector(key(first), key(second)))
  private def path(): Path = Bindings.path(tempDirectory("desk-bindings"))
  private def put(path: Path, text: String, mode: String = "rw-------"): Path =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(mode))
    path
  private def file(bindings: String): String = s"""{"version":1,"bindings":{$bindings}}"""
  private def refusal(reason: String) = Left(s"invalid key bindings: $reason (bindings.json)")
  private val notKey = Some("That is not one key")
  private val dead = Some("That key needs a second press on this layout")

  test("the defaults are today's keys plus x, s, t and o") {
    val expected = Vector(
      Bind.Quit -> "q",
      Bind.Prop -> "p",
      Bind.Live -> "l",
      Bind.Market -> "m",
      Bind.Chart -> "c",
      Bind.Sound -> "v",
      Bind.LongSmall -> "1",
      Bind.LongBig -> "2",
      Bind.ShortSmall -> "3",
      Bind.ShortBig -> "4",
      Bind.Close -> "x",
      Bind.Settings -> "s",
      Bind.OrderType -> "t",
      Bind.Orders -> "o"
    )
    assertEquals(Bind.values.toVector, expected.map(_._1))
    assertEquals(Bindings.defaults.map, expected.map((action, text) => action -> one(text)).toMap)
  }

  test("each action gives its command and the defaults give the old commands") {
    val commands = Vector(
      Bind.Quit -> Command.Quit,
      Bind.Prop -> Command.Mode("PROP"),
      Bind.Live -> Command.Mode("LIVE"),
      Bind.Market -> Command.StartMarketPrompt,
      Bind.Chart -> Command.ToggleCandles,
      Bind.Sound -> Command.ToggleMute,
      Bind.LongSmall -> Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)),
      Bind.LongBig -> Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard, big = true)),
      Bind.ShortSmall -> Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Hard)),
      Bind.ShortBig -> Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Hard, big = true)),
      Bind.Close -> Command.Close,
      Bind.Settings -> Command.Settings,
      Bind.OrderType -> Command.OrderType,
      Bind.Orders -> Command.Orders
    )
    for (action, command) <- commands do
      assertEquals(Bindings.command(action), command)
      assertEquals(
        Bindings.defaults.command(Bindings.defaults.map(action).keys.head),
        Some(command)
      )
    for text <- Vector("5", "6", "7", "8", "9", "0", "i", "Q", "a") do
      assertEquals(Bindings.defaults.command(key(text)), None)
  }

  test("actions have row labels and file names") {
    assertEquals(
      Bind.values.toVector.map(_.label),
      Vector(
        "Quit",
        "Prop",
        "Live",
        "Market",
        "Chart",
        "Sound",
        "Long small",
        "Long big",
        "Short small",
        "Short big",
        "Close",
        "Settings",
        "Order type",
        "Orders"
      )
    )
    assertEquals(
      Bind.values.toVector.map(_.field),
      Vector(
        "quit",
        "prop",
        "live",
        "market",
        "chart",
        "sound",
        "longSmall",
        "longBig",
        "shortSmall",
        "shortBig",
        "close",
        "settings",
        "orderType",
        "orders"
      )
    )
  }

  test("every key kind is one key") {
    val accepted = Vector(
      key("q"),
      key(" "),
      key("`"),
      key("~"),
      bytes(1),
      bytes(11),
      bytes(31),
      key("ç"),
      key("∫"),
      key("\uf8ff"),
      key("\ud834\udd1e"),
      key("\u001bOP"),
      key("\u001b[15~"),
      key("\u001b[1;2A"),
      key("\u001bO1;2P"),
      KeyGroup(Vector(27, 91) ++ Vector.fill(29)(49) :+ 126)
    )
    for group <- accepted do assertEquals(KeyGroup.refused(group), None, group)
    assertEquals(key("ç").bytes.size, 2)
    assertEquals(key("∫").bytes.size, 3)
    assertEquals(key("\ud834\udd1e").bytes.size, 4)
  }

  test("utf8Length reads lead bytes and refuses every other byte") {
    for byte <- 0 to 255 do
      val expected =
        if byte >= 194 && byte <= 223 then 2
        else if byte >= 224 && byte <= 239 then 3
        else if byte >= 240 && byte <= 244 then 4
        else 0
      assertEquals(KeyGroup.utf8Length(byte), expected, byte)
  }

  test("every reserved byte is refused with its reason") {
    val reasons = Map(
      0 -> "Control Space cannot be bound",
      3 -> "Control C quits Desk",
      4 -> "Control D ends input",
      8 -> "Control H erases in the market prompt",
      9 -> "Control I is Tab",
      10 -> "Control J is Enter",
      13 -> "Enter confirms",
      17 -> "Control Q is flow control",
      19 -> "Control S is flow control",
      26 -> "Control Z stops programs",
      27 -> "Esc cancels",
      28 -> "Control backslash quits programs",
      97 -> "a places at the selected order's price",
      127 -> "Delete erases in the market prompt"
    )
    for byte <- 0 to 255 do
      val expected =
        if byte >= 128 then notKey
        else reasons.get(byte)
      assertEquals(KeyGroup.refused(bytes(byte)), expected, byte)
  }

  test("every reserved group is refused with its reason") {
    val arrows = Some("The arrows move the chart and the ticket price")
    val mouse = Some("Mouse reports are not keys")
    val cases = Vector(
      "\u001b[A" -> arrows,
      "\u001b[B" -> arrows,
      "\u001b[C" -> arrows,
      "\u001b[D" -> arrows,
      "\u001bOA" -> arrows,
      "\u001bOB" -> arrows,
      "\u001bOC" -> arrows,
      "\u001bOD" -> arrows,
      "\u001b[200~" -> Some("Paste markers are not keys"),
      "\u001b[201~" -> Some("Paste markers are not keys"),
      "\u001b[<0;1;1M" -> mouse,
      "\u001b[<64;10;20M" -> mouse,
      "\u001b[<0;1;1m" -> mouse,
      "\u001b[M" -> mouse,
      "\u001b[t" -> mouse,
      "\u001b[T" -> mouse,
      "\u001bq" -> notKey,
      "\u001b1" -> notKey,
      "\u001b\u001b" -> notKey,
      "\u001b[" -> notKey,
      "\u001b[15" -> notKey,
      "\u001bO" -> notKey,
      "\u001b[1\u0001~" -> notKey,
      "qq" -> notKey,
      "q\u001b" -> notKey,
      "∫∫" -> notKey,
      "\u0085" -> notKey,
      "\u202e" -> notKey,
      "\u200e" -> notKey,
      "\u2028" -> notKey,
      "\ufeff" -> notKey,
      "\u0301" -> notKey,
      "\u20dd" -> notKey
    )
    for (text, reason) <- cases do assertEquals(KeyGroup.refused(key(text)), reason, text)
    val raw = Vector(
      Vector.empty[Int],
      Vector(155, 65),
      Vector(143, 80),
      Vector(155, 49, 53, 126),
      Vector(226, 136),
      Vector(226, 136, 171, 128),
      Vector(226, 49, 171),
      Vector(192, 128),
      Vector(193, 191),
      Vector(224, 128, 128),
      Vector(237, 160, 128),
      Vector(240, 128, 128, 128),
      Vector(244, 144, 128, 128),
      Vector(245, 128, 128, 128),
      Vector(-1),
      Vector(256),
      Vector(195, 423),
      Vector(27, 91, 305),
      Vector(27, 91) ++ Vector.fill(30)(49) :+ 126
    )
    for group <- raw do assertEquals(KeyGroup.refused(KeyGroup(group)), notKey, group)
  }

  test("dead-key characters are refused and direct Option characters are accepted") {
    val accents = Vector("\u00b4", "\u02c6", "\u02dc", "\u00a8")
    val letters = "àáâãäèéêëìíîïñòóôõöùúûüýÿ"
    val composed = letters ++ letters.toUpperCase + "Ÿ"
    for text <- accents ++ composed.map(_.toString) do
      assertEquals(KeyGroup.refused(key(text)), dead, text)
    for text <- Vector("å", "ç", "ø", "æ", "ß", "œ", "Å", "Ç", "Ø", "Æ", "Œ", "∫", "©", "¡", "º") do
      assertEquals(KeyGroup.refused(key(text)), None, text)
    assertEquals(KeyGroup.refused(key("`")), None)
    assertEquals(KeyGroup.refused(key("e\u0301")), notKey)
  }

  test("display names and short forms cover each kind") {
    val cases = Vector(
      key("q") -> ("q", "q"),
      key("Q") -> ("Q", "Q"),
      key("`") -> ("`", "`"),
      key(" ") -> ("Space", "Spc"),
      bytes(11) -> ("Control K", "^K"),
      bytes(1) -> ("Control A", "^A"),
      bytes(29) -> ("Control ]", "^]"),
      bytes(31) -> ("Control _", "^_"),
      bytes(27) -> ("Esc", "Esc"),
      bytes(127) -> ("Delete", "Del"),
      key("∫") -> ("∫ Option B", "∫"),
      key("å") -> ("å Option A", "å"),
      key("Ω") -> ("Ω Option Z", "Ω"),
      key("¡") -> ("¡ Option 1", "¡"),
      key("º") -> ("º Option 0", "º"),
      key("Å") -> ("Å", "Å"),
      key("\u001bOP") -> ("F1", "F1"),
      key("\u001bOS") -> ("F4", "F4"),
      key("\u001b[15~") -> ("F5", "F5"),
      key("\u001b[21~") -> ("F10", "F10"),
      key("\u001b[24~") -> ("F12", "F12"),
      key("\u001b[1;2A") -> ("1b 5b 31 3b 32 41", "Esc[1;2A"),
      KeyGroup(Vector(155, 65)) -> ("9b 41", "9b 41")
    )
    for (group, (name, short)) <- cases do
      assertEquals(KeyGroup.name(group), name, group)
      assertEquals(KeyGroup.short(group), short, group)
    val option = "abcdfghjklmopqrstvwxyz1234567890".zip("å∫ç∂ƒ©˙∆˚¬µøπœ®ß†√∑≈¥Ω¡™£¢∞§¶•ªº")
    for (letter, typed) <- option do
      assertEquals(KeyGroup.name(key(typed.toString)), s"$typed Option ${letter.toUpper}")
    assertEquals(two("g", "m").name, "g then m")
    assertEquals(two("g", "m").short, "g m")
    assertEquals(Binding(Vector(bytes(11), key("∫"))).short, "^K ∫")
    assertEquals(one("q").name, "q")
  }

  test("lookups match exact bytes and sequence first keys") {
    val bindings = Bindings.defaults
      .updated(Bind.Market, two("g", "m"))
      .flatMap(_.updated(Bind.Close, one("∫")))
      .flatMap(_.updated(Bind.Settings, Binding(Vector(key("g"), key("\u001b[15~")))))
      .toOption
      .get
    assertEquals(bindings.command(key("∫")), Some(Command.Close))
    assertEquals(bindings.command(KeyGroup(Vector(226, 136))), None)
    assertEquals(bindings.command(key("m")), None)
    assertEquals(bindings.command(key("g")), None)
    assertEquals(bindings.command(key("g"), key("m")), Some(Command.StartMarketPrompt))
    assertEquals(bindings.command(key("g"), key("\u001b[15~")), Some(Command.Settings))
    assertEquals(bindings.command(key("g"), key("x")), None)
    assertEquals(bindings.command(key("m"), key("g")), None)
    assertEquals(bindings.command(key("q"), key("q")), None)
    assert(bindings.prefix(key("g")))
    assert(!bindings.prefix(key("m")))
    assert(!bindings.prefix(key("q")))
    assert(!Bindings.defaults.prefix(key("q")))
    assertEquals(Bindings.defaults.command(KeyGroup(Vector(113, 113))), None)
    assertEquals(bindings.command(KeyGroup(key("∫").bytes :+ 120)), None)
    val sequence = Bindings.defaults.updated(Bind.Chart, two("∫", "c")).toOption.get
    assert(sequence.prefix(key("∫")))
    assert(!sequence.prefix(KeyGroup(Vector(226, 136))))
  }

  test("updated refuses equal bindings and prefix clashes and accepts shared first keys") {
    val d = Bindings.defaults
    assertEquals(d.updated(Bind.Market, one("q")), Left("q is already the key for Quit"))
    assertEquals(d.updated(Bind.Quit, one("q")).map(_.map(Bind.Quit)), Right(one("q")))
    val market = d.updated(Bind.Market, two("g", "m"))
    assert(market.isRight)
    assertEquals(
      market.flatMap(_.updated(Bind.Chart, one("g"))),
      Left("g starts the keys for Market")
    )
    assertEquals(
      market.flatMap(_.updated(Bind.Chart, two("g", "m"))),
      Left("g then m is already the key for Market")
    )
    val shared = market.flatMap(_.updated(Bind.Chart, two("g", "c")))
    assertEquals(shared.map(_.map(Bind.Chart)), Right(two("g", "c")))
    assertEquals(d.updated(Bind.Chart, two("q", "c")), Left("q is already the key for Quit"))
    assertEquals(
      d.updated(Bind.Chart, two("c", "c")).map(_.map),
      Right(d.map.updated(Bind.Chart, two("c", "c")))
    )
  }

  test("updated refuses reserved, dead and malformed bindings") {
    val d = Bindings.defaults
    assertEquals(d.updated(Bind.Quit, Binding(Vector(bytes(3)))), Left("Control C quits Desk"))
    assertEquals(
      d.updated(Bind.Quit, Binding(Vector(key("g"), key("\u001b[A")))),
      Left("The arrows move the chart and the ticket price")
    )
    assertEquals(d.updated(Bind.Quit, one("é")), Left(dead.get))
    assertEquals(
      d.updated(Bind.Quit, Binding(Vector.empty)),
      Left("A binding is one key or two keys")
    )
    assertEquals(
      d.updated(Bind.Quit, Binding(Vector(key("a"), key("b"), key("c")))),
      Left("A binding is one key or two keys")
    )
    assertEquals(d.updated(Bind.Quit, Binding(Vector(KeyGroup(Vector(155, 65))))), Left(notKey.get))
  }

  test("bindings live next to the settings") {
    val home = tempDirectory("desk-bindings-home")
    assertEquals(Bindings.path(home), home.resolve(".config/desk/bindings.json"))
  }

  test("an absent file is the defaults and loading creates nothing") {
    val p = path()
    assertEquals(Bindings.load(p), Right(Bindings.defaults))
    assert(!Files.exists(p.getParent))
  }

  test("a missing action keeps its default and a swap loads") {
    val loaded = Bindings.load(put(path(), file("\"quit\":[\"Q\"]")))
    assertEquals(loaded.map(_.map), Right(Bindings.defaults.map.updated(Bind.Quit, one("Q"))))
    val swapped = Bindings.load(put(path(), file("\"quit\":[\"m\"],\"market\":[\"q\"]")))
    assertEquals(swapped.map(_.map(Bind.Quit)), Right(one("m")))
    assertEquals(swapped.map(_.map(Bind.Market)), Right(one("q")))
    val example = Bindings.load(
      put(
        path(),
        "{\"version\":1,\"bindings\":{\"quit\":[\"q\"],\"market\":[\"g\",\"m\"]," +
          "\"close\":[\"∫\"],\"settings\":[\"\\u001bOP\"]}}"
      )
    )
    assertEquals(example.map(_.map(Bind.Market)), Right(two("g", "m")))
    assertEquals(example.map(_.map(Bind.Close)), Right(one("∫")))
    assertEquals(example.map(_.map(Bind.Settings)), Right(one("\u001bOP")))
  }

  test("a file without the new fields loads with t and o at their defaults") {
    val loaded = Bindings.load(put(path(), file("\"quit\":[\"Q\"]"))).toOption.get
    assertEquals(loaded.map(Bind.OrderType), one("t"))
    assertEquals(loaded.map(Bind.Orders), one("o"))
    assertEquals(loaded.command(key("t")), Some(Command.OrderType))
    assertEquals(loaded.command(key("o")), Some(Command.Orders))
  }

  test("a is reserved so no binding can take it") {
    assertEquals(
      KeyGroup.refused(bytes(97)),
      Some("a places at the selected order's price")
    )
    assert(Bindings.defaults.updated(Bind.Quit, one("a")).isLeft)
  }

  test("an owner binding on t leaves order type unbound and the file loads") {
    for field <- Vector("quit", "close", "settings") do
      val loaded = Bindings.load(put(path(), file(s"\"$field\":[\"t\"]")))
      val bindings = loaded.toOption.get
      val action = Bind.values.find(_.field == field).get
      assertEquals(bindings.map(action), one("t"))
      assertEquals(bindings.map.get(Bind.OrderType), None)
      assertEquals(bindings.map(Bind.Orders), one("o"))
      assertEquals(bindings.command(key("t")), Some(Bindings.command(action)))
  }

  test("an owner binding on o leaves orders unbound and the file loads") {
    val loaded = Bindings.load(put(path(), file("\"market\":[\"o\"]"))).toOption.get
    assertEquals(loaded.map(Bind.Market), one("o"))
    assertEquals(loaded.map.get(Bind.Orders), None)
    assertEquals(loaded.map(Bind.OrderType), one("t"))
    assertEquals(loaded.command(key("o")), Some(Command.StartMarketPrompt))
  }

  test("a sequence starting with t or o also leaves the new action unbound") {
    val first = Bindings.load(put(path(), file("\"quit\":[\"t\",\"q\"]"))).toOption.get
    assertEquals(first.map.get(Bind.OrderType), None)
    val second = Bindings.load(put(path(), file("\"quit\":[\"o\",\"q\"]"))).toOption.get
    assertEquals(second.map.get(Bind.Orders), None)
  }

  test("an unbound new action saves and loads back unbound") {
    val saved = put(path(), file("\"quit\":[\"t\"]"))
    Files.setPosixFilePermissions(saved.getParent, PosixFilePermissions.fromString("rwx------"))
    val bindings = Bindings.load(saved).toOption.get
    Bindings.save(saved, bindings)
    assertEquals(Bindings.load(saved).map(_.map), Right(bindings.map))
  }

  test("a clash between two explicit entries still refuses the file") {
    val loaded = Bindings.load(put(path(), file("\"quit\":[\"t\"],\"orderType\":[\"t\"]")))
    assert(loaded.isLeft, loaded)
  }

  test("save writes a private file that loads back exactly") {
    val p = path()
    val changed = Bindings.defaults
      .updated(Bind.Market, two("g", "m"))
      .flatMap(_.updated(Bind.Close, one("∫")))
      .flatMap(_.updated(Bind.Settings, one("\u001bOP")))
      .flatMap(_.updated(Bind.Quit, Binding(Vector(bytes(11)))))
      .flatMap(_.updated(Bind.Chart, one("\ud834\udd1e")))
      .toOption
      .get
    Bindings.save(p, changed)
    assertEquals(PosixFilePermissions.toString(Files.getPosixFilePermissions(p)), "rw-------")
    assertEquals(Bindings.load(p), Right(changed))
    val json = ujson.read(Files.readString(p))
    assertEquals(json("version").num, 1.0)
    assertEquals(json("bindings")("market").arr.map(_.str).toVector, Vector("g", "m"))
    assertEquals(json("bindings")("settings").arr.map(_.str).toVector, Vector("\u001bOP"))
    assertEquals(json("bindings")("longSmall").arr.map(_.str).toVector, Vector("1"))
    assert(Files.readString(p).contains("\\u001bOP"))
    assertEquals(json("bindings").obj.keySet.toSet, Bind.values.map(_.field).toSet)
    Bindings.save(p, Bindings.defaults)
    assertEquals(Bindings.load(p), Right(Bindings.defaults))
  }

  test("save refuses a folder others can read and leaves the file as it was") {
    val home = tempDirectory("desk-bindings-open")
    val p = Bindings.path(home)
    val before = file("\"quit\":[\"Q\"]")
    put(p, before)
    Files.setPosixFilePermissions(p.getParent, PosixFilePermissions.fromString("rwxr-xr-x"))
    val error = intercept[IllegalArgumentException](Bindings.save(p, Bindings.defaults))
    assert(error.getMessage.contains("local storage must be private"))
    assertEquals(Files.readString(p), before)
  }

  test("a hand edit is refused with the action named") {
    val cases = Vector(
      file("\"close\":[\"\\u0003\"]") -> "close: Control C quits Desk",
      file("\"close\":[\"\\u001b[A\"]") -> "close: The arrows move the chart and the ticket price",
      file("\"close\":[\"g\",\"\\u001b[200~\"]") -> "close: Paste markers are not keys",
      file("\"close\":[\"é\"]") -> "close: That key needs a second press on this layout",
      file("\"close\":[\"qq\"]") -> "close: That is not one key",
      file("\"close\":[\"\\u001bq\"]") -> "close: That is not one key",
      file("\"close\":[\"\\ud800\"]") -> "close: That is not one key",
      file("\"market\":[\"q\"]") -> "market: q is already the key for Quit",
      file("\"market\":[\"g\",\"m\"],\"chart\":[\"g\"]") -> "chart: g starts the keys for Market",
      file("\"quit\":[\"g\"],\"market\":[\"g\",\"m\"]") ->
        "market: g is already the key for Quit",
      file("\"close\":[]") -> "close must be a list of one or two keys",
      file("\"close\":[\"a\",\"b\",\"c\"]") -> "close must be a list of one or two keys",
      file("\"close\":[\"\"]") -> "close must be a list of one or two keys",
      file("\"close\":\"x\"") -> "close must be a list of one or two keys",
      file("\"close\":[1]") -> "close must be a list of one or two keys",
      file("\"jump\":[\"j\"]") -> "unknown action",
      file("\"\\u001b]0;x\\u0007\":[\"j\"]") -> "unknown action",
      "{\"version\":2,\"bindings\":{}}" -> "the file needs version 1 and bindings",
      "{\"bindings\":{}}" -> "the file needs version 1 and bindings",
      "{\"version\":1}" -> "the file needs version 1 and bindings",
      "{\"version\":1,\"bindings\":{},\"extra\":1}" -> "the file needs version 1 and bindings",
      "{\"version\":1,\"bindings\":[]}" -> "the file needs version 1 and bindings",
      "[]" -> "the file needs version 1 and bindings",
      "{\"version\":1,\"bindings\":" -> "the file is not JSON",
      "[" * 8000 + "]" * 8000 -> "the file needs version 1 and bindings"
    )
    for (text, reason) <- cases do assertEquals(Bindings.load(put(path(), text)), refusal(reason))
  }

  test("an open, oversized, linked or non UTF-8 file is refused") {
    val reason = "the file must be private, UTF-8 and at most 16 KiB"
    assertEquals(Bindings.load(put(path(), file(""), "rw-r--r--")), refusal(reason))
    assertEquals(Bindings.load(put(path(), file("") + " " * 16384)), refusal(reason))
    val target = put(path(), file(""))
    val link = path()
    Files.createDirectories(link.getParent)
    Files.createSymbolicLink(link, target)
    assertEquals(Bindings.load(link), refusal(reason))
    val binary = path()
    Files.createDirectories(binary.getParent)
    Files.write(binary, Array[Byte](123, -1, 125))
    Files.setPosixFilePermissions(binary, PosixFilePermissions.fromString("rw-------"))
    assertEquals(Bindings.load(binary), refusal(reason))
    assertEquals(
      Bindings.load(put(path(), file("") + " " * (16384 - file("").length))).map(_.map),
      Right(Bindings.defaults.map)
    )
  }

  test("invisible space characters are not keys, and the space bar keeps its name") {
    def typed(text: String): KeyGroup = KeyGroup(text.getBytes("UTF-8").toVector.map(_ & 0xff))
    // Option Space on Apple Terminal, a figure space, and the two-column ideographic space.
    for text <- Vector("\u00a0", "\u2007", "\u3000") do
      assertEquals(KeyGroup.refused(typed(text)), Some("That is not one key"), text)
    assertEquals(KeyGroup.refused(KeyGroup(Vector(32))), None)
    assertEquals(KeyGroup.name(KeyGroup(Vector(32))), "Space")
  }
