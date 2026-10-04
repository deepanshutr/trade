package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

class KeyNamesSpec extends TempDirectorySuite:
  private val first = "0x" + "a" * 40
  private val second = "0x" + "b" * 40
  private val third = "0x" + "c" * 40
  private val stamp = 1759300000000L

  private def home(): Path = tempDirectory("desk-keynames")
  private def namesPath(): Path = KeyNames.path(home())
  private def activePath(): Path = ActiveKey.path(home())

  private def createLine(
      address: String,
      name: String = "Main",
      network: String = "testnet",
      time: String = stamp.toString
  ): String =
    s"""{"kind":"create","address":"$address","name":"$name","network":"$network","time":$time}"""

  private def renameLine(address: String, name: String, time: Long = stamp + 1): String =
    s"""{"kind":"rename","address":"$address","name":"$name","time":$time}"""

  private def put(path: Path, text: String, mode: String = "rw-------"): Path =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(mode))
    path

  private def mode(path: Path): String =
    PosixFilePermissions.toString(Files.getPosixFilePermissions(path))

  private def lines(vector: String*): String = vector.map(_ + "\n").mkString

  private def invalidAt(line: Int) = Left(s"key names file is invalid at line $line")

  // Key names

  test("the key files live under the config folder of the home") {
    val h = home()
    assertEquals(KeyNames.path(h), h.resolve(".config/desk/key-names.jsonl"))
    assertEquals(ActiveKey.path(h), h.resolve(".config/desk/active-key.json"))
  }

  test("an absent key names file is empty and loading creates nothing") {
    val path = namesPath()
    assertEquals(KeyNames.load(path), Right(Vector.empty))
    assert(!Files.exists(path.getParent))
    val empty = put(path, "")
    assertEquals(KeyNames.load(empty), Right(Vector.empty))
  }

  test("create writes the exact line with mode 600 in a private folder") {
    val path = namesPath()
    KeyNames.create(path, KeyEntry(first, "Main", "testnet", stamp))
    assertEquals(Files.readString(path), createLine(first) + "\n")
    assertEquals(mode(path), "rw-------")
    assertEquals(mode(path.getParent), "rwx------")
    assertEquals(
      KeyNames.load(path),
      Right(Vector(KeyEntry(first, "Main", "testnet", stamp)))
    )
  }

  test("create then rename folds to the new name and keeps the creation time") {
    val path = namesPath()
    KeyNames.create(path, KeyEntry(first, "Main", "testnet", stamp))
    KeyNames.create(path, KeyEntry(second, "Spare", "mainnet", stamp + 5))
    KeyNames.rename(path, first, "Daily", stamp + 10)
    KeyNames.rename(path, first, "Daily two", stamp + 20)
    assertEquals(
      Files.readString(path),
      lines(
        createLine(first),
        createLine(second, "Spare", "mainnet", (stamp + 5).toString),
        renameLine(first, "Daily", stamp + 10),
        renameLine(first, "Daily two", stamp + 20)
      )
    )
    assertEquals(
      KeyNames.load(path),
      Right(
        Vector(
          KeyEntry(first, "Daily two", "testnet", stamp),
          KeyEntry(second, "Spare", "mainnet", stamp + 5)
        )
      )
    )
  }

  test("appends leave every earlier byte unchanged") {
    val path = namesPath()
    KeyNames.create(path, KeyEntry(first, "Main", "testnet", stamp))
    var before = Files.readAllBytes(path)
    val steps = Vector[() => Unit](
      () => KeyNames.create(path, KeyEntry(second, "Spare", "testnet", stamp + 1)),
      () => KeyNames.rename(path, first, "Daily", stamp + 2),
      () => KeyNames.rename(path, second, "Other", stamp + 3)
    )
    for step <- steps do
      step()
      val after = Files.readAllBytes(path)
      assert(after.length > before.length)
      assert(java.util.Arrays.equals(after.take(before.length), before))
      assertEquals(after.last, '\n'.toByte)
      assertEquals(mode(path), "rw-------")
      before = after
  }

  test("names with quotes and backslashes survive the file") {
    val path = namesPath()
    val name = "a\"b\\c /~"
    KeyNames.create(path, KeyEntry(first, name, "testnet", stamp))
    assertEquals(KeyNames.load(path).map(_.map(_.name)), Right(Vector(name)))
    KeyNames.rename(path, first, "\"\\", stamp + 1)
    assertEquals(KeyNames.load(path).map(_.map(_.name)), Right(Vector("\"\\")))
  }

  test("names are one to 32 printable ASCII characters without edge spaces") {
    val good = Vector("a", "Main", "a b", "a  b", "~", "!", "x" * 32, "\"", "\\")
    val bad = Vector(
      "",
      " ",
      "x" * 33,
      " a",
      "a ",
      "a\n",
      "\ta",
      "a\u001fb",
      "a\u007fb",
      "café",
      "\u0000",
      "a b"
    )
    good.foreach(name => assert(KeyNames.validName(name), name))
    bad.foreach(name => assert(!KeyNames.validName(name), name))
  }

  test("a bad file refuses the whole file and names the line") {
    val a = createLine(first)
    def extra(line: String, field: String): String = line.dropRight(1) + "," + field + "}"
    val cases = Vector[(String, String, Int)](
      ("rename before its create", lines(renameLine(first, "Daily")), 1),
      ("rename of an unknown address", lines(a, renameLine(second, "Daily")), 2),
      ("second create for the same address", lines(a, createLine(first, "Other")), 2),
      (
        "third line repeats the first address on another network",
        lines(a, createLine(second), createLine(first, "Third", "mainnet")),
        3
      ),
      ("unknown field", lines(a, extra(createLine(second), "\"x\":\"y\"")), 2),
      ("unknown field on a rename", lines(a, extra(renameLine(first, "D"), "\"x\":1")), 2),
      (
        "rename with a network",
        lines(a, extra(renameLine(first, "D"), "\"network\":\"testnet\"")),
        2
      ),
      ("missing time", lines(a, createLine(second).replace(s",\"time\":$stamp", "")), 2),
      ("missing name", lines(a, renameLine(first, "D").replace("\"name\":\"D\",", "")), 2),
      ("unknown kind", lines(a, createLine(second).replace("create", "delete")), 2),
      ("missing kind", lines(a, createLine(second).replace("\"kind\":\"create\",", "")), 2),
      ("upper case address", lines(createLine("0x" + "A" * 40)), 1),
      ("short address", lines(a, createLine("0x" + "b" * 39)), 2),
      ("long address", lines(a, createLine("0x" + "b" * 41)), 2),
      ("address without prefix", lines(a, createLine("b" * 40)), 2),
      ("address with a non hex digit", lines(a, createLine("0x" + "g" * 40)), 2),
      ("bad network", lines(a, createLine(second, network = "devnet")), 2),
      ("empty network", lines(a, createLine(second, network = "")), 2),
      ("empty name", lines(a, createLine(second, name = "")), 2),
      ("long name", lines(a, createLine(second, name = "x" * 33)), 2),
      ("name with an edge space", lines(a, createLine(second, name = " x")), 2),
      ("name with a control byte", lines(a, createLine(second, name = "x\\u0001")), 2),
      ("name with an escaped letter", lines(a, createLine(second, name = "\\u0041")), 2),
      ("rename to a bad name", lines(a, renameLine(first, "")), 2),
      ("time as text", lines(a, createLine(second, time = "\"5\"")), 2),
      ("negative time", lines(a, createLine(second, time = "-1")), 2),
      ("fractional time", lines(a, createLine(second, time = "1.5")), 2),
      ("exponent time", lines(a, createLine(second, time = "1e3")), 2),
      ("time past exact numbers", lines(a, createLine(second, time = "9007199254740992")), 2),
      ("not an object", lines(a, "[]"), 2),
      ("not json", lines(a, "broken"), 2),
      ("a blank line", lines(a, "", createLine(second)), 2),
      ("a lone newline", "\n", 1),
      (
        "a repeated key",
        lines(
          a,
          createLine(second).replace("\"name\":\"Main\"", "\"name\":\"Main\",\"name\":\"Alt\"")
        ),
        2
      ),
      (
        "spaces between tokens",
        lines(a, createLine(second).replace(",\"name\"", ", \"name\"")),
        2
      ),
      (
        "a different key order",
        lines(
          a,
          s"""{"address":"$second","kind":"create","name":"B","network":"testnet","time":5}"""
        ),
        2
      ),
      ("a carriage return", a + "\r\n", 1),
      ("a byte order mark", "\ufeff" + a + "\n", 1)
    )
    for (label, text, line) <- cases do
      val path = put(namesPath(), text)
      assertEquals(KeyNames.load(path), invalidAt(line), label)
      assertEquals(Files.readString(path), text, label)
  }

  test("a torn last line is refused and never repaired") {
    val a = createLine(first)
    val b = createLine(second)
    val cases = Vector[(String, Int)](
      (lines(a) + b.take(40), 2),
      (lines(a) + b, 2),
      (a, 1),
      (lines(a) + "{", 2),
      (lines(a, b) + b.dropRight(1), 3)
    )
    for (text, line) <- cases do
      val path = put(namesPath(), text)
      val before = Files.readAllBytes(path)
      assertEquals(KeyNames.load(path), invalidAt(line), text)
      assert(java.util.Arrays.equals(Files.readAllBytes(path), before))
  }

  test("writers refuse a bad file and leave it as it was") {
    val torn = lines(createLine(first)) + createLine(second).take(30)
    val path = put(namesPath(), torn)
    val before = Files.readAllBytes(path)
    intercept[IllegalStateException](
      KeyNames.create(path, KeyEntry(third, "Third", "testnet", stamp))
    )
    intercept[IllegalStateException](KeyNames.rename(path, first, "Daily", stamp))
    assert(java.util.Arrays.equals(Files.readAllBytes(path), before))
    val loose = put(namesPath(), lines(createLine(first)), "rw-r--r--")
    intercept[IllegalStateException](
      KeyNames.create(loose, KeyEntry(third, "Third", "testnet", stamp))
    )
    assertEquals(Files.readString(loose), lines(createLine(first)))
  }

  test("writers refuse a duplicate address, an unknown address and a bad value") {
    val path = namesPath()
    KeyNames.create(path, KeyEntry(first, "Main", "testnet", stamp))
    val before = Files.readAllBytes(path)
    val bad = Vector[() => Unit](
      () => KeyNames.create(path, KeyEntry(first, "Again", "testnet", stamp + 1)),
      () => KeyNames.create(path, KeyEntry(first, "Again", "mainnet", stamp + 1)),
      () => KeyNames.rename(path, second, "Daily", stamp + 1),
      () => KeyNames.create(path, KeyEntry("0x" + "B" * 40, "Spare", "testnet", stamp)),
      () => KeyNames.create(path, KeyEntry(second, "Spare", "devnet", stamp)),
      () => KeyNames.create(path, KeyEntry(second, "", "testnet", stamp)),
      () => KeyNames.create(path, KeyEntry(second, " Spare", "testnet", stamp)),
      () => KeyNames.create(path, KeyEntry(second, "Spare", "testnet", -1L)),
      () => KeyNames.create(path, KeyEntry(second, "Spare", "testnet", 9007199254740992L)),
      () => KeyNames.rename(path, first, "x" * 33, stamp + 1),
      () => KeyNames.rename(path, first, "Daily", -1L),
      () => KeyNames.rename(path, "0x" + "A" * 40, "Daily", stamp + 1)
    )
    for step <- bad do
      intercept[IllegalArgumentException](step())
      assert(java.util.Arrays.equals(Files.readAllBytes(path), before))
  }

  test("a file over one MiB is refused and the limit itself is accepted") {
    val one = lines(createLine("0x" + "0" * 40))
    val per = one.getBytes("UTF-8").length
    val fits = (1048576 / per).toInt
    def file(count: Int): String =
      (0 until count).map(i => createLine("0x" + f"$i%040x") + "\n").mkString
    val ok = put(namesPath(), file(fits))
    assert(Files.size(ok) <= 1048576L)
    assertEquals(KeyNames.load(ok).map(_.size), Right(fits))
    val over = put(namesPath(), file(fits + 1))
    assert(Files.size(over) > 1048576L)
    assert(KeyNames.load(over).isLeft)
    assertEquals(
      KeyNames.load(over),
      Left("key names file must be a private regular file under 1 MiB")
    )
  }

  test("a line over 512 bytes is refused") {
    val long = createLine(second) + " " * (513 - createLine(second).length)
    assertEquals(long.length, 513)
    val path = put(namesPath(), lines(createLine(first), long))
    assertEquals(KeyNames.load(path), invalidAt(2))
  }

  test("deeply nested input is refused and no error escapes") {
    val array = "[" * 512
    val objects = "{\"a\":" * 100
    assertEquals(KeyNames.load(put(namesPath(), lines(array))), invalidAt(1))
    assertEquals(KeyNames.load(put(namesPath(), lines(createLine(first), objects))), invalidAt(2))
    assertEquals(ActiveKey.load(put(activePath(), "[" * 1000)), Left("active key file is invalid"))
    assertEquals(ActiveKey.load(put(activePath(), objects)), Left("active key file is invalid"))
  }

  test("a symbolic link, a loose file and a folder are refused") {
    val dir = home()
    val real = put(dir.resolve("real.jsonl"), lines(createLine(first)))
    val link = dir.resolve("link.jsonl")
    Files.createSymbolicLink(link, real)
    val dangling = dir.resolve("dangling.jsonl")
    Files.createSymbolicLink(dangling, dir.resolve("nowhere"))
    val folder = Files.createDirectory(dir.resolve("folder.jsonl"))
    val message = Left("key names file must be a private regular file under 1 MiB")
    assertEquals(KeyNames.load(link), message)
    assertEquals(KeyNames.load(dangling), message)
    assertEquals(KeyNames.load(folder), message)
    for loose <- Vector("rw-r-----", "rw-r--r--", "rw-rw----", "rw----r--", "rwx------") do
      Files.setPosixFilePermissions(real, PosixFilePermissions.fromString(loose))
      val expected = if loose == "rwx------" then Right(1) else message
      assertEquals(KeyNames.load(real).map(_.size), expected, loose)
    intercept[IllegalStateException](
      KeyNames.create(link, KeyEntry(second, "Spare", "testnet", stamp))
    )
    assertEquals(Files.readString(real), lines(createLine(first)))
  }

  // Active key

  test("an absent active key file is empty and select creates it with mode 600") {
    val path = activePath()
    assertEquals(ActiveKey.load(path), Right(Map.empty[String, String]))
    assert(!Files.exists(path.getParent))
    ActiveKey.select(path, "testnet", first)
    assertEquals(Files.readString(path).trim, s"""{"testnet":"$first"}""")
    assertEquals(mode(path), "rw-------")
    assertEquals(mode(path.getParent), "rwx------")
    assertEquals(ActiveKey.load(path), Right(Map("testnet" -> first)))
  }

  test("select keeps the other network and replaces its own") {
    val path = activePath()
    ActiveKey.select(path, "testnet", first)
    ActiveKey.select(path, "mainnet", second)
    assertEquals(ActiveKey.load(path), Right(Map("testnet" -> first, "mainnet" -> second)))
    ActiveKey.select(path, "testnet", third)
    assertEquals(ActiveKey.load(path), Right(Map("testnet" -> third, "mainnet" -> second)))
    assertEquals(mode(path), "rw-------")
    val names = Files.list(path.getParent).iterator().asScala.map(_.getFileName.toString).toVector
    assertEquals(names, Vector("active-key.json"))
  }

  test("the active key accepts either network or none") {
    val ok = Vector(
      "{}" -> Map.empty[String, String],
      s"""{"mainnet":"$first"}""" -> Map("mainnet" -> first),
      s"""{"testnet":"$second"}""" -> Map("testnet" -> second),
      s"""{"mainnet":"$first","testnet":"$second"}""" -> Map(
        "mainnet" -> first,
        "testnet" -> second
      ),
      s"""{ "testnet" : "$second" }
""" -> Map("testnet" -> second)
    )
    for (text, expected) <- ok do
      assertEquals(ActiveKey.load(put(activePath(), text)), Right(expected), text)
  }

  test("the active key refuses unknown fields, bad addresses and bad files") {
    val bad = Vector(
      s"""{"devnet":"$first"}""",
      s"""{"testnet":"$first","extra":"x"}""",
      s"""{"testnet":"$first","extra":1}""",
      s"""{"Testnet":"$first"}""",
      s"""{"testnet":"${"0x" + "A" * 40}"}""",
      s"""{"testnet":"${"0x" + "a" * 39}"}""",
      s"""{"testnet":"${"a" * 40}"}""",
      """{"testnet":5}""",
      """{"testnet":null}""",
      """{"testnet":["0x"]}""",
      "[]",
      "\"0x\"",
      "",
      "broken",
      """{"testnet":"""
    )
    for text <- bad do
      val path = put(activePath(), text)
      assertEquals(ActiveKey.load(path), Left("active key file is invalid"), text)
      assertEquals(Files.readString(path), text)
  }

  test("the active key refuses a symbolic link, a loose file and a large file") {
    val dir = home()
    val real = put(dir.resolve("real.json"), s"""{"testnet":"$first"}""")
    val link = dir.resolve("link.json")
    Files.createSymbolicLink(link, real)
    assert(ActiveKey.load(link).isLeft)
    for loose <- Vector("rw-r-----", "rw-r--r--", "rw----rw-") do
      Files.setPosixFilePermissions(real, PosixFilePermissions.fromString(loose))
      assert(ActiveKey.load(real).isLeft, loose)
    Files.setPosixFilePermissions(real, PosixFilePermissions.fromString("rw-------"))
    assert(ActiveKey.load(real).isRight)
    val big = put(dir.resolve("big.json"), s"""{"testnet":"$first"}""" + " " * 2000)
    assert(ActiveKey.load(big).isLeft)
  }

  test("select refuses a bad network, a bad address and a bad existing file") {
    val path = activePath()
    ActiveKey.select(path, "testnet", first)
    val before = Files.readAllBytes(path)
    intercept[IllegalArgumentException](ActiveKey.select(path, "devnet", second))
    intercept[IllegalArgumentException](ActiveKey.select(path, "testnet", "0x" + "B" * 40))
    intercept[IllegalArgumentException](ActiveKey.select(path, "testnet", "0x1234"))
    intercept[IllegalArgumentException](ActiveKey.select(path, "", second))
    assert(java.util.Arrays.equals(Files.readAllBytes(path), before))
    val broken = put(activePath(), """{"testnet":""")
    intercept[IllegalStateException](ActiveKey.select(broken, "testnet", first))
    assertEquals(Files.readString(broken), """{"testnet":""")
    val loose = put(activePath(), s"""{"testnet":"$first"}""", "rw-r--r--")
    intercept[IllegalStateException](ActiveKey.select(loose, "mainnet", second))
    assertEquals(mode(loose), "rw-r--r--")
  }

  // Keychain items

  test("present builds the attribute only lookup and maps the exit codes") {
    var seen = Vector.empty[Seq[String]]
    def run(code: Int): Seq[String] => Int = args =>
      seen :+= args
      code
    assertEquals(KeychainItems.present("desk", first, run(0)), Right(true))
    assertEquals(KeychainItems.present("desk", first, run(44)), Right(false))
    for code <- Vector(1, 2, 36, 45, 51, 124, 255, -1) do
      assertEquals(
        KeychainItems.present("desk", first, run(code)),
        Left("Keychain lookup failed"),
        code.toString
      )
    val expected = Seq("find-generic-password", "-s", "desk", "-a", first)
    assert(seen.nonEmpty && seen.forall(_ == expected), seen.toString)
    assert(!expected.contains("-w") && !expected.contains("-g"))
    var other = Seq.empty[String]
    KeychainItems.present("desk-test", second, { args => other = args; 0 })
    assertEquals(other, Seq("find-generic-password", "-s", "desk-test", "-a", second))
  }

  test("present reports a runner that throws as a failed lookup") {
    val result = KeychainItems.present("desk", first, _ => throw new java.io.IOException("gone"))
    assertEquals(result, Left("Keychain lookup failed"))
  }

  test("the process runner returns the exit code and ignores a full output pipe") {
    assertEquals(KeychainItems.runProcess(Seq("/usr/bin/true"), 5000L), 0)
    assertEquals(KeychainItems.runProcess(Seq("/bin/sh", "-c", "exit 44"), 5000L), 44)
    assertEquals(KeychainItems.runProcess(Seq("/bin/sh", "-c", "exit 1"), 5000L), 1)
    val noisy = "head -c 3000000 /dev/zero; head -c 3000000 /dev/zero >&2; exit 3"
    assertEquals(KeychainItems.runProcess(Seq("/bin/sh", "-c", noisy), 10000L), 3)
    assertEquals(KeychainItems.runProcess(Seq("/bin/cat"), 5000L), 0)
  }

  test("the process runner destroys a process that passes the limit") {
    val pidFile = home().resolve("pid")
    val script = "echo $$ > '" + pidFile + "'; exec /bin/sleep 30"
    val began = System.nanoTime()
    val code = KeychainItems.runProcess(Seq("/bin/sh", "-c", script), 300L)
    val elapsed = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - began)
    assertNotEquals(code, 0)
    assertNotEquals(code, 44)
    assert(elapsed < 5000L, s"the limit took $elapsed ms")
    val pid = Files.readString(pidFile).trim.toLong
    val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
    while ProcessHandle.of(pid).filter(_.isAlive).isPresent && System.nanoTime() < end do
      Thread.sleep(10)
    assert(!ProcessHandle.of(pid).filter(_.isAlive).isPresent, "the process is still alive")
  }

  test("the Keychain runner puts the tool path first and uses an eight second limit") {
    var seen = Vector.empty[(Seq[String], Long)]
    val run = KeychainItems.securityRunner { (command, limit) =>
      seen :+= ((command, limit))
      44
    }
    assertEquals(run(Seq("find-generic-password", "-s", "desk", "-a", first)), 44)
    assertEquals(
      seen,
      Vector((Seq("/usr/bin/security", "find-generic-password", "-s", "desk", "-a", first), 8000L))
    )
  }

  // Usable key

  private val firstEntry = KeyEntry(first, "Main", "testnet", stamp)
  private val secondEntry = KeyEntry(second, "Spare", "mainnet", stamp)

  test("a usable key is the active address with a names entry and a Keychain item") {
    var asked = Vector.empty[String]
    val result = KeySetup.usable(
      "testnet",
      Vector(secondEntry, firstEntry),
      Map("testnet" -> first, "mainnet" -> second),
      a =>
        asked :+= a
        Right(true)
    )
    assertEquals(result, Right(first))
    assertEquals(asked, Vector(first))
  }

  test("usable names each reason and stops at the first") {
    val both = Map("testnet" -> first, "mainnet" -> second)
    val cases = Vector[
      (String, String, Vector[KeyEntry], Map[String, String], Either[String, Boolean], String, Int)
    ](
      (
        "no active key for the network",
        "testnet",
        Vector(firstEntry),
        Map("mainnet" -> second),
        Right(true),
        "No active key for testnet. Choose one in settings",
        0
      ),
      (
        "no active key at all",
        "mainnet",
        Vector(firstEntry),
        Map.empty,
        Right(true),
        "No active key for mainnet. Choose one in settings",
        0
      ),
      (
        "not in names",
        "testnet",
        Vector(secondEntry),
        both,
        Right(true),
        "The active key is not in key names",
        0
      ),
      (
        "empty names",
        "testnet",
        Vector.empty,
        both,
        Right(true),
        "The active key is not in key names",
        0
      ),
      (
        "names entry on mainnet",
        "testnet",
        Vector(firstEntry.copy(network = "mainnet")),
        both,
        Right(true),
        "The active key belongs to mainnet",
        0
      ),
      (
        "names entry on testnet",
        "mainnet",
        Vector(secondEntry.copy(network = "testnet")),
        both,
        Right(true),
        "The active key belongs to testnet",
        0
      ),
      (
        "item absent",
        "testnet",
        Vector(firstEntry),
        both,
        Right(false),
        "The active key is not in the Keychain",
        1
      ),
      (
        "lookup error",
        "testnet",
        Vector(firstEntry),
        both,
        Left("private-lookup-canary"),
        "Keychain lookup failed",
        1
      )
    )
    for (label, network, names, active, answer, reason, calls) <- cases do
      var asked = 0
      val result = KeySetup.usable(
        network,
        names,
        active,
        _ =>
          asked += 1
          answer
      )
      assertEquals(result, Left(reason), label)
      assertEquals(asked, calls, label)
  }

  // Switching keys

  private def plan: OrderPlan =
    val b = Book(Vector(Level("99", "50", 1)), Vector(Level("100", "50", 1)), 1L)
    Trading
      .plan(
        "XMR",
        MarketRules(0, 3, 5),
        b,
        BigDecimal(100),
        BigDecimal(0),
        OrderIntent(Side.Buy, OrderKind.Hard),
        TradeSizes(),
        1,
        Trading.defaultSlippage
      )
      .toOption
      .get

  private val saved = "A saved order for the current key must finish first"
  private val inUse = "Another Desk is using the current key"

  test("a switch with no current key is allowed and touches nothing") {
    val dir = home().resolve("state")
    assertEquals(KeySetup.switchAllowed(dir, "testnet", None)(()), Right(()))
    assert(!Files.exists(dir))
  }

  test("a switch is allowed for an empty journal and the journal is closed again") {
    val dir = home().resolve("state")
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
    OrderJournal.open(dir, "testnet-" + first.drop(2)).close()
  }

  test("a switch is refused while the current key holds a saved order") {
    val dir = home().resolve("state")
    val identity = "testnet-" + first.drop(2)
    val journal = OrderJournal.open(dir, identity)
    journal.reserve(plan, second, 1000L)
    journal.close()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(saved))
    val reopened = OrderJournal.open(dir, identity)
    try
      assert(reopened.pending.nonEmpty)
      reopened.finish(reopened.pending.get.id)
    finally reopened.close()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
  }

  test("a switch uses the same identity as the trading settings") {
    val dir = home().resolve("state")
    val config = TradingConfig(network = "testnet", agent = Some(first))
    val journal = OrderJournal.open(dir, config.identity)
    journal.reserve(plan, second, 1000L)
    journal.close()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(saved))
    assertEquals(KeySetup.switchAllowed(dir, "mainnet", Some(first))(()), Right(()))
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(second))(()), Right(()))
  }

  test("a switch is refused while this process holds the journal lock") {
    val dir = home().resolve("state")
    val journal = OrderJournal.open(dir, "testnet-" + first.drop(2))
    try
      assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(inUse))
      assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(inUse))
      assertEquals(journal.reserve(plan, second, 1000L).owner, Some(second))
    finally journal.close()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(saved))
  }

  test("a switch is refused while another process holds the journal lock") {
    val dir = home().resolve("state")
    OrderJournal.open(dir, "testnet-" + first.drop(2)).close()
    val lock = dir.resolve("testnet-" + first.drop(2) + ".lock")
    val signal = dir.resolve("held")
    val process = new ProcessBuilder(
      "python3",
      "-c",
      "import fcntl,pathlib,sys\nf=open(sys.argv[1],'r+')\nfcntl.lockf(f,fcntl.LOCK_EX)\npathlib.Path(sys.argv[2]).touch()\nsys.stdin.read(1)",
      lock.toString,
      signal.toString
    ).start()
    try
      val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !Files.exists(signal) && process.isAlive && System.nanoTime() < end do Thread.sleep(2)
      assert(Files.exists(signal), "Python did not take the journal lock")
      assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(inUse))
    finally
      process.getOutputStream.close()
      if !process.waitFor(3, TimeUnit.SECONDS) then process.destroyForcibly()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
  }

  test("a switch is refused with a fixed message when the saved state cannot be read") {
    val dir = home().resolve("state")
    OrderJournal.open(dir, "testnet-" + first.drop(2)).close()
    val file = dir.resolve("testnet-" + first.drop(2) + ".json")
    put(file, "private-state-canary")
    val result = KeySetup.switchAllowed(dir, "testnet", Some(first))(())
    assertEquals(result, Left("The saved state for the current key cannot be read"))
    assert(!result.swap.toOption.get.contains("private-state-canary"))
    assertEquals(
      KeySetup.switchAllowed(dir, "testnet", Some("not an address"))(()),
      Left("The saved state for the current key cannot be read")
    )
  }

  test("the switch write runs once while the journal lock is held and the lock is free after") {
    val dir = home().resolve("state")
    var runs = 0
    var inside = Option.empty[Either[String, Unit]]
    val result = KeySetup.switchAllowed(dir, "testnet", Some(first)) {
      runs += 1
      inside = Some(KeySetup.switchAllowed(dir, "testnet", Some(first))(()))
    }
    assertEquals(result, Right(()))
    assertEquals(runs, 1)
    assertEquals(inside, Some(Left(inUse)))
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
  }

  test("the switch write does not run when the switch is refused") {
    val dir = home().resolve("state")
    val identity = "testnet-" + first.drop(2)
    var runs = 0
    def attempt() = KeySetup.switchAllowed(dir, "testnet", Some(first)) { runs += 1 }
    val journal = OrderJournal.open(dir, identity)
    try assertEquals(attempt(), Left(inUse))
    finally journal.close()
    val reserved = OrderJournal.open(dir, identity)
    reserved.reserve(plan, second, 1000L)
    reserved.close()
    assertEquals(attempt(), Left(saved))
    put(dir.resolve(identity + ".json"), "private-state-canary")
    assertEquals(attempt(), Left("The saved state for the current key cannot be read"))
    assertEquals(runs, 0)
  }

  test("with no current key the switch write runs and no journal is opened") {
    val dir = home().resolve("state")
    var runs = 0
    assertEquals(KeySetup.switchAllowed(dir, "testnet", None) { runs += 1 }, Right(()))
    assertEquals(runs, 1)
    assert(!Files.exists(dir))
  }

  test("a switch write that fails reaches the caller and leaves the lock free") {
    val dir = home().resolve("state")
    val error = intercept[IllegalStateException](
      KeySetup.switchAllowed(dir, "testnet", Some(first))(
        throw new IllegalStateException("write broke")
      )
    )
    assertEquals(error.getMessage, "write broke")
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
  }

  test("a switch is refused while the current key has a resting order") {
    val dir = home().resolve("state")
    val identity = "testnet-" + first.drop(2)
    val journal = OrderJournal.open(dir, identity)
    val order = journal.reserve(plan, second, 1000L)
    journal.rest(order.id, 77738308L)
    journal.close()
    val message = "Cancel the resting orders for the current key first"
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(message))
    val reopened = OrderJournal.open(dir, identity)
    try reopened.settle(order.id)
    finally reopened.close()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Right(()))
    val pending = OrderJournal.open(dir, identity)
    try pending.reserve(plan, second, 2000L)
    finally pending.close()
    assertEquals(KeySetup.switchAllowed(dir, "testnet", Some(first))(()), Left(saved))
  }
