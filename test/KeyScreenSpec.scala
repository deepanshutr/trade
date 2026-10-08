package desk

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException, InputStream, PrintStream}
import java.nio.charset.StandardCharsets.{ISO_8859_1, US_ASCII, UTF_8}
import java.nio.file.{Files, Path}
import java.security.{MessageDigest, SecureRandom}
import scala.collection.mutable.ArrayBuffer

class KeyScreenSpec extends TempDirectorySuite:
  private val stamp = 1759300000000L
  private val esc = "\u001b"
  private val enter = "\r"
  private val interrupt = "\u0003"
  private val phrase = "pass 123"
  private val seedOne = Array.tabulate[Byte](32)(_.toByte)
  private val seedTwo = Array.tabulate[Byte](32)(i => (255 - i).toByte)
  private def seedText(seed: Array[Byte]): String = new String(SeedText.encode(seed), US_ASCII)
  private val textOne = seedText(seedOne)
  private val textTwo = seedText(seedTwo)
  private val groupsOne = textOne.split("-").toVector
  private val deriving = "Deriving the key. This takes a few seconds."
  private val up = esc + "[A"
  private val down = esc + "[B"
  private val python = Platform.absolute("/usr/bin/python3")
  private val helperScript = Platform.absolute("/opt/desk/helper.py")

  private def storedText(approval: String): String =
    "Stored. Approval at the exchange is your step. Approve this address from the owner wallet " +
      s"as a named API key called $approval. Until then this key cannot trade, and the role " +
      "check must pass."

  private def fakeKey(seed: Array[Byte], pass: Array[Byte], length: Int): Array[Byte] =
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update("fake-derive".getBytes(US_ASCII))
    digest.update(seed)
    digest.update(pass, 0, length)
    digest.digest()

  private def keyOf(seed: Array[Byte], pass: String): Array[Byte] =
    fakeKey(seed, pass.getBytes(US_ASCII), pass.length)

  private def addressOf(seed: Array[Byte], pass: String): String =
    OrderSigning.address(keyOf(seed, pass))

  private def hex(bytes: Array[Byte]): String = java.util.HexFormat.of().formatHex(bytes)

  private def script(parts: String*): Array[Byte] = parts.mkString.getBytes(ISO_8859_1)
  private def paste(text: String): String = esc + "[200~" + text + esc + "[201~"

  // A last character that is never the right check for the group.
  private def flip(group: String): String = group.init + (if group.last == '0' then '1' else '0')

  private val csi = "\u001b\\[[0-9;?]*[A-Za-z]"
  private def frames(output: String): Vector[Vector[String]] =
    output
      .split(esc + "\\[2J")
      .toVector
      .drop(1)
      .map(_.replaceAll(csi, "").split("\r\n").toVector)
  private def lines(output: String): Vector[String] = frames(output).flatten

  private def occurrences(text: String, part: String): Int =
    text.split(java.util.regex.Pattern.quote(part), -1).length - 1

  private final class Seeds(seeds: Vector[Array[Byte]]) extends SecureRandom:
    var served = 0
    val handed = ArrayBuffer.empty[Array[Byte]]
    override def nextBytes(bytes: Array[Byte]): Unit =
      System.arraycopy(seeds(served.min(seeds.size - 1)), 0, bytes, 0, 32)
      served += 1
      handed += bytes

  // Reads the script, then hands every later read to afterwards.
  private final class Scripted(bytes: Array[Byte], afterwards: () => Int) extends InputStream:
    private var at = 0
    override def read(): Int =
      if at < bytes.length then
        val next = bytes(at) & 0xff
        at += 1
        next
      else afterwards()
    override def available(): Int = bytes.length - at

  // Reads the script and tells watch the index of each read, so a test can look at a buffer
  // between two reads.
  private final class Spy(bytes: Array[Byte], watch: Int => Unit) extends InputStream:
    private var at = 0
    override def read(): Int =
      watch(at)
      if at < bytes.length then
        val next = bytes(at) & 0xff
        at += 1
        next
      else -1
    override def available(): Int = bytes.length - at

  // Fails every write once trouble says so.
  private final class Failing(trouble: () => Boolean) extends java.io.OutputStream:
    def write(b: Int): Unit = if trouble() then throw new IOException("terminal gone")

  private final case class Call(kind: String, stdin: Array[Byte], derivations: Int)
  private final case class Derivation(
      seed: Array[Byte],
      pass: Array[Byte],
      key: Array[Byte],
      seedValue: Array[Byte],
      passValue: Array[Byte],
      keyValue: Array[Byte],
      announced: Int
  )

  private final class Fixture(
      network: String = "testnet",
      seeds: Vector[Array[Byte]] = Vector(seedOne),
      derive: (Array[Byte], Array[Byte], Int) => Array[Byte] = fakeKey,
      env: Map[String, String] = Map(
        "DESK_PYTHON" -> python,
        "DESK_KEY_HELPER" -> helperScript
      )
  ):
    val home: Path = tempDirectory("desk-keyscreen")
    val names: Path = KeyNames.path(home)
    val active: Path = ActiveKey.path(home)
    val state: Path = home.resolve("state")
    val present = scala.collection.mutable.Set.empty[String]
    val random = new Seeds(seeds)
    val bytes = new ByteArrayOutputStream()
    val screen = new PrintStream(bytes, true, "UTF-8")
    var copyExit = 0
    var clearExit = 0
    var helperMode = "stored"
    val clipboardCalls = ArrayBuffer.empty[Call]
    val helperCalls = ArrayBuffer.empty[(Seq[String], Array[Byte])]
    val securityCalls = ArrayBuffer.empty[Seq[String]]
    val derivations = ArrayBuffer.empty[Derivation]
    // The very arrays handed to the runners, so a test can see they are zeroed.
    val clipboardInputs = ArrayBuffer.empty[Array[Byte]]
    val helperInputs = ArrayBuffer.empty[Array[Byte]]
    var writeActive: (Path, String, String) => Unit = (path, on, address) =>
      ActiveKey.select(path, on, address)

    def output: String = bytes.toString("UTF-8")
    def kinds: Vector[String] = clipboardCalls.map(_.kind).toVector

    private val clipboard = (args: Seq[String], stdin: Array[Byte]) =>
      val kind = if stdin.nonEmpty then "copy" else "clear"
      clipboardInputs += stdin
      clipboardCalls += Call(kind, stdin.clone(), derivations.size)
      ProcessRun(if kind == "copy" then copyExit else clearExit, Array.emptyByteArray)

    private val helper = (args: Seq[String], stdin: Array[Byte]) =>
      helperInputs += stdin
      helperCalls += ((args, stdin.clone()))
      val key = java.util.HexFormat.of().parseHex(new String(stdin, 0, 64, US_ASCII))
      val account = OrderSigning.address(key)
      def reply(state: String): Array[Byte] =
        ujson
          .write(
            ujson.Obj(
              "account" -> account,
              "role" -> "agent",
              "service" -> "desk-test",
              "state" -> state,
              "prompt_required" -> true
            )
          )
          .getBytes(US_ASCII)
      helperMode match
        case "stored" =>
          present += account
          ProcessRun(0, reply("stored"))
        case "exists" =>
          present += account
          ProcessRun(3, reply("exists"))
        case _ => ProcessRun(1, Array.emptyByteArray)

    private val security = (args: Seq[String]) =>
      securityCalls += args
      if present.contains(args(args.indexOf("-a") + 1)) then 0 else 44

    private val recorded = (seed: Array[Byte], pass: Array[Byte], length: Int) =>
      val announced = occurrences(output, deriving)
      val key = derive(seed, pass, length)
      derivations += Derivation(
        seed,
        pass,
        key,
        seed.clone(),
        pass.take(length),
        key.clone(),
        announced
      )
      key

    def context: KeyScreen.Context = KeyScreen.Context(
      network,
      "desk-test",
      names,
      active,
      state,
      random,
      () => stamp,
      clipboard,
      helper,
      security,
      env,
      recorded,
      (path, on, address) => writeActive(path, on, address)
    )

    def run(input: Array[Byte]): Option[String] =
      KeyScreen.run(context, new ByteArrayInputStream(input), screen)

    def name(address: String, name: String, on: String = network): Unit =
      KeyNames.create(names, KeyEntry(address, name, on, stamp - 1))
    def activate(address: String): Unit = ActiveKey.select(active, network, address)
    def namesText: String = if Files.exists(names) then Files.readString(names) else ""
    def activeText: String = if Files.exists(active) then Files.readString(active) else ""
    def nameLines: Vector[String] = namesText.split("\n").toVector.filter(_.nonEmpty)

  private def assertZero(arrays: Array[Byte]*): Unit =
    arrays.foreach(a => assert(a.forall(_ == 0), "a secret buffer was not zeroed"))

  private def assertNoSecrets(output: String, extra: String*): Unit =
    val forbidden = Vector(
      textOne,
      textTwo,
      hex(seedOne),
      hex(seedTwo),
      phrase,
      hex(keyOf(seedOne, phrase)),
      hex(keyOf(seedTwo, phrase))
    ) ++ extra
    forbidden.foreach(secret => assert(!output.contains(secret), "the output shows a secret"))

  // New key: confirm the copy, passphrase twice, address, then the drill, name and stored screens.
  private def newKey(
      seed: String = textOne,
      pass: String = phrase,
      name: String = "Main",
      tail: String = "q"
  ): Array[Byte] =
    script(
      "n",
      enter,
      pass,
      enter,
      pass,
      enter,
      enter,
      paste(seed),
      pass,
      enter,
      name,
      enter,
      enter,
      tail
    )

  // The new key flow

  test("the seed text of the fixed seed is the pinned fixture text") {
    assertEquals(
      textOne,
      "000GK-40R4N-0M30S-E2093-185GZ-R38EF-1W81Z-24GK0-2GAH4-C5RRP-34D1T-P70XK-3RFGA"
    )
  }

  test("a new key is stored, recorded once and made active, and the output has no secret") {
    val f = new Fixture()
    val address = addressOf(seedOne, phrase)
    assertEquals(f.run(newKey()), Some(address))
    assertEquals(
      f.namesText,
      s"""{"kind":"create","address":"$address","name":"Main","network":"testnet","time":$stamp}""" + "\n"
    )
    assertEquals(f.activeText, s"""{"testnet":"$address"}""" + "\n")
    assertEquals(f.helperCalls.size, 1)
    val (args, stdin) = f.helperCalls.head
    assertEquals(
      args,
      Seq(python, helperScript, "--store", "agent", "--service", "desk-test")
    )
    assertEquals(new String(stdin, US_ASCII), hex(keyOf(seedOne, phrase)) + "\n")
    assert(f.output.contains(address))
    assert(f.output.contains(AgentKey.approvalName(address)))
    assert(f.output.contains(storedText(AgentKey.approvalName(address))))
    assertNoSecrets(f.output)
  }

  test("the seed goes to the clipboard once, as the seed text, right after it is made") {
    val f = new Fixture()
    f.run(newKey())
    val copy = f.clipboardCalls.head
    assertEquals(copy.kind, "copy")
    assertEquals(new String(copy.stdin, US_ASCII), textOne)
    assertEquals(f.kinds.count(_ == "copy"), 1)
    assertEquals(copy.derivations, 0)
  }

  test("the clipboard is cleared after the save confirmation and after the drill paste") {
    val f = new Fixture()
    f.run(newKey())
    assertEquals(f.kinds, Vector("copy", "clear", "clear"))
    assertEquals(f.clipboardCalls.map(_.derivations).toVector, Vector(0, 0, 1))
    assert(f.clipboardCalls.tail.forall(_.stdin.isEmpty))
  }

  test("each derivation is announced before it starts") {
    val f = new Fixture()
    f.run(newKey())
    assertEquals(f.derivations.map(_.announced).toVector, Vector(1, 2))
    assertEquals(occurrences(f.output, deriving), 2)
  }

  test(
    "the address and the approval name are shown before the drill and the screen clears for it"
  ) {
    val f = new Fixture()
    f.run(newKey())
    val address = addressOf(seedOne, phrase)
    val shown = frames(f.output)
    val first = shown.indexWhere(_.exists(_.contains("Address: " + address)))
    assert(first >= 0, "the address was never shown")
    assert(shown(first).exists(_.contains("Approval name: " + AgentKey.approvalName(address))))
    val drill = shown.filter(_.headOption.contains("Drill"))
    assert(drill.nonEmpty)
    drill.foreach(frame => assert(!frame.exists(_.contains(address)), "the drill kept the address"))
  }

  test("the seed field shows # in groups of five with the group number") {
    val f = new Fixture()
    f.run(
      script("n", enter, phrase, enter, phrase, enter, enter, groupsOne.take(3).mkString, interrupt)
    )
    val shown = lines(f.output)
    assert(shown.contains("  ##### ##### #####"))
    assert(shown.contains("group 4 of 13"))
    assert(shown.contains("group 1 of 13"))
    assertNoSecrets(f.output)
  }

  test("passphrase fields show one star for each character") {
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, "pass", interrupt))
    val shown = lines(f.output)
    assert(shown.contains("Passphrase: ********"))
    assert(shown.contains("Again: ****"))
    assertNoSecrets(f.output)
  }

  test("a typo in group 4 names group 4 and drops it") {
    val f = new Fixture()
    val typed = groupsOne.take(3).mkString + flip(groupsOne(3)) + groupsOne.drop(3).mkString("-")
    f.run(
      script(
        "n",
        enter,
        phrase,
        enter,
        phrase,
        enter,
        enter,
        typed,
        phrase,
        enter,
        "Main",
        enter,
        enter,
        "q"
      )
    )
    assert(lines(f.output).contains("Group 4 has a typo. Type it again."))
    assertEquals(f.helperCalls.size, 1)
    val noted = frames(f.output).filter(_.contains("Group 4 has a typo. Type it again."))
    assert(noted.nonEmpty)
    noted.foreach { frame =>
      assert(frame.contains("  ##### ##### #####"), "the bad group stayed in the field")
      assert(frame.contains("group 4 of 13"))
    }
  }

  test("a character outside the seed alphabet is refused and not counted") {
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, "U", interrupt))
    val shown = lines(f.output)
    assert(shown.contains("That character is not used in a seed."))
    assert(shown.contains("group 1 of 13"))
    assert(!shown.exists(_.contains("#")))
  }

  test("a paste fills the seed field and the clipboard is cleared after it") {
    val f = new Fixture()
    val address = addressOf(seedOne, phrase)
    assertEquals(f.run(newKey(seed = textOne.toLowerCase)), Some(address))
    assertEquals(f.kinds, Vector("copy", "clear", "clear"))
  }

  // The seed text with spaces after it, which the seed field skips, to reach a length.
  private def padded(length: Int): String = textOne + " " * (length - textOne.length)

  test("a paste of exactly 128 bytes is processed and the screen moves on to the passphrase") {
    assertEquals(padded(128).length, 128)
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, paste(padded(128)), interrupt))
    val after = frames(f.output).last
    assertEquals(after.head, "Drill")
    assert(after.contains("Type the passphrase."), "the seed field did not take the paste")
    assert(!after.exists(_.startsWith("The paste was refused")))
    assertEquals(f.kinds, Vector("copy", "clear", "clear"))
  }

  test("a paste of 129 bytes is refused whole and the seed field stays empty") {
    assertEquals(padded(129).length, 129)
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, paste(padded(129)), interrupt))
    val after = frames(f.output).last
    assert(
      after.exists(_.startsWith("The paste was refused. It was longer than 128 bytes")),
      "the paste was not refused"
    )
    assert(after.contains("group 1 of 13"))
    assert(!after.contains("Type the passphrase."))
    assert(!after.exists(_.contains("#")))
    assertEquals(f.kinds, Vector("copy", "clear", "clear"))
  }

  test("a paste of 129 bytes made of dashes and seed characters is refused too") {
    val dashed = groupsOne.mkString("-") + "-" * (129 - textOne.length)
    assertEquals(dashed.length, 129)
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, paste(dashed), interrupt))
    assert(frames(f.output).last.exists(_.startsWith("The paste was refused.")))
  }

  test(
    "a paste with a bad group, a newline or an escape byte is refused whole and the field stays empty"
  ) {
    val badGroup = (groupsOne.take(4) :+ flip(groupsOne(4))).mkString("-")
    val bad = Vector(
      badGroup,
      textOne + "\n",
      "\n" + textOne,
      textOne.take(20) + "\u0007" + textOne.drop(20),
      textOne.take(30) + esc + "x" + textOne.drop(30),
      textOne + "\t"
    )
    val f = new Fixture()
    val pastes = bad.map(paste).mkString
    // After every refused paste the typed seed still completes the field, so nothing was kept.
    val flow = script(
      "n",
      enter,
      phrase,
      enter,
      phrase,
      enter,
      enter,
      pastes,
      textOne,
      phrase,
      enter,
      "Main",
      enter,
      enter,
      "q"
    )
    assertEquals(f.run(flow), Some(addressOf(seedOne, phrase)))
    assertEquals(f.helperCalls.size, 1)
    assertEquals(f.kinds, Vector("copy", "clear") ++ Vector.fill(bad.size)("clear"))
    assertNoSecrets(f.output)
  }

  test("a refused paste says so") {
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, paste(textOne + "\n"), interrupt))
    assert(lines(f.output).exists(_.startsWith("The paste was refused.")))
  }

  test("an empty paste keeps what was typed") {
    val f = new Fixture()
    val typed = groupsOne.take(2).mkString
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, typed, paste(""), interrupt))
    val last = frames(f.output).last
    assert(last.contains("  ##### #####"))
    assert(last.contains("group 3 of 13"))
  }

  test("a paste into a passphrase field or the name field changes nothing") {
    val f = new Fixture()
    val flow = script(
      "n",
      enter,
      paste("zzzzzzzzzz"),
      phrase,
      enter,
      paste("yyyyyyyyyy"),
      phrase,
      enter,
      enter,
      paste(textOne),
      phrase,
      enter,
      paste("Evil"),
      "Main",
      enter,
      enter,
      "q"
    )
    assertEquals(f.run(flow), Some(addressOf(seedOne, phrase)))
    assertEquals(f.derivations.head.passValue.toSeq, phrase.getBytes(US_ASCII).toSeq)
    assert(f.nameLines.head.contains(""""name":"Main""""))
    assert(!f.output.contains("zzzzzzzzzz"))
    assert(!f.output.contains("yyyyyyyyyy"))
    assert(!f.output.contains("Evil"))
  }

  test("a paste outside a seed field is wiped at once, before the screen closes") {
    val f = new Fixture()
    val parts = Vector(
      script("n", enter),
      script(paste("zzzzzzzzzz")), // the first passphrase field
      script(phrase, enter),
      script(paste("yyyyyyyyyy")), // the second passphrase field
      script(phrase, enter, enter),
      script(paste(textOne)), // the seed field takes this one
      script(phrase, enter),
      script(paste("Evil")), // the name field
      script("Main", enter, enter, "q")
    )
    val ends = parts.scanLeft(0)(_ + _.length).tail
    val outside = Vector(1, 3, 7).map(ends(_))
    var buffer = Array.emptyByteArray
    val seen = scala.collection.mutable.Map.empty[Int, Array[Byte]]
    val input =
      new Spy(parts.reduce(_ ++ _), at => if buffer.nonEmpty then seen(at) = buffer.clone())
    val result = KeyScreen.run(f.context, input, f.screen, handed => buffer = handed)
    assertEquals(result, Some(addressOf(seedOne, phrase)))
    outside.foreach { end =>
      // Six bytes before the end the paste is in the buffer, and on the next read it is gone.
      assert(seen(end - 6).exists(_ != 0), "the paste never reached the buffer")
      assert(seen(end).forall(_ == 0), "a discarded paste stayed in its buffer")
    }
  }

  test("a paste in the list or on a confirm screen is discarded too") {
    val f = new Fixture()
    f.run(script(paste("n"), paste("q"), "n", paste(enter), enter, interrupt))
    // The pasted n never started a flow and the pasted Enter never confirmed one.
    assertEquals(f.kinds.count(_ == "copy"), 1)
    // Each discarded paste clears the clipboard, so two come before the copy and one after it.
    assertEquals(f.kinds, Vector("clear", "clear", "copy", "clear", "clear"))
  }

  test("a passphrase mismatch asks again") {
    val f = new Fixture()
    f.run(
      script("n", enter, phrase, enter, "pass 124", enter, phrase, enter, phrase, enter, interrupt)
    )
    assert(lines(f.output).exists(_.contains("The two did not match. Start again.")))
    assertEquals(f.derivations.size, 1)
    assertEquals(f.derivations.head.passValue.toSeq, phrase.getBytes(US_ASCII).toSeq)
  }

  test("a passphrase of 7 characters is refused and 8 are accepted") {
    val f = new Fixture()
    f.run(script("n", enter, "pass 12", enter, "3", enter, phrase, enter, interrupt))
    assert(lines(f.output).exists(_.contains("Use 8 to 128 characters.")))
    assertEquals(f.derivations.head.passValue.toSeq, phrase.getBytes(US_ASCII).toSeq)
  }

  test("a passphrase stops growing at 128 characters") {
    val f = new Fixture()
    val long = "a" * 130
    f.run(script("n", enter, long, enter, long, enter, interrupt))
    assertEquals(f.derivations.head.passValue.length, 128)
    assert(lines(f.output).exists(_.contains("At most 128 characters.")))
  }

  test("backspace and enter work in both byte forms and unknown escape sequences are ignored") {
    val f = new Fixture()
    val back = 127.toChar.toString
    val backspace = 8.toChar.toString
    val name = "Mx" + back + "yz" + backspace + backspace + "ain" + esc + "[3~" + esc + "[1;5C"
    val flow = script(
      "n",
      "\n",
      phrase,
      "\n",
      phrase,
      "\n",
      "\n",
      paste(textOne),
      phrase,
      "\n",
      name,
      "\n",
      "\n",
      "q"
    )
    assertEquals(f.run(flow), Some(addressOf(seedOne, phrase)))
    assert(f.nameLines.head.contains(""""name":"Main""""))
  }

  test("a drill mismatch offers retry and start over") {
    val f = new Fixture(seeds = Vector(seedOne, seedTwo))
    val retry = script(
      "n",
      enter,
      phrase,
      enter,
      phrase,
      enter,
      enter,
      paste(textOne),
      "pass 124",
      enter,
      "r",
      paste(textOne),
      phrase,
      enter,
      "Main",
      enter,
      enter,
      "q"
    )
    assertEquals(f.run(retry), Some(addressOf(seedOne, phrase)))
    assert(lines(f.output).contains("The drill did not match this key."))
    assert(
      lines(f.output).contains(
        "r tries the drill again. s starts over with a new seed. Esc leaves."
      )
    )
    assertEquals(f.derivations.size, 3)
    assertEquals(f.helperCalls.size, 1)
    assertEquals(f.random.served, 1)
  }

  test("start over after a drill mismatch makes a new seed and copies it") {
    val f = new Fixture(seeds = Vector(seedOne, seedTwo))
    val flow = script(
      "n",
      enter,
      phrase,
      enter,
      phrase,
      enter,
      enter,
      paste(textOne),
      "pass 124",
      enter,
      "s",
      enter,
      phrase,
      enter,
      phrase,
      enter,
      enter,
      paste(textTwo),
      phrase,
      enter,
      "Second",
      enter,
      enter,
      "q"
    )
    assertEquals(f.run(flow), Some(addressOf(seedTwo, phrase)))
    assertEquals(f.random.served, 2)
    val copied = f.clipboardCalls.filter(_.kind == "copy").map(c => new String(c.stdin, US_ASCII))
    assertEquals(copied.toVector, Vector(textOne, textTwo))
    assertEquals(f.nameLines.size, 1)
    assertZero(f.random.handed.head)
  }

  test("a drill with a passphrase that is too short counts as a mismatch") {
    val f = new Fixture()
    f.run(
      script(
        "n",
        enter,
        phrase,
        enter,
        phrase,
        enter,
        enter,
        paste(textOne),
        "short",
        enter,
        interrupt
      )
    )
    assert(lines(f.output).contains("The drill did not match this key."))
    assertEquals(f.derivations.size, 1)
  }

  test("a copy failure wipes the seed and stops before any passphrase is asked") {
    val f = new Fixture()
    f.copyExit = 1
    assertEquals(f.run(script("n", "q")), None)
    assert(lines(f.output).exists(_.contains("Clipboard copy failed")))
    assert(!lines(f.output).exists(_.startsWith("Passphrase:")))
    assertEquals(f.derivations.size, 0)
    assertZero(f.random.handed.head)
    assertEquals(f.kinds, Vector("copy", "clear"))
  }

  test("a clipboard clear failure after the save confirmation stops before any passphrase") {
    val f = new Fixture()
    f.clearExit = 1
    assertEquals(f.run(script("n", enter, "q")), None)
    assert(lines(f.output).exists(_.contains("Clipboard clear failed")))
    assert(!lines(f.output).exists(_.startsWith("Passphrase:")))
    assertEquals(f.derivations.size, 0)
    assertZero(f.random.handed.head)
    // The seed text may still be there, so leaving tries the clear once more.
    assertEquals(f.kinds, Vector("copy", "clear", "clear"))
  }

  test("escape leaves a flow for the list and clears a seed still on the clipboard") {
    val f = new Fixture()
    assertEquals(f.run(script("n", esc, "q")), None)
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
    val g = new Fixture()
    assertEquals(g.run(script("n", enter, phrase, esc, "q")), None)
    assertEquals(g.kinds, Vector("copy", "clear"))
    assertEquals(g.derivations.size, 0)
  }

  // Every way out

  test("Ctrl-C with the seed on the clipboard clears it and wipes the seed") {
    val f = new Fixture()
    assertEquals(f.run(script("n", interrupt)), None)
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
  }

  test("end of input with the seed on the clipboard clears it and wipes the seed") {
    val f = new Fixture()
    assertEquals(f.run(script("n")), None)
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
  }

  test("an exception from the input with the seed on the clipboard clears it and wipes the seed") {
    val f = new Fixture()
    val failing = new Scripted(script("n"), () => throw new IOException("input broke"))
    intercept[IOException](KeyScreen.run(f.context, failing, f.screen))
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
  }

  test("a failing screen write with the seed on the clipboard clears it and wipes the seed") {
    val f = new Fixture()
    val broken = new PrintStream(new Failing(() => f.kinds.nonEmpty), true)
    intercept[IOException](KeyScreen.run(f.context, new ByteArrayInputStream(script("n")), broken))
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
  }

  test("Ctrl-C in the drill wipes the passphrase, the seed and the key") {
    val f = new Fixture()
    val flow = script("n", enter, phrase, enter, phrase, enter, enter, interrupt)
    assertEquals(f.run(flow), None)
    val d = f.derivations.head
    assertZero(d.seed, d.pass, d.key)
    assertEquals(f.kinds, Vector("copy", "clear"))
  }

  test("an exception in the derivation wipes the seed and the passphrase") {
    val f = new Fixture(derive = (_, _, _) => throw new IllegalStateException("derive broke"))
    intercept[IllegalStateException](f.run(script("n", enter, phrase, enter, phrase, enter)))
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
  }

  test("an exception in the derivation wipes the passphrase buffer it was given") {
    var seen = Array.emptyByteArray
    val f = new Fixture(derive = (_, pass, _) =>
      seen = pass
      throw new IllegalStateException("derive broke"))
    intercept[IllegalStateException](f.run(script("n", enter, phrase, enter, phrase, enter)))
    assertEquals(seen.length, 128)
    assertZero(seen)
  }

  test("Ctrl-C in the list makes no clipboard call") {
    val f = new Fixture()
    assertEquals(f.run(script(interrupt)), None)
    assertEquals(f.kinds, Vector.empty[String])
  }

  // The terminal

  private def onTerminal(f: Fixture, input: InputStream) =
    val events = ArrayBuffer.empty[String]
    val hooks = ArrayBuffer.empty[Thread]
    val removed = ArrayBuffer.empty[Thread]
    def run(): Option[String] = KeyScreen.runOnTerminal(
      f.context,
      enter = () =>
        events += "enter"
        "saved-settings"
      ,
      leave = saved => events += "leave:" + saved,
      addHook = hook => hooks += hook,
      removeHook = hook => removed += hook,
      input = input,
      output = f.screen
    )
    (events, hooks, removed, () => run())

  test("the terminal is restored on a normal leave and the shutdown hook is removed") {
    val f = new Fixture()
    val (events, hooks, removed, run) = onTerminal(f, new ByteArrayInputStream(script("q")))
    assertEquals(run(), None)
    assertEquals(events.toVector, Vector("enter", "leave:saved-settings"))
    assertEquals(hooks.size, 1)
    assertEquals(removed.toVector, hooks.toVector)
  }

  test("the terminal is restored and the clipboard cleared after Ctrl-C") {
    val f = new Fixture()
    val (events, _, _, run) = onTerminal(f, new ByteArrayInputStream(script("n", interrupt)))
    assertEquals(run(), None)
    assertEquals(events.toVector, Vector("enter", "leave:saved-settings"))
    assertEquals(f.kinds, Vector("copy", "clear"))
  }

  test("the terminal is restored and the clipboard cleared after end of input") {
    val f = new Fixture()
    val (events, _, _, run) = onTerminal(f, new ByteArrayInputStream(script("n")))
    assertEquals(run(), None)
    assertEquals(events.toVector, Vector("enter", "leave:saved-settings"))
    assertEquals(f.kinds, Vector("copy", "clear"))
  }

  test("the terminal is restored and the clipboard cleared after an exception") {
    val f = new Fixture()
    val input = new Scripted(script("n"), () => throw new IOException("input broke"))
    val (events, hooks, removed, run) = onTerminal(f, input)
    intercept[IOException](run())
    assertEquals(events.toVector, Vector("enter", "leave:saved-settings"))
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertEquals(removed.toVector, hooks.toVector)
  }

  test("a kill while the seed is on the clipboard runs the hook that clears and restores") {
    val f = new Fixture()
    var hooks = Vector.empty[Thread]
    val events = ArrayBuffer.empty[String]
    val input = new Scripted(script("n"), () => { hooks.foreach(_.run()); -1 })
    val result = KeyScreen.runOnTerminal(
      f.context,
      enter = () => "saved-settings",
      leave = saved => events += "leave:" + saved,
      addHook = hook => hooks = hooks :+ hook,
      removeHook = _ => (),
      input = input,
      output = f.screen
    )
    assertEquals(result, None)
    assertEquals(hooks.size, 1)
    assert(events.contains("leave:saved-settings"))
    assertEquals(f.kinds, Vector("copy", "clear"))
    assertZero(f.random.handed.head)
  }

  test("a terminal that cannot be entered is reset and no hook stays") {
    val f = new Fixture()
    val events = ArrayBuffer.empty[String]
    val hooks = ArrayBuffer.empty[Thread]
    intercept[IllegalStateException](
      KeyScreen.runOnTerminal(
        f.context,
        enter = () => throw new IllegalStateException("no terminal"),
        leave = saved => events += "leave:" + saved,
        addHook = hook => hooks += hook,
        removeHook = _ => (),
        input = new ByteArrayInputStream(Array.emptyByteArray),
        output = f.screen
      )
    )
    assertEquals(events.toVector, Vector("leave:"))
    assertEquals(hooks.size, 0)
    assertEquals(f.kinds, Vector.empty[String])
  }

  // Storing

  test("an exists reply from the helper counts as stored") {
    val f = new Fixture()
    f.helperMode = "exists"
    val address = addressOf(seedOne, phrase)
    assertEquals(f.run(newKey()), Some(address))
    assert(f.output.contains(storedText(AgentKey.approvalName(address))))
    assertEquals(f.nameLines.size, 1)
    assertEquals(f.activeText, s"""{"testnet":"$address"}""" + "\n")
  }

  test("a failed store shows the reason and keeps the record") {
    val f = new Fixture()
    f.helperMode = "failed"
    val address = addressOf(seedOne, phrase)
    assertEquals(f.run(newKey()), None)
    assert(lines(f.output).exists(_.contains("key store helper failed")))
    assert(!f.output.contains("Stored. Approval at the exchange"))
    assertEquals(f.nameLines.size, 1)
    assert(f.nameLines.head.contains(address))
    assertEquals(f.activeText, "")
    assert(lines(f.output).exists(l => l.contains("Main") && l.contains("not in Keychain")))
    assertNoSecrets(f.output)
  }

  test("the new key becomes active through the switch check and a refusal keeps the old key") {
    val f = new Fixture()
    val old = "0x" + "a" * 40
    f.name(old, "Old")
    f.present += old
    f.activate(old)
    val journal = OrderJournal.open(f.state, "testnet-" + old.drop(2))
    val address = addressOf(seedOne, phrase)
    try assertEquals(f.run(newKey()), Some(old))
    finally journal.close()
    assert(lines(f.output).exists(_.contains("Another Desk is using the current key")))
    assert(f.output.contains(storedText(AgentKey.approvalName(address))))
    assertEquals(f.activeText, s"""{"testnet":"$old"}""" + "\n")
    assertEquals(f.nameLines.size, 2)
  }

  test("the new key replaces an old active key when the switch is allowed") {
    val f = new Fixture()
    val old = "0x" + "a" * 40
    f.name(old, "Old")
    f.present += old
    f.activate(old)
    val address = addressOf(seedOne, phrase)
    assertEquals(f.run(newKey()), Some(address))
    assertEquals(f.activeText, s"""{"testnet":"$address"}""" + "\n")
  }

  test("an empty name or one with a space at the edge is refused and then corrected") {
    val f = new Fixture()
    val back = 127.toChar.toString
    val flow = script(
      "n",
      enter,
      phrase,
      enter,
      phrase,
      enter,
      enter,
      paste(textOne),
      phrase,
      enter,
      enter,
      " ",
      enter,
      back,
      "Main ",
      enter,
      back,
      enter,
      enter,
      "q"
    )
    assertEquals(f.run(flow), Some(addressOf(seedOne, phrase)))
    val notes =
      lines(f.output).count(_.contains("Use 1 to 32 characters with no space at either end."))
    assertEquals(notes, 3)
    assertEquals(f.nameLines.size, 1)
    assert(f.nameLines.head.contains(""""name":"Main""""))
  }

  test("a name stops growing at 32 characters") {
    val f = new Fixture()
    val flow = script(
      "n",
      enter,
      phrase,
      enter,
      phrase,
      enter,
      enter,
      paste(textOne),
      phrase,
      enter,
      "n" * 40,
      enter,
      enter,
      "q"
    )
    f.run(flow)
    assert(f.nameLines.head.contains(""""name":"""" + "n" * 32 + """""""))
  }

  // Restore

  test("a restore of a key that is already named stores it with no second record") {
    val f = new Fixture()
    val address = addressOf(seedOne, phrase)
    f.name(address, "Old")
    val before = f.namesText
    val flow = script("s", paste(textOne), phrase, enter, phrase, enter, enter, enter, "q")
    assertEquals(f.run(flow), Some(address))
    assertEquals(f.namesText, before)
    assertEquals(f.helperCalls.size, 1)
    assertEquals(new String(f.helperCalls.head._2, US_ASCII), hex(keyOf(seedOne, phrase)) + "\n")
    assertEquals(f.activeText, s"""{"testnet":"$address"}""" + "\n")
    assert(f.output.contains(storedText(AgentKey.approvalName(address))))
    assertEquals(f.kinds, Vector("clear"))
    assertNoSecrets(f.output)
  }

  test("a restore of a new key asks the name and writes one create record") {
    val f = new Fixture()
    val address = addressOf(seedOne, phrase)
    val flow = script("s", textOne, phrase, enter, phrase, enter, enter, "Back", enter, enter, "q")
    assertEquals(f.run(flow), Some(address))
    assertEquals(f.nameLines.size, 1)
    assert(f.nameLines.head.contains(""""name":"Back""""))
    assertEquals(f.derivations.size, 1)
    assertEquals(f.kinds, Vector.empty[String])
  }

  test("a restore shows the address before it stores and escape stores nothing") {
    val f = new Fixture()
    val address = addressOf(seedOne, phrase)
    f.run(script("s", textOne, phrase, enter, phrase, enter, esc, "q"))
    assert(lines(f.output).exists(_.contains("Address: " + address)))
    assertEquals(f.helperCalls.size, 0)
    assertEquals(f.nameLines.size, 0)
    assertZero(f.derivations.head.seed, f.derivations.head.pass, f.derivations.head.key)
  }

  test("a restore of a key named on the other network is refused before it stores") {
    val f = new Fixture()
    val address = addressOf(seedOne, phrase)
    f.name(address, "Old", on = "mainnet")
    f.run(script("s", textOne, phrase, enter, phrase, enter, "q"))
    assert(lines(f.output).exists(_.contains("That key is named for mainnet.")))
    assertEquals(f.helperCalls.size, 0)
  }

  test("a restore passphrase that is too short is refused and asked again") {
    val f = new Fixture()
    f.run(script("s", textOne, "short", enter, "123", enter, "short123", enter, interrupt))
    assert(lines(f.output).exists(_.contains("Use 8 to 128 characters.")))
    assertEquals(f.derivations.head.passValue.toSeq, "short123".getBytes(US_ASCII).toSeq)
  }

  // The list

  private def two(f: Fixture): (String, String) =
    val a = "0x" + "a" * 40
    val b = "0x" + "b" * 40
    f.name(a, "Main")
    f.name(b, "Spare")
    f.present += a
    f.present += b
    f.activate(a)
    (a, b)

  test("the list shows name, address, network, approval name and the active mark") {
    val f = new Fixture()
    val (a, b) = two(f)
    f.present -= b
    f.run(script("q"))
    val shown = frames(f.output).last
    assert(shown.contains("> Main  active"))
    assert(shown.contains(s"    $a  testnet  ${AgentKey.approvalName(a)}"))
    assert(shown.contains("  Spare  not in Keychain"))
    assert(shown.contains(s"    $b  testnet  ${AgentKey.approvalName(b)}"))
  }

  test("an empty list says so and offers new and restore") {
    val f = new Fixture()
    assertEquals(f.run(script("q")), None)
    assert(lines(f.output).contains("No keys yet."))
    assert(lines(f.output).exists(l => l.contains("n makes a new key") && l.contains("q leaves")))
  }

  test("up and down move the mark in both arrow forms") {
    val f = new Fixture()
    two(f)
    f.run(script(down, up, down, esc + "OA", esc + "OB", "q"))
    val marks = frames(f.output).map(_.filter(_.startsWith("> ")).headOption)
    assertEquals(
      marks.flatten,
      Vector("> Main  active", "> Spare", "> Main  active", "> Spare", "> Main  active", "> Spare")
    )
  }

  test("enter selects the key under the mark through the switch check") {
    val f = new Fixture()
    val (_, b) = two(f)
    assertEquals(f.run(script(down, enter, "q")), Some(b))
    assertEquals(f.activeText, s"""{"testnet":"$b"}""" + "\n")
    assert(lines(f.output).contains("Active key: Spare"))
  }

  test("a refused switch shows its reason and keeps the active key") {
    val f = new Fixture()
    val (a, _) = two(f)
    val journal = OrderJournal.open(f.state, "testnet-" + a.drop(2))
    try assertEquals(f.run(script(down, enter, "q")), Some(a))
    finally journal.close()
    assert(lines(f.output).exists(_.contains("Another Desk is using the current key")))
    assertEquals(f.activeText, s"""{"testnet":"$a"}""" + "\n")
  }

  test("a key without a Keychain item cannot be selected") {
    val f = new Fixture()
    val (a, b) = two(f)
    f.present -= b
    assertEquals(f.run(script(down, enter, "q")), Some(a))
    assert(lines(f.output).exists(_.contains("That key is not in the Keychain.")))
    assertEquals(f.activeText, s"""{"testnet":"$a"}""" + "\n")
    assert(!Files.exists(f.state), "the switch check ran for a key that cannot be chosen")
  }

  test("a key named for the other network cannot be selected") {
    val f = new Fixture()
    val (a, _) = two(f)
    val c = "0x" + "c" * 40
    f.name(c, "Other", on = "mainnet")
    f.present += c
    assertEquals(f.run(script(down, down, enter, "q")), Some(a))
    assert(lines(f.output).exists(_.contains("That key is for mainnet. Desk is set to testnet.")))
    assertEquals(f.activeText, s"""{"testnet":"$a"}""" + "\n")
  }

  test("the active key is selected again with a plain notice") {
    val f = new Fixture()
    val (a, _) = two(f)
    assertEquals(f.run(script(enter, "q")), Some(a))
    assert(lines(f.output).contains("That key is already active."))
  }

  test("rename appends a rename record and shows the new name") {
    val f = new Fixture()
    val (a, _) = two(f)
    assertEquals(f.run(script("r", paste("Evil"), "Daily", enter, "q")), Some(a))
    assertEquals(
      f.nameLines.last,
      s"""{"kind":"rename","address":"$a","name":"Daily","time":$stamp}"""
    )
    assertEquals(f.nameLines.size, 3)
    assert(frames(f.output).last.contains("> Daily  active"))
    assert(!f.output.contains("Evil"))
  }

  test("a rename with a bad name writes nothing and escape leaves the prompt") {
    val f = new Fixture()
    two(f)
    f.run(script("r", " ", enter, esc, "q"))
    assert(
      lines(f.output).exists(_.contains("Use 1 to 32 characters with no space at either end."))
    )
    assertEquals(f.nameLines.size, 2)
  }

  test("a bad key names file is shown with its line and only q leaves") {
    val f = new Fixture()
    Files.createDirectories(f.names.getParent)
    Files.writeString(f.names, "garbage\n")
    Platform.setMode(f.names, "rw-------")
    assertEquals(f.run(script("n", "s", "r", enter, "q")), None)
    assert(
      lines(f.output).exists(l =>
        l.contains("key names file is invalid at line 1") && l.contains("key-names.jsonl")
      )
    )
    assert(lines(f.output).exists(_.contains("Fix or move the file, then open settings again")))
    assertEquals(f.kinds, Vector.empty[String])
    assertEquals(Files.readString(f.names), "garbage\n")
  }

  test("q returns the active address only when the key is usable") {
    val f = new Fixture()
    val (a, _) = two(f)
    f.present -= a
    assertEquals(f.run(script("q")), None)
    f.present += a
    assertEquals(f.run(script("q")), Some(a))
  }

  // The real derivation, with the first pinned fixture vector

  private def realVector =
    val fixture =
      ujson.read(Files.readString(java.nio.file.Paths.get("fixtures", "agent-key-vectors.json")))
    fixture("vectors").arr.head

  test("a restore with the real derivation stores the first pinned vector key") {
    val vector = realVector
    val f = new Fixture(derive = AgentKey.derive)
    val expected = vector("address").str
    val pass = vector("passphrase").str
    val flow = script("s", vector("seedText").str, pass, enter, pass, enter, enter)
    assertEquals(f.run(flow ++ script("Back", enter, enter, "q")), Some(expected))
    assertEquals(new String(f.helperCalls.head._2, US_ASCII), vector("key").str + "\n")
    assert(f.nameLines.head.contains(expected))
    assertNoSecrets(f.output, vector("seedText").str, vector("seed").str, vector("key").str)
  }

  test(
    "the full new key flow with the real derivation stores the first pinned vector key"
      .tag(new munit.Tag("slow"))
  ) {
    val vector = realVector
    val seed = java.util.HexFormat.of().parseHex(vector("seed").str)
    val f = new Fixture(seeds = Vector(seed), derive = AgentKey.derive)
    val expected = vector("address").str
    val flow = newKey(seed = vector("seedText").str, pass = vector("passphrase").str)
    assertEquals(f.run(flow), Some(expected))
    assertEquals(new String(f.helperCalls.head._2, US_ASCII), vector("key").str + "\n")
    assert(f.nameLines.head.contains(expected))
    assertEquals(f.derivations.size, 2)
    assertNoSecrets(f.output, vector("seedText").str, vector("seed").str, vector("key").str)
  }

  // Release review fixes

  private val folderRule = "The folder for the key files must be private. Run chmod 700 on it."
  private val helperRule = "The key helper is not installed. Run make install."
  private val unknownAddress =
    "This address is not in your key names. Check it against the address you saved before you store it."
  private val asciiRule = "Only printable ASCII is accepted."
  private val dormant = "Another Desk is using the current key"

  // The key files folder with group and other access, which the screen must refuse.
  private def openFolder(f: Fixture): Path =
    val folder = f.names.getParent
    Files.createDirectories(folder)
    Platform.setMode(folder, "rwxr-xr-x")
    folder

  // Runs the screen and makes one change to the files when the read at that index is asked for.
  private def changing(f: Fixture, input: Array[Byte], at: Int)(change: => Unit): Option[String] =
    var done = false
    val spy = new Spy(
      input,
      index =>
        if index == at && !done then
          done = true
          change
    )
    KeyScreen.run(f.context, spy, f.screen)

  private def utf8(text: String): String = new String(text.getBytes(UTF_8), ISO_8859_1)

  // Checks before any secret is made

  test("a new key stops before any seed is made when the key files folder is not private") {
    val f = new Fixture()
    val folder = openFolder(f)
    assertEquals(f.run(script("n", "q")), None)
    assert(lines(f.output).contains(folderRule))
    assertEquals(f.random.served, 0)
    assertEquals(f.kinds, Vector.empty[String])
    assert(Platform.hasMode(folder, "rwxr-xr-x"))
  }

  test("a new key makes a missing key files folder private before it makes a seed") {
    val f = new Fixture()
    assert(!Files.exists(f.names.getParent))
    f.run(script("n", interrupt))
    assert(Platform.hasMode(f.names.getParent, "rwx------"))
    assertEquals(f.random.served, 1)
  }

  test("a new key stops before any seed is made when the key helper is not installed") {
    val f = new Fixture(env = Map("DESK_PYTHON" -> "python3", "DESK_KEY_HELPER" -> "helper.py"))
    assertEquals(f.run(script("n", "q")), None)
    assert(lines(f.output).contains(helperRule))
    assertEquals(f.random.served, 0)
    assertEquals(f.kinds, Vector.empty[String])
  }

  test("a restore stops before the seed is asked when the key files folder is not private") {
    val f = new Fixture()
    openFolder(f)
    assertEquals(f.run(script("s", "q")), None)
    assert(lines(f.output).contains(folderRule))
    assert(!frames(f.output).exists(_.headOption.contains("Restore a key")))
    assertEquals(f.derivations.size, 0)
  }

  test("a restore stops before the seed is asked when the key helper is not installed") {
    val f = new Fixture(env = Map.empty)
    assertEquals(f.run(script("s", "q")), None)
    assert(lines(f.output).contains(helperRule))
    assert(!frames(f.output).exists(_.headOption.contains("Restore a key")))
    assertEquals(f.derivations.size, 0)
  }

  test("the folder is checked before the helper") {
    val f = new Fixture(env = Map.empty)
    openFolder(f)
    f.run(script("n", "q"))
    assert(lines(f.output).contains(folderRule))
    assert(!f.output.contains(helperRule))
  }

  // Storage errors

  test("a failed write shows the fixed storage rule and nothing else from the error") {
    val f = new Fixture()
    two(f)
    val folder = f.names.getParent
    changing(f, script("r", "Daily", enter, "q"), 1)(
      Platform.setMode(folder, "rwxr-xr-x")
    )
    val said = "The key files could not be updated (local storage must be private)"
    assert(lines(f.output).contains(said))
    assertEquals(f.nameLines.size, 2)
  }

  test("a symbolic link as the key files folder shows its fixed storage rule") {
    assume(Platform.links, "symbolic links need developer mode on Windows")
    val f = new Fixture()
    two(f)
    val folder = f.names.getParent
    val moved = folder.resolveSibling("moved")
    changing(f, script("r", "Daily", enter, "q"), 1) {
      Files.move(folder, moved)
      Files.createSymbolicLink(folder, moved)
    }
    val said = "The key files could not be updated (local storage cannot be a symbolic link)"
    assert(lines(f.output).contains(said))
  }

  test("an error that is not a storage rule shows only its class name") {
    val f = new Fixture()
    two(f)
    changing(f, script("r", "Daily", enter, "q"), 1)(Files.writeString(f.names, ""))
    assert(
      lines(f.output).contains("The key files could not be updated (IllegalArgumentException)")
    )
    assert(!f.output.contains("key is not named"))
  }

  // Restore asks the passphrase twice

  test("a restore asks the passphrase twice and asks again when the two differ") {
    val f = new Fixture()
    f.run(
      script(
        "s",
        textOne,
        phrase,
        enter,
        "pass 124",
        enter,
        phrase,
        enter,
        phrase,
        enter,
        interrupt
      )
    )
    assert(lines(f.output).contains("Again: " + "*" * 8))
    assert(lines(f.output).exists(_.contains("The two did not match. Start again.")))
    assertEquals(f.derivations.size, 1)
    assertEquals(f.derivations.head.passValue.toSeq, phrase.getBytes(US_ASCII).toSeq)
  }

  test("a restore of an address that is not in key names warns before the store prompt") {
    val f = new Fixture()
    f.run(script("s", textOne, phrase, enter, phrase, enter, esc, "q"))
    val frame = frames(f.output).find(_.exists(_.startsWith("Address: "))).get
    val warning = frame.indexOf(unknownAddress)
    assert(warning >= 0, "the warning is missing")
    assert(frame.indexOf("Press Enter to store this key. Esc cancels.") > warning)
  }

  test("a restore of an address that is already in key names has no warning") {
    val f = new Fixture()
    f.name(addressOf(seedOne, phrase), "Old")
    f.run(script("s", textOne, phrase, enter, phrase, enter, esc, "q"))
    assert(lines(f.output).contains("It is already named Old."))
    assert(!f.output.contains(unknownAddress))
  }

  // The clipboard after a paste anywhere

  test("a paste outside a seed field clears the clipboard like a paste into a seed field") {
    val f = new Fixture()
    val flow = script(
      "n",
      enter,
      paste("zzzzzzzzzz"),
      phrase,
      enter,
      paste("yyyyyyyyyy"),
      phrase,
      enter,
      enter,
      paste(textOne),
      phrase,
      enter,
      paste("Evil"),
      "Main",
      enter,
      enter,
      "q"
    )
    assertEquals(f.run(flow), Some(addressOf(seedOne, phrase)))
    // One clear after the save confirmation and one after each of the four pastes.
    assertEquals(f.kinds, Vector("copy") ++ Vector.fill(5)("clear"))
    assert(!f.output.contains("zzzzzzzzzz"))
  }

  test("a refused paste outside a seed field clears the clipboard too") {
    val f = new Fixture()
    f.run(script("s", textOne, paste("zz\u0007zz"), paste("x" * 129), paste("fine"), interrupt))
    assertEquals(f.kinds, Vector("clear", "clear", "clear"))
    assert(!f.output.contains("fine"))
  }

  test("a clipboard clear that fails outside a seed field is told on the next screen") {
    val f = new Fixture()
    f.clearExit = 1
    f.run(script("s", textOne, paste("zzzzzzzzzz"), interrupt))
    assertEquals(f.kinds, Vector("clear"))
    assert(lines(f.output).exists(_.contains("Clipboard clear failed. Clear it yourself.")))
    assert(!f.output.contains("zzzzzzzzzz"))
  }

  // Printable ASCII only

  test("a byte of 128 or more in a passphrase field is refused and the field is unchanged") {
    val f = new Fixture()
    // A two byte letter, a three byte sign and one lone byte of 255.
    val typed = "pass " + utf8("\u00e9") + "1" + utf8("\u20ac") + "2" + "\u00ff" + "3"
    f.run(script("n", enter, typed, enter, phrase, enter, interrupt))
    assertEquals(f.derivations.head.passValue.toSeq, phrase.getBytes(US_ASCII).toSeq)
    assert(lines(f.output).contains(asciiRule))
    assert(lines(f.output).contains("Passphrase: ********"))
    assert(!lines(f.output).exists(_.startsWith("Passphrase: *********")))
  }

  test("a byte of 128 or more in a name is refused and the name is unchanged") {
    val f = new Fixture()
    val (a, _) = two(f)
    f.run(script("r", "Da", utf8("\u00e9"), "i", utf8("\u20ac"), "ly", enter, "q"))
    assert(lines(f.output).contains(asciiRule))
    assertEquals(
      f.nameLines.last,
      s"""{"kind":"rename","address":"$a","name":"Daily","time":$stamp}"""
    )
  }

  test("a byte of 128 or more in the seed field changes nothing") {
    val f = new Fixture()
    f.run(script("n", enter, phrase, enter, phrase, enter, enter, utf8("\u00e9"), interrupt))
    val last = frames(f.output).last
    assert(last.contains("group 1 of 13"))
    assert(!last.exists(_.contains("#")))
  }

  // Unbuffered terminal input

  test("the real terminal path reads the file descriptor directly with no buffer") {
    val defaults = KeyScreen.getClass.getMethods.filter(method =>
      method.getName.startsWith("runOnTerminal$default$") &&
        method.getReturnType == classOf[InputStream]
    )
    assertEquals(defaults.length, 1)
    val stream = defaults.head.invoke(KeyScreen)
    assert(stream.isInstanceOf[java.io.FileInputStream], "the default input is not a file stream")
    assert(!stream.isInstanceOf[java.io.BufferedInputStream])
    assert(!(stream eq System.in))
  }

  // The active key write holds the journal lock

  test("the active key is written while the order journal of the old key is locked") {
    val f = new Fixture()
    val (a, b) = two(f)
    var inside = Option.empty[Either[String, Unit]]
    f.writeActive = (path, on, address) =>
      inside = Some(KeySetup.switchAllowed(f.state, on, Some(a))(()))
      ActiveKey.select(path, on, address)
    assertEquals(f.run(script(down, enter, "q")), Some(b))
    assertEquals(inside, Some(Left(dormant)))
    assertEquals(f.activeText, s"""{"testnet":"$b"}""" + "\n")
    assertEquals(KeySetup.switchAllowed(f.state, "testnet", Some(a))(()), Right(()))
  }

  test("a new key is made active while the order journal of the old key is locked") {
    val f = new Fixture()
    val old = "0x" + "a" * 40
    f.name(old, "Old")
    f.present += old
    f.activate(old)
    var inside = Option.empty[Either[String, Unit]]
    f.writeActive = (path, on, address) =>
      inside = Some(KeySetup.switchAllowed(f.state, on, Some(old))(()))
      ActiveKey.select(path, on, address)
    val address = addressOf(seedOne, phrase)
    assertEquals(f.run(newKey()), Some(address))
    assertEquals(inside, Some(Left(dormant)))
    assertEquals(f.activeText, s"""{"testnet":"$address"}""" + "\n")
  }

  test("a failed active key write shows its class name, keeps the old key and frees the lock") {
    val f = new Fixture()
    val (a, _) = two(f)
    f.writeActive = (_, _, _) => throw new IllegalStateException("canary-text")
    assertEquals(f.run(script(down, enter, "q")), Some(a))
    val said =
      "The key files could not be updated (IllegalStateException). The current key stays active."
    assert(lines(f.output).contains(said))
    assert(!f.output.contains("canary-text"))
    assertEquals(KeySetup.switchAllowed(f.state, "testnet", Some(a))(()), Right(()))
  }

  // Secrets after a flow

  test("after a new key flow every secret buffer the screen used is all zeros") {
    val f = new Fixture()
    val kept = ArrayBuffer.empty[Array[Byte]]
    var pasted = Array.emptyByteArray
    val result = KeyScreen.run(
      f.context,
      new ByteArrayInputStream(newKey()),
      f.screen,
      handed => pasted = handed,
      kept += _
    )
    assertEquals(result, Some(addressOf(seedOne, phrase)))
    // The buffers held secrets while the flow ran, so the zeros below are a real result.
    assertEquals(f.derivations.size, 2)
    f.derivations.foreach { d =>
      assert(d.seedValue.exists(_ != 0) && d.passValue.exists(_ != 0) && d.keyValue.exists(_ != 0))
      assert(kept.exists(_ eq d.seed), "a seed buffer was not kept")
      assert(kept.exists(_ eq d.pass), "a passphrase field was not kept")
      assert(kept.exists(_ eq d.key), "a derived key was not kept")
    }
    assert(kept.exists(_ eq f.random.handed.head), "the new seed was not kept")
    assert(kept.exists(_ eq pasted), "the paste buffer was not kept")
    assert(kept.exists(_.length == 65), "the seed field was not kept")
    assert(kept.count(_.length == 128) >= 4, "a passphrase field was not kept")
    assertZero(kept.toSeq*)
    assertZero(f.random.handed.toSeq*)
    assertZero(f.clipboardInputs.toSeq*)
    assertZero(f.helperInputs.toSeq*)
    assert(f.clipboardInputs.exists(_.nonEmpty) && f.helperInputs.nonEmpty)
  }
