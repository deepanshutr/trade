package desk

import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.{Files, Paths}
import java.security.{MessageDigest, SecureRandom}
import scala.collection.mutable.ArrayBuffer

class AgentKeySpec extends munit.FunSuite:
  private val fixtures =
    ujson.read(Files.readString(Paths.get("fixtures", "agent-key-vectors.json")))
  private val format = java.util.HexFormat.of()
  private val alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  private val order = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"
  private val rfcTag = "0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659"

  private def hex(bytes: Array[Byte]): String = format.formatHex(bytes)
  private def ascii(text: String): Array[Byte] = text.getBytes(US_ASCII)
  private def zero(bytes: Array[Byte]): Boolean = bytes.forall(_ == 0)
  private def sha256(parts: Array[Byte]*): Array[Byte] =
    val digest = MessageDigest.getInstance("SHA-256")
    parts.foreach(digest.update(_))
    digest.digest()

  private val vectors = fixtures("vectors").arr.toVector
  private val fixtureSeeds = vectors.map(vector => format.parseHex(vector("seed").str))

  private def randomSeeds(count: Int, start: Long): Vector[Array[Byte]] =
    val random = new java.util.Random(start)
    Vector.fill(count) {
      val seed = new Array[Byte](32)
      random.nextBytes(seed)
      seed
    }

  // The seed text without dashes, as 65 normalised bytes.
  private def normalised(seed: Array[Byte]): Array[Byte] =
    SeedText.encode(seed).flatMap(byte => SeedText.accept(byte))

  private def groupOf(groups: Array[Byte], index: Int): Array[Byte] =
    groups.slice(index * 5, index * 5 + 5)

  // A second implementation of the check character, straight from the plan text.
  private def checkIndex(index: Int, data: Array[Byte]): Int =
    (sha256(ascii("desk-seed-v1"), Array(index.toByte), data)(0) & 0xff) >> 3

  private def withCheck(index: Int, data: String): Array[Byte] =
    ascii(data) :+ ascii(alphabet)(checkIndex(index, ascii(data)))

  private def fixedBuffer(passphrase: String): Array[Byte] =
    val buffer = Array.fill[Byte](128)(1)
    System.arraycopy(ascii(passphrase), 0, buffer, 0, passphrase.length)
    buffer

  // Argon2id, RFC 9106 section 5.3

  test("Argon2id gives the RFC 9106 section 5.3 tag through the call derive uses") {
    val tag = new Array[Byte](32)
    AgentKey.argon2id(
      Array.fill[Byte](32)(1),
      Array.fill[Byte](16)(2),
      32,
      3,
      4,
      tag,
      Array.fill[Byte](8)(3),
      Array.fill[Byte](12)(4)
    )
    assertEquals(hex(tag), rfcTag)
  }

  // Pinned vectors, recomputed in Python by test_agent_key_vectors.py

  vectors.foreach { vector =>
    test("seed text matches the pinned vector for " + vector("name").str) {
      val seed = format.parseHex(vector("seed").str)
      val text = SeedText.encode(seed)
      assertEquals(String(text, US_ASCII), vector("seedText").str)
      assertEquals(hex(seed), vector("seed").str)
    }

    test("derive matches the pinned vector for " + vector("name").str) {
      val seed = format.parseHex(vector("seed").str)
      val passphrase = vector("passphrase").str
      val buffer = fixedBuffer(passphrase)
      val before = buffer.clone()
      val key = AgentKey.derive(seed, buffer, passphrase.length)
      assertEquals(hex(key), vector("key").str)
      assertEquals(OrderSigning.address(key), vector("address").str)
      assertEquals(hex(seed), vector("seed").str)
      assertEquals(hex(buffer), hex(before))
    }
  }

  test("derive zeroes its salt and Argon2 output buffers and keeps the key") {
    val vector = vectors.head
    val passphrase = vector("passphrase").str
    val seen = ArrayBuffer.empty[(String, Array[Byte], Array[Byte])]
    val key = AgentKey.deriveObserved(
      format.parseHex(vector("seed").str),
      fixedBuffer(passphrase),
      passphrase.length
    ) { (name, buffer) => seen += ((name, buffer, buffer.clone())) }
    val names = seen.map(_._1).toVector
    assertEquals(names, Vector("salt", "output"))
    val salt = seen.head
    val output = seen(1)
    assertEquals(hex(salt._3), hex(ascii("desk-agent-v1") ++ Array[Byte](0) ++ ascii(passphrase)))
    assertEquals(hex(output._3), vector("output").str)
    assert(zero(salt._2), "salt buffer left dirty")
    assert(zero(output._2), "output buffer left dirty")
    assertEquals(hex(key), vector("key").str)
  }

  test("derive zeroes its buffers when the work is interrupted") {
    val vector = vectors.head
    val passphrase = vector("passphrase").str
    val seen = ArrayBuffer.empty[Array[Byte]]
    val failure = intercept[IllegalStateException] {
      AgentKey.deriveObserved(
        format.parseHex(vector("seed").str),
        fixedBuffer(passphrase),
        passphrase.length
      ) { (name, buffer) =>
        seen += buffer
        if name == "output" then throw new IllegalStateException("stop")
      }
    }
    assertEquals(failure.getMessage, "stop")
    assertEquals(seen.size, 2)
    seen.foreach(buffer => assert(zero(buffer), "buffer left dirty after a failure"))
  }

  test("derive refuses bad input with a fixed message") {
    val good = new Array[Byte](32)
    val passphrase = fixedBuffer("a good passphrase")
    Vector(
      () => AgentKey.derive(new Array[Byte](31), passphrase, 17),
      () => AgentKey.derive(new Array[Byte](33), passphrase, 17),
      () => AgentKey.derive(good, passphrase, 7),
      () => AgentKey.derive(good, passphrase, 129),
      () => AgentKey.derive(good, fixedBuffer("short pass\u0007"), 11)
    ).foreach { call =>
      val failure = intercept[IllegalArgumentException](call())
      assertEquals(failure.getMessage, "Unsupported key input")
    }
  }

  // Scalar reduction

  private def reduceFixture(name: String): ujson.Value =
    fixtures("reduce").arr.find(_("name").str == name).getOrElse(fail("missing case " + name))

  test("reduce returns an in range value unchanged and as a separate array") {
    Vector(
      "one" -> format.parseHex("00" * 31 + "01"),
      "order minus one" -> format.parseHex(order.dropRight(1) + "0"),
      "middle" -> format.parseHex("7f" + "00" * 30 + "ff")
    ).foreach { case (name, value) =>
      val result = AgentKey.reduce(value)
      assertEquals(hex(result), hex(value), name)
      assert(!(result eq value), name + " returned the caller's array")
    }
  }

  test("reduce on zero, the order itself and all ones uses counter one") {
    val counterOne = Array[Byte](0, 0, 0, 1)
    Vector(
      "zero" -> format.parseHex("00" * 32),
      "order itself" -> format.parseHex(order),
      "all ones" -> format.parseHex("ff" * 32)
    ).foreach { case (name, value) =>
      val expected = hex(sha256(value, counterOne))
      assertEquals(hex(AgentKey.reduce(value)), expected, name)
      assertEquals(reduceFixture(name)("key").str, expected, name)
    }
  }

  test("reduce matches every pinned case") {
    fixtures("reduce").arr.foreach { item =>
      val value = format.parseHex(item("input").str)
      assertEquals(hex(AgentKey.reduce(value)), item("key").str, item("name").str)
      assertEquals(hex(value), item("input").str)
    }
  }

  test("reduce walks the counter from one and zeroes what it rejects") {
    val value = format.parseHex("00" * 32)
    val inputs = ArrayBuffer.empty[String]
    val replies = Vector(order, "00" * 32, "ff" * 32, "01" + "00" * 30 + "02").map(format.parseHex)
    val seen = ArrayBuffer.empty[(String, Array[Byte])]
    def hash(input: Array[Byte]): Array[Byte] =
      inputs += hex(input)
      replies(inputs.size - 1).clone()
    def observe(name: String, buffer: Array[Byte]): Unit = seen += ((name, buffer))
    val result = AgentKey.reduceWith(value, hash, observe)
    assertEquals(hex(result), hex(replies(3)))
    assertEquals(
      inputs.toVector,
      Vector("00000001", "00000002", "00000003", "00000004").map("00" * 32 + _)
    )
    assertEquals(seen.map(_._1).distinct.toVector, Vector("counter input", "rejected candidate"))
    assertEquals(seen.count(_._1 == "rejected candidate"), 3)
    seen.foreach { case (name, buffer) => assert(zero(buffer), name + " left dirty") }
    assertEquals(hex(value), "00" * 32)
  }

  test("reduce refuses a value that is not 32 bytes") {
    Vector(0, 31, 33).foreach { size =>
      intercept[IllegalArgumentException](AgentKey.reduce(new Array[Byte](size)))
    }
  }

  // Seed text

  test("there are thirteen groups") {
    assertEquals(SeedText.groups, 13)
  }

  test("encode prints 77 bytes of groups joined by dashes and leaves the seed alone") {
    (fixtureSeeds ++ randomSeeds(50, 3)).foreach { seed =>
      val before = seed.clone()
      val text = SeedText.encode(seed)
      assertEquals(text.length, 77)
      text.zipWithIndex.foreach { case (byte, index) =>
        if index % 6 == 5 then assertEquals(byte.toChar, '-')
        else assert(alphabet.contains(byte.toChar), "byte outside the alphabet at " + index)
      }
      assertEquals(hex(seed), hex(before))
    }
  }

  test("encode refuses a seed that is not 32 bytes") {
    Vector(0, 31, 33).foreach { size =>
      intercept[IllegalArgumentException](SeedText.encode(new Array[Byte](size)))
    }
  }

  test("encode and decode round trip for 1000 seeded random seeds") {
    randomSeeds(1000, 4).foreach { seed =>
      val groups = normalised(seed)
      assertEquals(groups.length, 65)
      val decoded = SeedText.decode(groups)
      assert(decoded.exists(hex(_) == hex(seed)), "round trip failed for " + hex(seed))
    }
  }

  test("a sloppily typed seed text still reads back to the same seed") {
    val random = new java.util.Random(5)
    def pick(choices: String): Byte = choices.charAt(random.nextInt(choices.length)).toByte
    randomSeeds(1000, 6).foreach { seed =>
      val typed = SeedText.encode(seed).flatMap { byte =>
        byte.toChar match
          case '-' =>
            random.nextInt(3) match
              case 0 => Array(' '.toByte)
              case 1 => Array.empty[Byte]
              case _ => Array('-'.toByte)
          case '1'                                     => Array(pick("1IiLl"))
          case '0'                                     => Array(pick("0Oo"))
          case c if c.isLetter && random.nextBoolean() => Array(c.toLower.toByte)
          case _                                       => Array(byte)
      }
      val groups = typed.flatMap(byte => SeedText.accept(byte))
      assertEquals(groups.length, 65)
      assert(SeedText.decode(groups).exists(hex(_) == hex(seed)), "typed text read back wrong")
    }
  }

  test("reading rules decide every byte value") {
    val accepted = scala.collection.mutable.Map.empty[Int, Char]
    alphabet.foreach(c => accepted(c) = c)
    alphabet.filter(_.isLetter).foreach(c => accepted(c.toLower) = c)
    "Ii".foreach(c => accepted(c) = '1')
    "Ll".foreach(c => accepted(c) = '1')
    "Oo".foreach(c => accepted(c) = '0')
    (0 until 256).foreach { value =>
      val byte = value.toByte
      val expected = accepted.get(value).map(_.toByte)
      assertEquals(SeedText.accept(byte), expected, "accept " + value)
      val skipped = value == '-' || value == ' '
      assertEquals(SeedText.refused(byte), expected.isEmpty && !skipped, "refused " + value)
    }
    assertEquals(SeedText.accept('a'.toByte), Some('A'.toByte))
    assertEquals(SeedText.accept('L'.toByte), Some('1'.toByte))
    assertEquals(SeedText.accept('o'.toByte), Some('0'.toByte))
    assertEquals(SeedText.accept('-'.toByte), None)
    assertEquals(SeedText.accept(' '.toByte), None)
    assertEquals(SeedText.accept('U'.toByte), None)
    assert(SeedText.refused('U'.toByte))
    assert(SeedText.refused('u'.toByte))
    assert(!SeedText.refused('-'.toByte))
    assert(!SeedText.refused(' '.toByte))
  }

  test("groupValid agrees with an independent check character for random groups") {
    val random = new java.util.Random(7)
    (0 until 5000).foreach { _ =>
      val index = random.nextInt(13)
      val group = Array.fill[Byte](5)(alphabet.charAt(random.nextInt(32)).toByte)
      val data = group.take(4)
      val expected = alphabet.indexOf(group(4).toChar) == checkIndex(index, data) &&
        (index != 12 || (alphabet.indexOf(group(3).toChar) & 15) == 0)
      assertEquals(SeedText.groupValid(index, group), expected, "group " + index)
    }
  }

  test("every group of an encoded seed is valid at its own index") {
    (fixtureSeeds ++ randomSeeds(100, 8)).foreach { seed =>
      val groups = normalised(seed)
      (0 until 13).foreach(index => assert(SeedText.groupValid(index, groupOf(groups, index))))
    }
  }

  test("groupValid refuses a wrong length, a wrong index and bytes that are not normalised") {
    val group = groupOf(normalised(fixtureSeeds.head), 0)
    assert(SeedText.groupValid(0, group))
    assert(!SeedText.groupValid(-1, group))
    assert(!SeedText.groupValid(13, group))
    assert(!SeedText.groupValid(0, group.take(4)))
    assert(!SeedText.groupValid(0, group ++ Array[Byte]('0')))
    assert(!SeedText.groupValid(0, Array.empty[Byte]))
    val lower = group.clone()
    lower(0) = if lower(0) == 'A'.toByte then 'a'.toByte else 'z'.toByte
    assert(!SeedText.groupValid(0, lower))
    val dashed = group.clone()
    dashed(2) = '-'.toByte
    assert(!SeedText.groupValid(0, dashed))
  }

  test("every change to a check character fails its group") {
    (fixtureSeeds ++ randomSeeds(100, 9)).foreach { seed =>
      val groups = normalised(seed)
      (0 until 13).foreach { index =>
        val group = groupOf(groups, index)
        alphabet.map(_.toByte).filter(_ != group(4)).foreach { replacement =>
          val changed = group.clone()
          changed(4) = replacement
          assert(!SeedText.groupValid(index, changed), "check change passed in group " + index)
        }
      }
    }
  }

  test("a single data character change passes its group check less than one time in sixteen") {
    var trials = 0
    var passes = 0
    (fixtureSeeds ++ randomSeeds(20, 10)).foreach { seed =>
      val groups = normalised(seed)
      (0 until 13).foreach { index =>
        (0 until 4).foreach { position =>
          val at = index * 5 + position
          alphabet.map(_.toByte).filter(_ != groups(at)).foreach { replacement =>
            val changed = groups.clone()
            changed(at) = replacement
            trials += 1
            if SeedText.groupValid(index, groupOf(changed, index)) then
              passes += 1
              val decoded = SeedText.decode(changed)
              assert(
                !decoded.exists(hex(_) == hex(seed)),
                "a changed group decoded to the same seed"
              )
          }
        }
      }
    }
    assert(trials >= 2000, "trials " + trials)
    assert(passes * 16 < trials, s"$passes of $trials changes still passed")
  }

  test("a swap of two adjacent groups fails in at least 99 percent of trials") {
    var trials = 0
    var failures = 0
    (fixtureSeeds ++ randomSeeds(200, 11)).foreach { seed =>
      val groups = normalised(seed)
      (0 until 12).foreach { index =>
        val first = groupOf(groups, index)
        val second = groupOf(groups, index + 1)
        if hex(first) != hex(second) then
          val swapped = groups.clone()
          System.arraycopy(second, 0, swapped, index * 5, 5)
          System.arraycopy(first, 0, swapped, index * 5 + 5, 5)
          trials += 1
          val valid = (0 until 13).forall(i => SeedText.groupValid(i, groupOf(swapped, i)))
          assertEquals(SeedText.decode(swapped).isDefined, valid)
          if !valid then failures += 1
      }
    }
    assert(trials >= 2400, "trials " + trials)
    assert(failures * 100 >= trials * 99, s"only $failures of $trials swaps failed")
  }

  test("nonzero padding bits in the last group fail that group even with a correct check") {
    val zeroGroups = normalised(new Array[Byte](32))
    (0 until 32).foreach { value =>
      val data = "000" + alphabet.charAt(value)
      val group = withCheck(12, data)
      val clean = value == 0 || value == 16
      assertEquals(SeedText.groupValid(12, group), clean, data)
      val groups = zeroGroups.take(60) ++ group
      assertEquals(SeedText.decode(groups).isDefined, clean, data)
    }
    val lastBit = Array.fill[Byte](32)(0)
    lastBit(31) = 1
    assertEquals(
      hex(SeedText.decode(zeroGroups.take(60) ++ withCheck(12, "000G")).get),
      hex(lastBit)
    )
    // The padding rule belongs to the last group only.
    assert(SeedText.groupValid(11, withCheck(11, "0001")))
  }

  test("decode refuses a wrong length and any invalid group") {
    val groups = normalised(fixtureSeeds(1))
    assert(SeedText.decode(groups).isDefined)
    Vector(0, 64, 66, 70).foreach { size =>
      assertEquals(SeedText.decode(new Array[Byte](size)), None)
    }
    assertEquals(SeedText.decode(groups.take(64)), None)
    assertEquals(SeedText.decode(groups :+ '0'.toByte), None)
    (0 until 13).foreach { index =>
      val changed = groups.clone()
      val at = index * 5 + 4
      changed(at) = if changed(at) == '0'.toByte then '1'.toByte else '0'.toByte
      assertEquals(SeedText.decode(changed), None, "group " + index)
    }
    val letter = groups.indexWhere(_.toChar.isLetter)
    assert(letter >= 0)
    val lower = groups.clone()
    lower(letter) = lower(letter).toChar.toLower.toByte
    assertEquals(SeedText.decode(lower), None)
  }

  test("newSeed reads 32 bytes from the generator it is given") {
    val fixed = Array.tabulate[Byte](32)(i => (i * 3).toByte)
    val random = new SecureRandom:
      override def nextBytes(bytes: Array[Byte]): Unit =
        assertEquals(bytes.length, 32)
        System.arraycopy(fixed, 0, bytes, 0, 32)
    assertEquals(hex(AgentKey.newSeed(random)), hex(fixed))
    val real = new SecureRandom()
    assertNotEquals(hex(AgentKey.newSeed(real)), hex(AgentKey.newSeed(real)))
  }

  // Passphrase rule

  private def filled(length: Int, fill: Char = 'a'): Array[Byte] =
    Array.fill[Byte](length)(fill.toByte)

  test("passphraseValid accepts 8 to 128 printable bytes") {
    assert(!AgentKey.passphraseValid(filled(7), 7))
    assert(AgentKey.passphraseValid(filled(8), 8))
    assert(AgentKey.passphraseValid(filled(128), 128))
    assert(!AgentKey.passphraseValid(filled(129), 129))
    assert(!AgentKey.passphraseValid(filled(0), 0))
    assert(!AgentKey.passphraseValid(filled(10), -1))
  }

  test("passphraseValid takes a length within a larger buffer") {
    val buffer = filled(128)
    assert(AgentKey.passphraseValid(buffer, 8))
    assert(AgentKey.passphraseValid(buffer, 128))
    assert(!AgentKey.passphraseValid(buffer, 129))
    assert(!AgentKey.passphraseValid(filled(8), 9))
    val tail = filled(20)
    tail(15) = 7
    assert(AgentKey.passphraseValid(tail, 15))
    assert(!AgentKey.passphraseValid(tail, 16))
    assert(!AgentKey.passphraseValid(null, 8))
  }

  test("passphraseValid refuses bytes 31 and 127 and allows spaces and the printable edges") {
    Vector(0, 7, 10, 13, 27, 31, 127, 128, 200, 255).foreach { value =>
      (0 until 8).foreach { at =>
        val buffer = filled(8)
        buffer(at) = value.toByte
        assert(!AgentKey.passphraseValid(buffer, 8), s"byte $value at $at")
      }
    }
    Vector(32, 33, 126).foreach { value =>
      (0 until 8).foreach { at =>
        val buffer = filled(8)
        buffer(at) = value.toByte
        assert(AgentKey.passphraseValid(buffer, 8), s"byte $value at $at")
      }
    }
    assert(AgentKey.passphraseValid(ascii("        "), 8))
    assert(AgentKey.passphraseValid(ascii(" edge pass "), 11))
  }

  // Approval name

  test("approvalName is desk and the first ten hex characters of the address") {
    vectors.foreach { vector =>
      val address = vector("address").str
      val name = AgentKey.approvalName(address)
      assertEquals(name.length, 15)
      assert(name.matches("desk-[0-9a-f]{10}"), name)
      assertEquals(name, "desk-" + address.slice(2, 12))
    }
    assertEquals(
      AgentKey.approvalName("0xC0DF6975A6DBEE4A74449C6D70D830C224D196AE"),
      "desk-c0df6975a6"
    )
  }

  test("approvalName refuses text that is not an address") {
    Vector(
      "",
      "c0df6975a6dbee4a74449c6d70d830c224d196ae",
      "0xc0df6975a6dbee4a74449c6d70d830c224d196a",
      "0xc0df6975a6dbee4a74449c6d70d830c224d196aeff",
      "0xg0df6975a6dbee4a74449c6d70d830c224d196ae"
    ).foreach(text => intercept[IllegalArgumentException](AgentKey.approvalName(text)))
  }

  // Timing

  test("one derivation takes under five seconds".tag(new munit.Tag("slow"))) {
    val vector = vectors.head
    val passphrase = vector("passphrase").str
    val seed = format.parseHex(vector("seed").str)
    val started = System.nanoTime()
    val key = AgentKey.derive(seed, fixedBuffer(passphrase), passphrase.length)
    val milliseconds = (System.nanoTime() - started) / 1000000
    println(s"One agent key derivation took $milliseconds ms.")
    assertEquals(hex(key), vector("key").str)
    assert(milliseconds < 5000, s"derivation took $milliseconds ms")
  }
