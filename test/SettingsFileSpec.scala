package desk

import java.nio.file.{Files, Path}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import scala.jdk.CollectionConverters.*

class SettingsFileSpec extends TempDirectorySuite:
  private val changed = "Settings changed on disk. Leave and open settings again"
  private val owner = "0x" + "ab" * 20
  // Every field the owner or the vault may set, written with four-space indentation by hand.
  private val full = ujson.write(
    ujson.Obj(
      "small" -> "25",
      "big" -> "250",
      "target" -> "300",
      "network" -> "testnet",
      "owner" -> owner,
      "keyService" -> "desk-test",
      "references" -> ujson.Arr("BTC"),
      "quoteAgeMs" -> "1500",
      "positionAgeMs" -> "2500",
      "propBalance" -> "5000",
      "propFee" -> "0.0004",
      "slippage" -> "0.002",
      "onchain" -> ujson.Arr(
        ujson.Obj(
          "name" -> "TESTA",
          "pair" -> ("0x" + "1" * 40),
          "quotePool" -> ("0x" + "2" * 40),
          "rpc" -> ujson.Arr("https://rpc.example.org")
        )
      )
    ),
    indent = 4
  )
  private val terms =
    TradeTerms(TradeSizes(BigDecimal("40"), BigDecimal("400")), BigDecimal("0.0005"))

  private def mode(path: Path): String =
    PosixFilePermissions.toString(Files.getPosixFilePermissions(path))

  private def settingsFile(text: String): Path =
    val path = tempDirectory("settings").resolve("config.json")
    Files.writeString(path, text)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------"))
    path

  private def right[A](result: Either[String, A]): A =
    result.fold(message => fail(message), identity)

  private def snapshot(path: Path): SettingsSnapshot =
    right(SettingsFile.read(Some(path), path.getParent))

  // Anything in the folder besides the settings file, such as a temporary file left behind.
  private def leftovers(path: Path): Vector[String] =
    val names = Files.list(path.getParent)
    try names.iterator.asScala.map(_.getFileName.toString).filter(_ != "config.json").toVector
    finally names.close()

  test("a write changes only the three fields and keeps owner, network, onchain and the rest") {
    val path = settingsFile(full)
    val written = SettingsFile.write(snapshot(path), terms)
    val before = ujson.read(full).obj
    val text = Files.readString(path)
    val after = ujson.read(text).obj
    assertEquals(after.keys.toVector, before.keys.toVector)
    before.keys.filterNot(Set("small", "big", "slippage")).foreach { name =>
      assertEquals(after(name), before(name), name)
    }
    assertEquals(after("small").str, "40")
    assertEquals(after("big").str, "400")
    assertEquals(after("slippage").str, "0.0005")
    assert(text.startsWith("{\n  \"small\": \"40\",\n  \"big\": \"400\",\n"), text)
    val loaded = right(TradingConfig.load(Some(path)))
    assertEquals(loaded.owner, Some(owner))
    assertEquals(loaded.network, "testnet")
    assertEquals(loaded.onchain.map(_.name), Vector("TESTA"))
    assertEquals(loaded.sizes, TradeSizes(BigDecimal("40"), BigDecimal("400"), BigDecimal("300")))
    assertEquals(loaded.slippage, BigDecimal("0.0005"))
    assertEquals(written.map(_.config), Right(loaded))
    assertEquals(written.map(_.path), Right(path))
    assertEquals(written.map(_.bytes.toVector), Right(Files.readAllBytes(path).toVector))
  }

  test("a write replaces the file whole by an atomic move, private, with no temporary file left") {
    val path = settingsFile(full)
    val original = Files.readAttributes(path, classOf[BasicFileAttributes]).fileKey
    var seen = ""
    val written = SettingsFile.write(snapshot(path), terms, () => seen = Files.readString(path))
    assert(written.isRight, written)
    assertEquals(seen, full)
    assertNotEquals(Files.readAttributes(path, classOf[BasicFileAttributes]).fileKey, original)
    assertEquals(mode(path), "rw-------")
    assertEquals(leftovers(path), Vector.empty)
  }

  test("a file another program wrote since the read is refused and kept as that program left it") {
    val path = settingsFile(full)
    val read = snapshot(path)
    val other = full.replace("\"testnet\"", "\"mainnet\"")
    Files.writeString(path, other)
    assertEquals(SettingsFile.write(read, terms), Left(changed))
    assertEquals(Files.readString(path), other)
    assertEquals(leftovers(path), Vector.empty)
  }

  test("a write by another program just before the replace is caught by the compare") {
    val path = settingsFile(full)
    val other = full.replace("\"desk-test\"", "\"desk-other\"")
    val result = SettingsFile.write(snapshot(path), terms, () => Files.writeString(path, other))
    assertEquals(result, Left(changed))
    assertEquals(Files.readString(path), other)
    assertEquals(leftovers(path), Vector.empty)
  }

  test("a file removed since the read, or made since an absent read, is refused") {
    val path = settingsFile(full)
    val read = snapshot(path)
    Files.delete(path)
    assertEquals(SettingsFile.write(read, terms), Left(changed))
    assert(!Files.exists(path))
    val home = tempDirectory("settings-home")
    val absent = right(SettingsFile.read(None, home))
    val made = home.resolve(".config/desk/config.json")
    Files.createDirectories(
      made.getParent,
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------"))
    )
    Files.writeString(made, full)
    Files.setPosixFilePermissions(made, PosixFilePermissions.fromString("rw-------"))
    assertEquals(SettingsFile.write(absent, terms), Left(changed))
    assertEquals(Files.readString(made), full)
    assertEquals(leftovers(made), Vector.empty)
  }

  test("a refused value never reaches the file") {
    val sizeRefusal = "invalid trading settings: invalid small, big or target dollar size"
    val slippageRefusal = "invalid trading settings: invalid slippage"
    val cases = Vector(
      (TradeSizes(BigDecimal("0"), BigDecimal("400")), BigDecimal("0.0005"), sizeRefusal),
      (TradeSizes(BigDecimal("40"), BigDecimal("1000000001")), BigDecimal("0.0005"), sizeRefusal),
      (
        TradeSizes(BigDecimal("401"), BigDecimal("400")),
        BigDecimal("0.0005"),
        "invalid trading settings: small exceeds big"
      ),
      (TradeSizes(BigDecimal("40"), BigDecimal("400")), BigDecimal("0"), slippageRefusal),
      (TradeSizes(BigDecimal("40"), BigDecimal("400")), BigDecimal("0.0101"), slippageRefusal),
      (TradeSizes(BigDecimal("40"), BigDecimal("400")), BigDecimal("0.000000001"), slippageRefusal)
    )
    cases.foreach { (sizes, slippage, reason) =>
      val path = settingsFile(full)
      assertEquals(SettingsFile.write(snapshot(path), TradeTerms(sizes, slippage)), Left(reason))
      assertEquals(Files.readString(path), full)
      assertEquals(leftovers(path), Vector.empty)
    }
  }

  test("a missing settings file is created with the three fields in a private folder") {
    val home = tempDirectory("settings-home")
    val absent = right(SettingsFile.read(None, home))
    val path = home.resolve(".config/desk/config.json")
    assertEquals(absent.path, path)
    assertEquals(absent.bytes.toVector, Vector.empty)
    assertEquals(absent.config, TradingConfig())
    assert(!Files.exists(home.resolve(".config")))
    val written = right(SettingsFile.write(absent, terms))
    val json = ujson.read(Files.readString(path)).obj
    assertEquals(json.keys.toVector, Vector("small", "big", "slippage"))
    assertEquals(json.values.map(_.str).toVector, Vector("40", "400", "0.0005"))
    assertEquals(mode(path), "rw-------")
    assertEquals(mode(path.getParent), "rwx------")
    assertEquals(leftovers(path), Vector.empty)
    val again = SettingsFile.write(written, terms.copy(slippage = BigDecimal("0.01")))
    assertEquals(again.map(_.config.slippage), Right(BigDecimal("0.01")))
    assertEquals(SettingsFile.write(absent, terms), Left(changed))
    assertEquals(SettingsFile.write(written, terms), Left(changed))
  }

  test("a folder others can read refuses the write and leaves the file as it was") {
    val path = settingsFile(full)
    Files.setPosixFilePermissions(path.getParent, PosixFilePermissions.fromString("rwxr-xr-x"))
    val result = SettingsFile.write(snapshot(path), terms)
    assertEquals(result, Left(s"local storage must be private (${path.getParent})"))
    assertEquals(Files.readString(path), full)
    assertEquals(mode(path), "rw-------")
    assertEquals(leftovers(path), Vector.empty)
    val home = tempDirectory("settings-home")
    val folder = home.resolve(".config/desk")
    Files.createDirectories(folder)
    Files.setPosixFilePermissions(folder, PosixFilePermissions.fromString("rwxr-xr-x"))
    val absent = right(SettingsFile.read(None, home))
    assertEquals(
      SettingsFile.write(absent, terms),
      Left(s"local storage must be private ($folder)")
    )
    assertEquals(leftovers(folder.resolve("config.json")), Vector.empty)
    assert(!Files.exists(folder.resolve("config.json")))
  }

  test("read loads the given or default file and refuses one Desk cannot load") {
    val path = settingsFile(full)
    val read = snapshot(path)
    assertEquals(read.path, path)
    assertEquals(read.bytes.toVector, Files.readAllBytes(path).toVector)
    assertEquals(TradingConfig.load(Some(path)), Right(read.config))
    val home = tempDirectory("settings-home")
    val default = home.resolve(".config/desk/config.json")
    Files.createDirectories(default.getParent)
    Files.copy(path, default)
    Files.setPosixFilePermissions(default, PosixFilePermissions.fromString("rw-------"))
    val found = right(SettingsFile.read(None, home))
    assertEquals(found.path, default)
    assertEquals(found.config, read.config)
    val refusal = Left("invalid trading settings: settings must be a private regular file")
    assertEquals(SettingsFile.read(Some(path.resolveSibling("missing.json")), home), refusal)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-r--r--"))
    assertEquals(SettingsFile.read(Some(path), home), refusal)
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------"))
    Files.writeString(path, """{"small":"0"}""")
    assertEquals(
      SettingsFile.read(Some(path), home),
      Left("invalid trading settings: invalid small, big or target dollar size")
    )
  }

  test("percent shows slippage with at least two decimals and fraction reads it back") {
    Vector(
      "0.001" -> "0.10",
      "0.0005" -> "0.05",
      "0.01" -> "1.00",
      "0.0015" -> "0.15",
      "0.000001" -> "0.0001",
      "0.00012345" -> "0.012345"
    ).foreach { (fraction, percent) =>
      assertEquals(SettingsFile.percent(BigDecimal(fraction)), percent, fraction)
    }
    Vector("0.10", "0.05", "1.00", "0.0001", "0.1234", "0.25").foreach { percent =>
      assertEquals(SettingsFile.fraction(percent).map(SettingsFile.percent), Right(percent))
    }
    Vector("0.001", "0.0005", "0.01", "0.000001", "0.001234").map(BigDecimal(_)).foreach { f =>
      assertEquals(SettingsFile.fraction(SettingsFile.percent(f)), Right(f))
    }
    Vector("0.1" -> "0.001", ".5" -> "0.005", "1." -> "0.01", "1" -> "0.01", "0.0001" -> "0.000001")
      .foreach { (percent, fraction) =>
        assertEquals(SettingsFile.fraction(percent), Right(BigDecimal(fraction)), percent)
      }
    val refusal = "Slippage must be above 0% and at most 1%, with at most four decimals"
    Vector("", ".", "0", "0.0000", "1.0001", "2", "0.00001", "-0.1", "1e-1", "0.1.2", " 0.1", "0,1")
      .foreach { percent =>
        assertEquals(SettingsFile.fraction(percent), Left(refusal), percent)
      }
  }

  test("a slippage typed in percent is stored as its fraction and every other setting loads back") {
    val path = settingsFile(full)
    val read = snapshot(path)
    val slippage = right(SettingsFile.fraction("0.0001"))
    val written = SettingsFile.write(read, terms.copy(slippage = slippage))
    assertEquals(ujson.read(Files.readString(path))("slippage").str, "0.000001")
    val sizes = read.config.sizes.copy(small = BigDecimal("40"), big = BigDecimal("400"))
    val expected = read.config.copy(sizes = sizes, slippage = BigDecimal("0.000001"))
    assertEquals(written.map(_.config), Right(expected))
    assertEquals(TradingConfig.load(Some(path)), Right(expected))
  }
