package desk

import java.nio.file.Files
import java.nio.file.attribute.{AclEntryType, UserPrincipal}

class WindowsPortSpec extends TempDirectorySuite:
  private final case class Who(getName: String) extends UserPrincipal

  private val me = Who("me")
  private val system = Who("system")
  private val accounts = Some(LocalFiles.Accounts(me, Set[UserPrincipal](system)))
  private def access(owner: UserPrincipal, entries: (AclEntryType, UserPrincipal)*): Boolean =
    LocalFiles.privateAccess(owner, entries, accounts)

  test("an access list is private when it allows the user and no account beyond the allowed") {
    assert(access(me, AclEntryType.ALLOW -> me, AclEntryType.ALLOW -> system))
    assert(access(me, AclEntryType.ALLOW -> me, AclEntryType.DENY -> Who("everyone")))
    assert(!access(me, AclEntryType.ALLOW -> me, AclEntryType.ALLOW -> Who("everyone")))
  }

  test("an access list that does not allow the user is not private") {
    // The JDK reads a missing access list, which lets everyone in, as an empty one.
    assert(!access(me))
    assert(!access(me, AclEntryType.ALLOW -> system))
  }

  test("a path owned by another account is not private") {
    assert(!access(Who("someone"), AclEntryType.ALLOW -> me))
    assert(access(system, AclEntryType.ALLOW -> me))
  }

  test("unresolved accounts fail closed") {
    assert(!LocalFiles.privateAccess(me, Seq(AclEntryType.ALLOW -> me), None))
  }

  test("a real file with no access list is not private") {
    assume(Term.windows, "access lists exist only on Windows")
    val file = Files.writeString(tempDirectory("desk-null-acl").resolve("open.txt"), "x")
    val set = new ProcessBuilder(
      "powershell.exe",
      "-NoProfile",
      "-Command",
      "$a = Get-Acl -LiteralPath $env:DESK_TARGET; " +
        "$a.SetSecurityDescriptorSddlForm('D:NO_ACCESS_CONTROL'); Set-Acl -LiteralPath $env:DESK_TARGET $a"
    )
    set.environment().put("DESK_TARGET", file.toString)
    val run = set.redirectErrorStream(true).start()
    run.getInputStream.readAllBytes()
    assertEquals(run.waitFor(), 0)
    assert(!LocalFiles.isPrivate(file))
    assert(!TradingConfig.privateFile(file))
  }

  test("a state folder open to others is refused before a journal opens") {
    assume(Term.windows, "on macOS the journal sets the folder mode itself")
    val folder = tempDirectory("desk-open-state")
    Platform.setMode(folder, "rwxr-xr-x")
    interceptMessage[IllegalArgumentException](
      "requirement failed: state directory must be private"
    )(
      OrderJournal.open(folder, "testnet-" + "1" * 40).close()
    )
  }

  test("a folder Desk makes in a private parent is private and holds private files") {
    val folder = tempDirectory("desk-private").resolve("store")
    LocalFiles.directory(folder)
    assert(LocalFiles.isPrivate(folder))
    LocalFiles.write(folder.resolve("a.json"), "{}")
    assert(TradingConfig.privateFile(folder.resolve("a.json")))
    assertEquals(LocalFiles.read(folder.resolve("a.json"), 10L), Some("{}"))
  }

  test(
    "raw console input drops line input, echo, Control C handling and quick edit, adds escapes"
  ) {
    val raw = WindowsConsole.rawInput(0x1f7)
    assertEquals(raw & (0x1 | 0x2 | 0x4 | 0x40), 0)
    assertEquals(raw & (0x80 | 0x200), 0x80 | 0x200)
    assertEquals(raw & 0x10, 0x10)
  }

  test("raw console output keeps escapes on and line feeds without a carriage return") {
    assertEquals(WindowsConsole.rawOutput(0), 0x1 | 0x4 | 0x8)
    assertEquals(WindowsConsole.rawOutput(0x2) & 0x2, 0x2)
  }

  test("the pumped key stream counts waiting bytes and keeps their order") {
    val pumped = new Term.Pumped(new java.io.ByteArrayInputStream(Array[Byte](97, 27, 91, 65)))
    // The four bytes and the end of the stream.
    Wait.until("all bytes pumped")(pumped.available() == 5)
    assertEquals(Vector.fill(4)(pumped.read()), Vector(97, 27, 91, 65))
  }

  test("the pumped key stream reports the end again after it is reached") {
    val pumped = new Term.Pumped(new java.io.ByteArrayInputStream(Array.emptyByteArray))
    assertEquals(pumped.read(), -1)
    assertEquals(pumped.read(), -1)
  }

  test("a Windows junction counts as a link, so local storage refuses it") {
    assume(Term.windows, "junctions exist only on Windows")
    val root = tempDirectory("desk-junction")
    val real = Files.createDirectory(root.resolve("real"))
    val link = root.resolve("link")
    val made = new ProcessBuilder("cmd.exe", "/c", "mklink", "/J", link.toString, real.toString)
      .redirectErrorStream(true)
      .start()
    made.getInputStream.readAllBytes()
    assertEquals(made.waitFor(), 0)
    assert(!Files.isSymbolicLink(link), "the JDK sees no symbolic link here")
    assert(LocalFiles.linked(link))
    assert(!LocalFiles.linked(real))
    intercept[IllegalArgumentException](LocalFiles.directory(link))
  }

  test("a plain folder and a missing path are not links") {
    val root = tempDirectory("desk-plain")
    assert(!LocalFiles.linked(root))
    assert(!LocalFiles.linked(root.resolve("missing")))
  }

  test("no settings child starts on Windows") {
    var launched = false
    def launch(words: Seq[String]): Process =
      launched = true
      throw new IllegalStateException("launched")
    val windows = new SettingsWiring.Children(launch, windows = true)
    interceptMessage[IllegalStateException](Child.notOnWindows)(
      windows.start(Seq("desk", "--keys"))
    )
    assert(!launched)
    val mac = new SettingsWiring.Children(launch, windows = false)
    interceptMessage[IllegalStateException]("launched")(mac.start(Seq("desk", "--keys")))
    assert(launched)
  }

  test("a path that cannot be read is not taken as unlinked") {
    assume(Term.windows, "the folder is closed through its access list")
    val folder = Files.createDirectory(tempDirectory("desk-closed").resolve("closed"))
    val inside = Files.writeString(folder.resolve("a.json"), "{}")
    def icacls(words: String*): Unit =
      val run = new ProcessBuilder(("icacls.exe" +: folder.toString +: words)*)
        .redirectErrorStream(true)
        .start()
      run.getInputStream.readAllBytes()
      assertEquals(run.waitFor(), 0)
    icacls("/inheritance:r", "/grant:r", "*S-1-5-18:(OI)(CI)F")
    try intercept[java.nio.file.AccessDeniedException](LocalFiles.linked(inside))
    finally icacls("/grant", System.getProperty("user.name") + ":(OI)(CI)F")
  }
