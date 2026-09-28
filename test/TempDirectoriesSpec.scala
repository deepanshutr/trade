package desk

import java.nio.file.{Files, Path}

class TempDirectoriesSpec extends TempDirectorySuite:
  private var testFolder = Option.empty[Path]
  private val sharedFolder = suiteDirectory("suite-")

  test("successful cleanup removes nested files and links without following links") {
    val outside = tempDirectory("outside-")
    val target = Files.writeString(outside.resolve("saved"), "preserved")
    val root = tempDirectory("nested-", keepOnFailure = true)
    val ordinary = tempDirectory("ordinary-")
    val nested = Files.createDirectories(root.resolve("a/b"))
    Files.writeString(nested.resolve("data"), "data")
    Files.createSymbolicLink(root.resolve("link"), outside)

    TempDirectories.finish(List(root -> true, ordinary -> false), failed = false)

    assert(root.getFileName.toString.startsWith("desk-"))
    assert(!Files.exists(root))
    assert(!Files.exists(ordinary))
    assertEquals(Files.readString(target), "preserved")
  }

  test("failed cleanup deletes ordinary folders and retains only requested evidence") {
    val parent = tempDirectory("failure-check-")
    val ordinary = tempDirectory("ordinary-", parent = Some(parent))
    val evidence = tempDirectory("capture-", parent = Some(parent), keepOnFailure = true)
    Files.writeString(evidence.resolve("capture"), "evidence")
    val bytes = new java.io.ByteArrayOutputStream()
    val output = new java.io.PrintStream(bytes)
    val previous = System.err
    try
      System.setErr(output)
      TempDirectories.finish(List(ordinary -> false, evidence -> true), failed = true)
    finally
      System.setErr(previous)
      output.close()
    assert(!Files.exists(ordinary))
    assertEquals(Files.readString(evidence.resolve("capture")), "evidence")
    assertEquals(bytes.toString.trim, s"Test evidence: $evidence")
  }

  test("real suite test creates per-test and shared files") {
    val root = tempDirectory("real-test-", keepOnFailure = true)
    testFolder = Some(root)
    Files.writeString(root.resolve("saved"), "temporary")
    Files.writeString(sharedFolder.resolve("saved"), "shared")
    assert(Files.exists(root))
  }

  test("real suite removes the previous test folder and preserves shared files") {
    assert(!Files.exists(testFolder.get))
    assertEquals(Files.readString(sharedFolder.resolve("saved")), "shared")
  }

  override def afterAll(): Unit =
    super.afterAll()
    assert(!Files.exists(sharedFolder))
