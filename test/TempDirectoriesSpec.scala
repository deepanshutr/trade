package desk

import java.nio.file.{Files, Path}
import scala.concurrent.Await
import scala.concurrent.duration.*

class TempDirectoriesSpec extends TempDirectorySuite:
  private def run(suite: TempDirectorySuite): Unit =
    Await.result(suite.munitTests().head.body(), 5.seconds)

  test("successful tests remove nested files and links without following links") {
    val outside = tempDirectory("outside-")
    val target = Files.writeString(outside.resolve("saved"), "preserved")
    var created = Option.empty[Path]
    val suite = new TempDirectorySuite:
      test("nested files") {
        val root = tempDirectory("nested-", keepOnFailure = true)
        created = Some(root)
        val nested = Files.createDirectories(root.resolve("a/b"))
        Files.writeString(nested.resolve("data"), "data")
        Files.createSymbolicLink(root.resolve("link"), outside)
      }
    run(suite)
    assert(created.get.getFileName.toString.startsWith("desk-"))
    assert(!Files.exists(created.get))
    assertEquals(Files.readString(target), "preserved")
  }

  test("failed tests delete ordinary folders and retain only requested evidence") {
    var ordinary = Option.empty[Path]
    var evidence = Option.empty[Path]
    val parent = tempDirectory("failure-check-")
    val suite = new TempDirectorySuite:
      test("failure") {
        ordinary = Some(tempDirectory("ordinary-", parent = Some(parent)))
        evidence = Some(tempDirectory("capture-", parent = Some(parent), keepOnFailure = true))
        Files.writeString(evidence.get.resolve("capture"), "evidence")
        throw new IllegalStateException("expected failure")
      }
    val bytes = new java.io.ByteArrayOutputStream()
    val output = new java.io.PrintStream(bytes)
    val previous = System.err
    try
      System.setErr(output)
      interceptMessage[IllegalStateException]("expected failure")(run(suite))
    finally
      System.setErr(previous)
      output.close()
    assert(!Files.exists(ordinary.get))
    assertEquals(Files.readString(evidence.get.resolve("capture")), "evidence")
    assertEquals(bytes.toString.trim, s"Test evidence: ${evidence.get}")
  }

  test("suite folders survive individual tests and are removed by afterAll") {
    class SharedSuite extends TempDirectorySuite:
      val root = suiteDirectory("suite-")
      test("first") {
        Files.writeString(root.resolve("saved"), "shared")
      }
      test("second") {
        assertEquals(Files.readString(root.resolve("saved")), "shared")
      }
    val suite = new SharedSuite
    try
      suite.munitTests().foreach(test => Await.result(test.body(), 5.seconds))
      assert(Files.exists(suite.root))
    finally suite.afterAll()
    assert(!Files.exists(suite.root))
  }
