package desk

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class SuiteConstructionSpec extends munit.FunSuite:
  private def deskFolders(): Set[Path] =
    val paths = Files.list(Path.of(System.getProperty("java.io.tmpdir")))
    try
      paths
        .iterator()
        .asScala
        .filter { path =>
          val name = path.getFileName.toString
          Files.isDirectory(path) &&
          (name.startsWith("desk-live-readiness-") || name.startsWith("desk-suite-"))
        }
        .toSet
    finally paths.close()

  List[(String, () => TempDirectorySuite)](
    "LiveReadinessAuditSpec" -> (() => new LiveReadinessAuditSpec),
    "TempDirectoriesSpec" -> (() => new TempDirectoriesSpec)
  ).foreach { (name, construct) =>
    test(s"constructing $name without running tests creates no folders") {
      val before = deskFolders()
      val suite = construct()
      val created = deskFolders() -- before
      try assertEquals(created, Set.empty[Path])
      finally if created.nonEmpty then suite.afterAll()
    }
  }
