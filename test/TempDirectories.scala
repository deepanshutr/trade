package desk

import java.nio.file.{Files, Path}
import scala.collection.mutable.ArrayBuffer

object TempDirectories:
  def remove(directory: Path): Unit =
    if Files.exists(directory) then
      val paths = Files.walk(directory)
      try paths.sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))
      finally paths.close()

abstract class TempDirectorySuite extends munit.FunSuite:
  private val directories = ArrayBuffer.empty[(Path, Boolean)]
  private val suiteDirectories = ArrayBuffer.empty[Path]

  protected def tempDirectory(
      prefix: String,
      parent: Option[Path] = None,
      keepOnFailure: Boolean = false
  ): Path =
    val name = if prefix.startsWith("desk-") then prefix else "desk-" + prefix
    val path = parent match
      case Some(root) => Files.createTempDirectory(root, name)
      case None       => Files.createTempDirectory(name)
    directories += ((path, keepOnFailure))
    path

  protected def suiteDirectory(prefix: String): Path =
    val path = tempDirectory(prefix)
    directories.remove(directories.size - 1)
    suiteDirectories += path
    path

  override def munitTestTransforms: List[TestTransform] =
    super.munitTestTransforms :+ new TestTransform(
      "temporary directories",
      test =>
        test.withBodyMap(_.transform { result =>
          try
            directories.reverseIterator.foreach { (path, keep) =>
              if result.isFailure && keep then System.err.println(s"Test evidence: $path")
              else TempDirectories.remove(path)
            }
          finally directories.clear()
          result
        }(munitExecutionContext))
    )

  override def afterAll(): Unit =
    try suiteDirectories.reverseIterator.foreach(TempDirectories.remove)
    finally super.afterAll()
