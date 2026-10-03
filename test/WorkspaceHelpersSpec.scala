package desk

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.util.HexFormat

class WorkspaceHelpersSpec extends munit.FunSuite:
  private def digest(lines: Vector[String]): String =
    HexFormat
      .of()
      .formatHex(MessageDigest.getInstance("SHA-256").digest(lines.mkString("\n").getBytes(UTF_8)))

  test("indicator bytes stay fixed across every warm-up boundary and style") {
    val snapshot = CompactDisplayFixtures.snapshot()
    val ages = (Vector(0L) ++ (Gauges.bookLenses ++ Gauges.tapeLenses)
      .flatMap((_, seconds) => Vector(seconds * 1000 - 1, seconds * 1000, seconds * 1000 + 1)))
      .filter(_ >= 0)
      .distinct
      .sorted
    val hashes = Vector(Style.mono, Style.colour).map { style =>
      digest((for
        width <- Vector(24, 35, 50)
        age <- ages
        row <- Workspace.indicatorRows(
          snapshot.copy(since = CompactDisplayFixtures.now - age),
          CompactDisplayFixtures.now,
          width,
          style
        )
      yield row).toVector)
    }
    assertEquals(
      hashes,
      Vector(
        "eb5e5a7d07e8f9e3d1b38c11b60011b6d2dbd7c6359d11f6dea627bd75ec796c",
        "d514772990ceb8d3f1a392608b44f42cbc2ada0c424d87085882bc35ace30e7f"
      )
    )
  }

  test("workspace frame bytes stay fixed for compact display fixtures") {
    val lines = for
      coin <- Vector("XMR", "BTC", "PAXG")
      mode <- ChartMode.values.toVector
      style <- Vector(Style.mono, Style.colour)
      (cols, rows) <- CompactDisplayFixtures.sizes
      line <- CompactDisplayFixtures
        .draw(
          CompactDisplayFixtures.snapshot(coin, mode, cols, rows),
          CompactDisplayFixtures.panel(coin),
          style
        )
        .lines
    yield line
    assertEquals(digest(lines), "d6caf715a60cc2f0bec4027a2339f83a3aa36b3c5d36bea2c9826cdbcb5a3994")
  }
