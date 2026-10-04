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
        "e0547c3d959d6d05ca045d5b1f98ad07d73df592368eb8565b798818c0429191"
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
    assertEquals(digest(lines), "6955154499f4d7643110445073671f84867d7e2ffc0d782f5afc1bf21dc27652")
  }
