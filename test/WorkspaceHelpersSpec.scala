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
    val frames = for
      coin <- Vector("XMR", "BTC", "PAXG")
      mode <- ChartMode.values.toVector
      style <- Vector(Style.mono, Style.colour)
      (cols, rows) <- CompactDisplayFixtures.sizes
    yield (
      rows,
      CompactDisplayFixtures
        .draw(
          CompactDisplayFixtures.snapshot(coin, mode, cols, rows),
          CompactDisplayFixtures.panel(coin),
          style
        )
        .lines
    )
    assertEquals(
      digest(frames.flatMap(_._2)),
      "f3fa0c152f26e3f4413dfbab9f0eca8302d21ba6ce8ae2d41f4a8c27977d83a2"
    )
    // The band rows from tradeTop down hash as they did before the chart block changed.
    assertEquals(
      digest(frames.flatMap((rows, lines) => lines.drop(rows - 15))),
      "edf4e1a9bad4efb82d521023a1ea8fd742a4c249b3f018ec21c657b1412fbd9a"
    )
  }
