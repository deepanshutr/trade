package desk

import java.nio.file.{Files, Path}
import java.time.ZoneOffset

object LeanWorkspaceEvidence:
  @volatile private var sink: Any = null

  def main(args: Array[String]): Unit =
    require(args.length == 1, "provide one owned evidence directory")
    val directory = Path.of(args(0))
    Files.createDirectories(directory)
    import CompactDisplayFixtures.*
    val portfolio = PortfolioView(
      Some(BigDecimal("510.75")),
      Some(BigDecimal("9942.14")),
      Some(BigDecimal("12.34")),
      Some(BigDecimal("99.2")),
      Some(BigDecimal("96.4"))
    )
    val ready = trading().copy(position = Some(BigDecimal("5.1075")), portfolio = portfolio)
    val cases = Vector(
      "ready" -> ready,
      "sold" -> ready.copy(portfolio = portfolio.copy(tokenValue = Some(BigDecimal("-510.75")))),
      "saving" -> ready.copy(ready = false, pending = true, message = "PAPER saving 4s | XMR"),
      "waiting" -> ready.copy(
        ready = false,
        block = Some("selected depth stale"),
        portfolio = PortfolioView()
      )
    )
    val manifest = Vector.newBuilder[ujson.Value]
    for
      (cols, rows) <- Vector((100, 30), (101, 31), (120, 40), (160, 45))
      mode <- ChartMode.values
      (label, view) <- cases
      (styleName, style) <- Vector("mono" -> Style.mono, "colour" -> Style.colour)
    do
      val s = snapshot(mode = mode, cols = cols, rows = rows)
      val frame = Workspace.draw(s, view, panel(), cols, rows, now, ZoneOffset.UTC, style)
      val plain = frame.lines.map(Chart.plain)
      require(plain.size == rows && plain.forall(_.length == cols), "geometry")
      val name = s"$label-$mode-$cols-$rows-$styleName"
      Files.writeString(directory.resolve(name + ".ansi"), frame.lines.mkString("\n") + "\n")
      Files.writeString(directory.resolve(name + ".txt"), plain.mkString("\n") + "\n")
      manifest += ujson.Obj(
        "name" -> name,
        "cols" -> cols,
        "rows" -> rows,
        "buyCells" -> frame.controls
          .filter(_.action == DeskAction.Prepare(OrderIntent(Side.Buy, OrderKind.Hard)))
          .map(b => b.width * b.height)
          .sum
      )
    Files.writeString(
      directory.resolve("frames.json"),
      ujson.write(ujson.Arr.from(manifest.result()), indent = 2)
    )
    val retained = DisplayCost.retained(6, 200000).copy(workspace = true)
    val measures = Vector((100, 30), (120, 40), (160, 45)).map { (cols, rows) =>
      def draw(): Unit =
        sink = Workspace.draw(
          retained,
          ready,
          panel(),
          cols,
          rows,
          Golden.now,
          ZoneOffset.UTC,
          Style.colour
        )
      for _ <- 0 until 100 do draw()
      val samples = Vector
        .fill(300) {
          val at = System.nanoTime()
          draw()
          (System.nanoTime() - at).toDouble / 1000000
        }
        .sorted
      ujson.Obj(
        "cols" -> cols,
        "rows" -> rows,
        "samples" -> samples.size,
        "p50_ms" -> samples(149),
        "p95_ms" -> samples(284),
        "p99_ms" -> samples(296),
        "max_ms" -> samples.last
      )
    }
    val result = ujson.Obj(
      "retained_hours" -> 6,
      "retained_trades" -> 200000,
      "runs" -> ujson.Arr.from(measures)
    )
    Files.writeString(directory.resolve("render-cost.json"), ujson.write(result, indent = 2))
    println(ujson.write(result))
