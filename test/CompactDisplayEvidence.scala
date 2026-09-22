package desk

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.ZoneOffset

object CompactDisplayFixtures:
  val now = 1_800_000_000_000L
  val sizes = Vector((100, 30), (120, 40), (160, 45))
  val sourceText =
    "News partial. Missing source: calendar. Full source coverage remains available here."
  val sourceUrl = "https://example.invalid/markets/independent-reference-review/source-record"

  def mid(coin: String): Double = coin match
    case "BTC"  => 95000.0
    case "PAXG" => 2320.0
    case _      => 100.0

  private def referencePrice(coin: String, age: Int): Double =
    val relative = if age == 0 then 10 else age % 41
    coin match
      case "BTC"  => 90000.0 + relative * 500.0
      case "PAXG" => 2300.0 + relative * 2.0
      case _      => 96.0 + relative * 0.2

  def histories(coin: String): Map[String, Vector[Candle]] =
    val closes = (0 until 300).foldLeft(Vector(100.0)) { (values, i) =>
      values :+ values.last * math.exp((i % 7 - 3) * 0.002)
    }
    State.intervals.map { interval =>
      val step = State.intervalMs(interval)
      val end = now / step * step - step
      interval -> closes.zipWithIndex.map { (base, i) =>
        val price = coin match
          case "BTC"  => base * 950.0
          case "PAXG" => 232000.0 / base
          case _      => base
        Candle(end - (closes.size - 1L - i) * step, price, price * 1.002, price * 0.998, price, 1.0)
      }
    }.toMap

  def samples(coin: String): Vector[(Long, Double)] =
    (3600 to 0 by -1).toVector.map(age => (now / 1000 - age, referencePrice(coin, age)))

  def snapshot(
      coin: String = "XMR",
      mode: ChartMode = ChartMode.Line,
      cols: Int = 100,
      rows: Int = 30,
      offset: Long = 0L
  ): Snapshot =
    val price = mid(coin)
    Snapshot
      .initial(coin, now)
      .copy(
        book = Some(
          Book(
            Vector(Level(BigDecimal(price * 0.99).toString, "5", 1)),
            Vector(Level(BigDecimal(price * 1.01).toString, "8", 1)),
            now
          )
        ),
        feed = Health.Live(now),
        since = now - 20_000L,
        samples = samples(coin),
        candles = histories(coin),
        refs = Vector("BTC", "PAXG").map(c => c -> Ref(samples(c), histories(c))).toMap,
        tape = Vector(
          Trade(now - 500L, Side.Buy, price.toString, "3"),
          Trade(now - 15_000L, Side.Sell, price.toString, "1")
        ),
        workspace = true,
        chartMode = mode,
        zoom = if mode == ChartMode.Candles then State.candleMinZoom else 0,
        cols = cols,
        rows = rows,
        offsetSec = offset
      )

  def outlook(coin: String): OutlookView =
    val price = mid(coin)
    OutlookView(
      coin,
      now,
      Outlook.horizons.zipWithIndex.map { (h, i) =>
        HorizonOutlook(
          h,
          OutlookDirection.Up,
          "ready",
          Some(price * (0.92 + i * 0.02)),
          Some(price * (1.08 - i * 0.01)),
          Some(price * (1.08 - i * 0.01)),
          Some(price),
          None,
          HistoricalRate(60, 100, 100),
          HistoricalRate(35, 50, 50),
          100,
          None,
          None
        )
      }
    )

  def panel(coin: String = "XMR"): PanelView = PanelView(
    mode = "PAPER",
    outlook = outlook(coin),
    newsStatus = "News partial. Missing calendar",
    soundStatus = "Sound on",
    newsDetails = sourceText,
    alerts = Vector(
      Announcement(
        "fixture",
        now,
        "news",
        "A source headline remains fully readable in Details.",
        Some(sourceUrl),
        Some(now - 600000L),
        Some(now - 1000L)
      )
    ),
    messages = Vector(
      DeskMessage(
        1L,
        now,
        "analysis",
        "This visible analysis has both Accept and Reject actions. Inspect complete review values before submitting any order. " * 3
      )
    )
  )

  def trading(coin: String = "XMR"): TradingView = TradingView(
    coin,
    position = Some(BigDecimal(0)),
    message = "Paper ready",
    ready = true,
    rules = Some(MarketRules(0, 4, 5)),
    bid = Some(BigDecimal(mid(coin) * 0.99)),
    ask = Some(BigDecimal(mid(coin) * 1.01))
  )

  val review: OrderReview =
    val quantity = BigDecimal("5.4321")
    val limit = BigDecimal("101")
    val intent = OrderIntent(Side.Buy, OrderKind.Hard)
    OrderReview(
      intent,
      OrderPlan(
        "XMR",
        0,
        Side.Buy,
        OrderKind.Hard,
        quantity,
        limit,
        limit,
        quantity,
        quantity * limit,
        Some(limit),
        quantity
      ),
      now,
      now + 30000L
    )

  def inactive(kind: String): Snapshot =
    val original = snapshot()
    def changed(series: Map[String, Vector[Candle]]): Map[String, Vector[Candle]] =
      series.map { (interval, cs) =>
        val result = kind match
          case "empty"   => Vector.empty
          case "partial" => cs.dropRight(2) ++ cs.takeRight(1)
          case "stale"   => cs.dropRight(1)
          case "flat"    => cs.map(c => c.copy(o = 100, h = 100, l = 100, c = 100))
          case _         => cs
        interval -> result
      }
    original.copy(
      candles = changed(original.candles),
      refs = original.refs.map { (coin, ref) =>
        coin -> ref.copy(candles = changed(ref.candles))
      }
    )

  def draw(s: Snapshot, p: PanelView, style: Style = Style.mono): WorkspaceFrame =
    Workspace.draw(s, trading(s.coin), p, s.cols, s.rows, now, ZoneOffset.UTC, style)

object CompactDisplayEvidence:
  def main(args: Array[String]): Unit =
    require(args.length == 1, "provide one owned capture directory")
    val directory = Path.of(args(0))
    val _ = Files.createDirectories(directory)
    val metadata = Vector.newBuilder[ujson.Value]
    def capture(name: String, s: Snapshot, p: PanelView, style: Style): Unit =
      val frame = CompactDisplayFixtures.draw(s, p, style)
      val plain = frame.lines.map(Chart.plain)
      require(
        plain.size == s.rows && plain.forall(_.length == s.cols),
        name + " has wrong geometry"
      )
      val _ = Files.writeString(
        directory.resolve(name + ".ansi"),
        frame.lines.mkString("\n") + "\n",
        StandardCharsets.UTF_8
      )
      val _ = Files.writeString(
        directory.resolve(name + ".txt"),
        plain.mkString("\n") + "\n",
        StandardCharsets.UTF_8
      )
      metadata += ujson.Obj(
        "name" -> name,
        "cols" -> s.cols,
        "rows" -> s.rows,
        "coin" -> s.coin,
        "mode" -> s.chartMode.toString,
        "offsetSec" -> s.offsetSec,
        "details" -> p.details,
        "page" -> p.detailPage.toString,
        "completeReview" -> frame.completeReview,
        "controls" -> ujson.Arr.from(
          frame.controls.map(b =>
            ujson.Obj(
              "label" -> b.label,
              "x" -> b.x,
              "y" -> b.y,
              "width" -> b.width,
              "action" -> b.action.toString
            )
          )
        )
      )
    import CompactDisplayFixtures.*
    for
      (cols, rows) <- sizes
      mode <- ChartMode.values
      coin <- Vector("XMR", "BTC", "PAXG")
      history <- Vector(false, true)
      (styleName, style) <- Vector("mono" -> Style.mono, "colour" -> Style.colour)
    do
      val offset = if history then (if mode == ChartMode.Line then 60L else 600L) else 0L
      val s = snapshot(coin, mode, cols, rows, offset)
      capture(
        s"market-$coin-$mode-${cols}x$rows-${if history then "history" else "live"}-$styleName",
        s,
        panel(coin),
        style
      )
    for
      (cols, rows) <- sizes
      (kind, p) <- Vector(
        "method" -> panel().copy(details = true, detailPage = DetailPage.Method),
        "sources" -> panel().copy(details = true, detailPage = DetailPage.Sources),
        "answers" -> panel().copy(details = true, detailPage = DetailPage.Answers),
        "ask" -> panel().copy(
          details = true,
          detailPage = DetailPage.Answers,
          input = Some("Confirm the upper, current and lower reference prices.")
        ),
        "review" -> panel().copy(review = Some(review))
      )
      (styleName, style) <- Vector("mono" -> Style.mono, "colour" -> Style.colour)
    do capture(s"$kind-${cols}x$rows-$styleName", snapshot(cols = cols, rows = rows), p, style)
    for
      kind <- Vector("empty", "partial", "stale", "flat")
      (styleName, style) <- Vector("mono" -> Style.mono, "colour" -> Style.colour)
    do capture(s"correlation-$kind-100x30-$styleName", inactive(kind), panel(), style)
    val maximumTape =
      snapshot().copy(tape = Vector.fill(State.tapeCap)(Trade(now - 1L, Side.Buy, "100", "1")))
    for (styleName, style) <- Vector("mono" -> Style.mono, "colour" -> Style.colour) do
      capture(s"maximum-tape-100x30-$styleName", maximumTape, panel(), style)
    val all = metadata.result()
    val _ = Files.writeString(
      directory.resolve("manifest.json"),
      ujson.write(ujson.Arr.from(all), indent = 2),
      StandardCharsets.UTF_8
    )
    println(s"Captured ${all.size} frames in $directory")
