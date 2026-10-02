package desk

import java.time.ZoneOffset

object CompactDisplayFixtures:
  val now = 1_800_000_000_000L
  val sizes = Vector((100, 30), (120, 40), (160, 45))

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
          HistoricalRate(35, 50, 50)
        )
      }
    )

  def panel(coin: String = "XMR"): PanelView = PanelView(
    mode = "PROP",
    outlook = outlook(coin)
  )

  def trading(coin: String = "XMR"): TradingView = TradingView(
    coin,
    position = Some(BigDecimal(0)),
    message = "Prop ready",
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
