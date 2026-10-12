package desk

object Indicators:
  final case class Period(
      name: String,
      ms: Long,
      window: Polymarket.Window,
      scale: Double,
      scaleText: String,
      correlation: Option[(String, Int)]
  )
  val periods: Vector[Period] = Vector(
    Period("5m", 300_000L, Polymarket.Window.M5, 0.5, "±0.5%", None),
    Period("15m", 900_000L, Polymarket.Window.M15, 1.0, "±1%", Some(("1m", 15))),
    Period("1h", 3_600_000L, Polymarket.Window.H1, 2.0, "±2%", Some(("1m", 60))),
    Period("4h", 14_400_000L, Polymarket.Window.H4, 3.0, "±3%", Some(("5m", 48))),
    Period("24h", 86_400_000L, Polymarket.Window.D1, 6.0, "±6%", Some(("15m", 96)))
  )

  // One measure of a row: its value, its mark at a value, its text after the prefix and the
  // text's ink. A value that is absent or not finite gives no mark and the text --.
  private final case class Reading(
      value: Option[Double],
      mark: Double => RailMark,
      prefix: String,
      text: Double => String,
      ink: String
  )

  private def row(label: String, readings: Vector[Reading]): RailRow =
    val shown = readings.map(r => r -> r.value.filter(_.isFinite))
    RailRow(
      label,
      shown.flatMap((r, v) => v.map(r.mark)),
      shown.map((r, v) => (r.prefix + v.fold("--")(r.text), r.ink))
    )

  private def percent(v: Double): String = f"$v%+.2f%%"
  private def share(v: Double): String = s"${Screen.round(v)}%"
  private def label(p: Period): String = p.name.padTo(4, ' ') + p.scaleText
  private val sides = Vector("down", "neutral", "up")

  // The rails' four groups. Compute them once per frame: an hour or longer measure reads up to
  // 5200 one minute bars.
  def groups(
      s: Snapshot,
      chance: (String, Polymarket.Window) => Option[(Double, Boolean)],
      now: Long,
      style: Style
  ): Vector[RailGroup] =
    // Each coin's samples and its one minute closes, built once a frame.
    val selected = (s.samples, Quant.closes(s.candles.getOrElse("1m", Vector.empty)))
    def series(coin: String): (Vector[(Long, Double)], Map[Long, Double]) =
      if coin == s.coin then selected
      else
        val ref = s.refs.getOrElse(coin, Ref.empty)
        (ref.samples, Quant.closes(ref.candles.getOrElse("1m", Vector.empty)))
    def correlation(coin: String, p: Period): Option[Double] =
      val ref = s.refs.getOrElse(coin, Ref.empty)
      p.correlation match
        case None                => Quant.compareSamples(s.samples, ref.samples, now).value
        case Some((interval, n)) =>
          Quant
            .compare(
              s.candles.getOrElse(interval, Vector.empty),
              ref.candles.getOrElse(interval, Vector.empty),
              interval,
              n,
              now
            )
            .value
    // The chance of up, its mark and its value faint when stale.
    def up(coin: String, p: Period, ink: String): Reading =
      val odds = chance(coin, p.window)
      val shade = if odds.exists(_._2) then Screen.dim else ink
      Reading(odds.map(_._1), u => RailMark((u - 50) / 50, '●', shade), "●", share, shade)

    val bookTitle =
      Vector(("◆ book", style.book), ("● tape", style.tape), ("share of buying", Screen.dim))
    val marketTitle =
      Vector((s.coin, Screen.bold), ("▲ change", ""), ("◇ vs average", ""), ("● Polymarket up", ""))
    def assetTitle(name: String, ink: String) =
      Vector(
        (name, ink + Screen.bold),
        ("▲ change", ink),
        ("◆ correlation", ink),
        ("● Polymarket up", ink)
      )

    val book = RailGroup(
      bookTitle,
      4,
      0,
      100,
      Vector("0%", "50%", "100%"),
      13,
      periods.map { p =>
        row(
          p.name,
          Vector(
            Reading(
              Shares.book(s, p.ms, now),
              RailMark(_, '◆', style.book),
              "B ",
              share,
              style.book
            ),
            Reading(
              Shares.tape(s, p.ms, now),
              RailMark(_, '●', style.tape),
              "T ",
              share,
              style.tape
            )
          )
        )
      }
    )
    val market =
      val (samples, bars) = series(s.coin)
      RailGroup(
        marketTitle,
        10,
        -1,
        1,
        sides,
        21,
        periods.map { p =>
          row(
            label(p),
            Vector(
              Reading(
                Quant.change(samples, bars, p.ms, now).value,
                v => RailMark(v / p.scale, '▲', Screen.bold),
                "▲",
                percent,
                ""
              ),
              Reading(
                Quant.distance(samples, bars, p.ms, now).value,
                v => RailMark(v / p.scale, '◇', ""),
                "◇",
                percent,
                ""
              ),
              up(s.coin, p, "")
            )
          )
        }
      )
    def asset(coin: String, name: String, ink: String) =
      val (samples, bars) = series(coin)
      RailGroup(
        assetTitle(name, ink),
        10,
        -1,
        1,
        sides,
        21,
        periods.map { p =>
          row(
            label(p),
            Vector(
              Reading(
                Quant.change(samples, bars, p.ms, now).value,
                v => RailMark(v / p.scale, '▲', ink),
                "▲",
                percent,
                ink
              ),
              Reading(
                if coin == s.coin then None else correlation(coin, p),
                RailMark(_, '◆', ink),
                "◆",
                v => f"$v%+.2f",
                ink
              ),
              up(coin, p, ink)
            )
          )
        }
      )
    Vector(book, market, asset("BTC", "BTC", style.bitcoin), asset("PAXG", "Gold", style.gold))

  // The chart's key: each entry's glyph in its ink, then its word. A Polymarket window with no
  // market is faint.
  def key(coin: String, style: Style, live: Polymarket.Window => Boolean): String =
    import Polymarket.Window.*
    def window(w: Polymarket.Window, ink: String, word: String) =
      ("■", if live(w) then ink + Screen.bold else Screen.dim, word)
    Vector(
      ("■", Screen.bold, coin),
      ("■", style.bid + Screen.bold, "buy"),
      ("■", style.ask + Screen.bold, "sell"),
      ("▲", style.bid + Screen.bold, "buy fill"),
      ("▼", style.ask + Screen.bold, "sell fill"),
      window(M5, style.w5, "5m"),
      window(M15, style.w15, "15m"),
      window(H1, style.w1h, "1h"),
      window(H4, style.w4h, "4h"),
      ("■", style.w1d, "1d"),
      ("◆", style.book + Screen.bold, "book"),
      ("●", style.tape + Screen.bold, "tape"),
      ("■", style.bitcoin + Screen.bold, "BTC"),
      ("■", style.gold + Screen.bold, "gold"),
      ("▲", Screen.bold, "change"),
      ("◇", Screen.bold, "vs average"),
      ("◆", Screen.bold, "correlation"),
      ("●", Screen.bold, "Polymarket"),
      ("◉", style.overlap, "overlap")
    ).map((glyph, ink, word) => ink + glyph + Screen.reset + " " + word).mkString("  ")
