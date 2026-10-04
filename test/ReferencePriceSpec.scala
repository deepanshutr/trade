package desk

import java.time.ZoneOffset

class ReferencePriceSpec extends munit.FunSuite:
  private val now = 1800000000000L
  // A chart area of 100 columns with 19 price rows, the terminal being that plus the outlook.
  private def render(s: Snapshot): Screen.MarketFrame =
    Screen.renderMarket(
      s,
      now,
      ZoneOffset.UTC,
      Style.mono,
      Layout.regions(100 + Layout.outlookWidth + 1, 40).copy(priceRows = 19),
      AxisDecor.plain
    )
  private val prices =
    Vector((now / 1000 - 10, 200.0), (now / 1000 - 5, 240.0), (now / 1000, 210.0))

  test("reference chart keeps upper and lower bounds with the actual current price between") {
    val s = Snapshot
      .initial("XMR", now)
      .copy(
        book = Some(Book(Vector(Level("99", "10", 1)), Vector(Level("101", "10", 1)), now)),
        refs = Map(
          "BTC" -> Ref(prices, Map.empty),
          "PAXG" -> Ref(prices.map((t, p) => t -> (p * 10)), Map.empty)
        )
      )
    val frame = render(s)
    val first = frame.priceRows + 2
    // The price column is the 8 columns after the label of 30, beside the plot.
    val btc = frame.lines
      .slice(first, first + 3)
      .map(line => Chart.plain(line).slice(30, 38).trim)
    val gold = frame.lines
      .slice(first + 3, first + 6)
      .map(line => Chart.plain(line).slice(30, 38).trim)
    assertEquals(btc, Vector("242.00", "210>", "178.00"))
    assertEquals(gold, Vector("2420.0", "2100>", "1780.0"))
  }

  test("empty reference history never invents a current price") {
    val frame = render(Snapshot.initial("XMR", now))
    val first = frame.priceRows + 2
    assert(
      frame.lines
        .slice(first, first + 6)
        .forall(line => Chart.plain(line).slice(30, 38).trim.isEmpty)
    )
  }
