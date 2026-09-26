package desk

import java.time.ZoneOffset

class ReferencePriceSpec extends munit.FunSuite:
  private val now = 1800000000000L
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
    val frame = Screen.renderMarket(s, 100, 19, now, ZoneOffset.UTC, Style.mono)
    val first = frame.priceRows + 2
    val btc = frame.lines
      .slice(first, first + 3)
      .map(line => Chart.plain(line).takeRight(Layout.scaleW).trim)
    val gold = frame.lines
      .slice(first + 3, first + 6)
      .map(line => Chart.plain(line).takeRight(Layout.scaleW).trim)
    assertEquals(btc, Vector("242.00", "<210", "178.00"))
    assertEquals(gold, Vector("2420.0", "<2100", "1780.0"))
  }

  test("empty reference history never invents a current price") {
    val frame = Screen.renderMarket(
      Snapshot.initial("XMR", now),
      100,
      19,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    val first = frame.priceRows + 2
    assert(
      frame.lines
        .slice(first, first + 6)
        .forall(line => Chart.plain(line).takeRight(Layout.scaleW).trim.isEmpty)
    )
  }
