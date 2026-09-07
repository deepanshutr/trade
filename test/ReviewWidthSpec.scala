package desk

import java.util.concurrent.atomic.AtomicReference

class ReviewWidthSpec extends munit.FunSuite:
  for cols <- Vector(100, 120) do
    test(s"a selected integral limit longer than $cols columns cannot acknowledge a frame") {
      val now = 1000L
      val coin = "XMR"
      val rules = MarketRules(0, 0, 5)
      val bid = "8" * (cols + 1)
      val ask = "9" * (cols + 1)
      val book = Book(Vector(Level(bid, "1", 1)), Vector(Level(ask, "1", 1)), now)
      val snapshot = Snapshot.initial(coin, now).copy(book = Some(book), feed = Health.Live(now))
      val view = TradingView(
        coin,
        Some(BigDecimal(1)),
        "Ready",
        ready = true,
        rules = Some(rules),
        bid = Some(BigDecimal(bid)),
        ask = Some(BigDecimal(ask))
      )
      val (_, target) = Main.handleTrade(
        Keys.step(Vector.empty, 57, false)._2.get,
        snapshot,
        false,
        (cols, 30),
        view,
        Some(coin),
        0,
        None,
        (_, _) => fail("selection sent an order")
      )
      assertEquals(target, Some(Side.Buy -> BigDecimal(ask)))
      val preview: OrderIntent => Either[String, OrderPlan] = intent =>
        Trading.plan(
          coin,
          rules,
          book,
          view.reference(intent.side).get,
          BigDecimal(1),
          intent,
          TradeSizes()
        )
      val intent = OrderIntent(Side.Buy, OrderKind.Target, target = target.map(_._2))
      assertEquals(preview(intent), Left("Book contains an invalid number"))
      assertEquals(Trading.displayExact(target.get._2), ask)
      val footer = TradingScreen.rows(view, target, preview, cols = cols)
      val rendered = Screen
        .render(snapshot, cols, 30, now, java.time.ZoneId.of("UTC"), Style.mono, footer)
        .map(Chart.plain)
      val shown = new AtomicReference(Option.empty[String])
      Main.writeMarketFrame(
        coin,
        (cols, 30),
        shown,
        () => (),
        complete = footer.size == Layout.footerRows
      )
      var attempts = 0
      val _ = Main.handleTrade(
        Keys.step(Vector.empty, 13, false)._2.get,
        snapshot,
        false,
        (cols, 30),
        view,
        shown.get(),
        0,
        target,
        (requested, market) => {
          assertEquals(market, coin)
          attempts += 1
          preview(requested).isRight
        }
      )
      println(
        s"WIDTH $cols exact ${ask.length} footer lengths ${footer.map(_.length)} " +
          s"rendered exact ${rendered.exists(_.contains(ask))} shown ${shown.get()} attempts $attempts"
      )
      println(rendered.takeRight(4).mkString("\n"))
      assertEquals(shown.get(), None)
      assertEquals(attempts, 0)
      assertEquals(footer.size, 1)
      assert(footer.head.contains("Order keys disabled"))
      assertEquals(rendered.size, 30)
      assert(rendered.forall(_.length == cols))
    }
