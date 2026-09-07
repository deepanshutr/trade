package desk

import java.util.concurrent.atomic.AtomicReference

class ReviewReachabilitySpec extends munit.FunSuite:
  test("the accepted public maximum followed by Target Up requires an exact full frame") {
    val now = 1000L
    val coin = "XMR"
    val bidText = "9" + "0" * 99
    val askText = "1" + "0" * 100
    val rules = MarketRules(0, 0, 5)
    def level(price: String) = ujson.Obj("px" -> price, "sz" -> "1", "n" -> 1)
    val payload = ujson.Obj(
      "channel" -> "l2Book",
      "data" -> ujson.Obj(
        "coin" -> coin,
        "time" -> ujson.Num(now.toDouble),
        "levels" -> ujson.Arr(ujson.Arr(level(bidText)), ujson.Arr(level(askText)))
      )
    )
    val event = Events.parse(payload.render()).get
    val book = event match
      case Event.BookUpdate(`coin`, value, true) => value
      case other => fail(s"public parser rejected the allowed boundary: $other")
    val snapshot = State.reduce(Snapshot.initial(coin, now), event, now)
    val view = TradingView(
      coin,
      Some(BigDecimal(1)),
      "Ready",
      ready = true,
      rules = Some(rules),
      bid = Some(BigDecimal(book.bids.head.px)),
      ask = Some(BigDecimal(book.asks.head.px))
    )
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
    val cols = 100
    def handle(command: Command, target: Option[(Side, BigDecimal)]) =
      Main
        .handleTrade(
          command,
          snapshot,
          false,
          (cols, 30),
          view,
          Some(coin),
          0,
          target,
          (_, _) => fail("selection or its price key sent an order")
        )
        ._2
    val initial = handle(Keys.step(Vector.empty, 57, false)._2.get, None)
    assertEquals(initial, Some(Side.Buy -> BigDecimal(askText)))
    assertEquals(Trading.displayExact(initial.get._2), "1e100")
    val initialFooter = TradingScreen.rows(view, initial, preview, cols = cols)
    assertEquals(initialFooter.size, Layout.footerRows)
    assert(initialFooter.forall(_.length == cols))
    val (_, up) = Vector(27, 91, 65).foldLeft((Vector.empty[Int], Option.empty[Command])) {
      case ((pending, _), byte) => Keys.step(pending, byte, false)
    }
    val selected = handle(up.get, initial)
    val exact = "1" + "0" * 99 + "1"
    assertEquals(Trading.displayExact(selected.get._2), exact)
    assertEquals(exact.length, 101)
    assertEquals(
      preview(OrderIntent(Side.Buy, OrderKind.Target, target = selected.map(_._2))),
      Left("Book contains an invalid number")
    )
    val footer = TradingScreen.rows(view, selected, preview, cols = cols)
    val frame = Screen
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
      selected,
      (intent, _) => { attempts += 1; preview(intent).isRight }
    )
    println(
      s"PUBLIC maximum parsed, Target Up exact ${exact.length}, footer ${footer.map(_.length)}, " +
        s"rendered exact ${frame.exists(_.contains(exact))}, shown ${shown.get()}, attempts $attempts"
    )
    println(frame.takeRight(4).mkString("\n"))
    assertEquals(shown.get(), None)
    assertEquals(attempts, 0)
    assertEquals(footer.size, 1)
    assert(footer.head.contains("Order keys disabled"))
    assertEquals(frame.size, 30)
    assert(frame.forall(_.length == cols))
  }
