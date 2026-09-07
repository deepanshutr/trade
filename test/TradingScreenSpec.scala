package desk

class TradingScreenSpec extends munit.FunSuite:
  private val rules = MarketRules(0, 3, 5)
  private val ready = TradingView(
    "XMR",
    Some(BigDecimal(1)),
    "Ready",
    ready = true,
    rules = Some(rules),
    bid = Some(BigDecimal("99.99")),
    ask = Some(BigDecimal(100))
  )
  private val plan = OrderPlan(
    "XMR",
    0,
    Side.Buy,
    OrderKind.Target,
    BigDecimal(2),
    BigDecimal("100.1"),
    BigDecimal(100),
    BigDecimal("1.5"),
    BigDecimal(150),
    Some(BigDecimal(100)),
    BigDecimal("2.5")
  )
  private val maximum = BigDecimal("1000000000")
  private val sizes = TradeSizes(maximum, maximum, maximum)
  private def draw(
      view: TradingView = ready,
      target: Option[(Side, BigDecimal)] = None,
      preview: OrderIntent => Either[String, OrderPlan] = _ => Right(plan),
      cols: Int = 100,
      amounts: TradeSizes = TradeSizes()
  ): Vector[String] =
    val rows = TradingScreen.rows(view, target, preview, amounts, cols)
    assertEquals(rows.size, 4)
    assert(rows.map(Chart.plain).forall(_.length == cols), rows.mkString("\n"))
    assert(!rows.exists(_.contains("exceed this width")), rows.mkString("\n"))
    rows

  test("changing market clears every old display field while a matching view is retained") {
    for (wasReady, wasPending) <- Vector((true, false), (false, true)) do
      val old = ready.copy(
        message = "XMR filled 1 at 100",
        ready = wasReady,
        pending = wasPending,
        lastPlan = Some(plan),
        block = Some("account data stale"),
        outcome = Some("XMR filled 1 at 100")
      )
      assert(old.forMarket("XMR") eq old)
      assertEquals(
        old.forMarket("BTC"),
        TradingView("BTC", message = "changing market", pending = true)
      )
  }

  test("idle Hard and Best retain stable key columns at both widths") {
    for cols <- Vector(100, 120) do
      val calls = scala.collection.mutable.ArrayBuffer.empty[OrderIntent]
      val rows = draw(
        preview = intent => { calls += intent; Right(plan) },
        cols = cols,
        amounts = TradeSizes(BigDecimal("501.01"), BigDecimal(999), BigDecimal(800))
      )
      assert(rows(0).contains("1 buy $501.01") && rows(0).contains("4 sell $999"))
      assertEquals(calls.toVector, Vector.tabulate(4)(i => Trading.action(i + 4)))
      for (row, start) <- Vector(0 -> 0, 1 -> 4) do
        assertEquals(
          Vector.tabulate(4)(i => rows(row).indexOf(s"${start + i + 1} ")),
          Vector(5, 21, 37, 53)
        )
      val capped = draw(
        cols = cols,
        amounts = sizes,
        preview = _ => Right(plan.copy(estimatedValue = maximum))
      )
      val missing = draw(cols = cols, preview = _ => Left("waiting"))
      for (row, start) <- Vector(0 -> 0, 1 -> 4) do
        for i <- 0 until 4 do
          val key = s"${start + i + 1} "
          assertEquals(capped(row).indexOf(key), rows(row).indexOf(key))
          assertEquals(missing(row).indexOf(key), rows(row).indexOf(key))
      assert(missing(1).contains("5 buy --") && missing(1).contains("8 sell --"))
      assert(rows(0).contains("position long 1"))
      assert(rows(2).contains("Target 9 buy 0 sell") && rows(2).contains("up/down zoom"))
      assert(!rows.exists(_.contains("Enter sends")))
  }

  test(
    "selected and pending contexts skip unused Best previews and expose matching exact details"
  ) {
    for cols <- Vector(100, 120) do
      for side <- Vector(Side.Buy, Side.Sell) do
        val calls = scala.collection.mutable.ArrayBuffer.empty[OrderIntent]
        val target = Some(side -> BigDecimal("0.012345"))
        val selected = draw(
          target = target,
          cols = cols,
          preview = intent => {
            calls += intent; Right(plan.copy(side = side, average = Some(BigDecimal("0.012345"))))
          }
        )
        assertEquals(
          calls.toVector,
          Vector(OrderIntent(side, OrderKind.Target, target = target.map(_._2)))
        )
        assert(selected(0).contains("limit $0.012345"))
        assert(selected(0).contains("Enter sends") && selected(0).contains("Escape cancels"))
        assert(selected(1).contains("request 2 estimate 1.5 value $150 average $"))
        assert(selected(2).contains("target estimate long 2.5"))
        assert(selected(2).contains("1-8 and left/right inactive"))
        for kind <- Vector(OrderKind.Hard, OrderKind.Best, OrderKind.Target) do
          val pressed = plan.copy(kind = kind, side = side, positionAfter = BigDecimal("-0.5"))
          val pending = draw(
            ready.copy(
              pending = true,
              ready = false,
              position = Some(BigDecimal(40)),
              lastPlan = Some(pressed),
              message = "XMR waiting for order result"
            ),
            cols = cols,
            preview = _ => fail("pending must retain its captured preview")
          )
          assert(pending(0).startsWith(s"$kind XMR ${side.toString.toLowerCase} limit $$100.1"))
          assert(pending(1).contains("request 2 estimate 1.5 value $150 average $100"))
          assert(
            pending(2).contains("position long 40") && pending(2).contains(
              "press estimate short 0.5"
            )
          )
          assert(pending(3).startsWith("pending | XMR waiting for order result"))
  }

  test("failed and changed-market Target previews retain the reason and disable Enter") {
    for cols <- Vector(100, 120) do
      for failure <- Vector(
          "Target extends beyond the known exact book",
          "No visible quantity fits this order"
        )
      do
        val rows = draw(
          ready.copy(lastPlan = Some(plan)),
          Some(Side.Sell -> BigDecimal(99)),
          _ => Left(failure),
          cols
        )
        assert(rows(1).contains(failure))
        assert(rows(0).contains("Enter disabled") && rows(0).contains("Escape cancels"))
        assert(!rows.exists(_.contains("target estimate")))
        assert(!rows.exists(_.contains("request")))
      val changed = draw(
        target = Some(Side.Buy -> BigDecimal(100)),
        cols = cols,
        preview = _ => Right(plan.copy(coin = "BTC"))
      )
      assert(changed(1).contains("Waiting for the selected market"))
      assert(!changed.exists(_.contains("average")))
      val idleChanged = draw(cols = cols, preview = _ => Right(plan.copy(coin = "BTC")))
      assert(idleChanged(1).contains("5 buy --") && idleChanged(1).contains("8 sell --"))
      val stalePress =
        draw(ready.copy(coin = "BTC", pending = true, lastPlan = Some(plan)), cols = cols)
      assert(!stalePress.exists(_.contains("request")))
  }

  test(
    "unknown positions stay unknown and zero fill has no invented average or resulting position"
  ) {
    for cols <- Vector(100, 120) do
      for target <- Vector(None, Some(Side.Buy -> BigDecimal("100.1"))) do
        val unknown = ready.copy(
          position = None,
          ready = false,
          pending = target.isEmpty,
          lastPlan = Some(plan),
          message = "View only",
          block = Some("View only")
        )
        val rows = draw(unknown, target, cols = cols)
        assert(rows.mkString.contains("position unavailable"))
        assert(
          !rows.exists(l =>
            l.contains("target estimate") || l.contains("press estimate") || l.contains("flat")
          )
        )
        assert(rows(1).contains("request 2 estimate 1.5 value $150 average $100"))
        if target.nonEmpty then assert(rows(0).contains("Enter disabled"))
      val empty =
        plan.copy(estimatedQuantity = BigDecimal(0), estimatedValue = BigDecimal(0), average = None)
      val rows =
        draw(target = Some(Side.Buy -> BigDecimal(100)), preview = _ => Right(empty), cols = cols)
      assert(rows(1).contains("estimate 0 value $0 average unavailable"))
      assert(!rows.exists(_.contains("target estimate")))
      val flat = draw(ready.copy(position = Some(BigDecimal(0))), cols = cols)
      assert(flat.head.contains("position flat"))
  }

  test("full state table preserves long market outcomes current reasons and required details") {
    val observed = scala.collection.mutable.ArrayBuffer.empty[String]
    for cols <- Vector(100, 120) do
      for length <- Vector(20, 40) do
        val coin = "X" * length
        for direction <- Vector(Side.Buy, Side.Sell) do
          for current <- Vector(
              None,
              Some(BigDecimal("-999999999999999")),
              Some(BigDecimal("1e100"))
            )
          do
            val market = MarketRules(0, 0, 5)
            val book = Book(
              Vector(Level("0.000001", "1000000000000000", 1)),
              Vector(Level("0.000002", "1000000000000000", 1)),
              1000
            )
            val base = ready.copy(
              coin = coin,
              position = current,
              rules = Some(market),
              bid = Some(BigDecimal("0.000001")),
              ask = Some(BigDecimal("0.000002"))
            )
            val preview: OrderIntent => Either[String, OrderPlan] = intent =>
              Trading.plan(
                coin,
                market,
                book,
                base.reference(intent.side).get,
                current.getOrElse(BigDecimal(0)),
                intent,
                sizes
              )
            val price = base.reference(direction).get
            val selected =
              preview(OrderIntent(direction, OrderKind.Target, target = Some(price))).toOption.get
            val outcomes = Vector(
              coin + " filled 0.375 at 100",
              coin + " order rejected",
              coin + " recovered positionIncreaseAtOpenInterestCapRejected"
            )
            for outcome <- outcomes do
              for reason <- Vector(
                  None,
                  Some("selected depth stale"),
                  Some("account data stale"),
                  Some("connection disconnected")
                )
              do
                val state = base.copy(
                  ready = reason.isEmpty,
                  block = reason,
                  message = "Waiting for market or account data",
                  outcome = Some(outcome),
                  lastPlan = Some(selected)
                )
                for target <- Vector(None, Some(direction -> price)) do
                  val rows = draw(state, target, preview, cols, sizes)
                  val text = rows.mkString("\n")
                  assert(text.contains(coin) && text.contains(outcome), text)
                  reason.foreach(r => assert(text.contains(r), text))
                  assert(text.contains("position " + (if current.isEmpty then "unavailable"
                                                      else if current.get < 0 then "short"
                                                      else "long")))
                  if target.nonEmpty then
                    assert(text.contains("limit $" + Trading.format(price)), text)
                    for label <- Vector(
                        "request ",
                        "estimate ",
                        "value $",
                        "average $",
                        "Escape cancels",
                        "1-8 and left/right inactive"
                      )
                    do assert(text.contains(label), text)
                    assert(
                      text.contains(if reason.isEmpty then "Enter sends" else "Enter disabled"),
                      text
                    )
                  else assert(!text.contains("press estimate"))
                  if current.isEmpty then assert(!text.contains("target estimate"))
                  observed += s"width $cols name $length side $direction position $current reason $reason target ${target.nonEmpty}\n$text"
    sys.env.get("DESK_DISPLAY_EVIDENCE").foreach { directory =>
      val path = java.nio.file.Path.of(directory)
      java.nio.file.Files.createDirectories(path)
      java.nio.file.Files.writeString(path.resolve("state-table.txt"), observed.mkString("\n\n"))
      for (cols, height) <- Vector((100, 30), (120, 40)) do
        for mode <- Vector(ChartMode.Line, ChartMode.Candles) do
          val snapshot = Golden.snapshot.copy(
            chartMode = mode,
            zoom = if mode == ChartMode.Line then 0 else State.candleMinZoom
          )
          val book = snapshot.book.get
          val current = ready.copy(
            bid = Some(BigDecimal(book.bids.head.px)),
            ask = Some(BigDecimal(book.asks.head.px))
          )
          val preview: OrderIntent => Either[String, OrderPlan] = intent =>
            Trading.plan(
              snapshot.coin,
              rules,
              book,
              current.reference(intent.side).get,
              BigDecimal(1),
              intent,
              TradeSizes()
            )
          for (label, footer) <- Vector(
              "idle" -> draw(current, preview = preview, cols = cols),
              "target" -> draw(current, Some(Side.Buy -> current.ask.get), preview, cols),
              "pending" -> Golden.tradingRowsAt(cols)
            )
          do
            val frame = Screen.render(
              snapshot,
              cols,
              height,
              Golden.now,
              java.time.ZoneId.of("UTC"),
              if cols == 100 then Style.colour else Style.mono,
              footer
            )
            java.nio.file.Files.writeString(
              path.resolve(s"${label}_${mode}_${cols}x$height.txt"),
              frame.map(Chart.plain).mkString("\n") + "\n"
            )
    }
  }

  test("actual Target selection preserves huge integral prices and the real planner rejection") {
    for cols <- Vector(100, 120) do
      for (bid, ask) <- Vector(
          ("9" * 63, "1" + "0" * 63),
          ("9" + "0" * 99, "1" + "0" * 100),
          ("8" * 100, "9" * 100)
        )
      do
        val coin = "X" * 40
        val market = MarketRules(0, 0, 5)
        val book = Book(Vector(Level(bid, "1", 1)), Vector(Level(ask, "1", 1)), 1000)
        val view = ready.copy(
          coin = coin,
          rules = Some(market),
          bid = Some(BigDecimal(bid)),
          ask = Some(BigDecimal(ask))
        )
        for direction <- Vector(Side.Buy, Side.Sell) do
          val (_, target) = Main.handleTrade(
            Command.SelectTarget(direction),
            Snapshot.initial(coin, 1000),
            false,
            (cols, 30),
            view,
            Some(coin),
            0,
            None,
            (_, _) => fail("selection cannot send")
          )
          assertEquals(target, Some(direction -> view.reference(direction).get))
          val preview: OrderIntent => Either[String, OrderPlan] = intent =>
            Trading.plan(
              coin,
              market,
              book,
              view.reference(intent.side).get,
              BigDecimal(1),
              intent,
              sizes
            )
          val reason = preview(
            OrderIntent(direction, OrderKind.Target, target = target.map(_._2))
          ).swap.toOption.get
          assertEquals(
            reason,
            if bid.length > 64 || ask.length > 64 then "Book contains an invalid number"
            else "No visible quantity fits this order"
          )
          val rows = draw(view, target, preview, cols, sizes)
          val text = rows.mkString("\n")
          val exact = Trading.displayExact(target.get._2)
          assertEquals(BigDecimal(exact), target.get._2)
          assert(!exact.contains("~"))
          assert(text.contains(exact), text)
          if exact.length + 7 > cols then
            val index = rows.indexWhere(_.trim == exact)
            assert(index > 0 && rows(index - 1).stripTrailing().endsWith("limit $"), text)
          assert(
            text.contains(reason) && text.contains("Enter disabled") && text.contains(
              "Escape cancels"
            ),
            text
          )
          assert(
            text.contains("1-8 and left/right inactive") && text.contains("position long 1"),
            text
          )
          for block <- Vector(None, Some("account data stale"), Some("connection disconnected")) do
            val old = "Y" * 40 + " recovered positionIncreaseAtOpenInterestCapRejected"
            val crowded = view.copy(
              ready = block.isEmpty,
              block = block,
              outcome = Some(old),
              position = Some(BigDecimal("1e100"))
            )
            val all = draw(crowded, target, preview, cols, sizes).mkString("\n")
            assert(all.contains(exact) && all.contains(old) && all.contains(reason), all)
            assert(all.contains("Enter disabled") && all.contains("Escape cancels"), all)
            assert(
              all.contains("position long") && all.contains("1-8 and left/right inactive"),
              all
            )
            block.foreach(b => assert(all.contains(b), all))
  }

  test("selected help matches the actual decoded input path at both widths") {
    for cols <- Vector(100, 120) do
      val snapshot = Snapshot.initial("XMR", 1000)
      val target = Some(Side.Buy -> BigDecimal(100))
      val help = draw(target = target, cols = cols).mkString("\n")
      assert(help.contains("1-8 and left/right inactive"))
      for bytes <- (49 to 56).map(n => Vector(n)).toVector ++ Vector(
          Vector(27, 91, 67),
          Vector(27, 91, 68)
        )
      do
        val (_, commands) = bytes.foldLeft((Vector.empty[Int], Vector.empty[Command])) {
          (state, byte) =>
            val (pending, command) = Keys.step(state._1, byte, false)
            (pending, state._2 ++ command.toVector)
        }
        assertEquals(commands.size, 1)
        assertEquals(
          Main.handleTrade(
            commands.head,
            snapshot,
            false,
            (cols, 30),
            ready,
            Some("XMR"),
            0,
            target,
            (_, _) => fail("inactive key sent an order")
          ),
          (true, target)
        )
      val escape = Keys.expire(Vector(27), false)._2.get
      assertEquals(
        Main.handleTrade(
          escape,
          snapshot,
          false,
          (cols, 30),
          ready,
          Some("XMR"),
          0,
          target,
          (_, _) => fail("Escape sent an order")
        ),
        (true, None)
      )
      for permitted <- Vector(false, true) do
        var sends = 0
        val current = ready.copy(ready = permitted)
        val rows = draw(current, target, cols = cols).mkString("\n")
        assert(rows.contains(if permitted then "Enter sends" else "Enter disabled"))
        val command = Keys.step(Vector.empty, 13, false)._2.get
        val result = Main.handleTrade(
          command,
          snapshot,
          false,
          (cols, 30),
          current,
          Some("XMR"),
          0,
          target,
          (intent, coin) => {
            sends += 1; assertEquals(coin, "XMR");
            assertEquals(
              intent,
              OrderIntent(Side.Buy, OrderKind.Target, target = Some(BigDecimal(100)))
            );
            true
          }
        )
        assertEquals(sends, if permitted then 1 else 0)
        assertEquals(result._2, if permitted then None else target)
  }
