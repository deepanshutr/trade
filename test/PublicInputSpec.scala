package desk

import java.nio.file.{Files, Path}
import java.time.ZoneOffset
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

class PublicInputSpec extends munit.FunSuite:
  private def market(
      s: Snapshot,
      cols: Int,
      rows: Int,
      now: Long,
      zone: java.time.ZoneId,
      style: Style
  ): Vector[String] =
    Screen
      .renderMarket(
        s,
        cols,
        rows - 13 - 3 * s.referenceCoins.distinct.count(_ != s.coin),
        now,
        zone,
        style
      )
      .lines
  private val now = 1788345727341L
  private def fixture(name: String): ujson.Value =
    ujson.read(Files.readString(Path.of("fixtures", name)))

  private def folded(text: String): Snapshot =
    val before = Golden.snapshot
    val event = Events.parse(text).getOrElse(fail("a bad message was ignored"))
    val after = State.reduce(before, event, Golden.now)
    assertEquals(after.bad, before.bad + 1)
    assertEquals(after.book, before.book)
    assertEquals(after.tape, before.tape)
    assertEquals(after.candles, before.candles)
    assertEquals(after.ctx, before.ctx)
    val frames = market(after, 100, 30, Golden.now, ZoneOffset.UTC, Style.mono)
    assertEquals(frames.size, 20)
    assert(frames.forall(line => Chart.plain(line).length == 100))
    after

  private val unsafe = Vector(
    "broken",
    "NaN",
    "Infinity",
    "-Infinity",
    "1e309",
    "1e308",
    "1e200",
    "1e-200",
    "1e-10000",
    0x0661.toChar.toString
  )
  private val fields = Vector[(String, ujson.Value => ujson.Value, String)](
    ("l2Book.json", j => j("data")("levels")(0)(0), "px"),
    ("l2Book.json", j => j("data")("levels")(1)(19), "sz"),
    ("bbo.json", j => j("data")("bbo")(0), "px"),
    ("bbo.json", j => j("data")("bbo")(1), "sz"),
    ("trades.json", j => j("data")(0), "px"),
    ("trades.json", j => j("data")(29), "sz")
  ) ++ Vector("o", "h", "l", "c", "v").map(field =>
    ("candle.json", (j: ujson.Value) => j("data"), field)
  ) ++ Vector("markPx", "oraclePx", "funding", "openInterest", "dayNtlVlm", "prevDayPx", "midPx")
    .map(field => ("activeAssetCtx.json", (j: ujson.Value) => j("data")("ctx"), field))

  for (name, select, field) <- fields do
    test(s"unsafe $field in $name stays outside state and drawing") {
      for text <- unsafe do
        val message = fixture(name)
        select(message)(field) = ujson.Str(text)
        val _ = folded(message.render())
    }

  private def smallBook(price: String, size: String): String =
    ujson
      .Obj(
        "channel" -> "l2Book",
        "data" -> ujson.Obj(
          "coin" -> "XMR",
          "time" -> ujson.Num(now.toDouble),
          "levels" -> ujson.Arr(
            ujson.Arr(ujson.Obj("px" -> price, "sz" -> size, "n" -> 1)),
            ujson.Arr(ujson.Obj("px" -> "2", "sz" -> "1", "n" -> 1))
          )
        )
      )
      .render()

  private def freshDrawing(text: String): Snapshot =
    val state = State.reduce(Snapshot.initial("XMR", now), Events.parse(text).get, now)
    val frame = market(state, 100, 30, now, ZoneOffset.UTC, Style.mono)
    assertEquals(frame.size, 20)
    assert(frame.forall(line => Chart.plain(line).length == 100))
    state

  test("ordinary decimal text still reaches fresh state and drawing") {
    val state = freshDrawing(smallBook("1", "1"))
    assertEquals(state.book.get.bids.head, Level("1", "1", 1))
    assertEquals(state.bad, 0)
  }

  test("a non-ASCII decimal price becomes bad before fresh state and drawing") {
    val digit = 0x0661.toChar.toString
    assertEquals(BigDecimal(digit).toDouble, 1.0)
    val state = freshDrawing(smallBook(digit, "1"))
    assertEquals(state.bad, 1)
    assertEquals(state.book, None)
  }

  test("a non-ASCII decimal size becomes bad before fresh drawing") {
    val digit = 0x0661.toChar.toString
    assertEquals(BigDecimal(digit).toDouble, 1.0)
    val state = freshDrawing(smallBook("1", digit))
    assertEquals(state.bad, 1)
    assertEquals(state.book, None)
  }

  test("a request with a non-ASCII decimal price is rejected") {
    val body = ujson.read(smallBook(0x0661.toChar.toString, "1"))("data").render()
    assertEquals(Events.parseRestBook(body), None)
  }

  private val zeroFields = Vector[(String, ujson.Value => ujson.Value, String)](
    ("l2Book.json", j => j("data")("levels")(0)(0), "sz"),
    ("trades.json", j => j("data")(0), "sz"),
    ("candle.json", j => j("data"), "v"),
    ("activeAssetCtx.json", j => j("data")("ctx"), "funding"),
    ("activeAssetCtx.json", j => j("data")("ctx"), "openInterest"),
    ("activeAssetCtx.json", j => j("data")("ctx"), "dayNtlVlm")
  )
  for (name, select, field) <- zeroFields do
    test(s"zero $field in $name remains valid for drawing") {
      val message = fixture(name)
      select(message)(field) = "0"
      assertEquals(freshDrawing(message.render()).bad, 0)
    }

  test("price text must also be valid for the exact midpoint formatter") {
    for value <- Vector("0x1.0p0", " 1", "1 ", "0", "-1") do
      val message = fixture("l2Book.json")
      message("data")("levels")(0)(0)("px") = ujson.Str(value)
      val _ = folded(message.render())
  }

  test("counts and times reject fractional, negative and overflowing values") {
    for (field, value) <- Vector(
        ("n", ujson.Num(1.5)),
        ("n", ujson.Num(-1)),
        ("n", ujson.Num(Int.MaxValue.toDouble + 1)),
        ("time", ujson.Num(1.5)),
        ("time", ujson.Num(-1)),
        ("time", ujson.Num(Long.MaxValue.toDouble))
      )
    do
      val message = fixture("l2Book.json")
      val target = if field == "n" then message("data")("levels")(0)(0) else message("data")
      target(field) = value
      val _ = folded(message.render())
  }

  test("valid zero quantities and signed funding retain their source strings") {
    val book = fixture("l2Book.json")
    book("data")("levels")(0)(0)("sz") = "0.000"
    book("data")("levels")(0)(0)("n") = 0
    val context = fixture("activeAssetCtx.json")
    context("data")("ctx")("funding") = "-0.0001302411"
    context("data")("ctx")("openInterest") = "0"
    context("data")("ctx")("dayNtlVlm") = "0"
    val before = Snapshot.initial("XMR", now)
    val after = Vector(book, context).foldLeft(before) { (state, json) =>
      State.reduce(state, Events.parse(json.render()).get, now)
    }
    assertEquals(after.bad, 0)
    assertEquals(after.book.get.bids.head.sz, "0.000")
    assertEquals(after.ctx.get.funding, "-0.0001302411")
    assertEquals(after.ctx.get.openInterest, "0")
    assert(
      market(after, 100, 30, now, ZoneOffset.UTC, Style.mono).head.contains("fund -0.0130%")
    )
  }

  test("inclusive display bounds survive combined midpoint, ratio and volume arithmetic") {
    val book = fixture("l2Book.json")
    for side <- 0 to 1; level <- book("data")("levels")(side).arr do
      level("px") = (if side == 0 then "1e-100" else "1e100")
      level("sz") = (if side == 0 then "1e100" else "1e-100")
    val ctx = fixture("activeAssetCtx.json")
    ctx("data")("ctx")("prevDayPx") = "1e-100"
    ctx("data")("ctx")("funding") = "-1e100"
    val trades = fixture("trades.json")
    trades("data").arr.foreach(_("sz") = "1e100")
    val events = Vector(book, ctx, trades).map(j => Events.parse(j.render()).get)
    assert(!events.exists(_.isInstanceOf[Event.Bad]))
    val state = events.foldLeft(Snapshot.initial("XMR", now))((s, e) => State.reduce(s, e, now))
    val columns = Chart.columns(state, 122, now)
    val axis = Chart.axis(columns, state.book, 11).get
    assert(axis.lo.isFinite && axis.hi.isFinite)
    assert(State.mid(state.book.get).get.isFinite)
    assert(Gauges.imbalance(state.book.get).productIterator.forall(_.asInstanceOf[Double].isFinite))
    assert(Gauges.tapeStats(state.tape, now, Vector(14400L)).head._1.get.isFinite)
    assert(Chart.depthRows(Chart.spans(state.book.get, Map.empty), axis)._1.forall(_.isFinite))
    assert(columns.flatten.forall(_.volume.isFinite))
    assert(
      market(state, 100, 30, now, ZoneOffset.UTC, Style.mono)
        .forall(line =>
          !line.contains("NaN") && !line.contains("Infinity") && Chart.plain(line).length == 100
        )
    )
  }

  test("request parse failures preserve good state through polling and history retry") {
    val book = fixture("rest_l2Book.json")
    book("levels")(0)(0)("px") = "broken"
    val candles = fixture("rest_candles.json")
    candles(0)("v") = "Infinity"
    assertEquals(Events.parseRestBook(book.render()), None)
    assertEquals(Events.parseRestCandles(candles.render()), None)
    val bodies = new ConcurrentLinkedQueue[String]()
    val rest = new Rest:
      def post(body: String): Option[String] =
        val _ = bodies.add(body)
        Some(if body.contains("candleSnapshot") then candles.render() else book.render())
    val connector = new Connector:
      def connect(text: String => Unit, close: () => Unit): Socket = fail("unexpected connection")
    val queue = new ConcurrentLinkedQueue[Event]()
    val feed = new Feed(connector, rest, queue, () => now, "XMR")
    feed.poll()
    feed.pollCoarse()
    assert(queue.isEmpty)
    feed.fetchHistoryFor("XMR")
    assertEquals(queue.asScala.toVector, Vector.fill(6)(Event.CandleHistoryFailed))
    assertEquals(bodies.size(), 16)
    val before = Golden.snapshot
    val after = queue.asScala.foldLeft(before)((s, e) => State.reduce(s, e, now))
    assertEquals(after, before.copy(bad = before.bad + 6))
  }

  test("recorded socket data reaches the real fold and malformed data increments its count") {
    val queue = new LinkedBlockingQueue[Event]()
    val state = new AtomicReference(Snapshot.initial("XMR", now))
    val rest = new Rest:
      def post(body: String): Option[String] = Some("[]")
    val connector = new Connector:
      def connect(text: String => Unit, close: () => Unit): Socket = fail("unexpected connection")
    val feed = new Feed(connector, rest, queue, () => now, "XMR")
    val workers = Main.startFold(queue, state, feed, () => now)
    try
      queue.put(Events.parse(fixture("l2Book.json").render()).get)
      val bad = fixture("trades.json")
      bad("data")(0)("sz") = "NaN"
      queue.put(Events.parse(bad.render()).get)
      queue.put(Event.Key(Command.Quit))
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
      while !state.get().quit && System.nanoTime() < deadline do Thread.sleep(1)
      assert(state.get().quit)
      assertEquals(state.get().bad, 1)
      assertEquals(state.get().book.get.bids.head.px, "522.24")
      assertEquals(state.get().tape, Vector.empty)
    finally
      workers.foreach(_.interrupt())
      workers.foreach(_.join(3000))
  }
