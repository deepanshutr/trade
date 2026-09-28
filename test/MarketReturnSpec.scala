package desk

import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*

class RefineMarketSpec extends munit.FunSuite:
  test("returning to the oldest retained market restores history before cache eviction") {
    val retained = Ref(Vector((now / 1000 - 1) -> 1012.59), Map("5m" -> bars))
    val history = Vector.tabulate(State.marketHistoryLimit) { i =>
      s"M$i" -> (if i == 0 then retained else Ref.empty)
    }
    val original = Snapshot.initial("CURRENT", now).copy(marketHistory = history)
    val returned = select(original, "M0")
    assertEquals(returned.candles.getOrElse("5m", Vector.empty), bars)
    assertEquals(returned.samples, retained.samples)
    assertEquals(returned.marketHistory.size, State.marketHistoryLimit)
    assertEquals(returned.marketHistory.last._1, "CURRENT")
  }

  private val step = 300000L
  private val now = 1260 * step
  private val bars = Vector
    .tabulate(1260) { i =>
      val c = 1000.0 + i * 0.01
      Candle(i * step, c, c + 0.001, c - 0.001, c, 1)
    }
    .updated(1259, Candle(1259 * step, 1012.59, 1053.0936, 1012.589, 1012.59, 1))
  private def quote(s: Snapshot): Snapshot = s.copy(
    book = Some(Book(Vector(Level("1012.58", "1", 1)), Vector(Level("1012.60", "1", 1)), now)),
    feed = Health.Live(now)
  )
  private def select(s: Snapshot, coin: String): Snapshot =
    State.reduce(s.copy(prompt = Some(coin)), Event.Key(Command.PromptSubmit), now)

  test("returning to a market restores its graph and three-column evidence immediately") {
    val original = quote(
      Snapshot
        .initial("XMR", now)
        .copy(candles = Map("5m" -> bars), samples = Vector((now / 1000 - 1) -> 1012.59))
    )
    assert(Outlook.compute(original, now).columns.last.support.nonEmpty)
    val returned = quote(select(select(original, "TOKEN"), "XMR"))
    assertEquals(returned.candles.getOrElse("5m", Vector.empty), bars)
    assertEquals(returned.samples, original.samples)
    assertEquals(
      Outlook.compute(returned, now).columns.last.support,
      Outlook.compute(original, now).columns.last.support
    )
    assertEquals(returned.tape, Vector.empty)
  }

  test("choosing an already-followed reference uses its candles and line history") {
    val original = Snapshot
      .initial("XMR", now)
      .copy(refs =
        Map(
          "BTC" ->
            Ref(Vector((now / 1000 - 1) -> 1012.59), Map("5m" -> bars))
        )
      )
    val selected = select(original, "BTC")
    assertEquals(selected.candles.getOrElse("5m", Vector.empty), bars)
    assertEquals(selected.samples, original.refs("BTC").samples)
    assertEquals(selected.book, None)
  }

  test(
    "returning from an onchain market requests full venue history even if its subscription stayed selected"
  ) {
    val queue = new LinkedBlockingQueue[Event]
    var requested = Vector.empty[ujson.Value]
    val rest = new Rest:
      def post(body: String): Option[String] =
        val value = ujson.read(body)
        requested :+= value
        val req = value("req")
        val cs = if req("interval").str == "5m" then bars.filter(_.t >= req("startTime").num.toLong)
        else Vector.empty
        val text = ujson.write(
          ujson.Arr.from(
            cs.map(c =>
              ujson.Obj(
                "t" -> c.t.toDouble,
                "o" -> c.o.toString,
                "h" -> c.h.toString,
                "l" -> c.l.toString,
                "c" -> c.c.toString,
                "v" -> c.v.toString
              )
            )
          )
        )
        assert(
          Events.parseRestCandles(text).nonEmpty,
          "invalid synthetic candle response: " + text.take(160)
        )
        Some(text)
    val connector = new Connector:
      def connect(onText: String => Unit, onClose: () => Unit): Socket = fail("network")
    val venue = new Feed(connector, rest, queue, () => now, "XMR")
    val token =
      OnchainMarket("TOKEN", "0x" + "1" * 40, "0x" + "2" * 40, Vector("https://example.invalid"))
    val chain = new OnchainFeed(Vector(token), _ => fail("network"), queue, () => now, "XMR")
    val feed = new CompositeFeed(venue, chain, () => chain.coin)
    venue.repairHistory()
    val count = requested.size
    feed.setMarket("TOKEN")
    feed.setMarket("XMR")
    venue.repairHistory()
    assert(requested.size > count, "return skipped the history reload")
    val reloaded = requested.drop(count).filter(_("req")("interval").str == "5m")
    assert(reloaded.nonEmpty)
    assert(reloaded.head("req")("startTime").num.toLong <= bars.head.t)
  }

  test("held venue history cannot repopulate an onchain return with obsolete bars or cursor") {
    val queue = new LinkedBlockingQueue[Event]
    val requested = new LinkedBlockingQueue[ujson.Value]
    val failures = new LinkedBlockingQueue[Throwable]
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val first = new AtomicBoolean(true)
    val rest = new Rest:
      def post(body: String): Option[String] =
        val req = ujson.read(body)("req")
        requested.add(req)
        val obsolete = req("interval").str == "5m" && first.compareAndSet(true, false)
        if obsolete then
          entered.countDown()
          require(release.await(3, TimeUnit.SECONDS))
        val cs = if req("interval").str != "5m" then Vector.empty
        else if obsolete then Vector(Candle(now - step, 900, 901, 899, 900, 1))
        else bars.filter(_.t >= req("startTime").num.toLong)
        Some(
          ujson.write(
            ujson.Arr.from(
              cs.map(c =>
                ujson.Obj(
                  "t" -> c.t.toDouble,
                  "o" -> c.o.toString,
                  "h" -> c.h.toString,
                  "l" -> c.l.toString,
                  "c" -> c.c.toString,
                  "v" -> c.v.toString
                )
              )
            )
          )
        )
    val connector = new Connector:
      def connect(onText: String => Unit, onClose: () => Unit): Socket = fail("network")
    val venue = new Feed(connector, rest, queue, () => now, "XMR")
    val token =
      OnchainMarket("TOKEN", "0x" + "1" * 40, "0x" + "2" * 40, Vector("https://example.invalid"))
    val chain = new OnchainFeed(Vector(token), _ => fail("network"), queue, () => now, "XMR")
    val feed = new CompositeFeed(venue, chain, () => chain.coin)
    val repair = new Thread(() =>
      try
        venue.repairHistory()
        venue.awaitHistoryRepair()
      catch case error: Throwable => { val _ = failures.add(error) }
    )
    repair.setDaemon(true)
    repair.start()
    try
      assert(entered.await(3, TimeUnit.SECONDS))
      val selected = select(select(Snapshot.initial("XMR", now), "TOKEN"), "XMR")
      feed.setMarket("TOKEN")
      feed.setMarket("XMR")
      assertEquals(venue.coin, "XMR")
      release.countDown()
      repair.join(3000L)
      assert(!repair.isAlive, "replacement history stayed asleep")
      assertEquals(failures.asScala.toVector, Vector.empty)
      val fiveMinute =
        requested.asScala.toVector.filter(r => r("coin").str == "XMR" && r("interval").str == "5m")
      assertEquals(fiveMinute.size, 2, fiveMinute.toString)
      assert(
        fiveMinute.last("startTime").num.toLong <= bars.head.t,
        "obsolete completion restored its cursor"
      )
      val events = queue.asScala.toVector
      assertEquals(
        events.collect { case Event.CandleHistory("XMR", "5m", cs, _) => cs },
        Vector(bars)
      )
      val restored = quote(events.foldLeft(selected)((s, event) => State.reduce(s, event, now)))
      assertEquals(restored.candles("5m"), bars)
      assert(Outlook.compute(restored, now).columns.last.support.nonEmpty)
    finally
      release.countDown()
      if repair.isAlive then repair.interrupt()
      repair.join(1000L)
  }
