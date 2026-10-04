package desk

import java.nio.file.{Files, Paths}
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit
import java.util.concurrent.CountDownLatch
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import Polymarket.Window.*

class PolymarketSpec extends munit.FunSuite:
  private def ms(iso: String): Long = Instant.parse(iso).toEpochMilli
  private def iso(at: Long): String = Instant.ofEpochMilli(at).toString
  private def fixture(name: String): String =
    Files.readString(Paths.get("fixtures", name))
  private val markets = fixture("polymarket-markets.json")
  private val prices = fixture("polymarket-prices.json")
  private val both = Set("BTC", "PAXG")
  private val T0 = 1791085135000L // 2026-10-04T03:38:55Z, 11:38pm on October 3 in New York

  private def slugsAt(at: String, coins: Set[String] = Set("BTC")): Vector[String] =
    Polymarket.slugs(ms(at), coins).map(_._3)

  // ---- test 5: slugs ----

  test("summer clock gives every BTC window slug") {
    val got = Polymarket.slugs(ms("2026-07-15T16:23:10Z"), Set("BTC"))
    assertEquals(
      got.toSet,
      Set(
        ("BTC", M5, "btc-updown-5m-1784132400"),
        ("BTC", M15, "btc-updown-15m-1784132100"),
        ("BTC", H1, "bitcoin-up-or-down-july-15-2026-12pm-et"),
        ("BTC", H4, "btc-updown-4h-1784131200"),
        ("BTC", D1, "bitcoin-up-or-down-on-july-15-2026"),
        ("BTC", D1, "bitcoin-up-or-down-on-july-16-2026")
      )
    )
    assertEquals(got.size, 6)
  }

  test("winter clock gives the 1h slug in New York standard time, 12am for midnight") {
    assertEquals(
      Polymarket.slugs(ms("2026-01-15T05:10:00Z"), Set("BTC")).filter(_._2 == H1).map(_._3),
      Vector("bitcoin-up-or-down-january-15-2026-12am-et")
    )
    assertEquals(
      Polymarket.slugs(ms("2026-01-15T05:10:00Z"), Set("BTC")).filter(_._2 == M5).map(_._3),
      Vector("btc-updown-5m-1768453800")
    )
  }

  test("the 1h slug writes 12am, 1am to 11am, 12pm, 1pm to 11pm") {
    val hours = Seq(
      "2026-01-15T05:00:00Z" -> "12am",
      "2026-01-15T06:00:00Z" -> "1am",
      "2026-01-15T15:59:59Z" -> "10am",
      "2026-01-15T16:00:00Z" -> "11am",
      "2026-01-15T17:00:00Z" -> "12pm",
      "2026-01-15T18:00:00Z" -> "1pm",
      "2026-01-16T04:59:59Z" -> "11pm"
    )
    hours.foreach { (at, label) =>
      val h1 = Polymarket.slugs(ms(at), Set("BTC")).filter(_._2 == H1).map(_._3)
      assertEquals(h1.size, 1, at)
      assert(h1.head.endsWith("-" + label + "-et"), at + " " + h1.head)
    }
  }

  test("the fall back night asks the 1am slug once") {
    List("2026-11-01T05:30:00Z", "2026-11-01T06:30:00Z").foreach { at =>
      val all = slugsAt(at)
      assertEquals(all.count(_.contains("-1am-et")), 1, at)
      assertEquals(all.distinct.size, all.size, at)
    }
  }

  test("the spring forward night has no slug that names 2am") {
    (0 until 6 * 12).foreach { step =>
      val at = ms("2026-03-08T04:00:00Z") + step * 300000L
      assert(!Polymarket.slugs(at, Set("BTC")).exists(_._3.contains("-2am-")), iso(at))
    }
    assert(slugsAt("2026-03-08T06:30:00Z").exists(_.contains("-1am-et")))
    assert(slugsAt("2026-03-08T07:30:00Z").exists(_.contains("-3am-et")))
  }

  test("a 4h start is on a multiple of 14400 and the 5m and 15m starts on their periods") {
    val got = Polymarket.slugs(T0, Set("BTC")).toSet
    assert(got.contains(("BTC", H4, "btc-updown-4h-1791072000")))
    assert(got.contains(("BTC", M5, "btc-updown-5m-1791084900")))
    assert(got.contains(("BTC", M15, "btc-updown-15m-1791084600")))
    assertEquals(1791072000L % 14400L, 0L)
  }

  test("two daily slugs per series, today's and tomorrow's New York date") {
    val got = Polymarket.slugs(T0, both)
    assertEquals(
      got.filter(_._2 == D1).toSet,
      Set(
        ("BTC", D1, "bitcoin-up-or-down-on-october-3-2026"),
        ("BTC", D1, "bitcoin-up-or-down-on-october-4-2026"),
        ("PAXG", D1, "xauusd-up-or-down-on-october-3-2026"),
        ("PAXG", D1, "xauusd-up-or-down-on-october-4-2026")
      )
    )
  }

  test("gold has no slug but the daily ones and an unknown coin has none") {
    val gold = Polymarket.slugs(T0, Set("PAXG"))
    assertEquals(gold.map(_._2).toSet, Set(D1))
    assertEquals(gold.size, 2)
    assertEquals(Polymarket.slugs(T0, Set("XMR")), Vector.empty)
    assertEquals(Polymarket.slugs(T0, Set.empty), Vector.empty)
  }

  test("marketsRequest repeats the slug parameter") {
    assertEquals(
      Polymarket.marketsRequest(Vector("a-1", "b-2")),
      "https://gamma-api.polymarket.com/markets?slug=a-1&slug=b-2"
    )
  }

  test("midpointsRequest builds the batched body and caps it at eleven tokens") {
    val (url, body) = Polymarket.midpointsRequest(Vector("1", "2"))
    assertEquals(url, "https://clob.polymarket.com/midpoints")
    assertEquals(body, """[{"token_id":"1"},{"token_id":"2"}]""")
    val many = Polymarket.midpointsRequest((1 to 20).map(_.toString).toVector)._2
    assertEquals(ujson.read(many).arr.size, 11)
  }

  // ---- test 5: parseMarkets ----

  private val wantedAtFixture = Polymarket.slugs(T0, both)

  test("parseMarkets reads the saved Gamma reply, double parsing, Up at index 0") {
    val got = Polymarket.parseMarkets(markets, wantedAtFixture, T0).toOption.get
    val hour = got(("BTC", H1)).get
    assertEquals(hour.slug, "bitcoin-up-or-down-october-3-2026-11pm-et")
    assertEquals(
      hour.upToken,
      "59507067249038576071602910011856166788917716789779806593886142624691453931630"
    )
    assertEquals(hour.startsAt, ms("2026-10-04T03:00:00Z"))
    assertEquals(hour.endsAt, ms("2026-10-04T04:00:00Z"))
    assertEquals(got(("BTC", M5)).get.endsAt, ms("2026-10-04T03:40:00Z"))
    assertEquals(got(("BTC", M5)).get.startsAt, ms("2026-10-04T03:35:00Z"))
    assertEquals(got(("BTC", D1)).get.slug, "bitcoin-up-or-down-on-october-4-2026")
    assertEquals(got(("BTC", D1)).get.endsAt, ms("2026-10-04T16:00:00Z"))
    assertEquals(got(("PAXG", D1)), None)
    assertEquals(
      got.keySet,
      Set[(String, Polymarket.Window)](
        ("BTC", M5),
        ("BTC", M15),
        ("BTC", H1),
        ("BTC", H4),
        ("BTC", D1),
        ("PAXG", D1)
      )
    )
  }

  test("parseMarkets ignores startDate") {
    val got = Polymarket.parseMarkets(markets, wantedAtFixture, T0).toOption.get
    // startDate of the 5m market is a day early, the period start is eventStartTime.
    assertNotEquals(got(("BTC", M5)).get.startsAt, ms("2026-10-03T03:43:25Z"))
  }

  test("parseMarkets finds the gold daily on a trading day") {
    val at = ms("2026-10-04T20:00:00Z")
    val wanted = Polymarket.slugs(at, Set("PAXG"))
    val got = Polymarket.parseMarkets(markets, wanted, at).toOption.get
    val gold = got(("PAXG", D1)).get
    assertEquals(gold.slug, "xauusd-up-or-down-on-october-5-2026")
    assertEquals(gold.startsAt, ms("2026-10-05T13:30:00Z"))
    assertEquals(gold.endsAt, ms("2026-10-05T21:00:00Z"))
  }

  private def market(slug: String, up: String, start: String, end: String): ujson.Obj =
    ujson.Obj(
      "slug" -> slug,
      "outcomes" -> """["Up", "Down"]""",
      "clobTokenIds" -> s"""["$up", "dn-$up"]""",
      "startDate" -> "2020-01-01T00:00:00Z",
      "eventStartTime" -> start,
      "endDate" -> end
    )

  test("parseMarkets keeps a daily market whose endDate is ahead on a winter clock") {
    val body = ujson.write(
      ujson.Arr(
        market(
          "bitcoin-up-or-down-on-january-15-2026",
          "t15",
          "2026-01-14T16:00:00Z",
          "2026-01-15T16:00:00Z"
        ),
        market(
          "bitcoin-up-or-down-on-january-16-2026",
          "t16",
          "2026-01-15T16:00:00Z",
          "2026-01-16T16:00:00Z"
        )
      )
    )
    // 10:30 in New York in winter: the 16:00Z end is 11:00 there, not noon, and still ahead.
    val early = ms("2026-01-15T15:30:00Z")
    val a = Polymarket.parseMarkets(body, Polymarket.slugs(early, Set("BTC")), early).toOption.get
    assertEquals(a(("BTC", D1)).map(_.upToken), Some("t15"))
    // 11:30 in New York: today's is over, tomorrow's is the nearest future end.
    val late = ms("2026-01-15T16:30:00Z")
    val b = Polymarket.parseMarkets(body, Polymarket.slugs(late, Set("BTC")), late).toOption.get
    assertEquals(b(("BTC", D1)).map(_.upToken), Some("t16"))
  }

  test("parseMarkets maps an empty array to no market and a market past its end to none") {
    val empty = Polymarket.parseMarkets("[]", wantedAtFixture, T0).toOption.get
    assert(empty.nonEmpty && empty.values.forall(_.isEmpty))
    val past =
      Polymarket.parseMarkets(markets, wantedAtFixture, ms("2026-10-04T03:41:00Z")).toOption.get
    assertEquals(past(("BTC", M5)), None)
    assert(past(("BTC", M15)).nonEmpty)
    val after =
      Polymarket.parseMarkets(markets, wantedAtFixture, ms("2026-10-04T05:00:00Z")).toOption.get
    assertEquals(after(("BTC", H1)), None)
    assertEquals(after(("BTC", H4)), None)
  }

  test("parseMarkets fails on a reply that is not JSON, not an array, or lacks fields") {
    assert(Polymarket.parseMarkets("not json", wantedAtFixture, T0).isLeft)
    assert(Polymarket.parseMarkets("{}", wantedAtFixture, T0).isLeft)
    assert(Polymarket.parseMarkets("", wantedAtFixture, T0).isLeft)
    val noTokens =
      ujson.Obj("slug" -> "btc-updown-5m-1791084900", "endDate" -> "2026-10-04T03:40:00Z")
    assert(Polymarket.parseMarkets(ujson.write(ujson.Arr(noTokens)), wantedAtFixture, T0).isLeft)
    val oversize = "[" + " " * Polymarket.maxBody + "]"
    assert(Polymarket.parseMarkets(oversize, wantedAtFixture, T0).isLeft)
  }

  test("parseMidpoints reads the string prices into percent per token") {
    val got = Polymarket.parseMidpoints(prices).toOption.get
    assertEquals(got.size, 6)
    assertEquals(
      got("59507067249038576071602910011856166788917716789779806593886142624691453931630"),
      83.5
    )
    assertEquals(
      got("31750954484327394320672724772442108660811593152701484269129225540821664624104"),
      99.5
    )
  }

  test("parseMidpoints fails on no fields, not JSON, an array or a price that is not a number") {
    assert(Polymarket.parseMidpoints("{}").isLeft)
    assert(Polymarket.parseMidpoints("not json").isLeft)
    assert(Polymarket.parseMidpoints("[]").isLeft)
    assert(Polymarket.parseMidpoints("""{"a":"x"}""").isLeft)
    assert(Polymarket.parseMidpoints("""{"a":0.5}""").isLeft)
  }

  // ---- test 6: the reader ----

  private def syntheticMarkets(coinSet: Set[String], at: Long, tag: String): String =
    ujson.write(
      ujson.Arr.from(
        Polymarket.slugs(at, coinSet).map { (coin, window, slug) =>
          market(slug, s"tok-$tag-$coin-$window", iso(at - 1000), iso(at + 172800000L))
        }
      )
    )

  private def halves(body: String): Either[Int, String] =
    Right(
      ujson.write(ujson.Obj.from(ujson.read(body).arr.map(_("token_id").str -> ujson.Str("0.5"))))
    )

  private final class Rig(
      runMs: Long,
      coinSet: Set[String],
      getReply: (Int, Long, String) => Either[Int, String],
      postReply: (Int, Long, String) => Either[Int, String]
  ):
    val gets = ArrayBuffer.empty[Long]
    val getUrls = ArrayBuffer.empty[String]
    val posts = ArrayBuffer.empty[Long]
    val postTokens = ArrayBuffer.empty[Int]
    @volatile var clock = T0
    private val done = CountDownLatch(1)
    val reader = new PolymarketReader(
      url =>
        gets += clock
        getUrls += url
        getReply(gets.size, clock, url)
      ,
      (url, body) =>
        posts += clock
        postTokens += ujson.read(body).arr.size
        postReply(posts.size, clock, body)
      ,
      () => coinSet,
      () => clock,
      wait =>
        if clock + wait > T0 + runMs then
          done.countDown()
          Thread.sleep(Long.MaxValue)
        else clock += wait
    )
    def run(): Rig =
      reader.start()
      assert(done.await(20, TimeUnit.SECONDS), "the run did not reach its end")
      this
    def stop(): Unit =
      reader.close()
      Wait.until("reader thread ended")(!reader.alive)
    def requestTimes: Vector[Long] = (gets ++ posts).toVector.sorted

  private def fixtureRig(runMs: Long = 600000L) =
    new Rig(runMs, both, (_, _, _) => Right(markets), (_, _, _) => Right(prices))

  private def failureCount(status: Int): Long = Polymarket.failures.getOrElse(status, 0L)

  test("one slug read at start and at each boundary plus five seconds, never otherwise") {
    val rig = fixtureRig().run()
    rig.stop()
    assertEquals(rig.gets.toVector, Vector(T0, T0 + 70000L, T0 + 370000L))
    assert(rig.getUrls.head.startsWith("https://gamma-api.polymarket.com/markets?slug="))
    assert(rig.getUrls.head.contains("slug=btc-updown-5m-1791084900"))
    assert(rig.getUrls.head.contains("slug=xauusd-up-or-down-on-october-4-2026"))
  }

  test("one batched midpoints read every thirty seconds with at most eleven tokens") {
    val rig = fixtureRig().run()
    rig.stop()
    assertEquals(rig.posts.toVector, (0 to 20).map(k => T0 + k * 30000L).toVector)
    assertEquals(rig.postTokens.head, 5)
    assertEquals(rig.postTokens.last, 3)
    assert(rig.postTokens.forall(_ <= 11))
  }

  test("at most six requests in any minute over a ten minute run") {
    val rig = fixtureRig().run()
    rig.stop()
    val times = rig.requestTimes
    times.foreach(t => assert(times.count(x => x >= t && x < t + 60000L) <= 6, iso(t)))
  }

  test("states are Ready with the ring, NoMarket past the end or for a missing market") {
    val rig = fixtureRig().run()
    rig.stop()
    val r = rig.reader
    assert(!r.unreachable)
    r.state("BTC", H1) match
      case Polymarket.State.Ready(points, at) =>
        assertEquals(points.size, 21)
        assertEquals(points.head, Polymarket.Point(T0, 83.5))
        assertEquals(at, T0 + 600000L)
      case other => fail("expected Ready, got " + other)
    r.state("BTC", H4) match
      case Polymarket.State.Ready(points, _) => assertEquals(points.last.up, 93.5)
      case other                             => fail("expected Ready, got " + other)
    assertEquals(r.state("BTC", M5), Polymarket.State.NoMarket)
    assertEquals(r.state("BTC", M15), Polymarket.State.NoMarket)
    assertEquals(r.state("PAXG", D1), Polymarket.State.NoMarket)
    assertEquals(r.state("XMR", H1), Polymarket.State.NoMarket)
    assert(!r.isStale("BTC", H1))
  }

  test("a market past its end reads as NoMarket without waiting for the next read") {
    val rig = fixtureRig(runMs = 20000L).run()
    rig.stop()
    assert(rig.reader.state("BTC", M5).isInstanceOf[Polymarket.State.Ready])
    rig.clock = ms("2026-10-04T03:40:00Z")
    assertEquals(rig.reader.state("BTC", M5), Polymarket.State.NoMarket)
  }

  test("an empty reply is NoMarket for every window and sends no midpoints read") {
    val rig = new Rig(600000L, both, (_, _, _) => Right("[]"), (_, _, _) => Right(prices)).run()
    rig.stop()
    assertEquals(rig.posts.size, 0)
    assertEquals(rig.gets.toVector, Vector(T0, T0 + 70000L, T0 + 370000L))
    assertEquals(rig.reader.state("BTC", H1), Polymarket.State.NoMarket)
    assert(!rig.reader.unreachable)
  }

  test("backoff doubles on 429 up to five minutes and the state is Unreachable") {
    val requestsBefore = Polymarket.requests
    val failuresBefore = failureCount(429)
    val rig = new Rig(1100000L, both, (_, _, _) => Left(429), (_, _, _) => Right(prices)).run()
    rig.stop()
    assertEquals(
      rig.gets.toVector.map(_ - T0),
      Vector(0L, 60000L, 180000L, 420000L, 720000L, 1020000L)
    )
    assertEquals(rig.posts.size, 0)
    assertEquals(rig.reader.state("BTC", H1), Polymarket.State.Unreachable)
    assert(rig.reader.unreachable)
    assertEquals(Polymarket.requests - requestsBefore, 6L)
    assertEquals(failureCount(429) - failuresBefore, 6L)
  }

  test("a reply that is not JSON is a failure with its own status and backs off") {
    val before = failureCount(-1)
    val rig =
      new Rig(200000L, both, (_, _, _) => Right("not json"), (_, _, _) => Right(prices)).run()
    rig.stop()
    assertEquals(rig.gets.toVector.map(_ - T0), Vector(0L, 60000L, 180000L))
    assertEquals(failureCount(-1) - before, 3L)
    assert(rig.reader.unreachable)
  }

  test("a read that never connects counts under status 0") {
    val before = failureCount(0)
    val rig = new Rig(10000L, both, (_, _, _) => Left(0), (_, _, _) => Right(prices)).run()
    rig.stop()
    assertEquals(failureCount(0) - before, 1L)
  }

  test("the reader recovers and resets its backoff after a success") {
    val rig = new Rig(
      900000L,
      both,
      (n, at, _) => if n <= 2 then Left(500) else Right(syntheticMarkets(both, at, "a")),
      (_, _, body) => halves(body)
    ).run()
    rig.stop()
    assertEquals(rig.gets.toVector.take(4).map(_ - T0), Vector(0L, 60000L, 180000L, 370000L))
    assert(!rig.reader.unreachable)
    assert(rig.reader.state("BTC", H1).isInstanceOf[Polymarket.State.Ready])
  }

  test("a failing midpoints read backs off and the old points stay with their age") {
    val rig = new Rig(
      600000L,
      both,
      (_, _, _) => Right(markets),
      (n, _, _) => if n <= 3 then Right(prices) else Left(500)
    ).run()
    rig.stop()
    assertEquals(
      rig.posts.toVector.map(_ - T0),
      Vector(0L, 30000L, 60000L, 90000L, 150000L, 270000L, 510000L)
    )
    rig.reader.state("BTC", H1) match
      case Polymarket.State.Ready(points, at) =>
        assertEquals(points.size, 3)
        assertEquals(at, T0 + 60000L)
      case other => fail("expected Ready, got " + other)
    assert(rig.reader.isStale("BTC", H1))
    assert(rig.reader.unreachable)
  }

  test("data under two minutes old is not stale and over two minutes is") {
    val rig = fixtureRig(runMs = 60000L).run()
    rig.stop()
    assert(!rig.reader.isStale("BTC", H1))
    rig.clock = T0 + 60000L + Polymarket.staleAfterMs
    assert(!rig.reader.isStale("BTC", H4))
    rig.clock += 1
    assert(rig.reader.isStale("BTC", H4))
  }

  test("the ring holds 720 points and drops the oldest") {
    val rig = new Rig(
      6 * 3600000L + 1800000L,
      both,
      (_, at, _) => Right(syntheticMarkets(both, at, "a")),
      (_, _, body) => halves(body)
    ).run()
    rig.stop()
    assertEquals(rig.posts.size, 781)
    assert(rig.postTokens.forall(_ <= 11))
    rig.reader.state("BTC", H1) match
      case Polymarket.State.Ready(points, _) =>
        assertEquals(points.size, 720)
        assertEquals(points.head.at, T0 + 61 * 30000L)
        assertEquals(points.last.up, 50.0)
      case other => fail("expected Ready, got " + other)
  }

  test("a new token for a window resets its ring") {
    val rig = new Rig(
      600000L,
      both,
      (n, at, _) => Right(syntheticMarkets(both, at, if n == 1 then "a" else "b")),
      (_, _, body) => halves(body)
    ).run()
    rig.stop()
    rig.reader.state("BTC", H1) match
      case Polymarket.State.Ready(points, _) =>
        assertEquals(points.size, 18)
        assertEquals(points.head.at, T0 + 90000L)
      case other => fail("expected Ready, got " + other)
  }

  test("close ends the thread") {
    val rig = fixtureRig().run()
    assert(rig.reader.alive)
    rig.reader.close()
    Wait.until("thread ended")(!rig.reader.alive)
  }

  test(
    "Desk sets no header on a read and only Content-Type on the POST, with a three second timeout"
  ) {
    val get = Polymarket.getRequest("https://gamma-api.polymarket.com/markets?slug=a")
    assertEquals(get.method, "GET")
    assertEquals(get.headers.map.asScala.toMap, Map.empty[String, java.util.List[String]])
    assertEquals(get.timeout.get, Duration.ofSeconds(3))
    val post = Polymarket.postRequest("https://clob.polymarket.com/midpoints", "[]")
    assertEquals(post.method, "POST")
    assertEquals(post.headers.map.asScala.keySet.map(_.toLowerCase).toSet, Set("content-type"))
    assertEquals(post.headers.firstValue("Content-Type").get, "application/json")
    assertEquals(post.timeout.get, Duration.ofSeconds(3))
    assert(post.bodyPublisher.isPresent)
  }

  test("the reader sends only slugs and token ids") {
    val rig = fixtureRig().run()
    rig.stop()
    assert(
      rig.getUrls.forall(
        _.matches("https://gamma-api\\.polymarket\\.com/markets\\?(slug=[a-z0-9-]+&?)+")
      )
    )
  }
