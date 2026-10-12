package desk

import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicLong
import Polymarket.Window.*

// The 5m market rolls over at B. Discovery finds the new market at B + 5 s, the next midpoints
// read is due at B + 10 s. The ended market's last price is 97, the new market's 50.
class PolymarketRolloverSpec extends munit.FunSuite:
  private val B = Instant.parse("2026-10-04T03:40:00Z").toEpochMilli
  private val start = B - 20000L
  private def iso(at: Long) = Instant.ofEpochMilli(at).toString
  private val oldSlug = s"btc-updown-5m-${(B - 300000L) / 1000}"

  private def market(slug: String, from: Long, end: Long): ujson.Obj =
    ujson.Obj(
      "slug" -> slug,
      "clobTokenIds" -> ujson.write(ujson.Arr(s"up-$slug", s"dn-$slug")),
      "outcomes" -> ujson.write(ujson.Arr("Up", "Down")),
      "eventStartTime" -> iso(from),
      "endDate" -> iso(end)
    )

  private def asked(url: String): Vector[String] =
    url.drop(url.indexOf('?') + 1).split('&').toVector.map(_.stripPrefix("slug="))

  // A 5m market runs the period its slug names. Every other market runs two days from start.
  private def markets(url: String): String =
    ujson.write(ujson.Arr.from(asked(url).map { s =>
      if s.startsWith("btc-updown-5m-") then
        val t = s.stripPrefix("btc-updown-5m-").toLong * 1000L
        market(s, t, t + 300000L)
      else market(s, start - 1000L, start + 172800000L)
    }))

  private def price(token: String): String = if token == s"up-$oldSlug" then "0.97" else "0.50"

  private def midpoints(body: String): String =
    ujson.write(ujson.Obj.from(ujson.read(body).arr.map { t =>
      val token = t("token_id").str
      token -> ujson.Str(price(token))
    }))

  // Runs a reader from `from` until its clock would pass `end`. A midpoints read may move the
  // clock. Gives the stopped reader and its clock.
  private def run(
      from: Long,
      end: Long,
      coins: Set[String] = Set("BTC"),
      get: String => String = markets,
      post: (AtomicLong, String) => String = (_, body) => midpoints(body)
  ): (PolymarketReader, Long) =
    val clock = new AtomicLong(from)
    val done = new CountDownLatch(1)
    val reader = new PolymarketReader(
      url => Right(get(url)),
      (_, body) => Right(post(clock, body)),
      () => coins,
      () => clock.get,
      wait =>
        if clock.get + wait > end then
          done.countDown()
          Thread.sleep(Long.MaxValue)
        else
          val _ = clock.addAndGet(wait)
    )
    reader.start()
    assert(done.await(20, TimeUnit.SECONDS), "the run did not reach its end")
    reader.close()
    Wait.until("reader stopped")(!reader.alive)
    (reader, clock.get)

  private def rolled(): (PolymarketReader, Long) = run(start, B + 6000L)

  test("control: before the boundary the 5m chance is the old market's 97") {
    val (reader, _) = run(start, start)
    assertEquals(Odds.of(reader).chance("BTC", M5), Some((97.0, false)))
  }

  test("after the rollover the open market's chance is not the ended market's last price") {
    val (reader, now) = rolled()
    assertEquals(now, B + 5000L)
    assertEquals(Odds.of(reader).chance("BTC", M5), None)
  }

  test("after the rollover the rails do not read the ended market's price as the open market's") {
    val (reader, now) = rolled()
    val btc =
      Indicators.groups(Snapshot.initial("XMR", now), Odds.of(reader).chance, now, Style.mono)(2)
    assertEquals(btc.rows(0).values.last._1, "●--")
  }

  test("after the rollover the band does not run the old period's last point past its end") {
    val (reader, now) = rolled()
    // Ten cells at two seconds a cell end at now. The boundary B is dot column 14, cell 7.
    val row =
      Band.rows(Snapshot.initial("XMR", now), Odds.of(reader), now, 38, 10, 2, Style.mono)(0)
    assertEquals(Chart.plain(row).drop(38).drop(7), "   ")
  }

  test("a read sent before the boundary and answered after it is not the open market's chance") {
    // The second midpoints read starts at B - 2 s and answers at B + 1 s with the old price.
    val (reader, now) = run(
      B - 32000L,
      B + 6000L,
      post = (clock, body) =>
        if clock.get == B - 2000L then
          val _ = clock.addAndGet(3000L)
        midpoints(body)
    )
    assertEquals(now, B + 5000L)
    assertEquals(reader.points("BTC", M5).last, Polymarket.Point(B + 1000L, 97.0))
    assertEquals(Odds.of(reader).chance("BTC", M5), None)
  }

  test("a reply that leaves out the new market's token does not keep the old value") {
    // Every reply after the boundary leaves out the 5m tokens of the 03:40 and 03:45 markets.
    val withoutNew = (body: String) =>
      midpoints(ujson.write(ujson.Arr.from(ujson.read(body).arr.filter { t =>
        val token = t("token_id").str
        !token.startsWith("up-btc-updown-5m-") || token == s"up-$oldSlug"
      })))
    val (reader, now) = run(start, B + 326000L, post = (_, body) => withoutNew(body))
    assertEquals(now, B + 310000L)
    assertEquals(Odds.of(reader).chance("BTC", M5), None)
  }

  test("the new market's chance shows from its own first price") {
    val (reader, now) = run(start, B + 11000L)
    assertEquals(now, B + 10000L)
    assertEquals(Odds.of(reader).chance("BTC", M5), Some((50.0, false)))
  }

  test("a market read before its period starts shows its own price") {
    // On a Sunday the gold daily for Monday opens at 13:30 UTC Monday and is read from Sunday.
    val sunday = Instant.parse("2026-10-04T15:00:00Z").toEpochMilli
    val monday = "xauusd-up-or-down-on-october-5-2026"
    val gold = (url: String) =>
      ujson.write(ujson.Arr.from(asked(url).filter(_ == monday).map { s =>
        market(
          s,
          Instant.parse("2026-10-05T13:30:00Z").toEpochMilli,
          Instant.parse("2026-10-05T21:00:00Z").toEpochMilli
        )
      }))
    val (reader, _) = run(sunday, sunday, coins = Set("PAXG"), get = gold)
    assertEquals(Odds.of(reader).chance("PAXG", D1), Some((50.0, false)))
  }

  test(
    "a reply that leaves out an open market's token keeps that market's last price with its age"
  ) {
    val h1 = (t: String) => t.startsWith("up-") && t.endsWith("-et")
    def later(change: String => Option[String]) = (clock: AtomicLong, body: String) =>
      val tokens = ujson.read(body).arr.map(_("token_id").str)
      ujson.write(ujson.Obj.from(tokens.flatMap { t =>
        val p = price(t)
        (if clock.get > start && h1(t) then change(p) else Some(p)).map(t -> ujson.Str(_))
      }))
    val cases = Vector[(String, String => Option[String])](
      "left out" -> (_ => None),
      "out of range" -> (_ => Some("5"))
    )
    for (name, change) <- cases do
      val (reader, now) = run(start, B + 11000L, post = later(change))
      assertEquals(now, B + 10000L, name)
      assertEquals(Odds.of(reader).chance("BTC", H1), Some((50.0, false)), name)
  }

  test("a read after a market's end and before the next discovery leaves the ended market out") {
    val (reader, now) = run(B - 28000L, B + 3000L)
    assertEquals(now, B + 2000L)
    assertEquals(reader.points("BTC", M5).map(_.at), Vector(B - 28000L))
  }

  test("a reply with no price in range is no failed read") {
    val fives = (_: AtomicLong, body: String) =>
      ujson.write(ujson.Obj.from(ujson.read(body).arr.map(_("token_id").str -> ujson.Str("5"))))
    val (reader, _) = run(start, start, post = fives)
    assert(!reader.unreachable)
    assertEquals(Odds.of(reader).chance("BTC", H1), None)
  }

  test("a price of NaN or Infinity fails the whole reply") {
    assert(Polymarket.parseMidpoints("""{"a":"NaN","b":"0.5"}""").isLeft)
    assert(Polymarket.parseMidpoints("""{"a":"Infinity","b":"0.5"}""").isLeft)
  }
