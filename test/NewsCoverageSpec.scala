package desk

class NewsCoverageSpec extends munit.FunSuite:
  private val now = java.time.Instant.parse("2026-09-21T10:00:00Z").toEpochMilli
  private def rss(link: String, title: String = "New market report"): String =
    s"<rss><channel><item><title>$title</title><link>$link</link><pubDate>Mon, 21 Sep 2026 09:59:00 GMT</pubDate></item></channel></rss>"

  test("requested news topics have validated publisher paths") {
    val examples = Vector(
      "https://www.coindesk.com/arc/outboundfeeds/rss" -> "https://www.coindesk.com/markets/report",
      "https://www.fxstreet.com/rss/news" -> "https://www.fxstreet.com/news/gold-report",
      "https://news.un.org/feed/subscribe/en/news/topic/peace-and-security/feed/rss.xml" ->
        "https://news.un.org/en/story/2026/09/report",
      "https://zfnd.org/feed/" -> "https://zfnd.org/zcash-report/"
    )
    examples.foreach { (source, article) =>
      val items = News.feed(rss(article), source, now)
      assertEquals(items.map(_.url), Vector(article))
      assertEquals(items.head.publishedAt, Some(now - 60000L))
    }
  }

  test("one trusted source cannot attribute its item to another publisher") {
    intercept[IllegalArgumentException] {
      News.feed(rss("https://www.getmonero.org/unrelated"), News.fedNews, now)
    }
  }

  test("source validation rejects lookalike hosts, embedded credentials and alternate ports") {
    val source = "https://www.coindesk.com/arc/outboundfeeds/rss"
    for link <- Vector(
        "https://www.coindesk.com.attacker.invalid/report",
        "https://name@www.coindesk.com/report",
        "https://www.coindesk.com:444/report",
        "http://www.coindesk.com/report"
      )
    do intercept[IllegalArgumentException](News.feed(rss(link), source, now))
  }
