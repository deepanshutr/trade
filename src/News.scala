package desk

import java.io.{ByteArrayInputStream, StringReader}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{Duration, Instant, LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicBoolean
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Element
import org.xml.sax.InputSource
import scala.util.Try

final case class NewsItem(
    id: String,
    title: String,
    url: String,
    publishedAt: Option[Long],
    eventAt: Option[Long],
    retrievedAt: Long,
    eventDate: Option[String] = None,
    updatedAt: Option[Long] = None,
    status: String = "published"
)

object News:
  val bls = "https://www.bls.gov/schedule/news_release/bls.ics"
  val fedCalendar = "https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm"
  val fedNews = "https://www.federalreserve.gov/feeds/press_monetary.xml"
  val monero = "https://www.getmonero.org/feed.xml"
  private val sources = Vector(bls, fedCalendar, fedNews, monero)
  private def clean(text: String): String =
    Workspace.safeText(text.replaceAll("<[^>]*>", " ")).replaceAll("\\s+", " ").trim.take(500)
  private def url(text: String, source: String): String =
    val target = URI.create(source).resolve(text.trim)
    require(
      target.getScheme == "https" && target.getUserInfo == null && Set(-1, 443).contains(
        target.getPort
      ) && Set("www.bls.gov", "www.federalreserve.gov", "www.getmonero.org").contains(
        target.getHost
      )
    )
    target.toString
  private def instant(text: String): Option[Long] =
    Try(Instant.parse(text).toEpochMilli)
      .orElse(
        Try(ZonedDateTime.parse(text, DateTimeFormatter.RFC_1123_DATE_TIME).toInstant.toEpochMilli)
      )
      .toOption

  def feed(text: String, source: String, retrieved: Long): Vector[NewsItem] =
    require(text.length <= 2000000)
    val factory = DocumentBuilderFactory.newInstance()
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    factory.setXIncludeAware(false)
    factory.setExpandEntityReferences(false)
    val document = factory
      .newDocumentBuilder()
      .parse(new InputSource(new StringReader(text.stripPrefix("\ufeff"))))
    val atom = document.getElementsByTagName("entry")
    val nodes = if atom.getLength > 0 then atom else document.getElementsByTagName("item")
    val items = (0 until nodes.getLength.min(100)).toVector.flatMap { i =>
      Try {
        val item = nodes.item(i).asInstanceOf[Element]
        def value(name: String): Option[String] =
          val found = item.getElementsByTagName(name)
          Option.when(found.getLength > 0)(found.item(0).getTextContent.trim).filter(_.nonEmpty)
        val link = item.getElementsByTagName("link").item(0).asInstanceOf[Element]
        val target = url(
          if link.hasAttribute("href") then link.getAttribute("href") else link.getTextContent,
          source
        )
        val published = value("published").orElse(value("pubDate")).flatMap(instant)
        NewsItem(
          target,
          clean(value("title").get),
          target,
          published,
          None,
          retrieved,
          updatedAt = value("updated").flatMap(instant)
        )
      }.toOption
    }
    val dated = items.filter(_.publishedAt.nonEmpty)
    require(dated.nonEmpty, "news source returned no valid dated items")
    dated.groupBy(_.id).values.map(_.last).toVector.sortBy(_.publishedAt).reverse.take(20)

  def calendar(text: String, source: String, retrieved: Long): Vector[NewsItem] =
    val calendarText = text.stripPrefix("\ufeff").trim
    require(
      text.length <= 2000000 && calendarText.startsWith("BEGIN:VCALENDAR") && calendarText.endsWith(
        "END:VCALENDAR"
      )
    )
    val unfolded =
      text.replace("\r\n ", "").replace("\r\n\t", "").replace("\n ", "").replace("\n\t", "")
    val items = "(?s)BEGIN:VEVENT\\s*(.*?)END:VEVENT".r
      .findAllMatchIn(unfolded)
      .take(2000)
      .flatMap { event =>
        Try {
          val fields = event.group(1).split("\\r?\\n").toVector.flatMap { line =>
            val at = line.indexOf(':')
            Option.when(at > 0)((line.take(at), line.drop(at + 1)))
          }
          def value(name: String): Option[String] =
            fields.find(_._1.takeWhile(_ != ';') == name).map(_._2)
          val (key, date) = fields.find(_._1.takeWhile(_ != ';') == "DTSTART").get
          val isDate = key.contains("VALUE=DATE") || date.length == 8
          val day = if isDate then
            Some(LocalDate.parse(date, DateTimeFormatter.BASIC_ISO_DATE).toString)
          else None
          val at = if isDate then None
          else
            val zone = if date.endsWith("Z") then ZoneId.of("UTC")
            else
              val name = key
                .split(';')
                .find(_.startsWith("TZID="))
                .get
                .drop(5)
                .stripPrefix("\"")
                .stripSuffix("\"")
              ZoneId.of(if name == "US/Eastern" then "America/New_York" else name)
            Some(
              LocalDateTime
                .parse(
                  date.stripSuffix("Z"),
                  DateTimeFormatter
                    .ofPattern("uuuuMMdd'T'HHmmss")
                    .withResolverStyle(java.time.format.ResolverStyle.STRICT)
                )
                .atZone(zone)
                .toInstant
                .toEpochMilli
            )
          val title = clean(value("SUMMARY").get.replace("\\,", ",").replace("\\n", " "))
          val link = url(value("URL").getOrElse(source), source)
          NewsItem(
            value("UID").getOrElse(source + title + date),
            title,
            link,
            None,
            at,
            retrieved,
            day,
            status = if value("STATUS").contains("CANCELLED") then "cancelled" else "scheduled"
          )
        }.toOption
      }
      .toVector
      .groupBy(_.id)
      .values
      .map(_.last)
      .toVector
    require(items.nonEmpty, "calendar source returned no valid events")
    items

  def meetings(text: String, retrieved: Long): Vector[NewsItem] =
    require(text.length <= 2000000)
    val years = "([0-9]{4}) FOMC Meetings</a>".r.findAllMatchIn(text).toVector
    val months =
      Vector("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
    val result = years.zipWithIndex.flatMap { (section, i) =>
      val year = section.group(1).toInt
      val part = text.substring(section.end, years.lift(i + 1).map(_.start).getOrElse(text.length))
      "(?s)fomc-meeting__month[^>]*>\\s*<strong>([^<]+)</strong>.*?fomc-meeting__date[^>]*>([^<]+)</div>".r
        .findAllMatchIn(part)
        .flatMap { m =>
          Try {
            val month = months.indexOf(
              m.group(1).split('/').last.trim.take(3).toLowerCase(java.util.Locale.ROOT)
            ) + 1
            val day = "[0-9]+".r.findAllIn(m.group(2)).toVector.last.toInt
            val date = LocalDate.of(year, month, day).toString
            NewsItem(
              fedCalendar + "#" + date,
              "Federal Reserve policy meeting ends " + date + " (time not supplied by calendar)",
              fedCalendar,
              None,
              None,
              retrieved,
              Some(date),
              status = "scheduled"
            )
          }.toOption
        }
        .toVector
    }
    require(result.nonEmpty, "meeting calendar format unavailable")
    result

  def context(items: Vector[NewsItem], failures: Vector[String], now: Long): String =
    val today = Instant.ofEpochMilli(now).atZone(ZoneId.of("America/New_York")).toLocalDate.toString
    val upcoming = items
      .filter(i => i.eventAt.exists(_ >= now) || i.eventDate.exists(_ >= today))
      .sortBy(i => i.eventAt.map(Instant.ofEpochMilli(_).toString).orElse(i.eventDate))
      .take(12)
    val recent = items
      .filter(_.publishedAt.exists(t => t <= now && now - t <= 90L * 86400000L))
      .sortBy(_.publishedAt)
      .reverse
      .take(8)
    val lines = (upcoming ++ recent).map { item =>
      val time = item.eventAt
        .orElse(item.publishedAt)
        .map(t => Instant.ofEpochMilli(t).toString)
        .orElse(item.eventDate)
        .getOrElse("time unavailable")
      s"${item.status}: $time ${item.title} | ${item.url} | retrieved ${Instant.ofEpochMilli(item.retrievedAt)}" + item.updatedAt
        .filter(t => !item.publishedAt.contains(t))
        .fold("")(t => " | source updated " + Instant.ofEpochMilli(t))
    }
    (Vector(
      "Official-source sample, not complete market-news coverage. Reporting date is separate from the event date. Calendar absence does not prove no upcoming events."
    ) ++ failures ++ lines).mkString("\n")

  final class Worker(
      wall: () => Long,
      fetchText: Option[String => String] = None,
      refreshEveryMs: Long = 600000L
  ) extends AutoCloseable:
    require(refreshEveryMs > 0)
    private val closed = new AtomicBoolean(false)
    private val started = new AtomicBoolean(false)
    private val client = HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofSeconds(8))
      .followRedirects(HttpClient.Redirect.NEVER)
      .build()
    private final case class Cache(
        items: Map[String, Vector[NewsItem]],
        successfulAt: Map[String, Long],
        failures: Vector[String],
        attempted: Boolean
    )
    @volatile private var cache =
      Cache(Map.empty, Map.empty, Vector("Official news and calendars have not refreshed"), false)
    private var thread = Option.empty[Thread]
    private def stale(current: Cache, now: Long): Vector[String] =
      sources.filter(source =>
        current.successfulAt.get(source).forall(at => now < at || now - at > 1800000L)
      )
    def status: String =
      val current = cache
      if !current.attempted then "News waiting"
      else
        val state = if current.failures.nonEmpty then
          "News partial " + current.failures.size + " unavailable"
        else "News sources updated"
        if stale(current, wall()).nonEmpty then state + " (stale)" else state
    def context: String =
      val current = cache
      val now = wall()
      val aged =
        stale(current, now).map(source => "Source retrieval is stale or unavailable: " + source)
      News.context(current.items.valuesIterator.flatten.toVector, current.failures ++ aged, now)
    private def fetch(source: String): String =
      val request = HttpRequest
        .newBuilder(URI.create(source))
        .timeout(Duration.ofSeconds(12))
        .header("User-Agent", "Desk/1.0 public market monitor")
        .GET()
        .build()
      val reply = client.send(request, HttpResponse.BodyHandlers.ofInputStream())
      try
        require(reply.statusCode() == 200, "source unavailable")
        val bytes = reply.body().readNBytes(2000001)
        require(bytes.length <= 2000000, "source too large")
        new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
      finally reply.body().close()
    def start(): Unit = synchronized {
      if !closed.get() && started.compareAndSet(false, true) then
        val worker = new Thread(
          () => {
            try
              while !closed.get() do
                val results = sources.map { source =>
                  if closed.get() then throw new InterruptedException()
                  source -> Try {
                    val body = fetchText match
                      case Some(load) => load(source)
                      case None       => fetch(source)
                    if closed.get() then throw new InterruptedException()
                    val now = wall()
                    val items = if source == bls then calendar(body, source, now)
                    else if source == fedCalendar then meetings(body, now)
                    else feed(body, source, now)
                    (items, now)
                  }
                }
                val previous = cache
                val successful = results.flatMap { (source, result) =>
                  result.toOption.map(source -> _)
                }
                val next = Cache(
                  previous.items ++ successful.map((source, result) => source -> result._1),
                  previous.successfulAt ++ successful.map((source, result) => source -> result._2),
                  results.collect {
                    case (source, result) if result.isFailure => "Source unavailable: " + source
                  },
                  true
                )
                if !closed.get() then cache = next
                if !closed.get() then Thread.sleep(refreshEveryMs)
            catch case _: InterruptedException => ()
          },
          "official-news"
        )
        worker.setDaemon(true)
        thread = Some(worker)
        worker.start()
    }
    def close(): Unit =
      val current = synchronized {
        closed.set(true)
        thread
      }
      current.foreach(_.interrupt())
      current.foreach(_.join(1000L))
      client.close()
