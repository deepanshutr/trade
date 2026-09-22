package desk

import java.time.{Instant, ZoneOffset}
import java.util.concurrent.{CountDownLatch, TimeUnit}

class AlertsIntegrationSpec extends munit.FunSuite:
  private val now = Instant.parse("2026-09-21T12:00:00Z").toEpochMilli
  private val snapshot = Snapshot.initial("XMR", now)

  test("startup mute preserves the voice style when sound resumes") {
    for (flags, voice) <- Vector(
        Vector.empty[String] -> true,
        Vector("--mute") -> true,
        Vector("--chime") -> false
      )
    do
      val called = new CountDownLatch(1)
      @volatile var actual = Option.empty[Boolean]
      val output = new AnnouncementOutput:
        def play(text: String, speak: Boolean, canceled: () => Boolean): Unit =
          actual = Some(speak)
          called.countDown()
        def close(): Unit = ()
      val args = Main.parseArgs(flags).toOption.get
      val sound = Main.announcements(args, output)
      try
        assertEquals(sound.muted, flags.contains("--mute"))
        if sound.muted then assert(!sound.offer(Announcement("old", now, "news", "Old")))
        sound.setMuted(false)
        sound.start()
        assert(sound.offer(Announcement("new", now, "news", "Current headline")))
        assert(called.await(3, TimeUnit.SECONDS))
        assertEquals(actual, Some(voice))
      finally sound.close()
    assert(Main.parseArgs(Vector("--mute", "--chime")).isLeft)
  }

  test("news retains publication and retrieval times through the Sources view") {
    val policy = new AlertPolicy
    policy.news(Map(News.crypto -> Vector.empty), now)
    val item = NewsItem(
      "article",
      "Crypto report",
      "https://www.coindesk.com/markets/report",
      Some(now - 600000L),
      None,
      now - 1000L
    )
    val alerts = policy.news(Map(News.crypto -> Vector(item)), now)
    assertEquals(alerts.size, 1)
    assertEquals(alerts.head.publishedAt, item.publishedAt)
    assertEquals(alerts.head.retrievedAt, Some(item.retrievedAt))
    val panel = PanelView(
      details = true,
      detailPage = DetailPage.Sources,
      alerts = alerts,
      newsDetails = "News partial. Missing source: calendar"
    )
    val frame = Workspace.draw(
      snapshot,
      TradingView("XMR", message = "View only"),
      panel,
      160,
      45,
      now,
      ZoneOffset.UTC,
      Style.mono
    )
    val text = frame.lines
      .take(45 - 15)
      .map(_.drop(Layout.workspaceChartWidth(160) + 1))
      .mkString("\n")
    assert(text.contains("Published 2026-09-21T11:50:00Z"), text)
    assert(text.contains("Retrieved 2026-09-21T11:59:59Z"), text)
    assert(text.replaceAll("\\s", "").contains("Missingsource:calendar"), text)
    assert(text.replaceAll("\\s", "").contains(item.url), text)
  }

  test("full sound queues retain the visual notice without persisting on the input lock") {
    val output = new AnnouncementOutput:
      def play(text: String, voice: Boolean, canceled: () => Boolean): Unit =
        fail("no audio expected")
      def close(): Unit = ()
    val sound = new Announcements(output)
    val controller = new DeskController(
      PanelView(),
      () => now,
      _ => Left("disabled"),
      _ => fail("unexpected order"),
      _ => false,
      _ => Left("disabled"),
      persist = _ => fail("announcement attempted message storage")
    )
    try
      for i <- 1 to 5 do
        WorkspaceApp.announce(
          Announcement(i.toString, now, "news", s"Headline $i"),
          controller,
          sound
        )
      assertEquals(controller.panel.alerts.map(_.id), (1 to 5).map(_.toString).toVector)
      assert(sound.status.contains("1 dropped"))
      controller.alertStatus(sound.status, "News 8 sources")
      val frame = Workspace.draw(
        snapshot,
        TradingView("XMR", message = "View only"),
        controller.panel,
        120,
        40,
        now,
        ZoneOffset.UTC,
        Style.mono
      )
      assert(frame.lines.mkString.contains("Headline 5"))
      assert(frame.lines.mkString.contains("1 dropped"))
    finally sound.close()
  }
