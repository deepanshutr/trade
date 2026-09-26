package desk

import java.util.concurrent.{CountDownLatch, TimeUnit}

class AlertsIntegrationSpec extends munit.FunSuite:
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
        if sound.muted then assert(!sound.offer(Announcement("Old")))
        sound.setMuted(false)
        sound.start()
        assert(sound.offer(Announcement("Market moved")))
        assert(called.await(3, TimeUnit.SECONDS))
        assertEquals(actual, Some(voice))
      finally sound.close()
    assert(Main.parseArgs(Vector("--mute", "--chime")).isLeft)
  }

  test("workspace announcements use the bounded sound queue") {
    val output = new AnnouncementOutput:
      def play(text: String, voice: Boolean, canceled: () => Boolean): Unit =
        fail("no audio expected")
      def close(): Unit = ()
    val sound = new Announcements(output)
    try
      for i <- 1 to 5 do
        WorkspaceApp.announce(
          Announcement(s"Market moved $i"),
          sound
        )
      assert(sound.status.contains("4 queued"))
      assert(sound.status.contains("1 dropped"))
    finally sound.close()
  }
