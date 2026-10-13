package desk

import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*
import scala.util.{Success, Try}

object OfflinePolymarket:
  // Answers every read with no reply. Any test start of Desk passes it.
  val client: () => (String => Either[Int, String], (String, String) => Either[Int, String]) =
    () => (_ => Left(0), (_, _) => Left(0))

class PolymarketWiringSpec extends TempDirectorySuite:
  test("the workspace reader follows BTC and PAXG through the client it is given") {
    val urls = new ConcurrentLinkedQueue[String]()
    val reader = WorkspaceApp.polymarketReader(
      (
        url => { urls.add(url); Left(0) },
        (url, _) => { urls.add(url); Left(0) }
      ),
      () => System.currentTimeMillis
    )
    reader.start()
    try Wait.until("the first slug read")(!urls.isEmpty)
    finally
      reader.close()
      Wait.until("reader stopped")(!reader.alive)
    val first = urls.peek
    assert(first.startsWith("https://gamma-api.polymarket.com/markets?slug=btc-updown-5m-"), first)
    assert(first.contains("slug=xauusd-up-or-down-on-"), first)
    assertEquals(WorkspaceApp.followed, Set("BTC", "PAXG"))
  }

  test("recording never builds a Polymarket client") {
    val directory = tempDirectory("desk-record-polymarket")
    val config = directory.resolve("settings.json")
    Files.writeString(config, "{}")
    Platform.setMode(config, "rw-------")
    Files.writeString(directory.resolve("public"), "not a folder")
    val built = new AtomicInteger()
    val failing: TradingConfig => (Connector, Rest) = _ =>
      (
        new Connector {
          def connect(text: String => Unit, closed: () => Unit): Socket = fail("socket")
        },
        new Rest { def post(body: String): Option[String] = fail("public request") }
      )
    val result = scala.util.Try(
      Main.run(
        Main.Args(
          "XMR",
          true,
          config = Some(config.toString),
          record = true,
          stateDirectory = Some(directory.toString)
        ),
        failing,
        (_, _, _, _) => throw new IllegalStateException("account factory called"),
        keys = KeyStartup.Hooks(home = directory),
        polymarket = () => { built.incrementAndGet(); OfflinePolymarket.client() }
      )
    )
    assert(result.isFailure)
    assertEquals(built.get(), 0)
  }

  // Recording and the key screen never call the default, so it must build nothing until it is
  // called. Every HTTP client starts one selector thread when it is built.
  test("the default Polymarket client is built only when its thunk is called") {
    def clients =
      Thread.getAllStackTraces.keySet.asScala.count(_.getName.endsWith("-SelectorManager"))
    val before = clients
    val default = Main.getClass.getMethod("run$default$6").invoke(Main)
    assert(default.isInstanceOf[Function0[?]], default)
    assert(clients <= before, "the default built an HTTP client before it was called")
  }

  // This guards the offline fixture only. It calls the helper above, so no change under src can
  // fail it. An empty address is one the real client cannot send, so a helper that hands out the
  // real client fails here without reaching the network.
  test("the offline client answers every read with no reply") {
    val (get, post) = OfflinePolymarket.client()
    assertEquals(Try(get("")), Success(Left(0)))
    assertEquals(Try(post("", "[]")), Success(Left(0)))
  }
