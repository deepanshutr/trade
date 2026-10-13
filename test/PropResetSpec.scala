package desk

import java.io.{ByteArrayOutputStream, InputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicReference

// Reset prop through the router, as settings call it. Only prop resets, at the balance the
// router holds, and the spot wallet stays as it was.
class PropResetSpec extends TempDirectorySuite:
  private val at = 1800000000000L
  private def d(text: String): BigDecimal = BigDecimal(text)

  // A router as the running Desk builds it, over a real prop folder, with a disabled live worker
  // whose exchange fails every call.
  private final class Route(mode: String, config: TradingConfig = TradingConfig()):
    val directory: Path = tempDirectory("prop-reset-")
    private val rest = new Rest:
      def post(body: String): Option[String] =
        Some("""{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}""")
    private val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = fail("reset started a live account read")
      def post(body: ujson.Value): Option[ujson.Value] = fail("reset attempted a live order")
      def watch(
          coin: String,
          owner: String,
          generation: Long,
          receive: Option[ujson.Value] => Unit
      ): Unit =
        fail("reset opened a live account socket")
    private val live = new TradingWorker(
      config,
      false,
      "XMR",
      exchange,
      () => fail("reset loaded a key"),
      () => fail("reset opened a live journal"),
      () => at,
      () => at
    )
    val router = new ExecutionRouter(
      config,
      mode,
      "XMR",
      live,
      _ => fail("reset enabled live trading"),
      rest,
      directory,
      () => at,
      () => at
    )
    def prop: Path = directory.resolve("prop")
    def margin: Path = prop.resolve(PropLedger.file("mainnet"))
    def saved: ujson.Value = ujson.read(Files.readString(margin))
    def figure(name: String): BigDecimal = d(saved(name).str)

  private def withRoute(mode: String, config: TradingConfig = TradingConfig())(
      run: Route => Unit
  ): Unit =
    val r = new Route(mode, config)
    try run(r)
    finally r.router.close()

  test("reset in prop starts the margin wallet over") {
    withRoute("PROP") { r =>
      r.router.start()
      r.router.observe(Event.SocketUp)
      r.router.observe(
        Event.BookUpdate(
          "XMR",
          Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), at),
          true
        )
      )
      Wait.until("prop did not become ready")(r.router.view.ready)
      val intent = Trading.action(0)
      val terms = TradeTerms(TradingConfig().sizes, TradingConfig().slippage)
      val plan = r.router.preview(intent, terms).fold(reason => fail(reason), identity)
      assert(r.router.submitReviewed(OrderReview(intent, plan, terms, at, at + 30000L)))
      Wait.until("the prop buy did not finish")(
        r.router.view.outcome.exists(_.startsWith("PROP XMR buy filled "))
      )
      // The buy left a position and spent its fee, so the reset has something to undo.
      assert(r.saved("positions").obj.contains("XMR"), r.saved.toString)
      assert(r.figure("cash") < d("10000"), r.saved.toString)
      assertEquals(r.router.resetProp(), Right("PROP reset to $10000"))
      assertEquals(r.figure("cash"), d("10000"))
      assertEquals(r.figure("deposit"), d("10000"))
      assertEquals(r.saved("positions").obj.size, 0)
      // The status row shows the reset once settings close.
      assertEquals(r.router.view.outcome, Some("PROP reset to $10000"))
    }
  }

  test("reset in live or view creates no prop worker") {
    for mode <- Vector("LIVE", "VIEW") do
      withRoute(mode) { r =>
        assertEquals(r.router.resetProp(), Left("Switch to prop first"), mode)
        assert(!Files.exists(r.margin), mode)
        // Opening the prop ledger makes its folder and lock, so no folder means no worker.
        assert(!Files.exists(r.prop), mode)
        assertEquals(r.router.mode, mode)
      }
  }

  test("reset leaves the spot wallet byte for byte") {
    withRoute("PROP") { r =>
      val spot = r.prop.resolve("wallet-mainnet.json")
      LocalFiles.write(
        spot,
        """{"cash":"5","deposit":"5","realized":"0","fees":"0","positions":{},"orders":[],"count":0}"""
      )
      val before = Files.readAllBytes(spot)
      r.router.start()
      assertEquals(r.router.resetProp(), Right("PROP reset to $10000"))
      assert(Files.exists(r.margin))
      assert(java.util.Arrays.equals(Files.readAllBytes(spot), before))
    }
  }

  test("reset starts over at the router's balance, which a settings read sets") {
    withRoute("PROP", TradingConfig(propBalance = d("20000"))) { r =>
      r.router.start()
      assertEquals(r.router.resetProp(), Right("PROP reset to $20000"))
      assertEquals((r.figure("cash"), r.figure("deposit")), (d("20000"), d("20000")))
      r.router.followPropBalance(d("5000"))
      assertEquals(r.router.resetProp(), Right("PROP reset to $5000"))
      assertEquals((r.figure("cash"), r.figure("deposit")), (d("5000"), d("5000")))
    }
  }

  // Keys as one burst, then the end of input, which closes settings.
  private def keys(text: String): InputStream = new InputStream:
    private val bytes = text.getBytes(UTF_8)
    private var next = 0
    override def available(): Int = if next < bytes.length then bytes.length - next else 1
    override def read(): Int =
      if next >= bytes.length then -1
      else
        next += 1
        bytes(next - 1) & 0xff

  test("the confirm and the reset name the settings file's balance after a hand edit") {
    // The router started at the default 10000. The file now says 5000, and Desk has not read it.
    withRoute("PROP") { r =>
      r.router.start()
      val home = tempDirectory("prop-reset-home-")
      val file = home.resolve("settings.json")
      Files.writeString(file, """{"propBalance":"5000"}""")
      Platform.setMode(file, "rw-------")
      val controller = new DeskController(
        PanelView(mode = "PROP"),
        () => at,
        (_, _) => Left("no preview"),
        _ => false,
        _ => Right(())
      )
      val context = SettingsWiring.context(
        network = "mainnet",
        config = Some(file),
        home = home,
        bound = new AtomicReference(Bindings.defaults),
        controller = controller,
        keyRow = () => Left("no key"),
        reload = () => Right(()),
        ownerRead = () => Left("no owner"),
        reading = new SettingsScreen.ReadGate,
        handOff = _ => Left("no child"),
        keyChild = Left("no child"),
        env = Map.empty,
        copy = _ => Right(()),
        livePending = () => false,
        size = () => (100, 30),
        now = () => at,
        approve = Right(Nil),
        resetProp = () => r.router.resetProp(),
        followPropBalance = r.router.followPropBalance
      )
      val out = new ByteArrayOutputStream()
      val down = "\u001b[B"
      assert(SettingsScreen.run(context, keys(down * 3 + "\r" + "y"), new PrintStream(out, true)))
      val shown = out.toString(UTF_8)
      assert(shown.contains("Reset prop           starts over at $5000"), shown)
      assert(shown.contains("Reset the prop wallet to $5000? y confirms. Any other key cancels."))
      assert(shown.contains("PROP reset to $5000"), shown)
      assert(!shown.contains("PROP reset to $10000"), shown)
      assertEquals((r.figure("cash"), r.figure("deposit")), (d("5000"), d("5000")))
    }
  }
