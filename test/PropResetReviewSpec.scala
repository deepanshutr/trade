package desk

import java.io.{ByteArrayOutputStream, InputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.collection.mutable.ArrayBuffer

// Reset prop rules PropResetSpec leaves open: no reset without y, every settings read hands its
// balance, one reset per call, the refusals in live and while an order is pending, the rest of
// the config kept, and the two arguments the workspace hands settings.
class PropResetReviewSpec extends TempDirectorySuite:
  private val at = 1800000000000L
  private def d(text: String): BigDecimal = BigDecimal(text)
  private val meta = """{"universe":[{"name":"XMR","szDecimals":2,"maxLeverage":5}]}"""
  private val rest = new Rest:
    def post(body: String): Option[String] = Some(meta)
  private def failing = new Exchange:
    def info(body: ujson.Value): Option[ujson.Value] = fail("reset started a live account read")
    def post(body: ujson.Value): Option[ujson.Value] = fail("reset attempted a live order")
    def watch(c: String, o: String, g: Long, r: Option[ujson.Value] => Unit): Unit =
      fail("reset opened a live account socket")
  private def disabled(config: TradingConfig, exchange: Exchange = failing): TradingWorker =
    new TradingWorker(
      config,
      false,
      "XMR",
      exchange,
      () => fail("reset loaded a key"),
      () => fail("reset opened a live journal"),
      () => at,
      () => at
    )
  private def router(
      mode: String,
      directory: Path,
      config: TradingConfig = TradingConfig(),
      factory: String => TradingWorker = _ => fail("reset enabled live trading"),
      live: Option[TradingWorker] = None
  ): ExecutionRouter =
    new ExecutionRouter(
      config,
      mode,
      "XMR",
      live.getOrElse(disabled(config)),
      factory,
      rest,
      directory,
      () => at,
      () => at
    )
  private def margin(directory: Path, network: String = "mainnet"): Path =
    directory.resolve("prop").resolve(PropLedger.file(network))
  private def saved(directory: Path): ujson.Value =
    ujson.read(Files.readString(margin(directory)))

  private def keys(text: String): InputStream = new InputStream:
    private val bytes = text.getBytes(UTF_8)
    private var next = 0
    override def available(): Int = if next < bytes.length then bytes.length - next else 1
    override def read(): Int =
      if next >= bytes.length then -1
      else
        next += 1
        bytes(next - 1) & 0xff

  private def wiring(
      file: Path,
      home: Path,
      controller: DeskController,
      resetProp: () => Either[String, String],
      follow: BigDecimal => Unit
  ): SettingsScreen.Context =
    SettingsWiring.context(
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
      resetProp = resetProp,
      followPropBalance = follow
    )
  private def propController: DeskController = new DeskController(
    PanelView(mode = "PROP"),
    () => at,
    (_, _) => Left("no preview"),
    _ => false,
    _ => Right(())
  )
  private def settingsFile(balance: String): (Path, Path) =
    val home = tempDirectory("round-home-")
    val file = home.resolve("settings.json")
    Files.writeString(file, s"""{"propBalance":"$balance"}""")
    Platform.setMode(file, "rw-------")
    (file, home)

  // Opening settings and reading the file never resets the wallet.
  test("settings left without y save no prop wallet") {
    val directory = tempDirectory("round-noy-")
    val route = router("PROP", directory)
    try
      route.start()
      val (file, home) = settingsFile("5000")
      val context =
        wiring(file, home, propController, () => route.resetProp(), route.followPropBalance)
      val out = new ByteArrayOutputStream()
      val down = "\u001b[B"
      assert(SettingsScreen.run(context, keys(down * 3 + "\r" + "n"), new PrintStream(out, true)))
      val shown = out.toString(UTF_8)
      assert(shown.contains("Reset the prop wallet to $5000? y confirms. Any other key cancels."))
      assert(!shown.contains("PROP reset to"), shown)
      assert(!Files.exists(margin(directory)), "a reset ran without y")
      // Positive control: y on the same router does save.
      assertEquals(route.resetProp(), Right("PROP reset to $5000"))
      assert(Files.exists(margin(directory)))
    finally route.close()
  }

  // Every read of one settings session hands its balance, not only the first.
  test("each settings read of one session hands its balance") {
    val followed = ArrayBuffer.empty[BigDecimal]
    val (file, home) = settingsFile("5000")
    val context = wiring(file, home, propController, () => Left("no reset"), followed += _)
    assertEquals(context.settings().map(_.config.propBalance), Right(d("5000")))
    Files.writeString(file, """{"propBalance":"7000"}""")
    assertEquals(context.settings().map(_.config.propBalance), Right(d("7000")))
    assertEquals(followed.toVector, Vector(d("5000"), d("7000")))
  }

  // One reset is one save, one count and one history line.
  test("one reset adds one count and one history line") {
    val directory = tempDirectory("round-once-")
    val route = router("PROP", directory)
    try
      route.start()
      assertEquals(route.resetProp(), Right("PROP reset to $10000"))
      val account = saved(directory)
      assertEquals(account("count").toString.stripPrefix("\"").stripSuffix("\""), "1")
      assertEquals(account("orders").arr.count(_.str.startsWith("PROP reset to $10000 | ")), 1)
    finally route.close()
  }

  // A prop worker made earlier does not let live reset it.
  test("reset in live after prop leaves the prop wallet") {
    val directory = tempDirectory("round-live-")
    val config = TradingConfig(owner = Some("0x" + "1" * 40), agent = Some("0x" + "2" * 40))
    val quiet = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] = None
      def post(body: ujson.Value): Option[ujson.Value] = fail("reset attempted a live order")
      def watch(c: String, o: String, g: Long, r: Option[ujson.Value] => Unit): Unit = ()
    val route = router(
      "PROP",
      directory,
      config,
      _ => disabled(config, quiet),
      Some(disabled(config, quiet))
    )
    try
      route.start()
      assertEquals(route.resetProp(), Right("PROP reset to $10000"))
      val before = Files.readAllBytes(margin(directory))
      assertEquals(route.switchMode("LIVE"), Right(()))
      assertEquals(route.mode, "LIVE")
      assertEquals(route.resetProp(), Left("Switch to prop first"))
      assert(java.util.Arrays.equals(Files.readAllBytes(margin(directory)), before))
    finally route.close()
  }

  // While a prop order is pending, the router gives the worker's refusal unchanged.
  test("a reset while a prop order is pending answers wait and resets nothing") {
    val saving = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val hold = new AtomicBoolean(true)
    val last = new AtomicReference[PropAccount]()
    val persist: PropAccount => Unit = next =>
      if hold.get() then
        saving.countDown()
        if !release.await(5, TimeUnit.SECONDS) then throw new IllegalStateException("held")
      last.set(next)
    val ledger = new PropLedger(
      PropAccount(d("10000"), deposit = d("10000")),
      BigDecimal(0),
      persist,
      new AutoCloseable { def close(): Unit = () }
    )
    val worker = new PropWorker(TradingConfig(), "XMR", ledger, rest, () => at, () => at)
    val route = router("PROP", tempDirectory("round-pending-"))
    val field = classOf[ExecutionRouter].getDeclaredFields.find(_.getName == "prop").get
    field.setAccessible(true)
    field.set(route, Some(worker))
    try
      worker.start()
      route.start()
      route.observe(Event.SocketUp)
      val book = Book(Vector(Level("99.99", "50", 1)), Vector(Level("100", "50", 1)), at)
      route.observe(Event.BookUpdate("XMR", book, true))
      Wait.until("prop did not become ready")(route.view.ready)
      val intent = Trading.action(0)
      val terms = TradeTerms(TradingConfig().sizes, TradingConfig().slippage)
      val plan = route.preview(intent, terms).fold(reason => fail(reason), identity)
      assert(route.submitReviewed(OrderReview(intent, plan, terms, at, at + 30000L)))
      assert(saving.await(5, TimeUnit.SECONDS), "the fill never reached its save")
      assertEquals(route.resetProp(), Left("Wait for the pending prop order"))
      hold.set(false)
      release.countDown()
      Wait.until("the prop buy did not finish")(
        route.view.outcome.exists(_.startsWith("PROP XMR buy filled "))
      )
      val account = last.get()
      assert(account.positions.contains("XMR"), account.toString)
      assert(!account.orders.exists(_.startsWith("PROP reset")), account.toString)
    finally route.close()
  }

  // Following the settings balance changes only the balance.
  test("following the settings balance keeps the rest of the config") {
    val directory = tempDirectory("round-network-")
    val route = router("VIEW", directory, TradingConfig(network = "testnet"))
    try
      route.followPropBalance(d("5000"))
      assertEquals(route.switchMode("PROP"), Right(()))
      assertEquals(route.resetProp(), Right("PROP reset to $5000"))
      assert(Files.exists(margin(directory, "testnet")))
      assert(!Files.exists(margin(directory)))
    finally route.close()
  }

  // run needs a terminal, so no test reaches openSettings. This guard pins the one settings
  // context and the one router it builds, and that it hands settings that router's reset and
  // balance.
  test("the workspace hands settings the router's reset and balance") {
    val source = Files.readString(Path.of("src/WorkspaceApp.scala"))
    assertEquals("SettingsWiring\\.context\\(".r.findAllMatchIn(source).size, 1)
    assertEquals("val worker = new ExecutionRouter\\(".r.findAllMatchIn(source).size, 1)
    val reset = """resetProp\s*=\s*\(\)\s*=>\s*worker\.resetProp\(\)\s*,""".r
    val follow = """followPropBalance\s*=\s*worker\.followPropBalance\s*\)""".r
    assert(reset.findFirstIn(source).nonEmpty)
    assert(follow.findFirstIn(source).nonEmpty)
  }
