package desk

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.IdentityHashMap
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

object OfflinePlayback:
  private final class QueueProbe(report: ujson.Value => Unit) extends LinkedBlockingQueue[Event]:
    private val entered =
      new IdentityHashMap[Event, java.util.ArrayDeque[(Long, String)]]()
    private val taken = new ConcurrentLinkedQueue[ujson.Value]()
    private val gate = new CountDownLatch(1)
    private val armed = new AtomicBoolean(false)
    @volatile private var held = false
    @volatile private var releasedAt = 0L

    private def label(event: Event): String = event match
      case Event.Key(command)          => "key " + command.toString
      case Event.Resize(cols, rows)    => s"resize $cols $rows"
      case Event.BboUpdate(c, _, _, _) => "quote " + c
      case Event.BookUpdate(c, _, _)   => "book " + c
      case _                           => event.productPrefix

    private def enqueue[A](event: Event, method: String)(insert: => A): A =
      val result = entered.synchronized {
        var arrivals = entered.get(event)
        if arrivals == null then
          arrivals = new java.util.ArrayDeque[(Long, String)]()
          entered.put(event, arrivals)
        arrivals.addLast((System.nanoTime(), method))
        insert
      }
      if held then
        event match
          case Event.Key(_) =>
            report(ujson.Obj("kind" -> "queued-key", "event" -> label(event)))
          case _ => ()
      result

    override def put(event: Event): Unit = enqueue(event, "put")(super.put(event))
    override def offer(event: Event): Boolean =
      enqueue(event, "offer")(super.offer(event))

    override def take(): Event =
      if armed.compareAndSet(true, false) then
        held = true
        report(ujson.Obj("kind" -> "queue-held"))
        gate.await()
        held = false
      val event = super.take()
      val end = System.nanoTime()
      val start = entered.synchronized {
        val arrivals = entered.get(event)
        require(
          arrivals != null && !arrivals.isEmpty,
          "event lacks an arrival record"
        )
        val value = arrivals.removeFirst()
        if arrivals.isEmpty then entered.remove(event)
        value
      }
      val _ = taken.add(
        ujson.Obj(
          "event" -> label(event),
          "method" -> start._2,
          "entered_ns" -> start._1.toString,
          "taken_ns" -> end.toString,
          "age_ns" -> (end - start._1).toString
        )
      )
      event

    def hold(): Unit = armed.set(true)
    def release(): Unit =
      releasedAt = System.nanoTime()
      gate.countDown()
    def results: ujson.Value = ujson.Obj(
      "samples" -> ujson.Arr.from(taken.asScala),
      "queue_release_ns" -> releasedAt.toString
    )

  def main(args: Array[String]): Unit =
    require(
      args.length == 3 || (args.length == 4 && args(3) == "speed"),
      "give control socket, fixture directory and settings path"
    )
    val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
    channel.connect(UnixDomainSocketAddress.of(args(0)))
    def report(value: ujson.Value): Unit = channel.synchronized {
      val bytes = ByteBuffer.wrap((ujson.write(value) + "\n").getBytes(UTF_8))
      while bytes.hasRemaining do channel.write(bytes)
    }
    val fixtures = Path.of(args(1))
    def fixture(name: String): ujson.Value =
      ujson.read(Files.readString(fixtures.resolve(name)))
    val recordedBook = fixture("l2Book.json")("data")
    val recordedBbo = fixture("bbo.json")("data")
    val recordedTrades = fixture("trades.json")("data")
    val recordedContext = fixture("activeAssetCtx.json")("data")
    val connected = new CountDownLatch(1)
    val holdRelease = new CountDownLatch(1)
    val holdArmed = new AtomicBoolean(false)
    val holdFinished = new AtomicBoolean(false)
    val holdFinishedAt = new AtomicLong(0L)
    val forbidden = new ConcurrentLinkedQueue[String]()
    val metadataCalls = new AtomicInteger(0)
    def metadata: ujson.Value = ujson.Obj(
      "universe" -> ujson.Arr.from(
        Vector("XMR", "ETH", "BTC", "PAXG").map(c =>
          ujson.Obj("name" -> c, "szDecimals" -> 3, "maxLeverage" -> 20)
        )
      )
    )
    @volatile var holdKind = ""
    @volatile var receive: String => Unit = _ => ()
    @volatile var closed: () => Unit = () => ()
    @volatile var holding = false
    val queue = new QueueProbe(report)

    def deny(label: String): Nothing =
      val _ = forbidden.add(label)
      throw new AssertionError("offline playback forbids " + label)

    def hold(kind: String): Unit =
      if holdKind == kind && holdArmed.compareAndSet(true, false) then
        holding = true
        report(ujson.Obj("kind" -> "work-held", "work" -> kind))
        try holdRelease.await()
        finally
          holdFinishedAt.set(System.nanoTime())
          holdFinished.set(true)
          report(ujson.Obj("kind" -> "work-ended", "work" -> kind))

    def book(coin: String): ujson.Value =
      val value = ujson.read(ujson.write(recordedBook))
      value("coin") = coin
      value("time") = ujson.Num(System.currentTimeMillis().toDouble)
      value

    def quote(coin: String): Unit =
      require(
        connected.await(10, TimeUnit.SECONDS),
        "public connection did not start"
      )
      val time = System.currentTimeMillis()
      val bbo = ujson.read(ujson.write(recordedBbo))
      bbo("coin") = coin
      bbo("time") = ujson.Num(time.toDouble)
      receive(ujson.write(ujson.Obj("channel" -> "bbo", "data" -> bbo)))
      receive(
        ujson.write(ujson.Obj("channel" -> "l2Book", "data" -> book(coin)))
      )
      val context = ujson.read(ujson.write(recordedContext))
      context("coin") = coin
      receive(
        ujson.write(ujson.Obj("channel" -> "activeAssetCtx", "data" -> context))
      )
      val trades = ujson.read(ujson.write(recordedTrades))
      val latest = trades.arr.map(_("time").num.toLong).max
      trades.arr.foreach { trade =>
        trade("coin") = coin
        trade("time") = ujson.Num((time + trade("time").num.toLong - latest).toDouble)
      }
      receive(ujson.write(ujson.Obj("channel" -> "trades", "data" -> trades)))

    val connector = new Connector:
      def connect(onText: String => Unit, onClose: () => Unit): Socket =
        receive = onText
        closed = onClose
        connected.countDown()
        report(ujson.Obj("kind" -> "connected"))
        new Socket:
          def send(text: String): Unit =
            if text.contains("\"method\":\"unsubscribe\"") then hold("send")
            else if text == Feed.ping then onText("""{"channel":"pong"}""")
          def close(): Unit = closed()

    val rest = new Rest:
      def post(body: String): Option[String] =
        val request = ujson.read(body)
        request("type").str match
          case "meta" =>
            metadataCalls.incrementAndGet()
            Some(ujson.write(metadata))
          case "candleSnapshot" =>
            if request("req")("coin").str == "ETH" then hold("history")
            Some("[]")
          case "l2Book" =>
            val coin = request("coin").str
            if coin == "BTC" then hold("selection")
            Some(if Set("XMR", "ETH", "BTC", "PAXG").contains(coin) then ujson.write(book(coin))
            else "null")
          case other => deny("public request " + other)

    val exchange = new Exchange:
      def info(body: ujson.Value): Option[ujson.Value] =
        if body("type").str != "meta" then deny("account request")
        metadataCalls.incrementAndGet()
        Some(metadata)
      def post(body: ujson.Value): Option[ujson.Value] = deny("submission")
      def watch(
          c: String,
          o: String,
          g: Long,
          r: Option[ujson.Value] => Unit
      ): Unit =
        deny("account subscription")
      override def warm(): Unit = deny("exchange warmup")

    val commands = new Thread(
      () => {
        val buffer = ByteBuffer.allocate(8192)
        val pending = new StringBuilder
        while channel.read(buffer) >= 0 do
          buffer.flip()
          while buffer.hasRemaining do
            val byte = buffer.get()
            if byte == 10 then
              val command = ujson.read(pending.result())
              pending.clear()
              command("command").str match
                case "quote" =>
                  quote(command("coin").str)
                  report(
                    ujson.Obj("kind" -> "quoted", "coin" -> command("coin").str)
                  )
                case "arm" =>
                  holdKind = command("work").str
                  holdArmed.set(true)
                  report(ujson.Obj("kind" -> "armed", "work" -> holdKind))
                case "hold-queue"    => queue.hold()
                case "release-queue" =>
                  queue.release()
                  report(ujson.Obj("kind" -> "queue-released"))
                case other => deny("control command " + other)
            else pending.append(byte.toChar)
          buffer.clear()
      },
      "offline-control"
    )
    commands.setDaemon(true)
    commands.start()
    Runtime.getRuntime.addShutdownHook(
      new Thread(() =>
        report(
          ujson.Obj(
            "kind" -> "result",
            "queue" -> queue.results,
            "forbidden" -> ujson.Arr.from(forbidden.asScala),
            "metadata_calls" -> metadataCalls.get(),
            "held_work" -> holding,
            "hold_finished" -> holdFinished.get(),
            "hold_finished_ns" -> holdFinishedAt.get().toString
          )
        )
      )
    )
    Main.run(
      Main.Args(
        "XMR",
        true,
        config = Some(args(2)),
        prop = args.length == 4,
        stateDirectory = Some(Path.of(args(2)).getParent.resolve("state").toString),
        audio = "mute"
      ),
      _ => (connector, rest),
      (config, enabled, coin, _) =>
        require(!enabled && config.owner.isEmpty && config.agent.isEmpty)
        new TradingWorker(
          config,
          enabled,
          coin,
          exchange,
          () => deny("protected key"),
          () => deny("saved orders"),
          () => System.currentTimeMillis(),
          () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime()),
          warmSigning = () => deny("signing warmup")
        )
      ,
      queue
    )
