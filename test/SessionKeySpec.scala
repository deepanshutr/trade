package desk

import java.io.{ByteArrayInputStream, InputStream, OutputStream}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

class SessionKeySpec extends TempDirectorySuite:
  private val keyBytes = Array.fill[Byte](32)(1)
  private val agent = OrderSigning.address(keyBytes)
  private val owner = "0x" + "2" * 40
  private val zeros = new Array[Byte](32)

  private def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  test("load reads once across repeated calls") {
    val reads = new AtomicInteger()
    val session = new SessionKey(() =>
      reads.incrementAndGet()
      keyBytes.clone()
    )
    assertEquals(session.load(), Right(()))
    assertEquals(session.load(), Right(()))
    assertEquals(session.load(), Right(()))
    assertEquals(reads.get(), 1)
    session.close()
  }

  test("load reads once even after a failure and repeats the first result") {
    val reads = new AtomicInteger()
    val session = new SessionKey(() =>
      reads.incrementAndGet()
      throw new IllegalArgumentException("protected signing key is unavailable")
    )
    val first = session.load()
    assert(first.isLeft)
    assertEquals(session.load(), first)
    assertEquals(session.load(), first)
    assertEquals(reads.get(), 1)
  }

  test("a failed load reports a fixed reason and never the exception text") {
    val session = new SessionKey(() => throw new IllegalStateException("leaked-text-0123"))
    val reason = session.load().left.getOrElse("")
    assert(reason.nonEmpty)
    assert(!reason.contains("leaked-text-0123"))
  }

  test("copy returns equal but distinct arrays and leaves the held key alone") {
    val session = new SessionKey(() => keyBytes.clone())
    assertEquals(session.load(), Right(()))
    val one = session.copy()
    val two = session.copy()
    assert(java.util.Arrays.equals(one, keyBytes))
    assert(java.util.Arrays.equals(two, keyBytes))
    assert(!(one eq two))
    java.util.Arrays.fill(one, 9.toByte)
    assert(java.util.Arrays.equals(session.copy(), keyBytes))
    assert(java.util.Arrays.equals(two, keyBytes))
    session.close()
  }

  test("copy throws before a load and after a failed load") {
    val unloaded = new SessionKey(() => keyBytes.clone())
    intercept[IllegalStateException](unloaded.copy())
    val failed = new SessionKey(() => throw new IllegalArgumentException("unavailable"))
    assert(failed.load().isLeft)
    intercept[IllegalStateException](failed.copy())
  }

  test("close zeroes the held key and copy then throws") {
    val held = keyBytes.clone()
    val session = new SessionKey(() => held)
    assertEquals(session.load(), Right(()))
    assert(java.util.Arrays.equals(held, keyBytes))
    session.close()
    assert(java.util.Arrays.equals(held, zeros))
    intercept[IllegalStateException](session.copy())
    session.close()
  }

  test("two threads loading together read once and both get the result") {
    val reads = new AtomicInteger()
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val session = new SessionKey(() =>
      reads.incrementAndGet()
      entered.countDown()
      release.await()
      keyBytes.clone()
    )
    val results = new ConcurrentLinkedQueue[Either[String, Unit]]()
    val threads = Vector.fill(2)(new Thread(() => { val _ = results.add(session.load()) }))
    threads.foreach(_.setDaemon(true))
    threads.foreach(_.start())
    assert(entered.await(3, TimeUnit.SECONDS))
    release.countDown()
    threads.foreach(_.join(3000))
    assertEquals(results.asScala.toVector, Vector(Right(()), Right(())))
    assertEquals(reads.get(), 1)
    session.close()
  }

  test("close during a slow read does not wait for it and drops the late key") {
    val late = keyBytes.clone()
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val session = new SessionKey(() =>
      entered.countDown()
      release.await()
      late
    )
    val result = new AtomicReference[Either[String, Unit]]()
    val loader = new Thread(() => result.set(session.load()))
    loader.setDaemon(true)
    loader.start()
    assert(entered.await(3, TimeUnit.SECONDS))
    val closer = new Thread(() => session.close())
    closer.setDaemon(true)
    closer.start()
    closer.join(2000)
    assert(!closer.isAlive, "close waited for the key read")
    release.countDown()
    loader.join(3000)
    assert(result.get().isLeft)
    assert(java.util.Arrays.equals(late, zeros))
    intercept[IllegalStateException](session.copy())
  }

  test("load after close does not read the key") {
    val reads = new AtomicInteger()
    val session = new SessionKey(() =>
      reads.incrementAndGet()
      keyBytes.clone()
    )
    session.close()
    assert(session.load().isLeft)
    assertEquals(reads.get(), 0)
    intercept[IllegalStateException](session.copy())
  }

  private val metadata = ujson.Obj(
    "universe" -> ujson.Arr(ujson.Obj("name" -> "XMR", "szDecimals" -> 3, "maxLeverage" -> 5))
  )

  private val exchange = new Exchange:
    def watch(
        coin: String,
        user: String,
        generation: Long,
        receive: Option[ujson.Value] => Unit
    ): Unit = receive(
      Some(
        ujson.Obj(
          "user" -> user,
          "coin" -> coin,
          "leverage" -> ujson.Obj("type" -> "cross", "value" -> 5),
          "maxTradeSzs" -> ujson.Arr("100", "100")
        )
      )
    )
    def info(body: ujson.Value): Option[ujson.Value] = body("type").str match
      case "meta"     => Some(metadata)
      case "userRole" =>
        Some(ujson.Obj("role" -> "agent", "data" -> ujson.Obj("user" -> owner)))
      case "clearinghouseState" =>
        Some(
          ujson.Obj(
            "marginSummary" -> ujson.Obj("accountValue" -> "1000"),
            "assetPositions" -> ujson.Arr()
          )
        )
      case "userAbstraction" => Some(ujson.Str("disabled"))
      case _                 => None
    def post(body: ujson.Value): Option[ujson.Value] = None

  test("workers built with the loader take their own copies and the session reads once") {
    val reads = new AtomicInteger()
    val held = new AtomicReference[Array[Byte]]()
    val session = new SessionKey(() =>
      reads.incrementAndGet()
      val key = keyBytes.clone()
      held.set(key)
      key
    )
    assertEquals(session.load(), Right(()))
    val handed = new ConcurrentLinkedQueue[Array[Byte]]()
    val loader = () =>
      val copy = session.copy()
      handed.add(copy)
      copy
    val config = TradingConfig(owner = Some(owner), agent = Some(agent))
    val directory = tempDirectory("desk-session-key")
    def live(coin: String): TradingWorker =
      TradingWorker.withExchange(config, true, coin, directory, loader, exchange)
    val route = new ExecutionRouter(
      config,
      "LIVE",
      "XMR",
      live("XMR"),
      live,
      new Rest { def post(body: String): Option[String] = Some(ujson.write(metadata)) },
      directory,
      () => System.currentTimeMillis(),
      () => TimeUnit.NANOSECONDS.toMillis(System.nanoTime())
    )
    try
      route.start()
      Wait.until("the first live worker never took the key")(handed.size() == 1)
      assert(route.switchMode("PROP").isRight)
      assert(route.switchMode("LIVE").isRight)
      Wait.until("the second live worker never took the key")(handed.size() == 2)
      assertEquals(reads.get(), 1)
    finally route.close()
    val copies = handed.asScala.toVector
    assertEquals(copies.size, 2)
    assert(!(copies(0) eq copies(1)))
    copies.foreach(copy => assert(!(copy eq held.get())))
    copies.foreach(copy => assert(java.util.Arrays.equals(copy, zeros), "a worker kept its key"))
    assert(java.util.Arrays.equals(held.get(), keyBytes))
    assert(java.util.Arrays.equals(session.copy(), keyBytes))
    session.close()
    assert(java.util.Arrays.equals(held.get(), zeros))
  }

  private final class FakeProcess(
      output: Array[Byte],
      exit: Int = 0,
      finishes: Boolean = true
  ) extends Process:
    @volatile var waited = Option.empty[(Long, TimeUnit)]
    @volatile var destroyed = false
    def getOutputStream: OutputStream = OutputStream.nullOutputStream()
    def getInputStream: InputStream = new ByteArrayInputStream(output)
    def getErrorStream: InputStream = InputStream.nullInputStream()
    def waitFor(): Int = exit
    override def waitFor(timeout: Long, unit: TimeUnit): Boolean =
      // Keep the first wait. The cleanup after a timeout waits again for one second.
      if waited.isEmpty then waited = Some((timeout, unit))
      finishes
    def exitValue(): Int = exit
    def destroy(): Unit = destroyed = true
    override def isAlive: Boolean = !finishes && !destroyed

  private val config = TradingConfig(owner = Some(owner), agent = Some(agent), keyService = "svc")

  private def read(
      process: FakeProcess,
      wait: Int,
      commands: AtomicReference[Seq[String]] = new AtomicReference()
  ): Array[Byte] =
    ProtectedKey.readWith(
      config,
      wait,
      command =>
        commands.set(command)
        process
    )

  test("the protected key read runs the keychain tool for the configured service and agent") {
    val commands = new AtomicReference[Seq[String]]()
    val process = new FakeProcess((hex(keyBytes) + "\n").getBytes("US-ASCII"))
    val key = read(process, 120, commands)
    assertEquals(
      commands.get(),
      Seq("/usr/bin/security", "find-generic-password", "-s", "svc", "-a", agent, "-w")
    )
    assert(java.util.Arrays.equals(key, keyBytes))
  }

  test("the protected key read waits the given number of seconds") {
    val eight = new FakeProcess(hex(keyBytes).getBytes("US-ASCII"))
    read(eight, 8)
    assertEquals(eight.waited, Some((8L, TimeUnit.SECONDS)))
    val long = new FakeProcess(hex(keyBytes).getBytes("US-ASCII"))
    read(long, 120)
    assertEquals(long.waited, Some((120L, TimeUnit.SECONDS)))
  }

  test("the protected key read accepts a 0x prefix like before") {
    val process = new FakeProcess(("0x" + hex(keyBytes) + "\n").getBytes("US-ASCII"))
    assert(java.util.Arrays.equals(read(process, 8), keyBytes))
  }

  test("the protected key read refuses a key whose address differs and never prints it") {
    val other = Array.fill[Byte](32)(2)
    val process = new FakeProcess(hex(other).getBytes("US-ASCII"))
    val error = intercept[IllegalArgumentException](read(process, 8))
    assert(!error.getMessage.contains(hex(other)))
    assert(error.getMessage.contains("address differs"))
  }

  test("the protected key read refuses a malformed key") {
    val process = new FakeProcess("not a key".getBytes("US-ASCII"))
    intercept[IllegalArgumentException](read(process, 8))
  }

  test("the protected key read fails and stops the tool when the wait runs out") {
    val process = new FakeProcess(hex(keyBytes).getBytes("US-ASCII"), finishes = false)
    intercept[IllegalArgumentException](read(process, 120))
    assertEquals(process.waited, Some((120L, TimeUnit.SECONDS)))
    assert(process.destroyed)
  }

  test("the protected key read fails when the tool exits with an error") {
    val process = new FakeProcess(hex(keyBytes).getBytes("US-ASCII"), exit = 44)
    intercept[IllegalArgumentException](read(process, 8))
  }
