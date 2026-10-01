package desk

import java.nio.charset.StandardCharsets.UTF_8

class KeyStoreSpec extends munit.FunSuite:
  private val key = Array.tabulate[Byte](32)(i => (i * 7 + 3).toByte)
  private val keyHex = key.map(b => f"${b & 0xff}%02x").mkString
  private val expected = OrderSigning.address(key)
  private val python = "/opt/tools/bin/python3"
  private val helper = "/home/test/.local/lib/desk-keychain.py"
  private val env = Map("DESK_PYTHON" -> python, "DESK_KEY_HELPER" -> helper)
  private val missing = "key store helper is not installed. Run make install"

  private final class Runner(reply: ProcessRun = ProcessRun(0, Array.emptyByteArray)):
    var calls = Vector.empty[(Seq[String], Array[Byte], Array[Byte])]
    def apply(args: Seq[String], stdin: Array[Byte]): ProcessRun =
      calls = calls :+ ((args, stdin, stdin.clone()))
      reply

  private def json(value: ujson.Value): Array[Byte] = ujson.write(value).getBytes(UTF_8)

  private def stored(account: String = expected): ProcessRun = ProcessRun(
    0,
    json(
      ujson.Obj(
        "account" -> account,
        "role" -> "agent",
        "service" -> "desk",
        "state" -> "stored",
        "prompt_required" -> true
      )
    )
  )

  private def exists(account: String = expected): ProcessRun =
    ProcessRun(3, json(ujson.Obj("account" -> account, "state" -> "exists")))

  private def store(
      runner: Runner,
      environment: Map[String, String] = env,
      service: String = "desk"
  ): StoreResult =
    KeyStore.store(key.clone(), expected, service, environment, runner(_, _))

  test("store runs the helper with the service and sends 64 lowercase hex plus a newline") {
    val runner = new Runner(stored())
    assertEquals(store(runner, service = "desk-two"), StoreResult.Stored)
    assertEquals(runner.calls.size, 1)
    val (args, _, seen) = runner.calls.head
    assertEquals(args, Seq(python, helper, "--store", "agent", "--service", "desk-two"))
    assertEquals(new String(seen, UTF_8), keyHex + "\n")
    args.foreach(arg => assert(!arg.contains(keyHex), "the key reached an argument"))
  }

  test("store zeroes its hex buffer after the helper returns") {
    val runner = new Runner(stored())
    val _ = store(runner)
    val (_, stdin, _) = runner.calls.head
    assertEquals(stdin.length, 65)
    assert(stdin.forall(_ == 0), "the hex buffer was left behind")
  }

  test("store zeroes its hex buffer when the helper fails or the runner throws") {
    val runner = new Runner(ProcessRun(1, Array.emptyByteArray))
    assert(store(runner).isInstanceOf[StoreResult.Failed])
    assert(runner.calls.head._2.forall(_ == 0))
    var handed = Array.emptyByteArray
    val result = KeyStore.store(
      key.clone(),
      expected,
      "desk",
      env,
      (_, stdin) =>
        handed = stdin
        throw new java.io.IOException("leaked-error " + keyHex)
    )
    result match
      case StoreResult.Failed(reason) =>
        assert(!reason.contains(keyHex) && !reason.contains("leaked"))
      case other => fail("expected a failure, got " + other)
    assertEquals(handed.length, 65)
    assert(handed.forall(_ == 0))
  }

  test("a stored reply for the expected account is Stored") {
    assertEquals(store(new Runner(stored())), StoreResult.Stored)
  }

  test("the exists reply with the expected account is Exists") {
    assertEquals(store(new Runner(exists())), StoreResult.Exists)
  }

  test("a stored reply for a different account fails") {
    val result = store(new Runner(stored("0x" + "9" * 40)))
    assert(result.isInstanceOf[StoreResult.Failed], result.toString)
  }

  test("the exists reply for a different account fails") {
    val result = store(new Runner(exists("0x" + "9" * 40)))
    assert(result.isInstanceOf[StoreResult.Failed], result.toString)
  }

  test("an exists reply without exit code 3 fails and a stored reply with exit code 3 fails") {
    val asZero = exists().copy(exit = 0)
    assert(store(new Runner(asZero)).isInstanceOf[StoreResult.Failed])
    val asThree = stored().copy(exit = 3)
    assert(store(new Runner(asThree)).isInstanceOf[StoreResult.Failed])
  }

  test("bad JSON, empty output and a wrong shape fail") {
    Vector(
      ProcessRun(0, "not json".getBytes(UTF_8)),
      ProcessRun(0, Array.emptyByteArray),
      ProcessRun(0, json(ujson.Arr(1, 2))),
      ProcessRun(0, json(ujson.Obj("state" -> "stored"))),
      ProcessRun(0, json(ujson.Obj("account" -> expected))),
      ProcessRun(0, json(ujson.Obj("account" -> 5, "state" -> "stored"))),
      ProcessRun(3, "not json".getBytes(UTF_8))
    ).foreach { reply =>
      val result = store(new Runner(reply))
      assert(result.isInstanceOf[StoreResult.Failed], result.toString)
    }
  }

  test("exit code 1 fails even with a stored looking reply") {
    val result = store(new Runner(stored().copy(exit = 1)))
    assert(result.isInstanceOf[StoreResult.Failed])
  }

  private val badEnvironments = Vector(
    Map.empty[String, String],
    Map("DESK_PYTHON" -> python),
    Map("DESK_KEY_HELPER" -> helper),
    Map("DESK_PYTHON" -> "python3", "DESK_KEY_HELPER" -> helper),
    Map("DESK_PYTHON" -> python, "DESK_KEY_HELPER" -> "tools/keychain_wallet.py"),
    Map("DESK_PYTHON" -> "", "DESK_KEY_HELPER" -> helper),
    Map("DESK_PYTHON" -> python, "DESK_KEY_HELPER" -> ""),
    Map("DESK_PYTHON" -> "./python3", "DESK_KEY_HELPER" -> helper),
    Map("DESK_PYTHON" -> python, "DESK_KEY_HELPER" -> "~/.local/lib/desk-keychain.py")
  )

  test("missing or relative helper paths fail before the runner is called") {
    val runner = new Runner(stored())
    badEnvironments.foreach(environment =>
      assertEquals(store(runner, environment), StoreResult.Failed(missing))
    )
    assertEquals(runner.calls.size, 0)
  }

  test("the helper check accepts two absolute paths and refuses a missing or relative one") {
    assert(KeyStore.helperReady(env))
    assert(KeyStore.helperReady(env + ("DESK_EXTRA" -> "relative")))
    badEnvironments.foreach(environment =>
      assert(!KeyStore.helperReady(environment), environment.toString)
    )
  }

  test("the helper check and store agree on every environment") {
    (env +: badEnvironments).foreach { environment =>
      val refused = store(new Runner(stored()), environment) == StoreResult.Failed(missing)
      assertEquals(KeyStore.helperReady(environment), !refused, environment.toString)
    }
  }

  test("a key of the wrong length fails before the runner is called") {
    val runner = new Runner(stored())
    val result = KeyStore.store(new Array[Byte](31), expected, "desk", env, runner(_, _))
    assert(result.isInstanceOf[StoreResult.Failed])
    assertEquals(runner.calls.size, 0)
  }
