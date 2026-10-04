package desk

import java.nio.file.{Files, Path}
import scala.util.chaining.*

class PropWalletRecoverySpec extends TempDirectorySuite:
  private def savedWallet(directory: Path): Path =
    val ledger = PropLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    val plan =
      OrderPlan("XMR", 0, Side.Buy, OrderKind.Hard, 1, 100, 100, 1, 100, Some(100), 1, false)
    try assert(ledger.fill(plan, 1800000000000L).isRight)
    finally ledger.close()
    directory.resolve("wallet-mainnet.json")

  private def withDirectory(run: Path => Unit): Unit =
    val directory = tempDirectory("desk-wallet-recovery-")
    run(directory)

  test("a held legacy account lock leaves the separate wallet usable and single-writer") {
    withDirectory { directory =>
      val legacyPath = directory.resolve("paper-mainnet.json")
      val legacy = """{"cash":"1000","positions":{"XMR":{"quantity":"-2","entry":"100"}}}"""
      LocalFiles.write(legacyPath, legacy)
      val oldLock = LocalFiles.lock(directory.resolve("paper-mainnet.lock"))
      try
        val wallet = PropLedger.open(directory, "mainnet", BigDecimal(500), BigDecimal(0))
        try
          assertEquals(wallet.account, PropAccount(BigDecimal(500), deposit = BigDecimal(500)))
          intercept[Exception](
            PropLedger.open(directory, "mainnet", BigDecimal(900), BigDecimal(0))
          )
          assertEquals(Files.readString(legacyPath), legacy)
        finally wallet.close()
      finally oldLock.close()
    }
  }

  for field <- Vector("cash", "deposit", "realized", "fees", "positions", "orders", "count") do
    test("a saved wallet missing " + field + " is refused without replacing its contents") {
      withDirectory { directory =>
        val path = savedWallet(directory)
        val damaged = ujson.read(Files.readString(path))
        damaged.obj.remove(field)
        val bytes = ujson.write(damaged)
        LocalFiles.write(path, bytes)
        for _ <- 0 until 2 do
          intercept[Exception](
            PropLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
          )
          assertEquals(Files.readString(path), bytes)
        val released = LocalFiles.lock(directory.resolve("wallet-mainnet.lock"))
        released.close()
      }
    }

  for deposit <- Vector("0", "-1", "1000000001", "bad") do
    test("invalid stored deposit " + deposit + " is refused") {
      withDirectory { directory =>
        val path = savedWallet(directory)
        val damaged = ujson.read(Files.readString(path))
        damaged("deposit") = deposit
        val bytes = ujson.write(damaged)
        LocalFiles.write(path, bytes)
        intercept[Exception](
          PropLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
        )
        assertEquals(Files.readString(path), bytes)
      }
    }

  for (label, cash, quantity) <- Vector(
      ("negative cash", "-1", "2"),
      ("negative holding", "100", "-2"),
      ("zero holding", "100", "0")
    )
  do
    test(label + " in a saved wallet is refused and preserved") {
      withDirectory { directory =>
        val path = directory.resolve("wallet-mainnet.json")
        val saved = ujson.write(
          ujson.Obj(
            "cash" -> cash,
            "deposit" -> "1000",
            "realized" -> "0",
            "fees" -> "0",
            "orders" -> ujson.Arr(),
            "count" -> "0",
            "positions" -> ujson.Obj("XMR" -> ujson.Obj("quantity" -> quantity, "entry" -> "100"))
          )
        )
        LocalFiles.write(path, saved)
        for _ <- 0 until 2 do
          intercept[IllegalArgumentException](
            PropLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
          )
          assertEquals(Files.readString(path), saved)
        val released = LocalFiles.lock(directory.resolve("wallet-mainnet.lock"))
        released.close()
      }
    }

  private def walletWith(resting: ujson.Value*): String =
    val body = ujson.Obj(
      "cash" -> "1000",
      "deposit" -> "1000",
      "realized" -> "0",
      "fees" -> "0",
      "orders" -> ujson.Arr(),
      "count" -> "0",
      "positions" -> ujson.Obj()
    )
    if resting.nonEmpty then body("resting") = ujson.Arr.from(resting)
    ujson.write(body)

  private def restingOrder(id: String = "0x" + "a" * 32): ujson.Obj = ujson.Obj(
    "id" -> id,
    "coin" -> "XMR",
    "side" -> "Buy",
    "price" -> "546.00",
    "quantity" -> "0.2",
    "reduceOnly" -> false,
    "createdAt" -> "1800000000000"
  )

  test("a wallet saved before resting orders loads with none and saves the key") {
    withDirectory { directory =>
      val path = directory.resolve("wallet-mainnet.json")
      LocalFiles.write(path, walletWith())
      val wallet = PropLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
      try
        assertEquals(wallet.account.resting, Vector.empty)
        val plan =
          OrderPlan("XMR", 0, Side.Buy, OrderKind.Hard, 1, 100, 100, 1, 100, Some(100), 1, false)
        assert(wallet.fill(plan, 1800000000000L).isRight)
        assertEquals(ujson.read(Files.readString(path))("resting"), ujson.Arr())
      finally wallet.close()
    }
  }

  test("a saved resting order loads with its price as written") {
    withDirectory { directory =>
      LocalFiles.write(directory.resolve("wallet-mainnet.json"), walletWith(restingOrder()))
      val wallet = PropLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
      try
        assertEquals(
          wallet.account.resting,
          Vector(
            PropOrder(
              "0x" + "a" * 32,
              "XMR",
              Side.Buy,
              BigDecimal("546"),
              BigDecimal("0.2"),
              false,
              1800000000000L
            )
          )
        )
        assertEquals(wallet.account.resting.head.price.bigDecimal.toPlainString, "546.00")
        assertEquals(wallet.usable, BigDecimal("890.78362"))
      finally wallet.close()
    }
  }

  for (label, resting) <- Vector(
      "a malformed id" -> Vector(restingOrder("0x12")),
      "a repeated id" -> Vector(restingOrder(), restingOrder()),
      "an invalid market" -> Vector(restingOrder().tap(_("coin") = "xmr!")),
      "a zero price" -> Vector(restingOrder().tap(_("price") = "0")),
      "a zero quantity" -> Vector(restingOrder().tap(_("quantity") = "0")),
      "more than twenty orders" ->
        (1 to 21).toVector.map(n => restingOrder("0x%032x".format(n)))
    )
  do
    test("a saved wallet with " + label + " resting is refused and preserved") {
      withDirectory { directory =>
        val path = directory.resolve("wallet-mainnet.json")
        val saved = walletWith(resting*)
        LocalFiles.write(path, saved)
        intercept[Exception](
          PropLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
        )
        assertEquals(Files.readString(path), saved)
        val released = LocalFiles.lock(directory.resolve("wallet-mainnet.lock"))
        released.close()
      }
    }
