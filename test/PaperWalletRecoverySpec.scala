package desk

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class PaperWalletRecoverySpec extends munit.FunSuite:
  private def savedWallet(directory: Path): Path =
    val ledger = PaperLedger.open(directory, "mainnet", BigDecimal(1000), BigDecimal("0.001"))
    val plan = OrderPlan("XMR", 0, Side.Buy, OrderKind.Hard, 1, 100, 100, 1, 100, Some(100), 1)
    try assert(ledger.fill(plan, 1800000000000L).isRight)
    finally ledger.close()
    directory.resolve("wallet-mainnet.json")

  private def withDirectory(run: Path => Unit): Unit =
    val directory = Files.createTempDirectory("desk-wallet-recovery-")
    try run(directory)
    finally
      val paths = Files.walk(directory)
      try paths.iterator().asScala.toVector.reverse.foreach(path => Files.delete(path))
      finally paths.close()

  test("a held legacy account lock leaves the separate wallet usable and single-writer") {
    withDirectory { directory =>
      val legacyPath = directory.resolve("paper-mainnet.json")
      val legacy = """{"cash":"1000","positions":{"XMR":{"quantity":"-2","entry":"100"}}}"""
      LocalFiles.write(legacyPath, legacy)
      val oldLock = LocalFiles.lock(directory.resolve("paper-mainnet.lock"))
      try
        val wallet = PaperLedger.open(directory, "mainnet", BigDecimal(500), BigDecimal(0))
        try
          assertEquals(wallet.account, PaperAccount(BigDecimal(500), deposit = BigDecimal(500)))
          intercept[Exception](
            PaperLedger.open(directory, "mainnet", BigDecimal(900), BigDecimal(0))
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
            PaperLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
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
          PaperLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
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
            PaperLedger.open(directory, "mainnet", BigDecimal(50000), BigDecimal(0))
          )
          assertEquals(Files.readString(path), saved)
        val released = LocalFiles.lock(directory.resolve("wallet-mainnet.lock"))
        released.close()
      }
    }
