package desk

import java.nio.charset.StandardCharsets.UTF_8

class OwnerStatusSpec extends munit.FunSuite:
  private val python = "/opt/tools/bin/python3"
  private val folder = "/home/test/vault"
  private val env = Map("DESK_PYTHON" -> python, "DESK_VAULT" -> folder)
  private val unavailable = "Owner status unavailable"
  private val owner = "0x1234567890abcdef1234567890abcdef1234abcd"
  private val key = "0xABCDEF0123456789abcdef0123456789ABCD0123"

  private final class Runner(reply: => ProcessRun):
    var calls = Vector.empty[(Seq[String], Array[Byte])]
    def apply(args: Seq[String], stdin: Array[Byte]): ProcessRun =
      calls :+= ((args, stdin.clone()))
      reply

  private def full: ujson.Obj = ujson.Obj(
    "network" -> "mainnet",
    "owner" -> owner,
    "phase" -> "ready",
    "candidates" -> 1,
    "key" -> key,
    "approved" -> true,
    "balance" -> "12.5",
    // ujson writes a Long as a string, so the seconds go in as an Int.
    "balanceAt" -> 1_700_000_000,
    "due" -> ujson.Arr("approve the key"),
    "message" -> "ready"
  )

  private def bytes(value: ujson.Value): Array[Byte] = ujson.write(value).getBytes(UTF_8)

  private def read(stdout: Array[Byte], exit: Int = 0): (Either[String, OwnerStatus], Runner) =
    val runner = new Runner(ProcessRun(exit, stdout))
    (OwnerStatus.read("mainnet", env, runner(_, _)), runner)

  private def readJson(value: ujson.Value): Either[String, OwnerStatus] = read(bytes(value))._1

  test("a reply with exactly the ten keys parses, from one status command with no input") {
    val (status, runner) = read(bytes(full))
    assertEquals(
      status,
      Right(
        OwnerStatus(
          owner = Some(owner),
          phase = Some("ready"),
          candidates = 1,
          key = Some(key),
          approved = Some(true),
          balance = Some(BigDecimal("12.5")),
          balanceAt = Some(1_700_000_000L),
          due = Vector("approve the key"),
          message = "ready"
        )
      )
    )
    assertEquals(runner.calls.size, 1)
    assertEquals(
      runner.calls.head._1,
      Seq(
        python,
        "-B",
        folder + "/desk_vault.py",
        "--json",
        "owner",
        "status",
        "--network",
        "mainnet"
      )
    )
    assertEquals(runner.calls.head._2.toSeq, Seq.empty[Byte])
  }

  test("the keys may come in any order and the line may end with a newline") {
    val reversed = ujson.Obj.from(full.value.toSeq.reverse)
    assert(read(bytes(reversed))._1.isRight)
    assert(read(bytes(full) ++ "\n".getBytes(UTF_8))._1.isRight)
  }

  test("each null reads as absent and renders") {
    val empty = full
    Vector("owner", "phase", "key", "approved", "balance", "balanceAt").foreach { name =>
      empty(name) = ujson.Null
    }
    empty("due") = ujson.Arr()
    empty("candidates") = 0
    empty("message") = "no owner"
    val status = readJson(empty).toOption.get
    assertEquals(
      status,
      OwnerStatus(None, None, 0, None, None, None, None, Vector.empty, "no owner")
    )
    assertEquals(OwnerStatus.ownerText(status), "no owner")
    assertEquals(OwnerStatus.approvalText(status), "not checked")
    assertEquals(OwnerStatus.fundingText(status, 1_700_000_012L), "no owner yet")
  }

  test("a failed venue read leaves the owner and shows what was not read") {
    val partial = full
    partial("approved") = ujson.Null
    partial("balance") = ujson.Null
    partial("balanceAt") = ujson.Null
    val status = readJson(partial).toOption.get
    assertEquals(OwnerStatus.ownerText(status), "ready   0x1234...abcd")
    assertEquals(OwnerStatus.approvalText(status), "not checked")
    assertEquals(OwnerStatus.fundingText(status, 1_700_000_012L), "balance not read")
    val untimed = status.copy(balance = Some(BigDecimal("3")))
    assertEquals(OwnerStatus.fundingText(untimed, 1_700_000_012L), "$3.00 at the venue")
  }

  test("the approval row names each answer") {
    val status = readJson(full).toOption.get
    assertEquals(OwnerStatus.approvalText(status), "approved")
    assertEquals(OwnerStatus.approvalText(status.copy(approved = Some(false))), "not approved")
    assertEquals(OwnerStatus.approvalText(status.copy(approved = None)), "not checked")
  }

  test("the balance line shows the amount in cents, never rounded up, and its age") {
    val status = readJson(full).toOption.get
    assertEquals(
      OwnerStatus.fundingText(status, 1_700_000_012L),
      "$12.50 at the venue, read 12 s ago"
    )
    val odd = status.copy(balance = Some(BigDecimal("0.019999")))
    assertEquals(OwnerStatus.fundingText(odd, 1_700_000_000L), "$0.01 at the venue, read 0 s ago")
    // A clock behind the reply's time shows no negative age.
    assertEquals(
      OwnerStatus.fundingText(status, 1_699_999_990L),
      "$12.50 at the venue, read 0 s ago"
    )
  }

  test("a reply with a key missing or an extra key is unavailable") {
    full.value.keys.foreach { name =>
      val missing = full
      missing.value.remove(name)
      assertEquals(readJson(missing), Left(unavailable), name)
    }
    val eleven = full
    eleven("profile") = "main"
    assertEquals(readJson(eleven), Left(unavailable))
  }

  test("a short, empty, non-JSON or non-object reply is unavailable") {
    val text = ujson.write(full)
    Vector(
      "",
      "\n",
      text.take(text.length / 2),
      "{}",
      "owner ready",
      "[" + text + "]",
      "null",
      text + "\n" + text
    ).foreach { reply =>
      assertEquals(read(reply.getBytes(UTF_8))._1, Left(unavailable), reply)
    }
  }

  test("a reply of 4096 bytes or more is unavailable, since the runner keeps only 4096") {
    val text = ujson.write(full)
    def padded(size: Int): Array[Byte] = (text + " " * (size - text.length)).getBytes(UTF_8)
    assertEquals(padded(4095).length, 4095)
    assert(read(padded(4095))._1.isRight)
    assertEquals(read(padded(4096))._1, Left(unavailable))
    assertEquals(read(padded(4097))._1, Left(unavailable))
  }

  test("a value of the wrong kind is unavailable") {
    Vector[(String, ujson.Value)](
      "network" -> ujson.Null,
      "owner" -> "not an address",
      "owner" -> "0x1234",
      "owner" -> 1,
      "phase" -> 1,
      "candidates" -> "1",
      "candidates" -> -1,
      "candidates" -> 1.5,
      "candidates" -> ujson.Null,
      "key" -> (owner + "00"),
      "approved" -> "yes",
      "balance" -> 12.5,
      "balance" -> "12,5",
      "balance" -> "1e3",
      "balance" -> "",
      "balance" -> "-1",
      "balanceAt" -> "1700000000",
      "balanceAt" -> -1,
      "balanceAt" -> 1.5,
      "due" -> "approve",
      "due" -> ujson.Arr(1),
      "due" -> ujson.Null,
      "message" -> ujson.Null
    ).foreach { (name, value) =>
      val changed = full
      changed(name) = value
      assertEquals(readJson(changed), Left(unavailable), s"$name $value")
    }
  }

  test("a reply for another network is unavailable") {
    val other = full
    other("network") = "testnet"
    assertEquals(readJson(other), Left(unavailable))
  }

  test("an exit other than 0 or a runner that fails is unavailable") {
    Vector(1, 2, 124).foreach { exit =>
      assertEquals(read(bytes(full), exit)._1, Left(unavailable), exit.toString)
    }
    val runner = new Runner(throw new IllegalArgumentException("process did not finish"))
    assertEquals(OwnerStatus.read("mainnet", env, runner(_, _)), Left(unavailable))
  }

  test("a deeply nested reply is unavailable and does not throw") {
    val deep = ("[" * 4000 + "]" * 4000).getBytes(UTF_8)
    assertEquals(read(deep)._1, Left(unavailable))
  }

  test("without the owner tooling nothing runs and the rows say so") {
    val runner = new Runner(ProcessRun(0, bytes(full)))
    assertEquals(
      OwnerStatus.read("mainnet", Map("DESK_PYTHON" -> python), runner(_, _)),
      Left("Owner tooling is not installed")
    )
    assertEquals(runner.calls, Vector.empty)
  }
