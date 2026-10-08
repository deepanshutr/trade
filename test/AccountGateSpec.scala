package desk

import java.nio.file.{Files, Path}
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption
import java.util.concurrent.TimeUnit

class AccountGateSpec extends TempDirectorySuite:
  private val owner = "0x" + "1" * 40
  private def fixture(body: Path => Unit): Unit =
    val root = tempDirectory("desk-account-gate-")
    body(root)

  test("a paused owner never enters the order callback and another owner can trade") {
    fixture { root =>
      AccountGate.withOrder(root, "testnet", owner)(())
      Files.writeString(root.resolve("testnet-" + owner.drop(2) + ".paused"), "wallet change")
      var sent = false
      intercept[IllegalArgumentException] {
        AccountGate.withOrder(root, "testnet", owner) { sent = true }
      }
      assert(!sent)
      assert(AccountGate.paused(root, "testnet", owner))
      assert(!AccountGate.paused(root, "mainnet", owner))
      AccountGate.withOrder(root, "mainnet", owner)(())
      AccountGate.withOrder(root, "testnet", "0x" + "2" * 40) { sent = true }
      assert(sent)
    }
  }

  test("the account lock covers the callback until sending finishes") {
    fixture { root =>
      AccountGate.withOrder(root, "testnet", owner) {
        val lock = root.resolve("testnet-" + owner.drop(2) + ".lock")
        val process = Platform
          .lockChild(
            new ProcessBuilder(
              "/usr/bin/python3",
              "-c",
              "import fcntl,sys\nf=open(sys.argv[1],'r+')\ntry: fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)\nexcept BlockingIOError: sys.exit(73)",
              lock.toString
            ),
            "probe",
            lock.toString
          )
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        try
          assert(process.waitFor(3, TimeUnit.SECONDS))
          assertEquals(process.exitValue(), 73)
        finally if process.isAlive then process.destroyForcibly()
      }
      AccountGate.withOrder(root, "testnet", owner)(())
    }
  }

  test("symbolic account directory and lock fail closed") {
    assume(Platform.links, "symbolic links need developer mode on Windows")
    fixture { root =>
      val real = Files.createDirectory(root.resolve("real"))
      Platform.setMode(real, "rwx------")
      val symbolic = Files.createSymbolicLink(root.resolve("symbolic"), real)
      intercept[IllegalArgumentException] {
        AccountGate.withOrder(symbolic, "testnet", owner)(fail("sent"))
      }
      val lock = root.resolve("testnet-" + owner.drop(2) + ".lock")
      Files.createSymbolicLink(lock, root.resolve("elsewhere"))
      intercept[IllegalArgumentException] {
        AccountGate.withOrder(root, "testnet", owner)(fail("sent"))
      }
    }
  }

  test("an account lock held by another process refuses the callback") {
    fixture { root =>
      AccountGate.withOrder(root, "testnet", owner)(())
      val lock = root.resolve("testnet-" + owner.drop(2) + ".lock")
      val signal = root.resolve("held")
      val process = Platform
        .lockChild(
          new ProcessBuilder(
            "/usr/bin/python3",
            "-c",
            "import fcntl,pathlib,sys\nf=open(sys.argv[1],'r+')\nfcntl.lockf(f,fcntl.LOCK_EX)\npathlib.Path(sys.argv[2]).touch()\nsys.stdin.read(1)",
            lock.toString,
            signal.toString
          ),
          "hold",
          lock.toString,
          signal.toString
        )
        .start()
      try
        val end = System.nanoTime() + TimeUnit.SECONDS.toNanos(3)
        while !Files.exists(signal) && process.isAlive && System.nanoTime() < end do Thread.sleep(2)
        assert(Files.exists(signal), "Python did not acquire the account lock")
        var sent = false
        intercept[Exception] {
          AccountGate.withOrder(root, "testnet", owner) { sent = true }
        }
        assert(!sent)
      finally
        process.getOutputStream.close()
        if !process.waitFor(3, TimeUnit.SECONDS) then process.destroyForcibly()
      AccountGate.withOrder(root, "testnet", owner)(())
    }
  }

  test("invalid account identities and permissive roots fail closed") {
    fixture { root =>
      Vector(("unknown", owner), ("testnet", "0x" + "A" * 40), ("testnet", "../outside")).foreach {
        (network, address) =>
          assert(AccountGate.paused(root, network, address))
          var sent = false
          intercept[IllegalArgumentException] {
            AccountGate.withOrder(root, network, address) { sent = true }
          }
          assert(!sent)
      }
      Platform.setMode(root, "rwxr-xr-x")
      var sent = false
      intercept[IllegalArgumentException] {
        AccountGate.withOrder(root, "testnet", owner) { sent = true }
      }
      assert(!sent)
      Platform.setMode(root, "rwx------")
      AccountGate.withOrder(root, "testnet", owner)(())
    }
  }

  test("a symbolic pause marker blocks the account even when its target is missing") {
    assume(Platform.links, "symbolic links need developer mode on Windows")
    fixture { root =>
      Files.createSymbolicLink(
        root.resolve("testnet-" + owner.drop(2) + ".paused"),
        root.resolve("absent")
      )
      assert(AccountGate.paused(root, "testnet", owner))
      intercept[IllegalArgumentException] {
        AccountGate.withOrder(root, "testnet", owner)(fail("sent"))
      }
    }
  }

  test("a rejected acquisition in this JVM preserves the active account lock") {
    fixture { root =>
      AccountGate.withOrder(root, "testnet", owner) {
        intercept[Exception] {
          AccountGate.withOrder(root, "testnet", owner)(fail("nested send"))
        }
        val process = Platform
          .lockChild(
            new ProcessBuilder(
              "/usr/bin/python3",
              "-c",
              "import fcntl,sys\nf=open(sys.argv[1],'r+')\ntry: fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)\nexcept BlockingIOError: sys.exit(73)",
              root.resolve("testnet-" + owner.drop(2) + ".lock").toString
            ),
            "probe",
            root.resolve("testnet-" + owner.drop(2) + ".lock").toString
          )
          .start()
        try
          assert(process.waitFor(3, TimeUnit.SECONDS))
          assertEquals(process.exitValue(), 73)
        finally if process.isAlive then process.destroyForcibly()
      }
    }
  }

  test("canonical parent aliases cannot release an active account lock") {
    fixture { root =>
      val real = Files.createDirectory(root.resolve("real"))
      val accounts = Files.createDirectory(real.resolve("accounts"))
      Platform.setMode(accounts, "rwx------")
      val alias = Platform.alias(root.resolve("alias"), real).resolve("accounts")
      AccountGate.withOrder(accounts, "testnet", owner) {
        // The canonical key refuses it, before any lock call that could drop the held lock.
        interceptMessage[IllegalArgumentException](
          "requirement failed: an order already owns this account"
        ) {
          AccountGate.withOrder(alias, "testnet", owner)(fail("aliased nested send"))
        }
        val process = Platform
          .lockChild(
            new ProcessBuilder(
              "/usr/bin/python3",
              "-c",
              "import fcntl,sys\nf=open(sys.argv[1],'r+')\ntry: fcntl.lockf(f,fcntl.LOCK_EX|fcntl.LOCK_NB)\nexcept BlockingIOError: sys.exit(73)",
              accounts.resolve("testnet-" + owner.drop(2) + ".lock").toString
            ),
            "probe",
            accounts.resolve("testnet-" + owner.drop(2) + ".lock").toString
          )
          .start()
        try
          assert(process.waitFor(3, TimeUnit.SECONDS))
          assertEquals(process.exitValue(), 73)
        finally if process.isAlive then process.destroyForcibly()
      }
      AccountGate.withOrder(alias, "testnet", owner)(())
      // Java takes a junction for a folder, so cleanup would walk into it twice.
      Files.delete(alias.getParent)
    }
  }

  test("callback and channel-open failures release the process-local account guard") {
    fixture { root =>
      intercept[java.io.IOException] {
        AccountGate.withOrder(root, "testnet", owner)(
          throw new java.io.IOException("callback failed")
        )
      }
      AccountGate.withOrder(root, "testnet", owner)(())
      val lock = root.resolve("testnet-" + owner.drop(2) + ".lock")
      Files.delete(lock)
      Files.createDirectory(lock)
      intercept[java.io.IOException] {
        AccountGate.withOrder(root, "testnet", owner)(fail("sent with directory lock"))
      }
      Files.delete(lock)
      AccountGate.withOrder(root, "testnet", owner)(())
    }
  }
