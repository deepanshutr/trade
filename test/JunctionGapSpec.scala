package desk

import java.nio.file.{Files, Path}

// Each refusal of a symbolic link also refuses a Windows junction, by its own message.
class JunctionGapSpec extends TempDirectorySuite:
  private val owner = "0x" + "1" * 40
  private val name = "testnet-" + "1" * 40

  // Cleanup would walk into a junction as if it were a folder, so the alias goes first.
  private def aliased(body: (Path, Path, Path) => Unit): Unit =
    val root = tempDirectory("desk-junction-gap")
    val real = Files.createDirectory(root.resolve("real"))
    Platform.setMode(real, "rwx------")
    val alias = Platform.alias(root.resolve("alias"), real)
    try body(root, real, alias)
    finally Files.deleteIfExists(alias)

  test("a linked account folder reads as paused and refuses the order before any lock") {
    aliased { (_, real, alias) =>
      assert(AccountGate.paused(alias, "testnet", owner))
      interceptMessage[IllegalArgumentException]("requirement failed: account state is symbolic") {
        AccountGate.withOrder(alias, "testnet", owner)(fail("sent"))
      }
      assert(!Files.exists(real.resolve(name + ".lock")))
    }
  }

  test("a linked account lock is refused by name") {
    aliased { (root, real, _) =>
      val lock = Platform.alias(root.resolve(name + ".lock"), real)
      try
        interceptMessage[IllegalArgumentException]("requirement failed: account lock is symbolic") {
          AccountGate.withOrder(root, "testnet", owner)(fail("sent"))
        }
      finally Files.deleteIfExists(lock)
    }
  }

  test("a linked state folder is refused before a journal opens") {
    aliased { (_, real, alias) =>
      interceptMessage[IllegalArgumentException](
        "requirement failed: state directory is symbolic"
      )(OrderJournal.open(alias, name).close())
      assert(!Files.exists(real.resolve(name + ".lock")))
    }
  }

  test("a linked state lock is refused by name") {
    aliased { (root, real, _) =>
      val lock = Platform.alias(root.resolve(name + ".lock"), real)
      try
        interceptMessage[IllegalArgumentException]("requirement failed: state lock is symbolic")(
          OrderJournal.open(root, name).close()
        )
      finally Files.deleteIfExists(lock)
    }
  }
