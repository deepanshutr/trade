package desk

import java.nio.channels.FileChannel
import java.nio.file.{LinkOption, Path, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions

object PublicRequestBudget:
  private val monitor = new Object
  private lazy val shared = new PublicRequestBudget(
    Path.of(System.getProperty("user.home"), ".local/state/desk/public-request-budget"),
    () => System.currentTimeMillis()
  )
  def cost(body: String): Int =
    val request = ujson.read(body)
    request("type").str match
      case "l2Book"         => 2
      case "candleSnapshot" =>
        val range = request("req")
        val step = State.intervalMs(range("interval").str)
        val count = ((BigDecimal(range("endTime").num) - BigDecimal(range("startTime").num))
          .max(BigDecimal(0)) / step + 1).min(BigDecimal(5000))
        20 + (count / 60).setScale(0, BigDecimal.RoundingMode.CEILING).toInt
      case _ => 20
  def waitFor(body: String): Unit = Thread.sleep(shared.reserve(cost(body)))

final class PublicRequestBudget(directory: Path, wall: () => Long):
  def reserve(weight: Int): Long = PublicRequestBudget.monitor.synchronized {
    require(weight >= 1 && weight <= 104)
    LocalFiles.directory(directory)
    val path = directory.resolve("schedule.lock")
    val channel = FileChannel.open(
      path,
      java.util.Set.of[java.nio.file.OpenOption](
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        LinkOption.NOFOLLOW_LINKS
      ),
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    )
    try
      require(TradingConfig.privateFile(path), "public request lock is not private")
      val lock = channel.lock()
      try
        val now = wall()
        require(now >= 0)
        val saved = directory.resolve("schedule.txt")
        val next = LocalFiles.read(saved, 32L).map(_.trim.toLong).getOrElse(now)
        require(
          next >= 0 && BigInt(next) <= BigInt(now) + 300000L,
          "public request schedule is invalid or clock moved backwards"
        )
        val delay = (weight.toLong * 60000L + 699L) / 700L
        val deadline = Math.addExact(now.max(next), delay)
        LocalFiles.write(saved, deadline.toString)
        deadline - now
      finally lock.release()
    finally channel.close()
  }
