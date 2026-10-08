package desk

import java.nio.channels.FileChannel
import java.nio.file.{LinkOption, Path, StandardOpenOption}
import java.util.concurrent.locks.ReentrantLock

object PublicRequestBudget:
  private val monitor = new Object
  private val expensive = new ReentrantLock(true)
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
  def execute[A](body: String)(send: => A): A =
    shared.execute(cost(body), ms => Thread.sleep(ms))(send)

final class PublicRequestBudget(directory: Path, wall: () => Long):
  def execute[A](weight: Int, sleep: Long => Unit)(send: => A): A =
    require(weight >= 1 && weight <= 104)
    def paced(): A =
      var remaining = weight
      while remaining > 0 do
        val chunk = remaining.min(2)
        sleep(reserve(chunk))
        remaining -= chunk
      send
    if weight <= 2 then paced()
    else
      PublicRequestBudget.expensive.lockInterruptibly()
      try withLock("request.lock")(paced())
      finally PublicRequestBudget.expensive.unlock()

  private def withLock[A](name: String)(body: => A): A =
    LocalFiles.directory(directory)
    val path = directory.resolve(name)
    val channel = FileChannel.open(
      path,
      java.util.Set.of[java.nio.file.OpenOption](
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        LinkOption.NOFOLLOW_LINKS
      ),
      LocalFiles.privateFile*
    )
    try
      require(TradingConfig.privateFile(path), "public request lock is not private")
      val lock = channel.lock()
      try body
      finally lock.release()
    finally channel.close()

  def reserve(weight: Int): Long = PublicRequestBudget.monitor.synchronized {
    require(weight >= 1 && weight <= 104)
    withLock("schedule.lock") {
      val now = wall()
      require(now >= 0)
      val saved = directory.resolve("schedule.txt")
      val next = LocalFiles.read(saved, 32L).map(_.trim.toLong).getOrElse(now)
      require(next >= 0, "public request schedule is invalid")
      val base = if BigInt(next) > BigInt(now) + 300000L then now else next
      val delay = (weight.toLong * 60000L + 699L) / 700L
      val deadline = Math.addExact(now.max(base), delay)
      LocalFiles.write(saved, deadline.toString)
      deadline - now
    }
  }
