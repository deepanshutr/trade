package desk

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock}
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption, LinkOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.UUID

final case class SavedOrder(
    id: String,
    nonce: Long,
    coin: String,
    asset: Int,
    side: Side,
    quantity: String,
    limit: String,
    reference: String,
    expiresAfter: Long,
    createdAt: Long,
    owner: Option[String] = None,
    reduceOnly: Boolean = false,
    post: Boolean = false,
    oid: Option[Long] = None,
    remaining: Option[String] = None
)

final class OrderJournal private (
    directory: Path,
    file: Path,
    channel: FileChannel,
    lock: FileLock,
    beforeReplace: () => Unit,
    afterReplace: () => Unit
) extends AutoCloseable:
  private var lastNonce = 0L
  private var saved = Option.empty[SavedOrder]
  private var held = Vector.empty[SavedOrder]

  private def read(p: ujson.Value): SavedOrder =
    SavedOrder(
      p("id").str,
      p("nonce").str.toLong,
      p("coin").str,
      p("asset").num.toInt,
      Side.valueOf(p("side").str),
      p("quantity").str,
      p("limit").str,
      p("reference").str,
      p("expiresAfter").str.toLong,
      p("createdAt").str.toLong,
      p.obj.get("owner").flatMap(_.strOpt),
      p.obj.get("reduceOnly").exists(_.bool),
      p.obj.get("post").exists(_.bool),
      p.obj.get("oid").map(_.str.toLong),
      p.obj.get("remaining").map(_.str)
    )

  if Files.exists(file, LinkOption.NOFOLLOW_LINKS) then
    require(TradingConfig.privateFile(file), "saved state must be private")
    require(Files.size(file) <= 32768, "saved state is too large")
    val j = ujson.read(Files.readString(file))
    val version = j("version").num
    require(version == 1 || version == 2)
    lastNonce = j("lastNonce").str.toLong
    require(lastNonce >= 0)
    if !j("pending").isNull then
      saved = Some(read(j("pending")))
      require(saved.get.id.matches("0x[0-9a-f]{32}") && saved.get.nonce <= lastNonce)
    if version == 2 then
      held = j("resting").arr.toVector.map(read)
      require(held.nonEmpty)
      held.foreach { r =>
        require(
          r.id.matches("0x[0-9a-f]{32}") && r.nonce <= lastNonce && r.oid.exists(_ >= 0) &&
            r.remaining.nonEmpty
        )
      }
      require((held.map(_.id) ++ saved.map(_.id)).distinct.size == held.size + saved.size)

  def pending: Option[SavedOrder] = synchronized(saved)

  def resting: Vector[SavedOrder] = synchronized(held)

  private def encode(p: SavedOrder, resting: Boolean): ujson.Obj =
    val order = ujson.Obj(
      "id" -> p.id,
      "nonce" -> p.nonce.toString,
      "coin" -> p.coin,
      "asset" -> p.asset,
      "side" -> p.side.toString,
      "quantity" -> p.quantity,
      "limit" -> p.limit,
      "reference" -> p.reference,
      "expiresAfter" -> p.expiresAfter.toString,
      "createdAt" -> p.createdAt.toString,
      "owner" -> p.owner.fold[ujson.Value](ujson.Null)(ujson.Str(_))
    )
    // Desk writes a flag only when it is set, so an opening order keeps its version 1 shape.
    if p.reduceOnly then order("reduceOnly") = true
    if resting then
      order("post") = p.post
      order("oid") = p.oid.get.toString
      order("remaining") = p.remaining.get
    else if p.post then order("post") = true
    order

  private def persist(
      nonce: Long,
      next: Option[SavedOrder],
      nextResting: Vector[SavedOrder] = held
  ): Unit =
    val value = next.fold[ujson.Value](ujson.Null)(encode(_, resting = false))
    val body =
      if nextResting.isEmpty then
        ujson.Obj("version" -> 1, "lastNonce" -> nonce.toString, "pending" -> value)
      else
        ujson.Obj(
          "version" -> 2,
          "lastNonce" -> nonce.toString,
          "pending" -> value,
          "resting" -> ujson.Arr.from(nextResting.map(encode(_, resting = true)))
        )
    val bytes = ujson.write(body).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    require(bytes.length <= 32768, "saved state is too large")
    val temporary = directory.resolve(UUID.randomUUID().toString + ".tmp")
    val permissions =
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    try
      val out = FileChannel.open(
        temporary,
        java.util.Set.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE),
        permissions
      )
      try
        val buffer = ByteBuffer.wrap(bytes)
        while buffer.hasRemaining do out.write(buffer)
        out.force(true)
      finally out.close()
      beforeReplace()
      Files.move(
        temporary,
        file,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      afterReplace()
      val parent = FileChannel.open(directory, StandardOpenOption.READ)
      try parent.force(true)
      finally parent.close()
      lastNonce = nonce
      saved = next
      held = nextResting
    finally Files.deleteIfExists(temporary)

  def nextNonce(now: Long): Long = synchronized {
    require(now >= 0)
    if saved.nonEmpty then throw new IllegalStateException("an order is unresolved")
    val next = now.max(Math.addExact(lastNonce, 1L))
    persist(next, None)
    next
  }

  def reserve(plan: OrderPlan, owner: String, now: Long): SavedOrder = synchronized {
    require(now >= 0)
    if saved.nonEmpty then throw new IllegalStateException("an order is unresolved")
    val nonce = now.max(Math.addExact(lastNonce, 1L))
    val p = SavedOrder(
      "0x" + UUID.randomUUID().toString.replace("-", ""),
      nonce,
      plan.coin,
      plan.asset,
      plan.side,
      Trading.format(plan.quantity),
      Trading.format(plan.limit),
      Trading.format(plan.reference),
      Math.addExact(now, 5000L),
      now,
      Some(owner),
      plan.reduceOnly,
      plan.post
    )
    persist(nonce, Some(p))
    p
  }

  // The pending order is now on the book. Its remaining size starts as its whole quantity.
  def rest(id: String, oid: Long): SavedOrder = synchronized {
    require(saved.exists(_.id == id), "saved order does not match")
    require(oid >= 0)
    val order = saved.get.copy(oid = Some(oid), remaining = Some(saved.get.quantity))
    persist(lastNonce, None, held :+ order)
    order
  }

  def remaining(id: String, size: String): Unit = synchronized {
    require(held.exists(_.id == id), "resting order does not match")
    persist(
      lastNonce,
      saved,
      held.map(r => if r.id == id then r.copy(remaining = Some(size)) else r)
    )
  }

  def settle(id: String): Unit = synchronized {
    require(held.exists(_.id == id), "resting order does not match")
    persist(lastNonce, saved, held.filterNot(_.id == id))
  }

  def finish(id: String): Unit = synchronized {
    require(saved.exists(_.id == id), "saved order does not match")
    persist(lastNonce, None)
  }

  def close(): Unit =
    lock.release()
    channel.close()

object OrderJournal:
  def open(
      directory: Path,
      identity: String,
      beforeReplace: () => Unit = () => (),
      afterReplace: () => Unit = () => ()
  ): OrderJournal =
    require(identity.matches("(mainnet|testnet)-[0-9a-f]{40}"), "invalid state identity")
    require(!Files.isSymbolicLink(directory), "state directory is symbolic")
    Files.createDirectories(directory)
    Files.setPosixFilePermissions(directory, PosixFilePermissions.fromString("rwx------"))
    val lockPath = directory.resolve(identity + ".lock")
    require(!Files.isSymbolicLink(lockPath), "state lock is symbolic")
    val permissions =
      PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
    val options = java.util.Set.of[java.nio.file.OpenOption](
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
      LinkOption.NOFOLLOW_LINKS
    )
    val channel = FileChannel.open(lockPath, options, permissions)
    var held = Option.empty[FileLock]
    try
      held = Option(channel.tryLock())
      require(held.nonEmpty, "another process owns this signing state")
      new OrderJournal(
        directory,
        directory.resolve(identity + ".json"),
        channel,
        held.get,
        beforeReplace,
        afterReplace
      )
    catch
      case error: Throwable =>
        held.foreach(_.release())
        channel.close()
        throw error
