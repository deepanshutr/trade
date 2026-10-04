package desk

import java.nio.file.Path

final case class PropPosition(quantity: BigDecimal, entry: BigDecimal)
final case class PropAccount(
    cash: BigDecimal,
    realized: BigDecimal = BigDecimal(0),
    fees: BigDecimal = BigDecimal(0),
    positions: Map[String, PropPosition] = Map.empty,
    orders: Vector[String] = Vector.empty,
    count: Long = 0L,
    deposit: BigDecimal
)

object PropLedger:
  private def add(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.add(b.bigDecimal))
  private def subtract(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.subtract(b.bigDecimal))
  private def multiply(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.multiply(b.bigDecimal))

  private object Json extends upickle.AttributeTagged:
    override def serializeDefaults: Boolean = true
    private given ReadWriter[PropPosition] = macroRW
    private given ReadWriter[PropAccount] = macroRW
    def decode(text: String): PropAccount =
      val data = ujson.read(text)
      require(
        Set("cash", "deposit", "realized", "fees", "positions", "orders", "count")
          .subsetOf(data.obj.keySet.toSet),
        "incomplete saved wallet"
      )
      read[PropAccount](data)
    def encode(account: PropAccount): String = write(account)

  // The same immutable projection gates review and the authoritative save.
  private def project(
      saved: PropAccount,
      fee: BigDecimal,
      plan: OrderPlan
  ): Either[String, PropAccount] =
    val q = plan.estimatedQuantity
    val value = plan.estimatedValue
    if !State.validMarket(plan.coin) then Left("Prop rejected: invalid market")
    else if q <= 0 || q > plan.quantity || value <= 0 then
      Left("Prop rejected: no valid fill at this limit")
    else if saved.positions.keys.exists(_ != plan.coin) then
      Left(
        "Prop holds " + saved.positions.keys.head + ". Return to that market before opening another position"
      )
    else
      val price = value / q
      val boundary = multiply(q, plan.limit)
      val permitted = if plan.side == Side.Buy then value <= boundary else value >= boundary
      if !permitted then Left("Prop rejected: fill exceeds reviewed limit")
      else
        val old = saved.positions.getOrElse(plan.coin, PropPosition(BigDecimal(0), price))
        val buying = plan.side == Side.Buy
        val realized = if buying then BigDecimal(0) else subtract(value, multiply(q, old.entry))
        val quantity = if buying then add(old.quantity, q) else subtract(old.quantity, q)
        val entry =
          if quantity == 0 then price
          else if buying then add(multiply(old.quantity, old.entry), value) / quantity
          else old.entry
        val cost = multiply(value, fee)
        val cash =
          subtract(if buying then subtract(saved.cash, value) else add(saved.cash, value), cost)
        if !buying && q > old.quantity then Left("Prop limit: not enough " + plan.coin + " to sell")
        else if cash < 0 then
          Left(
            "Prop limit: not enough stables including fees. Lower order sizes in settings. " +
              "The change applies to the next order."
          )
        else
          val positions = if quantity == 0 then Map.empty[String, PropPosition]
          else Map(plan.coin -> PropPosition(quantity, entry))
          Right(
            saved.copy(
              cash = cash,
              realized = add(saved.realized, realized),
              fees = add(saved.fees, cost),
              positions = positions
            )
          )

  def open(directory: Path, network: String, balance: BigDecimal, fee: BigDecimal): PropLedger =
    require(Set("mainnet", "testnet").contains(network))
    require(
      balance > 0 && balance <= BigDecimal("1000000000") && fee >= 0 && fee <= BigDecimal("0.01")
    )
    val path = directory.resolve(s"wallet-$network.json")
    val lease = LocalFiles.lock(directory.resolve(s"wallet-$network.lock"))
    try
      val account = LocalFiles
        .read(path, 2_000_000L)
        .map(Json.decode)
        .getOrElse(PropAccount(balance, deposit = balance))
      require(account.deposit > 0 && account.deposit <= BigDecimal("1000000000"))
      require(
        account.cash >= 0 && account.count >= 0 && account.orders.size <= 1000 && account.fees >= 0
      )
      require(account.positions.size <= 1 && account.positions.forall { (coin, p) =>
        State.validMarket(coin) && p.entry > 0 && p.quantity > 0
      })
      new PropLedger(account, fee, next => LocalFiles.write(path, Json.encode(next)), lease)
    catch
      case error: Exception =>
        lease.close()
        throw error

final class PropLedger private[desk] (
    initial: PropAccount,
    fee: BigDecimal,
    persist: PropAccount => Unit,
    lease: AutoCloseable
) extends AutoCloseable:
  @volatile private var saved = initial
  @volatile private var healthy = true
  @volatile private var closed = false
  def account: PropAccount = saved
  def available: Boolean = healthy && !closed

  def check(plan: OrderPlan, snapshot: PropAccount): Either[String, Unit] =
    PropLedger.project(snapshot, fee, plan).map(_ => ())

  def fill(
      plan: OrderPlan,
      at: Long,
      expectedCount: Option[Long] = None
  ): Either[String, String] = synchronized {
    if !available then Left("Prop storage unavailable. Restart to recover")
    else if expectedCount.exists(_ != saved.count) then
      Left("Prop canceled: account changed after review")
    else
      PropLedger.project(saved, fee, plan).flatMap { projected =>
        val q = plan.estimatedQuantity
        val price = plan.estimatedValue / q
        val cost = PropLedger.multiply(plan.estimatedValue, fee)
        val status = if q < plan.quantity then "partial" else "filled"
        val count = Math.addExact(saved.count, 1L)
        val result =
          s"PROP ${plan.coin} ${plan.side.toString.toLowerCase} $status ${Trading.format(q)} at ${Trading.display(price)}\nFee $$${Trading.format(cost)} | #$count ${java.time.Instant.ofEpochMilli(at)}"
        val next = projected.copy(orders = (saved.orders :+ result).takeRight(1000), count = count)
        try
          persist(next)
          saved = next
          Right(result)
        catch
          case _: Exception =>
            healthy = false
            Left("Prop storage failed. Restart to recover")
      }
  }

  def close(): Unit = synchronized {
    if !closed then
      closed = true
      lease.close()
  }
