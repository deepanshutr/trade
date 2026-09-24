package desk

import java.nio.file.Path

final case class PaperPosition(quantity: BigDecimal, entry: BigDecimal)
final case class PaperAccount(
    cash: BigDecimal,
    realized: BigDecimal = BigDecimal(0),
    fees: BigDecimal = BigDecimal(0),
    positions: Map[String, PaperPosition] = Map.empty,
    orders: Vector[String] = Vector.empty,
    count: Long = 0L,
    deposit: BigDecimal
)

object PaperLedger:
  private def add(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.add(b.bigDecimal))
  private def subtract(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.subtract(b.bigDecimal))
  private def multiply(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.multiply(b.bigDecimal))

  private object Json extends upickle.AttributeTagged:
    override def serializeDefaults: Boolean = true
    private given ReadWriter[PaperPosition] = macroRW
    private given ReadWriter[PaperAccount] = macroRW
    def decode(text: String): PaperAccount =
      val data = ujson.read(text)
      require(
        Set("cash", "deposit", "realized", "fees", "positions", "orders", "count")
          .subsetOf(data.obj.keySet.toSet),
        "incomplete saved wallet"
      )
      read[PaperAccount](data)
    def encode(account: PaperAccount): String = write(account)

  // The same immutable projection gates review and the authoritative save.
  private def project(
      saved: PaperAccount,
      fee: BigDecimal,
      plan: OrderPlan
  ): Either[String, PaperAccount] =
    val q = plan.estimatedQuantity
    val value = plan.estimatedValue
    if !plan.coin.matches("[A-Za-z0-9][A-Za-z0-9:._-]{0,39}") then
      Left("Paper rejected: invalid market")
    else if q <= 0 || q > plan.quantity || value <= 0 then
      Left("Paper rejected: no valid fill at this limit")
    else if saved.positions.keys.exists(_ != plan.coin) then
      Left(
        "Paper holds " + saved.positions.keys.head + ". Return to that market before opening another position"
      )
    else
      val price = value / q
      val boundary = multiply(q, plan.limit)
      val permitted = if plan.side == Side.Buy then value <= boundary else value >= boundary
      if !permitted then Left("Paper rejected: fill exceeds reviewed limit")
      else
        val old = saved.positions.getOrElse(plan.coin, PaperPosition(BigDecimal(0), price))
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
        if !buying && q > old.quantity then
          Left("Paper limit: not enough " + plan.coin + " to sell")
        else if cash < 0 then
          Left(
            "Paper limit: not enough stables including fees. Lower order sizes in settings, then restart."
          )
        else
          val positions = if quantity == 0 then Map.empty[String, PaperPosition]
          else Map(plan.coin -> PaperPosition(quantity, entry))
          Right(
            saved.copy(
              cash = cash,
              realized = add(saved.realized, realized),
              fees = add(saved.fees, cost),
              positions = positions
            )
          )

  def open(directory: Path, network: String, balance: BigDecimal, fee: BigDecimal): PaperLedger =
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
        .getOrElse(PaperAccount(balance, deposit = balance))
      require(account.deposit > 0 && account.deposit <= BigDecimal("1000000000"))
      require(
        account.cash >= 0 && account.count >= 0 && account.orders.size <= 1000 && account.fees >= 0
      )
      require(account.positions.size <= 1 && account.positions.forall { (coin, p) =>
        coin.matches("[A-Za-z0-9][A-Za-z0-9:._-]{0,39}") && p.entry > 0 && p.quantity > 0
      })
      new PaperLedger(account, fee, next => LocalFiles.write(path, Json.encode(next)), lease)
    catch
      case error: Exception =>
        lease.close()
        throw error

final class PaperLedger private (
    initial: PaperAccount,
    fee: BigDecimal,
    persist: PaperAccount => Unit,
    lease: AutoCloseable
) extends AutoCloseable:
  @volatile private var saved = initial
  @volatile private var healthy = true
  @volatile private var closed = false
  def account: PaperAccount = saved
  def available: Boolean = healthy && !closed

  def check(plan: OrderPlan, snapshot: PaperAccount): Either[String, Unit] =
    PaperLedger.project(snapshot, fee, plan).map(_ => ())

  def fill(
      plan: OrderPlan,
      at: Long,
      expectedCount: Option[Long] = None
  ): Either[String, String] = synchronized {
    if !available then Left("Paper storage unavailable. Restart to recover")
    else if expectedCount.exists(_ != saved.count) then
      Left("Paper canceled: account changed after review")
    else
      PaperLedger.project(saved, fee, plan).flatMap { projected =>
        val q = plan.estimatedQuantity
        val price = plan.estimatedValue / q
        val cost = PaperLedger.multiply(plan.estimatedValue, fee)
        val status = if q < plan.quantity then "partial" else "filled"
        val count = Math.addExact(saved.count, 1L)
        val result =
          s"PAPER ${plan.coin} ${plan.side.toString.toLowerCase} $status ${Trading.format(q)} at ${Trading.display(price)}\nFee $$${Trading.format(cost)} | #$count ${java.time.Instant.ofEpochMilli(at)}"
        val next = projected.copy(orders = (saved.orders :+ result).takeRight(1000), count = count)
        try
          persist(next)
          saved = next
          Right(result)
        catch
          case _: Exception =>
            healthy = false
            Left("Paper storage failed. Restart to recover")
      }
  }

  def close(): Unit = synchronized {
    if !closed then
      closed = true
      lease.close()
  }
