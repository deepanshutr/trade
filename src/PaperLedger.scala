package desk

import java.nio.file.Path
import upickle.default.{ReadWriter, macroRW, read, write}

final case class PaperPosition(quantity: BigDecimal, entry: BigDecimal)
final case class PaperAccount(
    cash: BigDecimal,
    realized: BigDecimal = BigDecimal(0),
    fees: BigDecimal = BigDecimal(0),
    positions: Map[String, PaperPosition] = Map.empty,
    orders: Vector[String] = Vector.empty,
    count: Long = 0L
)

object PaperLedger:
  private given ReadWriter[PaperPosition] = macroRW
  private given ReadWriter[PaperAccount] = macroRW

  def open(directory: Path, network: String, balance: BigDecimal, fee: BigDecimal): PaperLedger =
    require(Set("mainnet", "testnet").contains(network))
    require(
      balance > 0 && balance <= BigDecimal("1000000000") && fee >= 0 && fee <= BigDecimal("0.01")
    )
    val path = directory.resolve(s"paper-$network.json")
    val lease = LocalFiles.lock(directory.resolve(s"paper-$network.lock"))
    try
      val account = LocalFiles
        .read(path, 2_000_000L)
        .map(read[PaperAccount](_))
        .getOrElse(PaperAccount(balance))
      require(account.count >= 0 && account.orders.size <= 1000 && account.fees >= 0)
      require(account.positions.size <= 1 && account.positions.forall { (coin, p) =>
        coin.matches("[A-Za-z0-9][A-Za-z0-9:._-]{0,39}") && p.entry > 0 && p.quantity != 0
      })
      new PaperLedger(account, fee, next => LocalFiles.write(path, write(next)), lease)
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
  private var saved = initial
  private var healthy = true
  private var closed = false
  def account: PaperAccount = synchronized(saved)
  def available: Boolean = synchronized(healthy && !closed)

  def fill(plan: OrderPlan, at: Long): Either[String, String] = synchronized {
    val q = plan.estimatedQuantity
    val value = plan.estimatedValue
    if !available then Left("Paper storage unavailable. Restart to recover")
    else if !plan.coin.matches("[A-Za-z0-9][A-Za-z0-9:._-]{0,39}") then
      Left("Paper rejected: invalid market")
    else if q <= 0 || q > plan.quantity || value <= 0 then
      Left("Paper rejected: no valid fill at this limit")
    else if saved.positions.keys.exists(_ != plan.coin) then
      Left("Close the other paper position before changing market")
    else
      val price = value / q
      val permitted = if plan.side == Side.Buy then price <= plan.limit else price >= plan.limit
      if !permitted then Left("Paper rejected: fill exceeds reviewed limit")
      else
        val old = saved.positions.getOrElse(plan.coin, PaperPosition(BigDecimal(0), price))
        val signed = if plan.side == Side.Buy then q else -q
        val closing =
          if old.quantity.signum != signed.signum then old.quantity.abs.min(q) else BigDecimal(0)
        val realized = closing * (price - old.entry) * old.quantity.signum
        val quantity = old.quantity + signed
        val entry =
          if quantity == 0 then price
          else if old.quantity.signum == signed.signum then
            (old.quantity.abs * old.entry + value) / quantity.abs
          else if quantity.signum == old.quantity.signum then old.entry
          else price
        val cost = value * fee
        val cash = saved.cash + realized - cost
        val equity = cash + quantity * (price - entry)
        val exposure = quantity.abs * price
        val reducing =
          quantity.abs < old.quantity.abs && (quantity == 0 || quantity.signum == old.quantity.signum)
        if exposure > equity && !reducing then Left("Paper rejected: one-times exposure limit")
        else
          val positions = if quantity == 0 then Map.empty[String, PaperPosition]
          else Map(plan.coin -> PaperPosition(quantity, entry))
          val status = if q < plan.quantity then "partial" else "filled"
          val result =
            s"PAPER ${saved.count + 1} $at ${plan.coin} ${plan.side.toString.toLowerCase} $status ${Trading.format(q)} at ${Trading.display(price)} fee ${Trading.format(cost)}"
          val next = PaperAccount(
            cash,
            saved.realized + realized,
            saved.fees + cost,
            positions,
            (saved.orders :+ result).takeRight(1000),
            Math.addExact(saved.count, 1L)
          )
          try
            persist(next)
            saved = next
            Right(result)
          catch
            case _: Exception =>
              healthy = false
              Left("Paper storage failed. Restart to recover")
  }

  def close(): Unit = synchronized {
    if !closed then
      closed = true
      lease.close()
  }
