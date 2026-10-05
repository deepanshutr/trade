package desk

import java.nio.file.Path

final case class PropPosition(quantity: BigDecimal, entry: BigDecimal)
final case class PropOrder(
    id: String,
    coin: String,
    side: Side,
    price: BigDecimal,
    quantity: BigDecimal,
    reduceOnly: Boolean,
    createdAt: Long
)
final case class PropAccount(
    cash: BigDecimal,
    realized: BigDecimal = BigDecimal(0),
    fees: BigDecimal = BigDecimal(0),
    positions: Map[String, PropPosition] = Map.empty,
    orders: Vector[String] = Vector.empty,
    count: Long = 0L,
    deposit: BigDecimal,
    resting: Vector[PropOrder] = Vector.empty
)

object PropLedger:
  private def add(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.add(b.bigDecimal))
  private def subtract(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.subtract(b.bigDecimal))
  private def multiply(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.multiply(b.bigDecimal))

  private[desk] val noStables =
    "Prop limit: not enough stables including fees. Lower order sizes in settings. " +
      "The change applies to the next order."
  // A ticket meets this one, so it fits the panel's 38 columns.
  private[desk] val restingNoStables = "Not enough stables for order and fee"
  private val changed = "Prop canceled: account changed after review"
  private[desk] val unavailable = "Prop storage unavailable. Restart Desk"
  private val missing = "Prop order not found"

  private def valid(order: PropOrder): Boolean =
    order.id.matches("0x[0-9a-f]{32}") && State.validMarket(order.coin) && order.price > 0 &&
      order.quantity > 0

  // A resting buy holds its value and its maker fee. A resting sell holds no cash.
  private def reservation(order: PropOrder): BigDecimal =
    if order.side == Side.Sell then BigDecimal(0)
    else multiply(multiply(order.quantity, order.price), add(BigDecimal(1), Trading.baseMakerFee))

  private def usable(account: PropAccount): BigDecimal =
    account.resting.foldLeft(account.cash)((left, order) => subtract(left, reservation(order)))

  private def without(account: PropAccount, id: String): PropAccount =
    account.copy(resting = account.resting.filterNot(_.id == id))

  private def dollars(order: PropOrder, quantity: BigDecimal): String =
    "$" + Trading.money(quantity, order.price, Some(1)).fold("--")(_.bigDecimal.toPlainString)

  // The price keeps the tick decimals it was placed with, so 552.00 reads as placed.
  // A market fill names the money it put up at the fill price, as a live fill does, then its
  // fee and its count. Prop trades at one times leverage, so that money is the filled value.
  private[desk] def fillText(plan: OrderPlan, cost: BigDecimal, count: Long, at: Long): String =
    val q = plan.estimatedQuantity
    val price = plan.estimatedValue / q
    val status = if q < plan.quantity then "partial" else "filled"
    val money =
      Trading.dollars(Some(plan.estimatedValue.setScale(2, BigDecimal.RoundingMode.HALF_UP)), 0)
    s"PROP ${plan.coin} ${plan.side.toString.toLowerCase} $status $money at ${Trading.display(price)}\nFee $$${Trading.format(cost)} | #$count ${java.time.Instant.ofEpochMilli(at)}"

  private def text(order: PropOrder, words: String, amount: String): String =
    s"${order.coin} $words $amount at ${order.price.bigDecimal.toPlainString}"

  private object Json extends upickle.AttributeTagged:
    override def serializeDefaults: Boolean = true
    private given ReadWriter[PropPosition] = macroRW
    private given ReadWriter[Side] = readwriter[String].bimap[Side](_.toString, Side.valueOf)
    private given ReadWriter[PropOrder] = macroRW
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
        else if cash < 0 then Left(noStables)
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
      require(
        account.resting.size <= Trading.restingCap &&
          account.resting.map(_.id).distinct.size == account.resting.size &&
          account.resting.forall(valid)
      )
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
  def usable: BigDecimal = PropLedger.usable(saved)

  def check(plan: OrderPlan, snapshot: PropAccount): Either[String, Unit] =
    PropLedger.project(snapshot, fee, plan).map(_ => ())

  def fill(
      plan: OrderPlan,
      at: Long,
      expectedCount: Option[Long] = None
  ): Either[String, String] = synchronized {
    if !available then Left(PropLedger.unavailable)
    else if expectedCount.exists(_ != saved.count) then
      Left("Prop canceled: account changed after review")
    else
      PropLedger.project(saved, fee, plan).flatMap { projected =>
        val cost = PropLedger.multiply(plan.estimatedValue, fee)
        save(projected, PropLedger.fillText(plan, cost, Math.addExact(saved.count, 1L), at))
      }
  }

  // Every change is one save under the next count, and its text joins the history with the
  // trade time when a trade caused it.
  private def save(
      next: PropAccount,
      text: String,
      at: Option[Long] = None
  ): Either[String, String] =
    val count = Math.addExact(saved.count, 1L)
    val entry = text + at.fold("")(time => s" | ${java.time.Instant.ofEpochMilli(time)}")
    val changed = next.copy(orders = (saved.orders :+ entry).takeRight(1000), count = count)
    try
      persist(changed)
      saved = changed
      Right(text)
    catch
      case _: Exception =>
        healthy = false
        Left("Prop storage failed. Restart to recover")

  // A buy rests only when its value and maker fee fit the cash that other resting buys leave.
  def checkResting(order: PropOrder, snapshot: PropAccount): Either[String, Unit] =
    val others = PropLedger.without(snapshot, order.id)
    if order.side == Side.Sell || PropLedger.reservation(order) <= PropLedger.usable(others) then
      Right(())
    else Left(PropLedger.restingNoStables)

  def rest(order: PropOrder, expectedCount: Long): Either[String, String] = synchronized {
    if !available then Left(PropLedger.unavailable)
    else if expectedCount != saved.count then Left(PropLedger.changed)
    else if saved.resting.size >= Trading.restingCap then Left(Trading.restingCapText)
    else if !PropLedger.valid(order) || saved.resting.exists(_.id == order.id) then
      Left("Prop rejected: invalid order")
    else
      checkResting(order, saved).flatMap(_ =>
        save(
          saved.copy(resting = saved.resting :+ order),
          PropLedger.text(order, "resting", PropLedger.dollars(order, order.quantity))
        )
      )
  }

  def cancel(id: String): Either[String, String] = synchronized {
    if !available then Left(PropLedger.unavailable)
    else
      saved.resting
        .find(_.id == id)
        .toRight(PropLedger.missing)
        .flatMap(order =>
          save(
            PropLedger.without(saved, id),
            PropLedger.text(order, "canceled", PropLedger.dollars(order, order.quantity))
          )
        )
  }

  // The order fills whole at its own price with the maker fee. A sell fills only what the
  // position holds. What cannot fill is removed with its reason. The quantity is what filled.
  private[desk] def resolve(id: String, at: Long): Either[String, (String, BigDecimal)] =
    synchronized {
      if !available then Left(PropLedger.unavailable)
      else
        saved.resting.find(_.id == id).toRight(PropLedger.missing).flatMap { order =>
          val remaining = PropLedger.without(saved, id)
          val held = remaining.positions.get(order.coin).fold(BigDecimal(0))(_.quantity)
          val buying = order.side == Side.Buy
          val quantity = if buying then order.quantity else order.quantity.min(held)
          def canceled(quantity: BigDecimal, reason: String): String =
            PropLedger.text(order, "canceled", PropLedger.dollars(order, quantity)) + s" ($reason)"
          if quantity <= 0 then
            save(remaining, canceled(order.quantity, "nothing left to sell"), Some(at))
              .map((_, BigDecimal(0)))
          else
            val plan = OrderPlan(
              order.coin,
              0,
              order.side,
              OrderKind.Hard,
              order.quantity,
              order.price,
              order.price,
              quantity,
              PropLedger.multiply(quantity, order.price),
              Some(order.price),
              if buying then PropLedger.add(held, quantity)
              else PropLedger.subtract(held, quantity),
              order.reduceOnly,
              post = true
            )
            PropLedger.project(remaining, Trading.baseMakerFee, plan) match
              case Left(error) =>
                val reason = if error == PropLedger.noStables then "not enough stables" else error
                save(remaining, canceled(order.quantity, reason), Some(at)).map((_, BigDecimal(0)))
              case Right(next) =>
                val amount = PropLedger.dollars(order, quantity)
                val result =
                  if quantity == order.quantity then PropLedger.text(order, "filled", amount)
                  else
                    PropLedger.text(
                      order,
                      "part filled",
                      amount + " of " + PropLedger.dollars(order, order.quantity)
                    ) + "\n" + canceled(
                      PropLedger.subtract(order.quantity, quantity),
                      "nothing left to sell"
                    )
                save(next, result, Some(at)).map((_, quantity))
        }
    }

  def close(): Unit = synchronized {
    if !closed then
      closed = true
      lease.close()
  }
