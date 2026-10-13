package desk

import java.nio.file.Path

// The quantity is signed: negative is short.
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
    resting: Vector[PropOrder] = Vector.empty,
    // The funding paid, less what was received.
    funding: BigDecimal = BigDecimal(0)
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
  private val exceeds = "Prop rejected: reduce exceeds the position"
  // Prop holds one market at a time. A ticket meets this, so it fits the panel's 38 columns for
  // a market name of up to four letters.
  private[desk] def holds(coin: String): String = "Prop holds " + coin + ". Return to that market"

  private def valid(order: PropOrder): Boolean =
    order.id.matches("0x[0-9a-f]{32}") && State.validMarket(order.coin) && order.price > 0 &&
      order.quantity > 0

  private def without(account: PropAccount, id: String): PropAccount =
    account.copy(resting = account.resting.filterNot(_.id == id))

  // Prop's texts name the money put up at its leverage, as live's do.
  private def dollars(order: PropOrder, quantity: BigDecimal): String =
    "$" + Trading
      .money(quantity, order.price, Some(PropMargin.leverage))
      .fold("--")(_.bigDecimal.toPlainString)

  // The margin an opening value needs. One over leverage is exact for prop's leverage of 5.
  private def margin(value: BigDecimal): BigDecimal =
    BigDecimal(value.bigDecimal.divide(BigDecimal(PropMargin.leverage).bigDecimal))

  // The price keeps the tick decimals it was placed with, so 552.00 reads as placed.
  // A market fill names the money it put up at the fill price, as a live fill does, then its
  // fee and its count. The money is the filled value over the leverage, in cents.
  private[desk] def fillText(
      plan: OrderPlan,
      cost: BigDecimal,
      count: Long,
      at: Long,
      leverage: Int = 1
  ): String =
    val q = plan.estimatedQuantity
    val price = plan.estimatedValue / q
    val status = if q < plan.quantity then "partial" else "filled"
    val money = Trading.dollars(
      Some(
        BigDecimal(
          plan.estimatedValue.bigDecimal
            .divide(BigDecimal(leverage).bigDecimal, 2, java.math.RoundingMode.HALF_UP)
        )
      ),
      0
    )
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

  // The same immutable projection gates review and the authoritative save. Cash is the
  // collateral: a fill moves it only by its fee and by what it realizes. A fill that faces the
  // position reduces it first, and what is left of the fill opens on the fill's side.
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
    else if saved.positions.keys.exists(_ != plan.coin) then Left(holds(saved.positions.keys.head))
    else
      val price = value / q
      val boundary = multiply(q, plan.limit)
      val permitted = if plan.side == Side.Buy then value <= boundary else value >= boundary
      if !permitted then Left("Prop rejected: fill exceeds reviewed limit")
      else
        val side = BigDecimal(if plan.side == Side.Buy then 1 else -1)
        val old = saved.positions.getOrElse(plan.coin, PropPosition(BigDecimal(0), price))
        val size = old.quantity.abs
        val reducing = if old.quantity.signum == -side.signum then q.min(size) else BigDecimal(0)
        val opening = subtract(q, reducing)
        if plan.reduceOnly && opening > 0 then Left(exceeds)
        else
          // A whole reduce realizes its exact value. A flip, which only a resting order makes,
          // reduces at the fill price and opens the rest there.
          val reduced = if opening == 0 then value else multiply(reducing, price)
          val realized = multiply(
            BigDecimal(old.quantity.signum),
            subtract(reduced, multiply(reducing, old.entry))
          )
          val left = add(old.quantity, multiply(side, reducing))
          val quantity = add(left, multiply(side, opening))
          val entry =
            if opening == 0 then old.entry
            else if reducing > 0 then price
            else add(multiply(size, old.entry), value) / add(size, q)
          def holding(quantity: BigDecimal, entry: BigDecimal): Map[String, PropPosition] =
            if quantity == 0 then Map.empty else Map(plan.coin -> PropPosition(quantity, entry))
          val cost = multiply(value, fee)
          val cash = subtract(add(saved.cash, realized), cost)
          // The opening part needs free collateral at the fill price once the reduce is done.
          val afterReduce =
            saved.copy(cash = add(saved.cash, realized), positions = holding(left, old.entry))
          val covered = opening == 0 ||
            PropMargin.free(afterReduce, price) >= add(margin(subtract(value, reduced)), cost)
          if !covered || cash < 0 then Left(noStables)
          else
            Right(
              saved.copy(
                cash = cash,
                realized = add(saved.realized, realized),
                fees = add(saved.fees, cost),
                positions = holding(quantity, entry)
              )
            )

  // The margin wallet. The spot wallet, wallet-<network>.json, is never read or written again.
  def file(network: String): String = "margin-" + network + ".json"

  def open(directory: Path, network: String, balance: BigDecimal, fee: BigDecimal): PropLedger =
    require(Set("mainnet", "testnet").contains(network))
    require(
      balance > 0 && balance <= BigDecimal("1000000000") && fee >= 0 && fee <= BigDecimal("0.01")
    )
    val path = directory.resolve(file(network))
    val lease = LocalFiles.lock(directory.resolve(s"margin-$network.lock"))
    try
      val account = LocalFiles
        .read(path, 2_000_000L)
        .map(Json.decode)
        .getOrElse(PropAccount(balance, deposit = balance))
      require(account.deposit > 0 && account.deposit <= BigDecimal("1000000000"))
      // A count past 10^18 is refused, so a save each microsecond would take over 250,000 years
      // to overflow it.
      require(
        account.cash >= 0 && account.count >= 0 && account.count <= 1_000_000_000_000_000_000L &&
          account.orders.size <= 1000 && account.fees >= 0
      )
      require(account.positions.size <= 1 && account.positions.forall { (coin, p) =>
        State.validMarket(coin) && p.entry > 0 && p.quantity != 0
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
        val count = Math.addExact(saved.count, 1L)
        save(projected, PropLedger.fillText(plan, cost, count, at, PropMargin.leverage))
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
    store(next.copy(orders = (saved.orders :+ entry).takeRight(1000), count = count))
      .map(_ => text)

  private def store(next: PropAccount): Either[String, Unit] =
    try
      persist(next)
      saved = next
      Right(())
    catch
      case _: Exception =>
        healthy = false
        Left("Prop storage failed. Restart to recover")

  // Orders rest on one market at a time: the one prop holds, or the one where orders already
  // rest. So none rests only to be cancelled at its fill. An opening order, buy or sell, rests
  // only when its margin and maker fee fit what the account leaves free at its price with the
  // other orders resting. A reduce-only order holds nothing.
  def checkResting(order: PropOrder, snapshot: PropAccount): Either[String, Unit] =
    val others = PropLedger.without(snapshot, order.id)
    snapshot.positions.keys
      .find(_ != order.coin)
      .orElse(others.resting.map(_.coin).find(_ != order.coin)) match
      case Some(held) => Left(PropLedger.holds(held))
      case None       =>
        if order.reduceOnly || PropMargin.reserve(order) <= PropMargin.free(others, order.price)
        then Right(())
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

  // The order fills at its own price with the maker fee. A reduce-only order fills only what
  // faces it in the position. Any other order fills whole, and one larger than the position it
  // faces flips it. What cannot fill is removed with its reason. The quantity is what filled.
  private[desk] def resolve(id: String, at: Long): Either[String, (String, BigDecimal)] =
    synchronized {
      if !available then Left(PropLedger.unavailable)
      else
        saved.resting.find(_.id == id).toRight(PropLedger.missing).flatMap { order =>
          val remaining = PropLedger.without(saved, id)
          val held = remaining.positions.get(order.coin).fold(BigDecimal(0))(_.quantity)
          val buying = order.side == Side.Buy
          val facing = held.signum == (if buying then -1 else 1)
          val quantity =
            if !order.reduceOnly then order.quantity
            else if facing then order.quantity.min(held.abs)
            else BigDecimal(0)
          def canceled(quantity: BigDecimal, reason: String): String =
            PropLedger.text(order, "canceled", PropLedger.dollars(order, quantity)) + s" ($reason)"
          if quantity <= 0 then
            save(remaining, canceled(order.quantity, "nothing left to reduce"), Some(at))
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
                      "nothing left to reduce"
                    )
                save(next, result, Some(at)).map((_, quantity))
        }
    }

  // The hourly payment on the position held on coin moves cash and the funding sum, and takes
  // cash to zero at most. It saves without a count or a history line, so a review open across
  // the hour still fills against the charged cash.
  def fund(coin: String, oracle: BigDecimal, rate: BigDecimal): Either[String, BigDecimal] =
    synchronized {
      if !available then Left(PropLedger.unavailable)
      else
        saved.positions.get(coin).fold[Either[String, BigDecimal]](Right(BigDecimal(0))) {
          position =>
            val charge = PropMargin.payment(position.quantity, oracle, rate).min(saved.cash)
            store(
              saved.copy(
                cash = PropLedger.subtract(saved.cash, charge),
                funding = PropLedger.add(saved.funding, charge)
              )
            ).map(_ => charge)
        }
    }

  // At or past maintenance the position closes at the mark with every resting order. Cash
  // becomes the equity there, at least zero, and realized takes what moved cash, so cash stays
  // deposit + realized - fees - funding.
  def liquidate(
      coin: String,
      mark: BigDecimal,
      maintenance: BigDecimal,
      at: Long
  ): Either[String, Option[String]] = synchronized {
    if !available then Left(PropLedger.unavailable)
    else if !PropMargin.liquidated(saved, coin, mark, maintenance) then Right(None)
    else
      val position = saved.positions(coin)
      val result = PropLedger.multiply(PropLedger.subtract(mark, position.entry), position.quantity)
      val cash = PropLedger.add(saved.cash, result).max(BigDecimal(0))
      val money = Trading.money(position.quantity.abs, mark, Some(PropMargin.leverage))
      val removed = saved.resting.size
      val text = s"$coin liquidated $$${money.fold("--")(_.bigDecimal.toPlainString)} at " +
        Trading.display(mark) + (if removed > 0 then s"\nResting orders removed: $removed" else "")
      save(
        saved.copy(
          cash = cash,
          realized = PropLedger.add(saved.realized, PropLedger.subtract(cash, saved.cash)),
          positions = saved.positions - coin,
          resting = Vector.empty
        ),
        text,
        Some(at)
      ).map(Some(_))
  }

  // A fresh account at the balance in one save. The history stays and gains the reset line.
  def reset(balance: BigDecimal, at: Long): Either[String, String] = synchronized {
    if !available then Left(PropLedger.unavailable)
    else if balance <= 0 || balance > BigDecimal("1000000000") then
      Left("Prop rejected: invalid balance")
    else
      save(
        PropAccount(balance, deposit = balance),
        "PROP reset to $" + Trading.format(balance),
        Some(at)
      )
  }

  def close(): Unit = synchronized {
    if !closed then
      closed = true
      lease.close()
  }
