package desk

import java.math.{MathContext, RoundingMode}

enum OrderKind:
  case Hard, Best, Target, Close

enum Effect:
  case Opens, Reduces, Flips, Void

final case class MarketRules(asset: Int, szDecimals: Int, maxLeverage: Int)

final case class TradeSizes(
    small: BigDecimal = BigDecimal("500"),
    big: BigDecimal = BigDecimal("1000"),
    target: BigDecimal = BigDecimal("1000")
)

final case class TradeTerms(sizes: TradeSizes, slippage: BigDecimal)

final case class OrderIntent(
    side: Side,
    kind: OrderKind,
    big: Boolean = false,
    target: Option[BigDecimal] = None,
    price: Option[BigDecimal] = None
)

final case class OrderPlan(
    coin: String,
    asset: Int,
    side: Side,
    kind: OrderKind,
    quantity: BigDecimal,
    limit: BigDecimal,
    reference: BigDecimal,
    estimatedQuantity: BigDecimal,
    estimatedValue: BigDecimal,
    average: Option[BigDecimal],
    positionAfter: BigDecimal,
    reduceOnly: Boolean,
    post: Boolean = false
)

final case class RestingOrder(
    id: String,
    coin: String,
    asset: Int,
    side: Side,
    price: BigDecimal,
    remaining: BigDecimal,
    original: BigDecimal,
    reduceOnly: Boolean,
    createdAt: Long,
    cancelling: Boolean = false
)

final case class Fill(
    coin: String,
    side: Side,
    time: Long,
    price: BigDecimal,
    quantity: BigDecimal
)

object Trading:
  val defaultSlippage: BigDecimal = BigDecimal("0.001")
  val baseMakerFee: BigDecimal = BigDecimal("0.00015")
  val restingCap: Int = 20
  // Live and prop refuse a resting order past the cap with this text.
  val restingCapText: String = s"$restingCap orders rest. Cancel one first"
  private val zero = BigDecimal("0")
  private val one = BigDecimal("1")
  private val ten = BigDecimal("10")
  private val averageContext = new MathContext(34, RoundingMode.HALF_EVEN)
  private final case class Quote(price: BigDecimal, quantity: BigDecimal)

  private def add(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.add(b.bigDecimal))
  private def subtract(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.subtract(b.bigDecimal))
  private def multiply(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.multiply(b.bigDecimal))
  private def power(exponent: Int): BigDecimal =
    BigDecimal(java.math.BigDecimal.ONE.scaleByPowerOfTen(exponent))
  private def check(condition: Boolean, message: String): Either[String, Unit] =
    if condition then Right(()) else Left(message)

  def format(value: BigDecimal): String = value.bigDecimal.stripTrailingZeros().toPlainString()

  def displayExact(value: BigDecimal): String =
    val decimal = value.bigDecimal.stripTrailingZeros()
    Vector(decimal.toPlainString, decimal.toString.replace("E+", "e").replace("E", "e"))
      .minBy(_.length)

  def display(value: BigDecimal, width: Int = 8): String =
    val exact = format(value)
    if exact.length <= width then exact
    else
      (7 to 1 by -1).iterator
        .map { precision =>
          val rounded = value.bigDecimal
            .round(new MathContext(precision, RoundingMode.DOWN))
            .stripTrailingZeros()
            .toString
          "~" + rounded.replace("E+", "e").replace("E", "e")
        }
        .find(_.length <= width)
        .getOrElse(
          "~" + value.bigDecimal
            .round(new MathContext(1, RoundingMode.DOWN))
            .toString
            .replace("E+", "e")
            .replace("E", "e")
        )

  def action(index: Int): OrderIntent = OrderIntent(
    if index % 4 < 2 then Side.Buy else Side.Sell,
    if index < 4 then OrderKind.Hard else OrderKind.Best,
    big = index % 2 == 1
  )

  private def validDecimals(rules: MarketRules): Boolean =
    rules.szDecimals >= 0 && rules.szDecimals <= 6

  def validPrice(value: BigDecimal, rules: MarketRules): Boolean =
    if value <= zero || !validDecimals(rules) then false
    else
      val plain = value.bigDecimal.stripTrailingZeros()
      plain.scale() <= 6 - rules.szDecimals &&
      (plain.scale() <= 0 || plain.precision() <= 5)

  def priceStep(value: BigDecimal, rules: MarketRules): Option[BigDecimal] =
    if value <= zero || !validDecimals(rules) then None
    else
      val plain = value.bigDecimal.stripTrailingZeros()
      val digits = plain.precision().toLong - plain.scale().toLong
      val decimals = (5L - digits).max(0L).min((6 - rules.szDecimals).toLong).toInt
      Some(power(-decimals))

  def nextPrice(value: BigDecimal, rules: MarketRules, up: Boolean): Option[BigDecimal] =
    if !validPrice(value, rules) then None
    else
      val probe = if up then value else subtract(value, power(-(6 - rules.szDecimals)))
      priceStep(probe, rules)
        .map { step =>
          if up then add(value, step) else subtract(value, step)
        }
        .filter(validPrice(_, rules))

  def movePrice(
      value: BigDecimal,
      rules: MarketRules,
      delta: BigDecimal,
      up: Boolean
  ): Option[BigDecimal] =
    nextPrice(value, rules, up).map { tick =>
      val raw = if up then add(value, delta) else subtract(value, delta)
      boundary(raw, rules, up, strict = false) match
        case Some(far) => if up then far.max(tick) else far.min(tick)
        case None      => tick
    }

  private val takesLiquidity = "Takes liquidity. Move "

  def crossing(side: Side, price: BigDecimal, bid: BigDecimal, ask: BigDecimal): Option[String] =
    if side == Side.Buy then if price >= ask then Some(takesLiquidity + "below the ask") else None
    else if price <= bid then Some(takesLiquidity + "above the bid")
    else None

  // Each stretch of price with one step is counted at its own step, so a count across a power
  // of ten stays true. The sign says whether the price is above or below the touch.
  def ticks(price: BigDecimal, touch: BigDecimal, rules: MarketRules): Long =
    val (low, high) = if price >= touch then (touch, price) else (price, touch)
    def count(from: BigDecimal): Long =
      if from >= high then 0L
      else
        priceStep(from, rules) match
          case None       => 0L
          case Some(step) =>
            val plain = from.bigDecimal.stripTrailingZeros()
            val next = BigDecimal(
              java.math.BigDecimal.ONE.scaleByPowerOfTen(plain.precision() - plain.scale())
            )
            val end = high.min(next)
            val here = subtract(end, from).bigDecimal
              .divide(step.bigDecimal, 0, RoundingMode.HALF_UP)
              .longValue
            here + count(next)
    val total = if low <= zero then 0L else count(low)
    if price >= touch then total else -total

  private def facing(position: BigDecimal, order: RestingOrder): Boolean =
    position.signum != 0 && order.side == (if position.signum > 0 then Side.Sell else Side.Buy)

  def effects(position: BigDecimal, resting: Vector[RestingOrder]): Vector[(RestingOrder, Effect)] =
    val (near, rest) = resting.partition(facing(position, _))
    val nearest =
      if position.signum > 0 then near.sortBy(o => (o.price, o.createdAt))
      else near.sortBy(o => (-o.price, o.createdAt))
    val taken = nearest.scanLeft(zero)((sum, o) => add(sum, o.remaining))
    val ranked = nearest.zip(taken).map { (order, before) =>
      val free = subtract(position.abs, before)
      val effect =
        if order.remaining <= free then Effect.Reduces
        else if order.reduceOnly then Effect.Reduces
        else Effect.Flips
      order -> effect
    }
    val others = rest.map { order =>
      order -> (if order.reduceOnly then Effect.Void else Effect.Opens)
    }
    ranked ++ others

  def reserved(position: BigDecimal, resting: Vector[RestingOrder]): BigDecimal =
    resting
      .filter(facing(position, _))
      .foldLeft(zero)((sum, o) => add(sum, o.remaining))
      .min(position.abs)

  def money(quantity: BigDecimal, price: BigDecimal, leverage: Option[Int]): Option[BigDecimal] =
    leverage.filter(_ >= 1).map { l =>
      BigDecimal(
        multiply(quantity, price).bigDecimal
          .divide(BigDecimal(l).bigDecimal, 2, RoundingMode.HALF_UP)
      )
    }

  def dollars(value: Option[BigDecimal], width: Int): String =
    val text = value match
      case None    => "--"
      case Some(v) =>
        val plain = "$" + v.bigDecimal.toPlainString
        if plain.length <= 7 then plain else "$" + display(v, 6)
    text.reverse.padTo(width, ' ').reverse

  def priceText(price: BigDecimal, rules: MarketRules): String =
    priceStep(price, rules)
      .map(step =>
        price.bigDecimal.setScale(step.bigDecimal.scale(), RoundingMode.HALF_UP).toPlainString
      )
      .getOrElse(format(price))

  private def boundary(
      raw: BigDecimal,
      rules: MarketRules,
      up: Boolean,
      strict: Boolean
  ): Option[BigDecimal] =
    priceStep(raw, rules).flatMap { step =>
      val mode = if up then RoundingMode.CEILING else RoundingMode.FLOOR
      val rounded = BigDecimal(raw.bigDecimal.setScale(step.bigDecimal.scale(), mode))
      if !validPrice(rounded, rules) then None
      else if strict && rounded == raw then nextPrice(rounded, rules, up)
      else Some(rounded)
    }

  private def quantityFor(
      dollars: BigDecimal,
      price: BigDecimal,
      lot: BigDecimal,
      strict: Boolean = false
  ): BigDecimal =
    val denominator = multiply(price, lot)
    val mode = if strict then RoundingMode.CEILING else RoundingMode.FLOOR
    val count = BigInt(
      dollars.bigDecimal.divide(denominator.bigDecimal, 0, mode).toBigIntegerExact()
    )
    multiply(BigDecimal(if strict then count - 1 else count), lot)

  private def decimal(text: String): Either[String, BigDecimal] =
    if text == null || text.length > 64 || !text.matches("[0-9]+(?:\\.[0-9]+)?") then
      Left("Book contains an invalid number")
    else Right(BigDecimal(text))

  private def parseLevel(level: Level, rules: MarketRules): Either[String, Quote] =
    for
      price <- decimal(level.px)
      quantity <- decimal(level.sz)
      _ <- check(validPrice(price, rules), "Book price does not match market precision")
      _ <- check(quantity > zero && level.n > 0, "Book quantity and order count must be positive")
      _ <- check(
        quantity.bigDecimal.stripTrailingZeros().scale() <= rules.szDecimals,
        "Book quantity does not match market precision"
      )
    yield Quote(price, quantity)

  private def levels(
      raw: Vector[Level],
      rules: MarketRules,
      ascending: Boolean
  ): Either[String, Vector[Quote]] =
    val parsed = raw.foldLeft[Either[String, Vector[Quote]]](Right(Vector.empty)) { (acc, level) =>
      for previous <- acc; next <- parseLevel(level, rules) yield previous :+ next
    }
    parsed.flatMap { values =>
      val ordered = values.zip(values.drop(1)).forall { (a, b) =>
        if ascending then a.price < b.price else a.price > b.price
      }
      check(values.nonEmpty && ordered, "Book sides must be nonempty and strictly ordered")
        .map(_ => values)
    }

  private def limitFor(
      intent: OrderIntent,
      reference: BigDecimal,
      rules: MarketRules,
      slippage: BigDecimal
  ): Either[String, BigDecimal] =
    if intent.kind == OrderKind.Target then
      intent.target.filter(validPrice(_, rules)).toRight("Select a valid target price")
    else if intent.target.nonEmpty then Left("Only Target uses a selected price")
    else
      val buy = intent.side == Side.Buy
      val raw = multiply(reference, if buy then add(one, slippage) else subtract(one, slippage))
      boundary(raw, rules, up = !buy, strict = intent.kind == OrderKind.Best)
        .toRight("No valid price fits the selected boundary")

  private def walk(levels: Vector[Quote], requested: BigDecimal): (BigDecimal, BigDecimal) =
    var remaining = requested
    var value = zero
    levels.foreach { level =>
      val take = remaining.min(level.quantity)
      value = add(value, multiply(take, level.price))
      remaining = subtract(remaining, take)
    }
    (subtract(requested, remaining), value)

  // Execute an already reviewed prop request against current visible depth.
  // Quantity and limit remain exactly the terms the user saw.
  def fillAtLimit(
      plan: OrderPlan,
      rules: MarketRules,
      book: Book,
      currentQuote: BigDecimal,
      position: BigDecimal
  ): Either[String, OrderPlan] =
    for
      _ <- check(plan.asset == rules.asset && validDecimals(rules), "Prop market rules changed")
      _ <- check(validPrice(plan.limit, rules), "Prop limit does not match market precision")
      _ <- check(validPrice(currentQuote, rules), "Prop quote does not match market precision")
      _ <- check(
        plan.quantity > zero && plan.quantity.bigDecimal
          .stripTrailingZeros()
          .scale() <= rules.szDecimals,
        "Prop quantity does not match market precision"
      )
      bids <- levels(book.bids, rules, ascending = false)
      asks <- levels(book.asks, rules, ascending = true)
      _ <- check(bids.head.price < asks.head.price, "Book sides cross")
      eligible = (if plan.side == Side.Buy then asks else bids).filter(q =>
        if plan.side == Side.Buy then currentQuote <= q.price && q.price <= plan.limit
        else plan.limit <= q.price && q.price <= currentQuote
      )
      (filled, value) = walk(eligible, plan.quantity)
      _ <- check(filled > zero, "Prop no fill: no depth inside reviewed limit")
      average = BigDecimal(value.bigDecimal.divide(filled.bigDecimal, averageContext))
    yield plan.copy(
      estimatedQuantity = filled,
      estimatedValue = value,
      average = Some(average),
      positionAfter =
        if plan.side == Side.Buy then add(position, filled) else subtract(position, filled)
    )

  def plan(
      coin: String,
      rules: MarketRules,
      book: Book,
      reference: BigDecimal,
      position: BigDecimal,
      intent: OrderIntent,
      sizes: TradeSizes,
      leverage: Int,
      slippage: BigDecimal,
      reserved: BigDecimal = zero
  ): Either[String, OrderPlan] =
    // A press against the open side only shrinks the position. It is sent reduce only.
    val reducing = position.signum == (if intent.side == Side.Buy then -1 else 1)
    for
      _ <- check(coin.nonEmpty, "Select a market")
      _ <- check(
        rules.asset >= 0 && validDecimals(rules) && rules.maxLeverage >= 1,
        "Market rules contain invalid asset, precision or leverage limits"
      )
      _ <- check(
        leverage >= 1 && leverage <= rules.maxLeverage,
        "Leverage is outside the market limit"
      )
      _ <- check(
        sizes.small > zero && sizes.big >= sizes.small && sizes.target > zero,
        "Dollar sizes must be positive and big must be at least small"
      )
      _ <- check(validPrice(reference, rules), "Captured quote does not match market precision")
      _ <- check(
        position.bigDecimal.stripTrailingZeros().scale() <= rules.szDecimals,
        "Position does not match market precision"
      )
      _ <- check(intent.kind != OrderKind.Close || reducing, "No position to close")
      bids <- levels(book.bids, rules, ascending = false)
      asks <- levels(book.asks, rules, ascending = true)
      _ <- check(bids.head.price < asks.head.price, "Book sides cross")
      _ <- intent.price match
        case Some(p) =>
          check(intent.kind == OrderKind.Hard, "A limit order takes a sized press")
            .flatMap(_ =>
              check(validPrice(p, rules), "Price does not match market precision")
                .flatMap(_ => crossing(intent.side, p, reference, reference).toLeft(()))
            )
        case None => Right(())
      limit <- intent.price.map(Right(_)).getOrElse(limitFor(intent, reference, rules, slippage))
      // A posted order is sized and judged at its own price.
      ref = intent.price.getOrElse(reference)
      free =
        if intent.kind == OrderKind.Close then position.abs else subtract(position.abs, reserved)
      _ <- check(
        !reducing || intent.kind == OrderKind.Close || free > zero,
        "Resting orders already reduce the whole position"
      )
      lot = power(-rules.szDecimals)
      side = if intent.side == Side.Buy then asks else bids
      eligible =
        if intent.price.nonEmpty then Vector.empty[Quote]
        else
          side.takeWhile(q =>
            if intent.side == Side.Buy then q.price <= limit else q.price >= limit
          )
      available = eligible.foldLeft(zero)((sum, q) => add(sum, q.quantity))
      // An amount is the money put up. The position value is the amount times the leverage.
      dollars = multiply(
        if intent.kind == OrderKind.Target then sizes.target
        else if intent.big then sizes.big
        else sizes.small,
        BigDecimal(leverage)
      )
      cap = quantityFor(dollars, ref.max(limit), lot)
      reachesTarget =
        if intent.side == Side.Buy then limit <= side.last.price else limit >= side.last.price
      _ <- check(
        intent.kind != OrderKind.Target || reachesTarget || available >= cap,
        "Target extends beyond the known exact book"
      )
      requested = intent.kind match
        case OrderKind.Hard => quantityFor(dollars, ref, lot)
        case OrderKind.Best =>
          available.min(cap).min(quantityFor(dollars, ref, lot, strict = true))
        case OrderKind.Target => available.min(cap)
        case OrderKind.Close  => position.abs
      _ <- check(requested > zero, "No visible quantity fits this order")
      // A reduce that would leave under ten dollars at the quote closes the whole position.
      // This also stops every reduce at flat.
      closing = reducing && multiply(subtract(free, requested), ref) < ten
      quantity = if closing then free else requested
      kind = if closing then OrderKind.Close else intent.kind
      _ <- check(
        kind == OrderKind.Close || multiply(quantity, ref.min(limit)) >= ten,
        "Requested order must reach ten dollars at the quote and limit"
      )
      estimate = walk(eligible, quantity)
      (filled, value) = estimate
      average =
        if filled == zero then None
        else Some(BigDecimal(value.bigDecimal.divide(filled.bigDecimal, averageContext)))
      after = if intent.side == Side.Buy then add(position, filled) else subtract(position, filled)
    yield OrderPlan(
      coin,
      rules.asset,
      intent.side,
      kind,
      quantity,
      limit,
      ref,
      filled,
      value,
      average,
      after,
      reducing,
      intent.price.nonEmpty
    )
