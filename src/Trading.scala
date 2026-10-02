package desk

import java.math.{MathContext, RoundingMode}

enum OrderKind:
  case Hard, Best, Target, Close

final case class MarketRules(asset: Int, szDecimals: Int, maxLeverage: Int)

final case class TradeSizes(
    small: BigDecimal = BigDecimal("500"),
    big: BigDecimal = BigDecimal("1000"),
    target: BigDecimal = BigDecimal("1000")
)

final case class OrderIntent(
    side: Side,
    kind: OrderKind,
    big: Boolean = false,
    target: Option[BigDecimal] = None
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
    reduceOnly: Boolean
)

object Trading:
  val defaultSlippage: BigDecimal = BigDecimal("0.001")
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
      slippage: BigDecimal
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
      limit <- limitFor(intent, reference, rules, slippage)
      lot = power(-rules.szDecimals)
      side = if intent.side == Side.Buy then asks else bids
      eligible = side.takeWhile(q =>
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
      cap = quantityFor(dollars, reference.max(limit), lot)
      reachesTarget =
        if intent.side == Side.Buy then limit <= side.last.price else limit >= side.last.price
      _ <- check(
        intent.kind != OrderKind.Target || reachesTarget || available >= cap,
        "Target extends beyond the known exact book"
      )
      requested = intent.kind match
        case OrderKind.Hard => quantityFor(dollars, reference, lot)
        case OrderKind.Best =>
          available.min(cap).min(quantityFor(dollars, reference, lot, strict = true))
        case OrderKind.Target => available.min(cap)
        case OrderKind.Close  => position.abs
      _ <- check(requested > zero, "No visible quantity fits this order")
      // A reduce that would leave under ten dollars at the quote closes the whole position.
      // This also stops every reduce at flat.
      closing = reducing && multiply(subtract(position.abs, requested), reference) < ten
      quantity = if closing then position.abs else requested
      kind = if closing then OrderKind.Close else intent.kind
      _ <- check(
        kind == OrderKind.Close || multiply(quantity, reference.min(limit)) >= ten,
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
      reference,
      filled,
      value,
      average,
      after,
      reducing
    )
