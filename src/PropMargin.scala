package desk

import java.math.{MathContext, RoundingMode}

// The prop margin rules: one cross account with at most one position.
object PropMargin:
  val leverage: Int = 5
  val mode: String = "cross"
  private val zero = BigDecimal(0)
  private val one = BigDecimal(1)
  // The initial margin, one over leverage, exact. A leverage with no exact reciprocal, such as 3,
  // fails on first use rather than round every margin.
  private val initial = BigDecimal(one.bigDecimal.divide(BigDecimal(leverage).bigDecimal))
  private val context = new MathContext(34, RoundingMode.HALF_EVEN)

  private def add(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.add(b.bigDecimal))
  private def subtract(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.subtract(b.bigDecimal))
  private def multiply(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.multiply(b.bigDecimal))
  private def divide(a: BigDecimal, b: BigDecimal): BigDecimal =
    BigDecimal(a.bigDecimal.divide(b.bigDecimal, context))
  private def size(position: PropPosition): BigDecimal =
    BigDecimal(position.quantity.bigDecimal.abs)

  // The venue liquidates at half the initial margin of the market's maximum leverage.
  def maintenance(rules: MarketRules): BigDecimal =
    require(rules.maxLeverage >= 1)
    divide(one, multiply(BigDecimal(2), BigDecimal(rules.maxLeverage)))

  def equity(account: PropAccount, price: BigDecimal): BigDecimal =
    account.positions.values.foldLeft(account.cash)((sum, position) =>
      add(sum, multiply(subtract(price, position.entry), position.quantity))
    )

  def used(account: PropAccount, price: BigDecimal): BigDecimal =
    account.positions.values.foldLeft(zero)((sum, position) =>
      add(sum, multiply(multiply(size(position), price), initial))
    )

  // An opening order holds its margin and its maker fee. A reduce-only order holds nothing.
  def reserve(order: PropOrder): BigDecimal =
    if order.reduceOnly then zero
    else multiply(multiply(order.quantity, order.price), add(initial, Trading.baseMakerFee))

  def free(account: PropAccount, price: BigDecimal): BigDecimal =
    val left = subtract(equity(account, price), used(account, price))
    account.resting.foldLeft(left)((sum, order) => subtract(sum, reserve(order)))

  // The price where equity meets maintenance on the position's value. A long whose cash covers
  // its whole entry has none.
  def liquidationPrice(
      account: PropAccount,
      coin: String,
      maintenance: BigDecimal
  ): Option[BigDecimal] =
    account.positions.get(coin).flatMap { position =>
      val perUnit = divide(account.cash, size(position))
      if position.quantity > 0 then
        Some(divide(subtract(position.entry, perUnit), subtract(one, maintenance))).filter(_ > 0)
      else Some(divide(add(position.entry, perUnit), add(one, maintenance)))
    }

  def liquidated(
      account: PropAccount,
      coin: String,
      mark: BigDecimal,
      maintenance: BigDecimal
  ): Boolean =
    account.positions.get(coin).exists { position =>
      equity(account, mark) <= multiply(multiply(maintenance, size(position)), mark)
    }

  // Positive when the account pays: a long at a positive rate or a short at a negative one.
  def payment(quantity: BigDecimal, oracle: BigDecimal, rate: BigDecimal): BigDecimal =
    multiply(multiply(quantity, oracle), rate)
