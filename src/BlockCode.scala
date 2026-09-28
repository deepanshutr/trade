package desk

/** Stable display and diagnostic categories. These never decide order readiness. */
enum BlockCode:
  case Blocked, Market, Account, Leverage, Storage, Prices, Connection

private[desk] final case class ExecutionBlock(code: BlockCode, text: String)
