package desk

final case class Style(colour: Boolean):
  private def code(n: String): String = if colour then Screen.esc + "[" + n + "m" else ""
  val bid: String = code("32")
  val ask: String = code("31")
  val btc: String = if colour then code("38;5;208") else Screen.dim
  val gold: String = if colour then code("38;5;178") else Screen.dim
  def wrap(style: String, text: String): String =
    if style.isEmpty || text.trim.isEmpty then text else style + text + Screen.reset

object Style:
  val colour: Style = Style(true)
  val mono: Style = Style(false)
