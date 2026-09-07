package desk

final case class Style(colour: Boolean):
  private def code(n: String): String = if colour then Screen.esc + "[" + n + "m" else ""
  val bid: String = code("32")
  val ask: String = code("31")
  def wrap(style: String, text: String): String =
    if style.isEmpty || text.trim.isEmpty then text else style + text + Screen.reset

object Style:
  val colour: Style = Style(true)
  val mono: Style = Style(false)
