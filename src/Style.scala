package desk

final case class Style(colour: Boolean):
  private def code(n: String): String = if colour then Screen.esc + "[" + n + "m" else ""
  val bid: String = code("32")
  val ask: String = code("31")
  val book: String = code("38;2;0;145;97")
  val tape: String = code("38;2;17;135;184")
  val bitcoin: String = code("38;2;217;102;31")
  val gold: String = code("38;2;166;124;0")
  def wrap(style: String, text: String): String =
    if style.isEmpty || text.trim.isEmpty then text else style + text + Screen.reset

object Style:
  val colour: Style = Style(true)
  val mono: Style = Style(false)
