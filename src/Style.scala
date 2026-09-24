package desk

final case class Style(colour: Boolean):
  private def code(n: String): String = if colour then Screen.esc + "[" + n + "m" else ""
  val bid: String = code("32")
  val ask: String = code("31")
  val book: String = code("38;2;186;161;247")
  val tape: String = code("38;2;94;214;208")
  val bitcoin: String = code("38;2;94;170;255")
  val gold: String = code("38;2;242;206;104")
  def wrap(style: String, text: String): String =
    if style.isEmpty || text.trim.isEmpty then text else style + text + Screen.reset

object Style:
  val colour: Style = Style(true)
  val mono: Style = Style(false)
