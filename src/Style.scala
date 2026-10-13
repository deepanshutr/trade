package desk

final case class Style(colour: Boolean):
  private def code(n: String): String = if colour then Screen.esc + "[" + n + "m" else ""
  val bid: String = code("32")
  val ask: String = code("31")
  val book: String = code("38;2;0;145;97")
  val tape: String = code("38;2;17;135;184")
  val bitcoin: String = code("38;2;217;102;31")
  val gold: String = code("38;2;166;124;0")
  // The Polymarket windows: 5m, 15m, 1h and 4h in their colours, the daily in faint text, and
  // two lines on one cell in bold text colour.
  val w5: String = code("38;2;73;119;241")
  val w15: String = code("38;2;166;95;213")
  val w1h: String = code("38;2;212;68;169")
  val w4h: String = code("38;2;231;55;119")
  val w1d: String = Screen.dim
  val overlap: String = Screen.bold
  def wrap(style: String, text: String): String =
    if style.isEmpty || text.trim.isEmpty then text else style + text + Screen.reset

object Style:
  val colour: Style = Style(true)
  val mono: Style = Style(false)
