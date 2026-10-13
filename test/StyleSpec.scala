package desk

class StyleSpec extends munit.FunSuite:
  private def sgr(rgb: String): String = Screen.esc + "[" + rgb + "m"

  private def channel(v: Int): Double =
    val c = v / 255.0
    if c <= 0.03928 then c / 12.92 else math.pow((c + 0.055) / 1.055, 2.4)

  private def luminance(rgb: (Int, Int, Int)): Double =
    0.2126 * channel(rgb._1) + 0.7152 * channel(rgb._2) + 0.0722 * channel(rgb._3)

  private def contrast(a: (Int, Int, Int), b: (Int, Int, Int)): Double =
    val (hi, lo) = (math.max(luminance(a), luminance(b)), math.min(luminance(a), luminance(b)))
    (hi + 0.05) / (lo + 0.05)

  private def parse(code: String): (Int, Int, Int) =
    val Array(r, g, b) =
      code.stripPrefix(Screen.esc + "[38;2;").stripSuffix("m").split(";").map(_.toInt)
    (r, g, b)

  test("the four fixed colours have the palette values") {
    assertEquals(Style.colour.book, sgr("38;2;0;145;97"))
    assertEquals(Style.colour.tape, sgr("38;2;17;135;184"))
    assertEquals(Style.colour.bitcoin, sgr("38;2;217;102;31"))
    assertEquals(Style.colour.gold, sgr("38;2;166;124;0"))
  }

  test("the four Polymarket colours have the palette values") {
    assertEquals(Style.colour.w5, sgr("38;2;73;119;241"))
    assertEquals(Style.colour.w15, sgr("38;2;166;95;213"))
    assertEquals(Style.colour.w1h, sgr("38;2;212;68;169"))
    assertEquals(Style.colour.w4h, sgr("38;2;231;55;119"))
    val m = Style.mono
    assertEquals(Vector(m.w5, m.w15, m.w1h, m.w4h), Vector.fill(4)(""))
    for style <- Vector(Style.colour, Style.mono) do
      assertEquals(style.w1d, Screen.dim)
      assertEquals(style.overlap, Screen.bold)
  }

  test("bid and ask keep the basic colour codes") {
    assertEquals(Style.colour.bid, sgr("32"))
    assertEquals(Style.colour.ask, sgr("31"))
  }

  test("mono has empty strings for all six") {
    val m = Style.mono
    assertEquals(Vector(m.bid, m.ask, m.book, m.tape, m.bitcoin, m.gold), Vector.fill(6)(""))
  }

  test("the contrast helper matches known values") {
    assertEqualsDouble(contrast((255, 255, 255), (0, 0, 0)), 21.0, 0.01)
    assertEqualsDouble(contrast((217, 102, 31), (0xfd, 0xf6, 0xe3)), 3.32, 0.01)
  }

  test("each fixed colour reads at 3 to 1 on all four backgrounds") {
    val backgrounds = Vector((0xfd, 0xf6, 0xe3), (0x00, 0x2b, 0x36), (0xff, 0xff, 0xff), (0, 0, 0))
    val colours = Vector(
      "book" -> Style.colour.book,
      "tape" -> Style.colour.tape,
      "bitcoin" -> Style.colour.bitcoin,
      "gold" -> Style.colour.gold,
      "w5" -> Style.colour.w5,
      "w15" -> Style.colour.w15,
      "w1h" -> Style.colour.w1h,
      "w4h" -> Style.colour.w4h
    )
    for (name, code) <- colours; bg <- backgrounds do
      val ratio = contrast(parse(code), bg)
      assert(ratio >= 3.0, s"$name on $bg is $ratio")
  }
