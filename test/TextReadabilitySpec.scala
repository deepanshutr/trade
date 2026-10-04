package desk

class TextReadabilitySpec extends munit.FunSuite:
  test("order negative results and comparison signs retain their meaning") {
    assertEquals(
      Workspace.safeText("Mean net return was −0.3242%, range ±0.1%, price ≤500 or ≥600."),
      "Mean net return was -0.3242%, range +/-0.1%, price <=500 or >=600."
    )
    val controller = new DeskController(
      PanelView(),
      () => 1L,
      (_, _) => Left("unused"),
      _ => false,
      _ => Left("unused")
    )
    controller.addMessage("order", "Across nine samples, mean net return was −0.3242%.")
    assert(controller.panel.actionNotice.get.contains("-0.3242%"))
  }

  test("readable punctuation never reintroduces terminal control characters") {
    val plain = Workspace.safeText("“−0.3%” — ‘down’… " + 27.toChar + "[2J" + 7.toChar)
    assertEquals(plain.trim, "\"-0.3%\" - 'down'...")
    assert(plain.forall(c => c == '\n' || (c >= ' ' && c <= '~')))
    assertEquals(Workspace.safeText("…" * 30000).length, 30000)
  }
