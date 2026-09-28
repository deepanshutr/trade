package desk

class KeysSpec extends munit.FunSuite:
  test("removed details feedback and scroll keys emit no command") {
    assertEquals(feed(sequence("hyn[]"))._2, Vector.empty)
  }

  private def feed(
      bytes: Seq[Int],
      prompt: Boolean = false,
      initial: Vector[Int] = Vector.empty
  ): (Vector[Int], Vector[Command]) =
    bytes.foldLeft((initial, Vector.empty[Command])) { case ((pending, out), byte) =>
      val (next, command) = Keys.step(pending, byte, prompt)
      (next, out ++ command.toVector)
    }
  private def sequence(text: String): Vector[Int] = text.toVector.map(_.toInt)
  private val start = Vector(27) ++ sequence("[200~")
  private val end = Vector(27) ++ sequence("[201~")

  test(
    "escape intermediates consume every final byte including order digits and delayed prefixes"
  ) {
    for intermediate <- 32 to 47 do
      for last <- 48 to 126 do
        for prompt <- Vector(false, true) do
          for delayed <- Vector(false, true) do
            val initial = if delayed then Keys.expire(Vector(27), prompt)._1 else Vector.empty
            val prefix = if delayed then Vector.empty else Vector(27)
            val bytes = prefix ++ Vector(intermediate, last)
            assertEquals(feed(bytes, prompt, initial), (Vector.empty, Vector.empty))
            assertEquals(
              feed(bytes :+ 49, prompt, initial)._2,
              Keys.step(Vector.empty, 49, prompt)._2.toVector
            )
  }

  test(
    "multiple escape intermediates retain bounded pending state through waits and control with c"
  ) {
    for prompt <- Vector(false, true) do
      var pending = Vector(27)
      for byte <- Vector.fill(4096)(35) ++ Vector(32, 40, 47) do
        val (next, command) = Keys.step(pending, byte, prompt)
        assert(command.isEmpty)
        assert(next.nonEmpty && next.size <= 1)
        assertEquals(Keys.expire(next, prompt), (next, None))
        assertEquals(Keys.step(next, 3, prompt), (Vector.empty, Some(Command.Quit)))
        pending = next
      assertEquals(Keys.step(pending, 48, prompt), (Vector.empty, None))
  }

  test("legacy mouse and highlight reports consume numeric fields at every read split") {
    for (last, count) <- Vector(77 -> 3, 116 -> 2, 84 -> 6) do
      for prefix <- Vector(Vector(27, 91), Vector(155)) do
        for prompt <- Vector(false, true) do
          val bytes = prefix ++ Vector(last) ++ (49 until 49 + count)
          assertEquals(feed(bytes, prompt), (Vector.empty, Vector.empty))
          assertEquals(
            feed(bytes :+ 49, prompt)._2,
            Keys.step(Vector.empty, 49, prompt)._2.toVector
          )
          for cut <- prefix.size until bytes.size do
            val partial = feed(bytes.take(cut), prompt)
            assert(partial._1.nonEmpty)
            assertEquals(partial._2, Vector.empty)
            assertEquals(Keys.expire(partial._1, prompt), (partial._1, None))
            assertEquals(Keys.step(partial._1, 3, prompt), (Vector.empty, Some(Command.Quit)))
            assertEquals(feed(bytes.drop(cut), prompt, partial._1), (Vector.empty, Vector.empty))
          val delayed = Keys.expire(Vector(27), prompt)._1
          assertEquals(
            feed(Vector(91, last) ++ (49 until 49 + count), prompt, delayed),
            (Vector.empty, Vector.empty)
          )
  }

  test("mouse reports consume UTF-8 fields before later numeric fields and ordinary keys") {
    for (last, count) <- Vector(77 -> 3, 116 -> 2, 84 -> 6) do
      for encodedAt <- 0 until count do
        for encoded <- Vector(Vector(194, 160), Vector(223, 191)) do
          for prefix <- Vector(Vector(27, 91), Vector(155)) do
            for prompt <- Vector(false, true) do
              val fields = (0 until count).toVector.flatMap { index =>
                if index == encodedAt then encoded else Vector(49 + index)
              }
              val bytes = prefix ++ Vector(last) ++ fields
              assertEquals(feed(bytes, prompt), (Vector.empty, Vector.empty))
              assertEquals(
                feed(bytes :+ 49, prompt)._2,
                Keys.step(Vector.empty, 49, prompt)._2.toVector
              )
              for cut <- prefix.size + 1 until bytes.size do
                val partial = feed(bytes.take(cut), prompt)
                assert(partial._1.nonEmpty && partial._1.size <= 3)
                assertEquals(partial._2, Vector.empty)
                assertEquals(Keys.expire(partial._1, prompt), (partial._1, None))
                assertEquals(Keys.step(partial._1, 3, prompt), (Vector.empty, Some(Command.Quit)))
                assertEquals(
                  feed(bytes.drop(cut), prompt, partial._1),
                  (Vector.empty, Vector.empty)
                )
              val delayed = Keys.expire(Vector(27), prompt)._1
              assertEquals(
                feed(Vector(91, last) ++ fields, prompt, delayed),
                (Vector.empty, Vector.empty)
              )
  }

  test("a raw high mouse field reprocesses a noncontinuation as the next field") {
    val bytes = Vector(27, 91, 77, 194, 49, 50)
    assertEquals(feed(bytes), (Vector.empty, Vector.empty))
    assertEquals(feed(bytes :+ 49)._2, Keys.step(Vector.empty, 49, false)._2.toVector)
  }

  test("parameterized mouse and highlight control sequences have no fixed field tail") {
    for text <- Vector("1M", "2t", "1;2T", "<0;1;2M") do
      for prefix <- Vector(Vector(27, 91), Vector(155)) do
        for prompt <- Vector(false, true) do
          val bytes = prefix ++ sequence(text)
          assertEquals(feed(bytes, prompt), (Vector.empty, Vector.empty))
          assertEquals(
            feed(bytes :+ 49, prompt)._2,
            Keys.step(Vector.empty, 49, prompt)._2.toVector
          )
  }

  test("the eight digits have separate hard and best actions") {
    val actions = feed(49 to 56)._2
    val expected = Vector(
      OrderIntent(Side.Buy, OrderKind.Hard),
      OrderIntent(Side.Buy, OrderKind.Hard, big = true),
      OrderIntent(Side.Sell, OrderKind.Hard),
      OrderIntent(Side.Sell, OrderKind.Hard, big = true),
      OrderIntent(Side.Buy, OrderKind.Best),
      OrderIntent(Side.Buy, OrderKind.Best, big = true),
      OrderIntent(Side.Sell, OrderKind.Best),
      OrderIntent(Side.Sell, OrderKind.Best, big = true)
    ).map(Command.PlaceOrder(_))
    assertEquals(actions, expected)
    assertEquals(actions.distinct.size, 8)
  }

  test("target selectors and ordinary controls remain separate") {
    assertEquals(
      feed(sequence("90cmq"))._2,
      Vector(
        Command.SelectTarget(Side.Buy),
        Command.SelectTarget(Side.Sell),
        Command.ToggleCandles,
        Command.StartMarketPrompt,
        Command.Quit
      )
    )
    assertEquals(
      feed(Vector(13, 10, 3))._2,
      Vector(Command.Confirm, Command.Confirm, Command.Quit)
    )
    assertEquals(feed(sequence("xi"))._2, Vector.empty)
  }

  test("both arrow forms zoom in upward and out downward") {
    for lead <- Vector(91, 79) do
      assertEquals(feed(Vector(27, lead, 65))._2, Vector(Command.ZoomIn))
      assertEquals(feed(Vector(27, lead, 66))._2, Vector(Command.ZoomOut))
      assertEquals(feed(Vector(27, lead, 67))._2, Vector(Command.ScrubForward))
      assertEquals(feed(Vector(27, lead, 68))._2, Vector(Command.ScrubBack))
  }

  test("function keys consume their digits until the complete final byte") {
    for text <- Vector("[15~", "[24~", "[1;5A", "OP", "O1;2P") do
      assertEquals(feed(Vector(27) ++ sequence(text))._2, Vector.empty)
    val partial = feed(Vector(27) ++ sequence("[15"))
    assertEquals(partial._2, Vector.empty)
    assertEquals(Keys.expire(partial._1, false), (partial._1, None))
    assertEquals(Keys.step(partial._1, 126, false), (Vector.empty, None))
  }

  test("an overlong sequence has bounded state and remains discarded through its final byte") {
    var pending = Vector.empty[Int]
    val bytes = Vector(27, 91) ++ Vector.fill(1000)(49) ++ Vector(126)
    bytes.foreach { byte =>
      val (next, command) = Keys.step(pending, byte, false)
      assert(command.isEmpty)
      assert(next.size <= 32)
      pending = next
    }
    assertEquals(pending, Vector.empty)
    assert(Keys.step(pending, 49, false)._2.exists(_.isInstanceOf[Command.PlaceOrder]))
  }

  test("bracketed paste cannot place orders or select a target") {
    val pasted = start ++ sequence("1234567890cmq") ++ Vector(13, 27, 91, 65) ++ end
    assertEquals(feed(pasted), (Vector.empty, Vector.empty))
    assertEquals(
      feed(pasted :+ 56)._2,
      Vector(Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Best, big = true)))
    )
    assertEquals(feed(pasted, prompt = true), (Vector.empty, Vector.empty))
  }

  test("a partial or false paste end cannot release pasted digits") {
    val bytes = start ++ sequence("12") ++ Vector(27) ++ sequence("[20")
    val partial = feed(bytes)
    assertEquals(partial._2, Vector.empty)
    assertEquals(Keys.expire(partial._1, false), (partial._1, None))
    val continued = (sequence("0~345") ++ end).foldLeft(partial) { case ((pending, out), byte) =>
      val (next, command) = Keys.step(pending, byte, false)
      (next, out ++ command.toVector)
    }
    assertEquals(continued, (Vector.empty, Vector.empty))
  }

  test("a lone escape cancels only after expiry and never becomes an order") {
    val pending = feed(Vector(27))
    assertEquals(pending, (Vector(27), Vector.empty))
    val cancelled = Keys.expire(pending._1, false)
    assertEquals(cancelled._2, Some(Command.Cancel))
    assertEquals(Keys.expire(pending._1, true)._2, Some(Command.PromptCancel))
    assertEquals(
      Keys.step(cancelled._1, 49, false)._2,
      Some(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
    )
    assertEquals(feed(Vector(27, 49))._2, Vector.empty)
    assertEquals(feed(Vector(27, 113))._2, Vector(Command.Quit))
  }

  test("a paste prefix delayed beyond lone escape expiry still cannot release order digits") {
    val expired = Keys.expire(Vector(27), false)
    val bytes = start.tail ++ sequence("1234567890") ++ end
    val result = bytes.foldLeft((expired._1, Vector.empty[Command])) {
      case ((pending, out), byte) =>
        val (next, command) = Keys.step(pending, byte, false)
        (next, out ++ command.toVector)
    }
    assertEquals(result, (Vector.empty, Vector.empty))
  }

  test("prompt text is typed and arrows are consumed without placing orders") {
    assertEquals(
      feed(sequence("12390c"), prompt = true)._2,
      sequence("12390c").map(n => Command.PromptChar(n.toChar))
    )
    assertEquals(feed(Vector(27, 91, 65, 27, 79, 66), prompt = true)._2, Vector.empty)
    assertEquals(
      feed(Vector(13, 10, 127, 8, 3), prompt = true)._2,
      Vector(
        Command.PromptSubmit,
        Command.PromptSubmit,
        Command.PromptBackspace,
        Command.PromptBackspace,
        Command.Quit
      )
    )
  }

  test("each terminal string discards numeric payload through its proper terminator") {
    val payload = sequence("11;1234567890cmq") ++ Vector(13, 10, 27, 91, 65)
    for opener <- Vector(80, 88, 93, 94, 95) do
      val bytes = Vector(27, opener) ++ payload ++ Vector(27, 92)
      assertEquals(feed(bytes), (Vector.empty, Vector.empty))
      assertEquals(feed(bytes, prompt = true), (Vector.empty, Vector.empty))
      assertEquals(
        feed(bytes :+ 49)._2,
        Vector(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
      )
    assertEquals(feed(Vector(27, 93) ++ payload :+ 7), (Vector.empty, Vector.empty))
  }

  test("a bell cannot end the four non-operating strings") {
    for opener <- Vector(80, 88, 94, 95) do
      val bytes = Vector(27, opener, 7) ++ sequence("1234567890")
      val partial = feed(bytes)
      assert(partial._1.nonEmpty)
      assertEquals(partial._2, Vector.empty)
      assertEquals(Keys.expire(partial._1, false), (partial._1, None))
      assertEquals(feed(bytes ++ Vector(27, 92)), (Vector.empty, Vector.empty))
  }

  test("eight-bit string controls use the same bounded discard and terminator") {
    for opener <- Vector(144, 152, 157, 158, 159) do
      assertEquals(
        feed(Vector(opener) ++ sequence("1234567890") :+ 156),
        (Vector.empty, Vector.empty)
      )
    assertEquals(feed(Vector(157) ++ sequence("1234567890") :+ 7), (Vector.empty, Vector.empty))
  }

  test("long strings retain at most the family and a possible terminating escape") {
    for opener <- Vector(80, 88, 93, 94, 95) do
      var pending = feed(Vector(27, opener))._1
      for byte <- Vector.fill(4096)(49) ++ Vector(27, 27, 49, 27) do
        val (next, command) = Keys.step(pending, byte, false)
        assert(command.isEmpty)
        assert(next.size <= 2)
        pending = next
      assertEquals(Keys.expire(pending, false), (pending, None))
      assertEquals(Keys.step(pending, 92, false), (Vector.empty, None))
  }

  test("a delayed string opener after escape expiry still discards every payload digit") {
    for opener <- Vector(80, 88, 93, 94, 95) do
      val expired = Keys.expire(Vector(27), false)
      val bytes = Vector(opener) ++ sequence("11;1234567890") ++ Vector(27, 92)
      val result = bytes.foldLeft((expired._1, Vector.empty[Command])) {
        case ((pending, out), byte) =>
          val (next, command) = Keys.step(pending, byte, false)
          (next, out ++ command.toVector)
      }
      assertEquals(result, (Vector.empty, Vector.empty))
  }

  test("control with c quits from every buffered and discard state without an order") {
    val states = Vector(
      Vector.empty[Int],
      Vector(27),
      Vector(27, 91),
      Vector(27, 79),
      feed(Vector(27, 91) ++ Vector.fill(40)(49))._1,
      feed(start ++ sequence("123"))._1,
      Keys.expire(Vector(27), false)._1
    ) ++
      Vector(80, 88, 93, 94, 95).flatMap(opener =>
        Vector(feed(Vector(27, opener, 49))._1, feed(Vector(27, opener, 27))._1)
      )
    for pending <- states do
      for prompt <- Vector(false, true) do
        assertEquals(Keys.step(pending, 3, prompt), (Vector.empty, Some(Command.Quit)))
  }
