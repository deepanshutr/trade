package desk

class KeysSpec extends munit.FunSuite:
  test("removed details feedback and scroll keys emit no command") {
    assertEquals(feed(sequence("hyn[]"))._2, Vector.empty)
  }

  test("a places, t picks the order type and o opens the orders") {
    assertEquals(feed(Vector(97))._2, Vector(Command.Place))
    assertEquals(feed(Vector(116))._2, Vector(Command.OrderType))
    assertEquals(feed(Vector(111))._2, Vector(Command.Orders))
    assertEquals(feed(Vector(98))._2, Vector.empty)
    assertEquals(feed(Vector(97), prompt = true)._2, Vector(Command.PromptChar('a')))
  }

  test("an unbound order type leaves t alone and the owner's t binding wins") {
    val owner = Bindings
      .load(
        {
          val dir = java.nio.file.Files.createTempDirectory("desk-keys")
          val file = dir.resolve("b.json")
          java.nio.file.Files.writeString(
            file,
            """{"version":1,"bindings":{"quit":["t"]}}"""
          )
          java.nio.file.Files.setPosixFilePermissions(
            file,
            java.nio.file.attribute.PosixFilePermissions.fromString("rw-------")
          )
          file
        }
      )
      .toOption
      .get
    assertEquals(feed(Vector(116), bindings = owner)._2, Vector(Command.Quit))
  }

  test("the three new commands leave a snapshot unchanged") {
    val now = 1_700_000_000_000L
    val snapshot = Snapshot.initial("XMR", now)
    for command <- Vector(Command.OrderType, Command.Orders, Command.Place) do
      assertEquals(State.reduce(snapshot, Event.Key(command), now), snapshot, command)
  }

  test("a panel starts in market mode with no ticket and no focus") {
    val panel = PanelView()
    assertEquals(panel.orderMode, OrderMode.Market)
    assertEquals(panel.ticket, None)
    assertEquals(panel.focus, None)
  }

  private def feed(
      bytes: Seq[Int],
      prompt: Boolean = false,
      initial: Vector[Int] = Vector.empty,
      bindings: Bindings = Bindings.defaults,
      mouse: Boolean = false
  ): (Vector[Int], Vector[Command]) =
    bytes.foldLeft((initial, Vector.empty[Command])) { case ((pending, out), byte) =>
      val (next, command) = Keys.step(pending, byte, prompt, mouse, bindings)
      (next, out ++ command.toVector)
    }
  // Every state other than a lone escape or a held first key keeps its bytes however long it waits.
  private val forever = Long.MaxValue
  private val escapeWait = Main.escapeWaitNanos
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
            val initial =
              if delayed then Keys.expire(Vector(27), prompt, escapeWait)._1 else Vector.empty
            val prefix = if delayed then Vector.empty else Vector(27)
            val bytes = prefix ++ Vector(intermediate, last)
            assertEquals(feed(bytes, prompt, initial), (Vector.empty, Vector.empty))
            assertEquals(
              feed(bytes :+ 49, prompt, initial)._2,
              Keys.step(Vector.empty, 49, prompt, bindings = Bindings.defaults)._2.toVector
            )
  }

  test(
    "multiple escape intermediates retain bounded pending state through waits and control with c"
  ) {
    for prompt <- Vector(false, true) do
      var pending = Vector(27)
      for byte <- Vector.fill(4096)(35) ++ Vector(32, 40, 47) do
        val (next, command) = Keys.step(pending, byte, prompt, bindings = Bindings.defaults)
        assert(command.isEmpty)
        assert(next.nonEmpty && next.size <= 1)
        assertEquals(Keys.expire(next, prompt, forever), (next, None))
        assertEquals(
          Keys.step(next, 3, prompt, bindings = Bindings.defaults),
          (Vector.empty, Some(Command.Quit))
        )
        pending = next
      assertEquals(
        Keys.step(pending, 48, prompt, bindings = Bindings.defaults),
        (Vector.empty, None)
      )
  }

  test("legacy mouse and highlight reports consume numeric fields at every read split") {
    for (last, count) <- Vector(77 -> 3, 116 -> 2, 84 -> 6) do
      for prefix <- Vector(Vector(27, 91), Vector(155)) do
        for prompt <- Vector(false, true) do
          val bytes = prefix ++ Vector(last) ++ (49 until 49 + count)
          assertEquals(feed(bytes, prompt), (Vector.empty, Vector.empty))
          assertEquals(
            feed(bytes :+ 49, prompt)._2,
            Keys.step(Vector.empty, 49, prompt, bindings = Bindings.defaults)._2.toVector
          )
          for cut <- prefix.size until bytes.size do
            val partial = feed(bytes.take(cut), prompt)
            assert(partial._1.nonEmpty)
            assertEquals(partial._2, Vector.empty)
            assertEquals(Keys.expire(partial._1, prompt, forever), (partial._1, None))
            assertEquals(
              Keys.step(partial._1, 3, prompt, bindings = Bindings.defaults),
              (Vector.empty, Some(Command.Quit))
            )
            assertEquals(feed(bytes.drop(cut), prompt, partial._1), (Vector.empty, Vector.empty))
          val delayed = Keys.expire(Vector(27), prompt, escapeWait)._1
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
                Keys.step(Vector.empty, 49, prompt, bindings = Bindings.defaults)._2.toVector
              )
              for cut <- prefix.size + 1 until bytes.size do
                val partial = feed(bytes.take(cut), prompt)
                assert(partial._1.nonEmpty && partial._1.size <= 3)
                assertEquals(partial._2, Vector.empty)
                assertEquals(Keys.expire(partial._1, prompt, forever), (partial._1, None))
                assertEquals(
                  Keys.step(partial._1, 3, prompt, bindings = Bindings.defaults),
                  (Vector.empty, Some(Command.Quit))
                )
                assertEquals(
                  feed(bytes.drop(cut), prompt, partial._1),
                  (Vector.empty, Vector.empty)
                )
              val delayed = Keys.expire(Vector(27), prompt, escapeWait)._1
              assertEquals(
                feed(Vector(91, last) ++ fields, prompt, delayed),
                (Vector.empty, Vector.empty)
              )
  }

  test("a raw high mouse field reprocesses a noncontinuation as the next field") {
    val bytes = Vector(27, 91, 77, 194, 49, 50)
    assertEquals(feed(bytes), (Vector.empty, Vector.empty))
    assertEquals(
      feed(bytes :+ 49)._2,
      Keys.step(Vector.empty, 49, false, bindings = Bindings.defaults)._2.toVector
    )
  }

  test("parameterized mouse and highlight control sequences have no fixed field tail") {
    for text <- Vector("1M", "2t", "1;2T", "<0;1;2M") do
      for prefix <- Vector(Vector(27, 91), Vector(155)) do
        for prompt <- Vector(false, true) do
          val bytes = prefix ++ sequence(text)
          assertEquals(feed(bytes, prompt), (Vector.empty, Vector.empty))
          assertEquals(
            feed(bytes :+ 49, prompt)._2,
            Keys.step(Vector.empty, 49, prompt, bindings = Bindings.defaults)._2.toVector
          )
  }

  test("the four order digits are the four hard actions and 5 to 0 do nothing") {
    val expected = Vector(
      OrderIntent(Side.Buy, OrderKind.Hard),
      OrderIntent(Side.Buy, OrderKind.Hard, big = true),
      OrderIntent(Side.Sell, OrderKind.Hard),
      OrderIntent(Side.Sell, OrderKind.Hard, big = true)
    ).map(Command.PlaceOrder(_))
    assertEquals(feed(sequence("1234"))._2, expected)
    assertEquals(feed(sequence("567890")), (Vector.empty, Vector.empty))
  }

  test("ordinary controls remain separate") {
    assertEquals(
      feed(sequence("cmqvplxs"))._2,
      Vector(
        Command.ToggleCandles,
        Command.StartMarketPrompt,
        Command.Quit,
        Command.ToggleMute,
        Command.Mode("PROP"),
        Command.Mode("LIVE"),
        Command.Close,
        Command.Settings
      )
    )
    assertEquals(
      feed(Vector(13, 10, 3))._2,
      Vector(Command.Confirm, Command.Confirm, Command.Quit)
    )
    assertEquals(feed(sequence("i"))._2, Vector.empty)
  }

  test("s opens settings, is text in the market prompt and changes nothing in the fold") {
    assertEquals(feed(sequence("s"), prompt = true)._2, Vector(Command.PromptChar('s')))
    assertEquals(feed(Vector(27, 115))._2, Vector.empty)
    val snapshot = Snapshot.initial("XMR", 1L)
    assertEquals(State.reduce(snapshot, Event.Key(Command.Settings), 2L), snapshot)
  }

  test("x presses CLOSE but stays text in the market prompt, a paste or after escape") {
    assertEquals(feed(sequence("x"))._2, Vector(Command.Close))
    assertEquals(feed(sequence("x"), prompt = true)._2, Vector(Command.PromptChar('x')))
    assertEquals(feed(start ++ sequence("x") ++ end), (Vector.empty, Vector.empty))
    assertEquals(feed(Vector(27, 120))._2, Vector.empty)
    val snapshot = Snapshot.initial("XMR", 1L)
    assertEquals(State.reduce(snapshot, Event.Key(Command.Close), 2L), snapshot)
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
    assertEquals(Keys.expire(partial._1, false, forever), (partial._1, None))
    assertEquals(
      Keys.step(partial._1, 126, false, bindings = Bindings.defaults),
      (Vector.empty, None)
    )
  }

  test("an overlong sequence has bounded state and remains discarded through its final byte") {
    var pending = Vector.empty[Int]
    val bytes = Vector(27, 91) ++ Vector.fill(1000)(49) ++ Vector(126)
    bytes.foreach { byte =>
      val (next, command) = Keys.step(pending, byte, false, bindings = Bindings.defaults)
      assert(command.isEmpty)
      assert(next.size <= 32)
      pending = next
    }
    assertEquals(pending, Vector.empty)
    assert(
      Keys
        .step(pending, 49, false, bindings = Bindings.defaults)
        ._2
        .exists(_.isInstanceOf[Command.PlaceOrder])
    )
  }

  test("bracketed paste cannot place orders or select a target") {
    val pasted = start ++ sequence("1234567890cmq") ++ Vector(13, 27, 91, 65) ++ end
    assertEquals(feed(pasted), (Vector.empty, Vector.empty))
    assertEquals(
      feed(pasted :+ 52)._2,
      Vector(Command.PlaceOrder(OrderIntent(Side.Sell, OrderKind.Hard, big = true)))
    )
    assertEquals(feed(pasted, prompt = true), (Vector.empty, Vector.empty))
  }

  test("a partial or false paste end cannot release pasted digits") {
    val bytes = start ++ sequence("12") ++ Vector(27) ++ sequence("[20")
    val partial = feed(bytes)
    assertEquals(partial._2, Vector.empty)
    assertEquals(Keys.expire(partial._1, false, forever), (partial._1, None))
    val continued = (sequence("0~345") ++ end).foldLeft(partial) { case ((pending, out), byte) =>
      val (next, command) = Keys.step(pending, byte, false, bindings = Bindings.defaults)
      (next, out ++ command.toVector)
    }
    assertEquals(continued, (Vector.empty, Vector.empty))
  }

  test("a lone escape cancels only after expiry and never becomes an order") {
    val pending = feed(Vector(27))
    assertEquals(pending, (Vector(27), Vector.empty))
    assertEquals(Keys.expire(pending._1, false, escapeWait - 1), (Vector(27), None))
    val cancelled = Keys.expire(pending._1, false, escapeWait)
    assertEquals(cancelled._2, Some(Command.Cancel))
    assertEquals(Keys.expire(pending._1, true, escapeWait)._2, Some(Command.PromptCancel))
    assertEquals(
      Keys.step(cancelled._1, 49, false, bindings = Bindings.defaults)._2,
      Some(Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard)))
    )
    assertEquals(feed(Vector(27, 49))._2, Vector.empty)
    assertEquals(feed(Vector(27, 113))._2, Vector(Command.Quit))
  }

  test("the after-escape state resets after a second escape wait") {
    val afterEscape = Keys.expire(Vector(27), false, escapeWait)._1
    for prompt <- Vector(false, true) do
      assertEquals(Keys.expire(afterEscape, prompt, 2 * escapeWait - 1), (afterEscape, None))
      assertEquals(Keys.expire(afterEscape, prompt, 2 * escapeWait), (Vector.empty, None))
    val reset = Keys.expire(afterEscape, false, 2 * escapeWait)._1
    assertEquals(feed(sequence("Px"), initial = reset), (Vector.empty, Vector(Command.Close)))
    assertEquals(feed(sequence("Px"), initial = afterEscape)._2, Vector.empty)
    val bang = bound(Bind.Close -> Vector("!"))
    assertEquals(
      feed(sequence("!1"), initial = reset, bindings = bang)._2,
      Vector(Command.Close, longSmall)
    )
  }

  test("a paste prefix delayed beyond lone escape expiry still cannot release order digits") {
    val expired = Keys.expire(Vector(27), false, escapeWait)
    val bytes = start.tail ++ sequence("1234567890") ++ end
    val result = bytes.foldLeft((expired._1, Vector.empty[Command])) {
      case ((pending, out), byte) =>
        val (next, command) = Keys.step(pending, byte, false, bindings = Bindings.defaults)
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
      assertEquals(Keys.expire(partial._1, false, forever), (partial._1, None))
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
        val (next, command) = Keys.step(pending, byte, false, bindings = Bindings.defaults)
        assert(command.isEmpty)
        assert(next.size <= 2)
        pending = next
      assertEquals(Keys.expire(pending, false, forever), (pending, None))
      assertEquals(
        Keys.step(pending, 92, false, bindings = Bindings.defaults),
        (Vector.empty, None)
      )
  }

  test("a delayed string opener after escape expiry still discards every payload digit") {
    for opener <- Vector(80, 88, 93, 94, 95) do
      val expired = Keys.expire(Vector(27), false, escapeWait)
      val bytes = Vector(opener) ++ sequence("11;1234567890") ++ Vector(27, 92)
      val result = bytes.foldLeft((expired._1, Vector.empty[Command])) {
        case ((pending, out), byte) =>
          val (next, command) = Keys.step(pending, byte, false, bindings = Bindings.defaults)
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
      Keys.expire(Vector(27), false, escapeWait)._1
    ) ++
      Vector(80, 88, 93, 94, 95).flatMap(opener =>
        Vector(feed(Vector(27, opener, 49))._1, feed(Vector(27, opener, 27))._1)
      )
    for pending <- states do
      for prompt <- Vector(false, true) do
        assertEquals(
          Keys.step(pending, 3, prompt, bindings = Bindings.defaults),
          (Vector.empty, Some(Command.Quit))
        )
  }

  // Bound keys: exact byte groups, UTF-8 characters and two-key sequences.

  private def text(value: String): Vector[Int] =
    value.getBytes(java.nio.charset.StandardCharsets.UTF_8).toVector.map(_ & 0xff)
  private def group(value: String): KeyGroup = KeyGroup(text(value))
  private def bound(changes: (Bind, Vector[String])*): Bindings =
    changes
      .foldLeft[Either[String, Bindings]](Right(Bindings.defaults)) { case (all, (action, keys)) =>
        all.flatMap(_.updated(action, Binding(keys.map(group))))
      }
      .fold(reason => fail(reason), identity)
  private val longSmall = Command.PlaceOrder(OrderIntent(Side.Buy, OrderKind.Hard))
  private val sequenceWait = Main.sequenceWaitNanos

  test("a UTF-8 key gives its command on its last byte and never a partial") {
    for character <- Vector("ç", "∫", "𝄞") do
      val bindings = bound(Bind.Close -> Vector(character))
      val bytes = text(character)
      for cut <- 1 until bytes.size do
        val partial = feed(bytes.take(cut), bindings = bindings)
        assert(partial._1.nonEmpty, character)
        assertEquals(partial._2, Vector.empty)
        assertEquals(Keys.expire(partial._1, false, forever), (partial._1, None))
        assertEquals(
          Keys.step(partial._1, 3, false, bindings = Bindings.defaults),
          (Vector.empty, Some(Command.Quit))
        )
      assertEquals(feed(bytes, bindings = bindings), (Vector.empty, Vector(Command.Close)))
      assertEquals(
        feed(bytes ++ bytes :+ 49, bindings = bindings)._2,
        Vector(Command.Close, Command.Close, longSmall)
      )
      assertEquals(feed(bytes), (Vector.empty, Vector.empty))
      assertEquals(feed(bytes, prompt = true, bindings = bindings), (Vector.empty, Vector.empty))
  }

  test("a torn UTF-8 character is dropped and the next byte reads on its own") {
    val bindings = bound(Bind.Close -> Vector("∫"))
    for cut <- 1 to 2 do
      val torn = text("∫").take(cut)
      assertEquals(feed(torn :+ 49, bindings = bindings), (Vector.empty, Vector(longSmall)))
      assertEquals(
        feed(torn ++ text("∫"), bindings = bindings),
        (Vector.empty, Vector(Command.Close))
      )
      val escaped = feed(torn :+ 27, bindings = bindings)
      assertEquals(escaped, (Vector(27), Vector.empty))
      assertEquals(Keys.expire(escaped._1, false, escapeWait)._2, Some(Command.Cancel))
      assertEquals(feed(torn :+ 49, prompt = true)._2, Vector(Command.PromptChar('1')))
    assertEquals(feed(Vector(226, 136, 171, 171) :+ 49)._2, Vector(longSmall))
    for lead <- Vector(192, 193, 245, 255, 128, 191) do
      assertEquals(feed(Vector(lead, 49)), (Vector.empty, Vector(longSmall)))
  }

  test("a torn character drops a held first key") {
    val bindings = bound(Bind.Market -> Vector("g", "m"))
    assertEquals(feed(Vector(103, 226, 109), bindings = bindings), (Vector.empty, Vector.empty))
  }

  test("a continuation byte that looks like an 8-bit opener stays inside its character") {
    // U+021B is C8 9B. Byte 155 alone opens a control sequence.
    assertEquals(feed(text("\u021b") :+ 49), (Vector.empty, Vector(longSmall)))
    assertEquals(feed(text("\u03cf") :+ 49), (Vector.empty, Vector(longSmall)))
  }

  test("a two-key sequence gives its command and a wrong second key drops both") {
    val bindings = bound(Bind.Market -> Vector("g", "m"))
    val held = feed(text("g"), bindings = bindings)
    assert(held._1.nonEmpty)
    assertEquals(held._2, Vector.empty)
    assertEquals(
      feed(text("gm"), bindings = bindings),
      (Vector.empty, Vector(Command.StartMarketPrompt))
    )
    assertEquals(feed(text("m"), bindings = bindings), (Vector.empty, Vector.empty))
    for wrong <- Vector("1", "q", "x", "g", "∫") do
      assertEquals(feed(text("g" + wrong), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(text("g1") :+ 49, bindings = bindings)._2, Vector(longSmall))
    assertEquals(feed(Vector(103, 13), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(text("g\u001b[A"), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(text("g\u001b1"), bindings = bindings), (Vector.empty, Vector.empty))
  }

  test("a sequence takes UTF-8 and escape groups in either place") {
    val bindings = bound(
      Bind.Market -> Vector("g", "∫"),
      Bind.Chart -> Vector("∫", "\u001b[15~"),
      Bind.Settings -> Vector("\u001bOP", "s")
    )
    val cases = Vector(
      "g∫" -> Command.StartMarketPrompt,
      "∫\u001b[15~" -> Command.ToggleCandles,
      "\u001bOPs" -> Command.Settings
    )
    for (keys, command) <- cases do
      val bytes = text(keys)
      for cut <- 1 until bytes.size do
        assertEquals(feed(bytes.take(cut), bindings = bindings)._2, Vector.empty, keys)
      assertEquals(feed(bytes, bindings = bindings), (Vector.empty, Vector(command)))
    assertEquals(feed(text("\u001bOP"), bindings = bindings)._2, Vector.empty)
  }

  test("two sequences may share a first key") {
    val bindings = bound(Bind.Market -> Vector("g", "h"), Bind.Chart -> Vector("g", "j"))
    assertEquals(
      feed(text("ghgj"), bindings = bindings)._2,
      Vector(Command.StartMarketPrompt, Command.ToggleCandles)
    )
  }

  test("Esc cancels a held first key and the next press reads on its own") {
    val bindings = bound(Bind.Market -> Vector("g", "m"))
    val escaped = feed(Vector(103, 27), bindings = bindings)
    assert(escaped._1.nonEmpty)
    assertEquals(Keys.expire(escaped._1, false, escapeWait - 1), (escaped._1, None))
    val cancelled = Keys.expire(escaped._1, false, escapeWait)
    assertEquals(cancelled._2, Some(Command.Cancel))
    assertEquals(
      Keys.step(cancelled._1, 49, false, false, bindings),
      (Vector.empty, Some(longSmall))
    )
    assertEquals(
      feed(Vector(103, 27, 113), bindings = bindings),
      (Vector.empty, Vector(Command.Quit))
    )
    assertEquals(feed(Vector(103, 3), bindings = bindings), (Vector.empty, Vector(Command.Quit)))
  }

  test("a held first key expires after one second with no command") {
    val bindings = bound(Bind.Market -> Vector("g", "m"))
    val held = feed(text("g"), bindings = bindings)._1
    assertEquals(sequenceWait, 1_000_000_000L)
    assertEquals(Keys.expire(held, false, sequenceWait - 1), (held, None))
    assertEquals(Keys.expire(held, false, escapeWait), (held, None))
    assertEquals(Keys.expire(held, false, sequenceWait), (Vector.empty, None))
    assertEquals(Keys.expire(held, true, forever), (Vector.empty, None))
    assertEquals(Keys.step(Vector.empty, 109, false, false, bindings), (Vector.empty, None))
    assertEquals(
      Keys.step(Vector.empty, 49, false, false, bindings),
      (Vector.empty, Some(longSmall))
    )
    val inside = feed(text("g\u001b[1"), bindings = bindings)._1
    assertEquals(Keys.expire(inside, false, forever), (inside, None))
  }

  test("a mouse report leaves a held first key held") {
    val bindings = bound(Bind.Market -> Vector("g", "h"))
    val report = text("\u001b[<0;3;4M")
    assertEquals(
      feed(text("g") ++ report ++ text("h"), bindings = bindings, mouse = true)._2,
      Vector(Command.PointerPress(3, 4, 0), Command.StartMarketPrompt)
    )
    val legacy = Vector(27, 91, 77, 32, 33, 34)
    assertEquals(
      feed(text("g") ++ legacy ++ text("h"), bindings = bindings)._2,
      Vector(Command.StartMarketPrompt)
    )
  }

  test("a rebound quit works and the old key is free") {
    val bindings = bound(Bind.Quit -> Vector("Q"))
    assertEquals(feed(text("Q"), bindings = bindings)._2, Vector(Command.Quit))
    assertEquals(feed(text("q"), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(Vector(27, 113), bindings = bindings)._2, Vector(Command.Quit))
    assertEquals(feed(Vector(27, 49), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(Vector(3), bindings = bindings)._2, Vector(Command.Quit))
    val control = bound(Bind.Settings -> Vector("\u000b"))
    assertEquals(feed(Vector(11), bindings = control)._2, Vector(Command.Settings))
    assertEquals(feed(Vector(11), prompt = true, bindings = control), (Vector.empty, Vector.empty))
  }

  test("a bound function key is matched exactly and its 8-bit form is not") {
    val bindings = bound(Bind.Settings -> Vector("\u001b[15~"))
    assertEquals(feed(text("\u001b[15~"), bindings = bindings)._2, Vector(Command.Settings))
    assertEquals(feed(Vector(155, 49, 53, 126), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(text("\u001b[15;2~"), bindings = bindings), (Vector.empty, Vector.empty))
    assertEquals(feed(Vector(155, 65), bindings = bindings)._2, Vector(Command.ZoomIn))
    assertEquals(feed(Vector(143, 66), bindings = bindings)._2, Vector(Command.ZoomOut))
  }

  test("the market prompt ignores bindings and a held first key") {
    val bindings = bound(Bind.Market -> Vector("g", "m"), Bind.Close -> Vector("∫"))
    assertEquals(
      feed(text("gm"), prompt = true, bindings = bindings)._2,
      Vector(Command.PromptChar('g'), Command.PromptChar('m'))
    )
    val held = feed(text("g"), bindings = bindings)._1
    assert(held.nonEmpty)
    assertEquals(
      Keys.step(held, 109, true, false, bindings),
      (Vector.empty, Some(Command.PromptChar('m')))
    )
  }

  test("control with c quits from the character and held states") {
    val bindings = bound(Bind.Market -> Vector("g", "m"))
    val states = Vector(
      feed(Vector(226), bindings = bindings)._1,
      feed(Vector(226, 136), bindings = bindings)._1,
      feed(text("g"), bindings = bindings)._1,
      feed(text("g\u001b"), bindings = bindings)._1,
      feed(text("g\u001b[1"), bindings = bindings)._1,
      feed(Vector(103, 226), bindings = bindings)._1
    )
    for pending <- states do
      assert(pending.nonEmpty)
      for prompt <- Vector(false, true) do
        assertEquals(
          Keys.step(pending, 3, prompt, false, bindings),
          (Vector.empty, Some(Command.Quit))
        )
  }
