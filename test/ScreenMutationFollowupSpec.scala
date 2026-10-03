package desk

class ScreenMutationFollowupSpec extends munit.FunSuite:
  private def decode(bytes: Vector[Int], prompt: Boolean = false): (Vector[Int], Vector[Command]) =
    bytes.foldLeft((Vector.empty[Int], Vector.empty[Command])) { case ((pending, out), byte) =>
      val (next, command) = Keys.step(pending, byte, prompt, bindings = Bindings.defaults)
      (next, out ++ command.toVector)
    }

  test("a paste closing marker needs its initial escape byte") {
    val start = Vector(27, 91, 50, 48, 48, 126)
    val end = Vector(27, 91, 50, 48, 49, 126)
    for prompt <- Vector(false, true) do
      val malformed = Vector(end.tail) ++ end.indices.map(i => end.updated(i, end(i) + 1))
      for marker <- malformed do
        val partial = start ++ marker ++ Vector(49, 57)
        val held = decode(partial, prompt)
        assert(held._1.nonEmpty)
        assertEquals(held._2, Vector.empty)
        assertEquals(decode(partial ++ end, prompt), (Vector.empty, Vector.empty))
        assertEquals(
          decode(partial ++ end :+ 49, prompt)._2,
          Keys.step(Vector.empty, 49, prompt, bindings = Bindings.defaults)._2.toVector
        )
      for cut <- 0 until end.size do
        val held = decode(start ++ end.take(cut), prompt)
        assert(held._1.nonEmpty)
        assertEquals(held._2, Vector.empty)
        assertEquals(Keys.expire(held._1, prompt, Long.MaxValue), (held._1, None))
  }

  test("a backslash without escape cannot end a terminal string") {
    for opener <- Vector(
        Vector(27, 80),
        Vector(27, 88),
        Vector(27, 93),
        Vector(27, 94),
        Vector(27, 95),
        Vector(144),
        Vector(152),
        Vector(157),
        Vector(158),
        Vector(159)
      )
    do
      for prompt <- Vector(false, true) do
        val partial = opener ++ Vector(92, 49, 57)
        val held = decode(partial, prompt)
        assert(held._1.nonEmpty)
        assertEquals(held._2, Vector.empty)
        assertEquals(decode(partial ++ Vector(27, 92), prompt), (Vector.empty, Vector.empty))
  }

  test("mouse fields consume three and four byte encodings through the final continuation") {
    val encodings = Vector(
      Vector(224, 160, 128),
      Vector(239, 191, 191),
      Vector(240, 144, 128, 128),
      Vector(244, 143, 191, 191)
    )
    for (last, count) <- Vector(77 -> 3, 116 -> 2, 84 -> 6) do
      for encodedAt <- 0 until count do
        for encoded <- encodings do
          for prefix <- Vector(Vector(27, 91), Vector(155)) do
            for prompt <- Vector(false, true) do
              val fields = (0 until count).toVector.flatMap { index =>
                if index == encodedAt then encoded else Vector(49 + index)
              }
              val bytes = prefix ++ Vector(last) ++ fields
              assertEquals(decode(bytes, prompt), (Vector.empty, Vector.empty))
              assertEquals(
                decode(bytes :+ 49, prompt)._2,
                Keys.step(Vector.empty, 49, prompt, bindings = Bindings.defaults)._2.toVector
              )
              for cut <- prefix.size + 1 until bytes.size do
                val partial = decode(bytes.take(cut), prompt)
                assert(partial._1.nonEmpty)
                assertEquals(partial._2, Vector.empty)
                assertEquals(Keys.expire(partial._1, prompt, Long.MaxValue), (partial._1, None))
                val continued = bytes.drop(cut).foldLeft(partial) { case ((pending, out), byte) =>
                  val (next, command) =
                    Keys.step(pending, byte, prompt, bindings = Bindings.defaults)
                  (next, out ++ command.toVector)
                }
                assertEquals(continued, (Vector.empty, Vector.empty))
  }
