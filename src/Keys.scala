package desk

object Keys:
  private val discard = -1
  private val paste = -2
  private val afterEscape = -3
  private val operatingString = -4
  private val controlString = -5
  private val escapeIntermediate = -6
  private val mouseReport = -7
  // A UTF-8 character in progress: the marker, then its bytes so far.
  private val character = -8
  // The first key of a sequence: the marker, its size, its bytes, then the second key so far.
  private val held = -9
  private val limit = 32
  private val pasteStart = Vector(27, 91, 50, 48, 48, 126)
  private val pasteEnd = Vector(27, 91, 50, 48, 49, 126)

  // What one byte completed.
  private enum Read:
    case Waiting // more bytes are due, or the input was not a key
    case Group(group: KeyGroup)
    case Pointer(command: Command)
    case Broken // an escape and a plain byte: no key
    case Quit

  private def finalByte(byte: Int): Boolean = byte >= 64 && byte <= 126

  private def stringStart(byte: Int): Option[Int] = byte match
    case 93 | 157              => Some(operatingString)
    case 80 | 88 | 94 | 95     => Some(controlString)
    case 144 | 152 | 158 | 159 => Some(controlString)
    case _                     => None

  private def prefix(byte: Int): Option[Vector[Int]] =
    if byte >= 32 && byte <= 47 then Some(Vector(escapeIntermediate))
    else if byte == 91 || byte == 79 then Some(Vector(27, byte))
    else stringStart(byte).map(Vector(_))

  // A group opened by an 8-bit CSI or SS3 byte keeps that byte. It reads as its 7-bit form.
  private def sevenBit(bytes: Vector[Int]): Vector[Int] = bytes.headOption match
    case Some(155) => Vector(27, 91) ++ bytes.tail
    case Some(143) => Vector(27, 79) ++ bytes.tail
    case _         => bytes

  private def consumeReport(pending: Vector[Int], byte: Int): Vector[Int] =
    val continuing = pending(2) > 0 && byte >= 128 && byte <= 191
    val fields = if continuing then pending(1) else (pending(1) - 1).max(0)
    val following =
      if continuing then pending(2) - 1
      else if byte >= 194 && byte <= 223 then 1
      else if byte >= 224 && byte <= 239 then 2
      else if byte >= 240 && byte <= 244 then 3
      else 0
    if fields == 0 && following == 0 then Vector.empty
    else Vector(mouseReport, fields, following)

  private def split(pending: Vector[Int]): (Option[KeyGroup], Vector[Int]) =
    if pending.headOption.contains(held) then
      val size = pending(1)
      (Some(KeyGroup(pending.slice(2, 2 + size))), pending.drop(2 + size))
    else (None, pending)

  private def hold(first: Option[KeyGroup], rest: Vector[Int]): Vector[Int] =
    first.fold(rest)(key => Vector(held, key.bytes.size) ++ key.bytes ++ rest)

  def step(
      pending: Vector[Int],
      byte: Int,
      promptOpen: Boolean,
      mouseEnabled: Boolean = false,
      bindings: Bindings = Bindings.defaults
  ): (Vector[Int], Option[Command]) =
    val (heldKey, inner) = split(pending)
    // The market prompt ignores bindings, so it drops a held first key.
    val first = heldKey.filter(_ => !promptOpen)
    if byte == 3 then (Vector.empty, Some(Command.Quit))
    else if inner.headOption.contains(character) && (byte < 128 || byte > 191) then
      // A torn character is dropped with any held first key, and the byte reads on its own.
      step(Vector.empty, byte, promptOpen, mouseEnabled, bindings)
    else
      val (next, read) = scan(inner, byte, mouseEnabled)
      read match
        case Read.Waiting          => (hold(first, next), None)
        case Read.Pointer(command) => (hold(first, next), Some(command))
        case Read.Broken           => (next, None)
        case Read.Quit             => (Vector.empty, Some(Command.Quit))
        case Read.Group(group)     =>
          first match
            case Some(key)                      => (next, bindings.command(key, group))
            case None if promptOpen             => (next, prompt(group))
            case None if bindings.prefix(group) => (hold(Some(group), next), None)
            case None => (next, bindings.command(group).orElse(fixed(group)))

  // Reads one byte of one group. The result says whether a group, a mouse report or nothing ended.
  private def scan(pending: Vector[Int], byte: Int, mouseEnabled: Boolean): (Vector[Int], Read) =
    if pending == Vector(afterEscape) then
      prefix(byte) match
        case Some(next) => (next, Read.Waiting)
        case None       => scan(Vector.empty, byte, mouseEnabled)
    else if pending == Vector(escapeIntermediate) then
      (if byte >= 48 && byte <= 126 then Vector.empty else pending, Read.Waiting)
    else if pending.headOption.contains(mouseReport) then
      (consumeReport(pending, byte), Read.Waiting)
    else if pending.headOption.contains(paste) then
      val tail = pending.tail :+ byte
      if tail.endsWith(pasteEnd) then (Vector.empty, Read.Waiting)
      else (Vector(paste) ++ tail.takeRight(pasteEnd.size - 1), Read.Waiting)
    else if pending.headOption.exists(n => n == operatingString || n == controlString) then
      val state = pending.head
      val ended = byte == 156 || (pending.lastOption.contains(27) && byte == 92) ||
        (state == operatingString && byte == 7)
      if ended then (Vector.empty, Read.Waiting)
      else (if byte == 27 then Vector(state, 27) else Vector(state), Read.Waiting)
    else if pending == Vector(discard) then
      (if finalByte(byte) then Vector.empty else pending, Read.Waiting)
    else if pending.headOption.contains(character) then
      val bytes = pending.tail :+ byte
      if bytes.size == KeyGroup.utf8Length(bytes.head) then
        (Vector.empty, Read.Group(KeyGroup(bytes)))
      else (character +: bytes, Read.Waiting)
    else
      pending match
        case Vector() =>
          if byte == 27 || byte == 155 || byte == 143 then (Vector(byte), Read.Waiting)
          else if KeyGroup.utf8Length(byte) > 0 then (Vector(character, byte), Read.Waiting)
          else if byte >= 128 then
            (stringStart(byte).map(Vector(_)).getOrElse(Vector.empty), Read.Waiting)
          else (Vector.empty, Read.Group(KeyGroup(Vector(byte))))
        case Vector(27) =>
          prefix(byte) match
            case Some(next)          => (next, Read.Waiting)
            case None if byte == 27  => (Vector(27), Read.Waiting)
            case None if byte == 113 => (Vector.empty, Read.Quit)
            case None                => (Vector.empty, Read.Broken)
        case _ =>
          val next = pending :+ byte
          val seen = sevenBit(next)
          if finalByte(byte) then
            if seen == pasteStart then (Vector(paste), Read.Waiting)
            else if seen == Vector(27, 91, 77) then (Vector(mouseReport, 3, 0), Read.Waiting)
            else if seen == Vector(27, 91, 116) then (Vector(mouseReport, 2, 0), Read.Waiting)
            else if seen == Vector(27, 91, 84) then (Vector(mouseReport, 6, 0), Read.Waiting)
            else if mouseEnabled && seen.startsWith(Vector(27, 91, 60)) then
              val report = "<(0|64|65);([1-9][0-9]{0,3});([1-9][0-9]{0,3})([Mm])".r
              val command = seen.drop(2).map(_.toChar).mkString match
                case report("0", x, y, "M") =>
                  Some(Command.PointerPress(x.toInt, y.toInt, 0))
                case report(button, x, y, end)
                    if (button == "0" && end == "m") || (button != "0" && end == "M") =>
                  Some(Command.Pointer(x.toInt, y.toInt, button.toInt))
                case _ => None
              (Vector.empty, command.fold(Read.Waiting)(Read.Pointer(_)))
            else (Vector.empty, Read.Group(KeyGroup(next)))
          else if next.size >= limit then (Vector(discard), Read.Waiting)
          else (next, Read.Waiting)

  // A lone escape cancels after the escape wait. A held first key is dropped after the sequence
  // wait. Every other state waits for its bytes.
  def expire(
      pending: Vector[Int],
      promptOpen: Boolean,
      elapsedNanos: Long
  ): (Vector[Int], Option[Command]) =
    val (first, inner) = split(pending)
    if inner == Vector(27) && elapsedNanos >= Main.escapeWaitNanos then
      (Vector(afterEscape), Some(if promptOpen then Command.PromptCancel else Command.Cancel))
    else if first.nonEmpty && inner.isEmpty && elapsedNanos >= Main.sequenceWaitNanos then
      (Vector.empty, None)
    else (pending, None)

  // Enter and the arrows, when no binding took the group.
  private def fixed(group: KeyGroup): Option[Command] = sevenBit(group.bytes) match
    case Vector(13) | Vector(10) => Some(Command.Confirm)
    case Vector(27, 91, last)    => arrow(last)
    case Vector(27, 79, last)    => arrow(last)
    case _                       => None

  private def arrow(byte: Int): Option[Command] = byte match
    case 65 => Some(Command.ZoomIn)
    case 66 => Some(Command.ZoomOut)
    case 67 => Some(Command.ScrubForward)
    case 68 => Some(Command.ScrubBack)
    case _  => None

  private def prompt(group: KeyGroup): Option[Command] = group.bytes match
    case Vector(13 | 10)                 => Some(Command.PromptSubmit)
    case Vector(127 | 8)                 => Some(Command.PromptBackspace)
    case Vector(c) if c >= 32 && c < 127 => Some(Command.PromptChar(c.toChar))
    case _                               => None
