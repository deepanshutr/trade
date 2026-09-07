package desk

object Keys:
  private val discard = -1
  private val paste = -2
  private val afterEscape = -3
  private val operatingString = -4
  private val controlString = -5
  private val escapeIntermediate = -6
  private val mouseReport = -7
  private val limit = 32
  private val pasteStart = Vector(27, 91, 50, 48, 48, 126)
  private val pasteEnd = Vector(27, 91, 50, 48, 49, 126)

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

  def step(pending: Vector[Int], byte: Int, promptOpen: Boolean): (Vector[Int], Option[Command]) =
    if byte == 3 then (Vector.empty, Some(Command.Quit))
    else if pending == Vector(afterEscape) then
      prefix(byte) match
        case Some(next) => (next, None)
        case None       => step(Vector.empty, byte, promptOpen)
    else if pending == Vector(escapeIntermediate) then
      (if byte >= 48 && byte <= 126 then Vector.empty else pending, None)
    else if pending.headOption.contains(mouseReport) then (consumeReport(pending, byte), None)
    else if pending.headOption.contains(paste) then
      val tail = pending.tail :+ byte
      if tail.endsWith(pasteEnd) then (Vector.empty, None)
      else (Vector(paste) ++ tail.takeRight(pasteEnd.size - 1), None)
    else if pending.headOption.exists(n => n == operatingString || n == controlString) then
      val state = pending.head
      val ended = byte == 156 || (pending.lastOption.contains(27) && byte == 92) ||
        (state == operatingString && byte == 7)
      if ended then (Vector.empty, None)
      else (if byte == 27 then Vector(state, 27) else Vector(state), None)
    else if pending == Vector(discard) then
      (if finalByte(byte) then Vector.empty else pending, None)
    else
      pending match
        case Vector() =>
          if byte == 27 then (Vector(27), None)
          else if byte == 155 then (Vector(27, 91), None)
          else if byte == 143 then (Vector(27, 79), None)
          else if byte >= 128 then (stringStart(byte).map(Vector(_)).getOrElse(Vector.empty), None)
          else (Vector.empty, if promptOpen then prompt(byte) else plain(byte))
        case Vector(27) =>
          prefix(byte) match
            case Some(next)                       => (next, None)
            case None if byte == 27               => (Vector(27), None)
            case None if byte == 3 || byte == 113 => (Vector.empty, Some(Command.Quit))
            case None                             => (Vector.empty, None)
        case _ =>
          val next = pending :+ byte
          if finalByte(byte) then
            if next == pasteStart then (Vector(paste), None)
            else if next == Vector(27, 91, 77) then (Vector(mouseReport, 3, 0), None)
            else if next == Vector(27, 91, 116) then (Vector(mouseReport, 2, 0), None)
            else if next == Vector(27, 91, 84) then (Vector(mouseReport, 6, 0), None)
            else if promptOpen then (Vector.empty, None)
            else
              val command = next match
                case Vector(27, 91, last) => arrow(last)
                case Vector(27, 79, last) => arrow(last)
                case _                    => None
              (Vector.empty, command)
          else if next.size >= limit then (Vector(discard), None)
          else (next, None)

  def expire(pending: Vector[Int], promptOpen: Boolean): (Vector[Int], Option[Command]) =
    if pending == Vector(27) then
      (Vector(afterEscape), Some(if promptOpen then Command.PromptCancel else Command.CancelTarget))
    else (pending, None)

  private def plain(byte: Int): Option[Command] = byte match
    case 113 | 3                 => Some(Command.Quit)
    case 109                     => Some(Command.StartMarketPrompt)
    case 99                      => Some(Command.ToggleCandles)
    case 13 | 10                 => Some(Command.SubmitTarget)
    case 57                      => Some(Command.SelectTarget(Side.Buy))
    case 48                      => Some(Command.SelectTarget(Side.Sell))
    case n if n >= 49 && n <= 56 => Some(Command.PlaceOrder(Trading.action(n - 49)))
    case _                       => None

  private def arrow(byte: Int): Option[Command] = byte match
    case 65 => Some(Command.ZoomIn)
    case 66 => Some(Command.ZoomOut)
    case 67 => Some(Command.ScrubForward)
    case 68 => Some(Command.ScrubBack)
    case _  => None

  private def prompt(byte: Int): Option[Command] = byte match
    case 13 | 10                 => Some(Command.PromptSubmit)
    case 127 | 8                 => Some(Command.PromptBackspace)
    case 3                       => Some(Command.Quit)
    case c if c >= 32 && c < 127 => Some(Command.PromptChar(c.toChar))
    case _                       => None
