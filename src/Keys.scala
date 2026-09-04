package desk

object Keys:
  def step(pending: Vector[Int], byte: Int, promptOpen: Boolean): (Vector[Int], Option[Command]) =
    if promptOpen then (Vector.empty, prompt(byte))
    else
      pending match
        case Vector()   => (if byte == 27 then Vector(27) else Vector.empty, plain(byte))
        case Vector(27) =>
          if byte == 91 || byte == 79 then (Vector(27, byte), None)
          else step(Vector.empty, byte, promptOpen)
        case _ => (Vector.empty, arrow(byte))

  private def plain(byte: Int): Option[Command] = byte match
    case 113 | 3 => Some(Command.Quit)
    case 109     => Some(Command.StartMarketPrompt)
    case _       => None

  private def arrow(byte: Int): Option[Command] = byte match
    case 65 => Some(Command.ZoomOut)
    case 66 => Some(Command.ZoomIn)
    case 67 => Some(Command.ScrubForward)
    case 68 => Some(Command.ScrubBack)
    case _  => None

  private def prompt(byte: Int): Option[Command] = byte match
    case 13 | 10                 => Some(Command.PromptSubmit)
    case 27                      => Some(Command.PromptCancel)
    case 127 | 8                 => Some(Command.PromptBackspace)
    case 3                       => Some(Command.Quit)
    case c if c >= 32 && c < 127 => Some(Command.PromptChar(c.toChar))
    case _                       => None
