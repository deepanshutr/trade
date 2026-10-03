package desk

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.text.Normalizer
import scala.util.Try

// The actions a key can be bound to. Enter, Esc, the arrows, Control C, mouse, paste and the
// market prompt text are fixed.
enum Bind:
  case Quit, Prop, Live, Market, Chart, Sound, LongSmall, LongBig, ShortSmall, ShortBig, Close,
    Settings

  private def words: Vector[String] = toString.split("(?=[A-Z])").toVector
  def label: String = (words.head +: words.tail.map(_.toLowerCase)).mkString(" ")
  def field: String = words.head.toLowerCase + words.tail.mkString

// One key as the terminal sends it: one byte, one UTF-8 character, or one CSI or SS3 group.
final case class KeyGroup(bytes: Vector[Int])

object KeyGroup:
  private val notKey = "That is not one key"
  private val reserved = Map(
    0 -> "Control Space cannot be bound",
    3 -> "Control C quits Desk",
    4 -> "Control D ends input",
    8 -> "Control H erases in the market prompt",
    9 -> "Control I is Tab",
    10 -> "Control J is Enter",
    13 -> "Enter confirms",
    17 -> "Control Q is flow control",
    19 -> "Control S is flow control",
    26 -> "Control Z stops programs",
    27 -> "Esc cancels",
    28 -> "Control backslash quits programs",
    127 -> "Delete erases in the market prompt"
  )
  private def codes(text: String): Vector[Int] = text.toVector.map(_.toInt)
  private val pasteMarkers = Set(codes("\u001b[200~"), codes("\u001b[201~"))
  // ESC [ M, ESC [ t and ESC [ T open the legacy mouse and highlight reports.
  private val mouseOpeners = Set(77, 116, 84)
  private val functionKeys: Map[Vector[Int], String] = Vector(
    "OP",
    "OQ",
    "OR",
    "OS",
    "[15~",
    "[17~",
    "[18~",
    "[19~",
    "[20~",
    "[21~",
    "[23~",
    "[24~"
  ).zipWithIndex.map((text, index) => codes("\u001b" + text) -> s"F${index + 1}").toMap
  // What Option types on the US layout. A display nicety: a wrong entry changes no binding.
  private val option: Map[String, String] =
    "abcdfghjklmopqrstvwxyz1234567890"
      .zip("å∫ç∂ƒ©˙∆˚¬µøπœ®ß†√∑≈¥Ω¡™£¢∞§¶•ªº")
      .map((key, typed) => typed.toString -> key.toUpper.toString)
      .toMap
  private val bareAccents = Set("\u00b4", "\u02c6", "\u02dc", "\u00a8")
  private val accentMarks = Set('\u0300', '\u0301', '\u0302', '\u0303', '\u0308')
  // Shown alone, these act on the terminal or draw onto the cell before them.
  private val unprintable = Set[Int](
    Character.CONTROL,
    Character.FORMAT,
    Character.LINE_SEPARATOR,
    Character.PARAGRAPH_SEPARATOR,
    Character.NON_SPACING_MARK,
    Character.ENCLOSING_MARK
  )

  def utf8Length(lead: Int): Int =
    if lead >= 194 && lead <= 223 then 2
    else if lead >= 224 && lead <= 239 then 3
    else if lead >= 240 && lead <= 244 then 4
    else 0

  // One printable character in strict UTF-8, or None.
  private def character(bytes: Vector[Int]): Option[String] =
    Option
      .when(bytes.size >= 2 && bytes.size == utf8Length(bytes.head))(bytes)
      .flatMap(found =>
        Try(
          UTF_8.newDecoder().decode(ByteBuffer.wrap(found.map(_.toByte).toArray)).toString
        ).toOption
      )
      .filter(text =>
        text.codePointCount(0, text.length) == 1 &&
          !unprintable.contains(Character.getType(text.codePointAt(0)))
      )

  // A complete CSI or SS3 group as the workspace reader ends one.
  private def escapeGroup(bytes: Vector[Int]): Boolean =
    bytes.size >= 3 && bytes.size <= 32 && bytes.head == 27 &&
      (bytes(1) == 91 || bytes(1) == 79) && bytes.last >= 64 && bytes.last <= 126 &&
      bytes.slice(2, bytes.size - 1).forall(b => b >= 32 && b <= 63)

  // A dead key on the US layout sends nothing until the next press.
  private def dead(text: String): Boolean =
    bareAccents.contains(text) || {
      val parts = Normalizer.normalize(text, Normalizer.Form.NFD)
      parts.length == 2 && parts(0) < 128 && parts(0).isLetter && accentMarks.contains(parts(1))
    }

  def refused(group: KeyGroup): Option[String] = group.bytes match
    case bytes if bytes.isEmpty || bytes.exists(b => b < 0 || b > 255) => Some(notKey)
    case Vector(byte) => reserved.get(byte).orElse(Option.when(byte > 126)(notKey))
    case bytes if bytes.headOption.contains(27) =>
      if !escapeGroup(bytes) then Some(notKey)
      else if bytes.size == 3 && bytes(2) >= 65 && bytes(2) <= 68 then
        Some("The arrows move the chart")
      else if pasteMarkers.contains(bytes) then Some("Paste markers are not keys")
      else if bytes(1) == 91 && (bytes(2) == 60 || bytes.size == 3 && mouseOpeners(bytes(2))) then
        Some("Mouse reports are not keys")
      else None
    case bytes =>
      character(bytes) match
        case None                     => Some(notKey)
        case Some(text) if dead(text) => Some("That key needs a second press on this layout")
        case Some(_)                  => None

  private def hex(bytes: Vector[Int]): String = bytes.map(b => f"$b%02x").mkString(" ")

  private def single(byte: Int, control: String): Option[String] = byte match
    case 0                      => Some(control + "Space")
    case 27                     => Some("Esc")
    case n if n < 32 && n > 0   => Some(control + (n + 64).toChar)
    case n if n > 32 && n < 127 => Some(n.toChar.toString)
    case _                      => None

  def name(group: KeyGroup): String = group.bytes match
    case Vector(32)                                => "Space"
    case Vector(127)                               => "Delete"
    case Vector(byte) if single(byte, "").nonEmpty => single(byte, "Control ").get
    case bytes                                     =>
      functionKeys
        .get(bytes)
        .orElse(character(bytes).map(text => text + option.get(text).fold("")(" Option " + _)))
        .getOrElse(hex(bytes))

  def short(group: KeyGroup): String = group.bytes match
    case Vector(32)                                => "Spc"
    case Vector(127)                               => "Del"
    case Vector(byte) if single(byte, "").nonEmpty => single(byte, "^").get
    case bytes                                     =>
      functionKeys
        .get(bytes)
        .orElse(character(bytes))
        .orElse(Option.when(escapeGroup(bytes))("Esc" + bytes.tail.map(_.toChar).mkString))
        .getOrElse(hex(bytes))

// One key, or a sequence of two keys.
final case class Binding(keys: Vector[KeyGroup]):
  def name: String = keys.map(KeyGroup.name).mkString(" then ")
  def short: String = keys.map(KeyGroup.short).mkString(" ")

// Only defaults, load and updated make bindings, so every stored binding has passed the rules.
final case class Bindings private (map: Map[Bind, Binding]):
  def command(group: KeyGroup): Option[Command] =
    Bind.values.find(map.get(_).contains(Binding(Vector(group)))).map(Bindings.command)

  def command(first: KeyGroup, second: KeyGroup): Option[Command] =
    Bind.values.find(map.get(_).contains(Binding(Vector(first, second)))).map(Bindings.command)

  def prefix(group: KeyGroup): Boolean =
    map.values.exists(binding => binding.keys.size == 2 && binding.keys.head == group)

  def updated(action: Bind, binding: Binding): Either[String, Bindings] =
    Bindings
      .problem(
        binding,
        Bind.values.toVector.filter(_ != action).flatMap(a => map.get(a).map(a -> _))
      )
      .toLeft(Bindings(map.updated(action, binding)))

object Bindings:
  private val maximumBytes = 16384L

  val defaults: Bindings = Bindings(
    Bind.values
      .zip("qplmcv1234xs")
      .map((action, key) => action -> Binding(Vector(KeyGroup(Vector(key.toInt)))))
      .toMap
  )

  def command(action: Bind): Command = action match
    case Bind.Quit       => Command.Quit
    case Bind.Prop       => Command.Mode("PROP")
    case Bind.Live       => Command.Mode("LIVE")
    case Bind.Market     => Command.StartMarketPrompt
    case Bind.Chart      => Command.ToggleCandles
    case Bind.Sound      => Command.ToggleMute
    case Bind.LongSmall  => Command.PlaceOrder(Trading.action(0))
    case Bind.LongBig    => Command.PlaceOrder(Trading.action(1))
    case Bind.ShortSmall => Command.PlaceOrder(Trading.action(2))
    case Bind.ShortBig   => Command.PlaceOrder(Trading.action(3))
    case Bind.Close      => Command.Close
    case Bind.Settings   => Command.Settings

  def path(home: Path): Path = home.resolve(".config/desk/bindings.json")

  // The reason a binding cannot join the others, or None.
  private def problem(binding: Binding, others: Vector[(Bind, Binding)]): Option[String] =
    def clash(other: Bind, theirs: Binding): Option[String] =
      val first = theirs.keys.headOption
      if binding == theirs then Some(s"${binding.name} is already the key for ${other.label}")
      else if binding.keys.size == 1 && theirs.keys.size == 2 && first == binding.keys.headOption
      then Some(s"${binding.name} starts the keys for ${other.label}")
      else if binding.keys.size == 2 && theirs.keys.size == 1 && first == binding.keys.headOption
      then Some(s"${theirs.name} is already the key for ${other.label}")
      else None
    if binding.keys.isEmpty || binding.keys.size > 2 then Some("A binding is one key or two keys")
    else
      binding.keys.iterator
        .flatMap(KeyGroup.refused)
        .nextOption()
        .orElse(others.iterator.flatMap((other, theirs) => clash(other, theirs)).nextOption())

  private def keys(value: ujson.Value): Option[Binding] =
    value.arrOpt
      .map(_.toVector)
      .filter(list => list.size == 1 || list.size == 2)
      .flatMap(list => Option.when(list.forall(_.strOpt.exists(_.nonEmpty)))(list.map(_.str)))
      .map(texts =>
        Binding(texts.map { text =>
          // A lone surrogate cannot be encoded and becomes no key at all.
          val bytes = Try(UTF_8.newEncoder().encode(java.nio.CharBuffer.wrap(text))).toOption
          KeyGroup(bytes.fold(Vector.empty[Int]) { buffer =>
            val array = new Array[Byte](buffer.remaining)
            buffer.get(array)
            array.toVector.map(_ & 0xff)
          })
        })
      )

  private def parse(text: String): Either[String, Bindings] =
    Try(ujson.read(text)).toOption
      .toRight("the file is not JSON")
      .flatMap { json =>
        json.objOpt
          .filter(_.keySet == Set("version", "bindings"))
          .filter(_("version").numOpt.contains(1.0))
          .flatMap(_("bindings").objOpt)
          .toRight("the file needs version 1 and bindings")
      }
      .flatMap { entries =>
        entries.toVector.foldLeft[Either[String, Map[Bind, Binding]]](Right(defaults.map)) {
          case (all, (field, value)) =>
            all.flatMap { found =>
              Bind.values.find(_.field == field).toRight("unknown action").flatMap { action =>
                keys(value)
                  .toRight(s"$field must be a list of one or two keys")
                  .map(binding => found.updated(action, binding))
              }
            }
        }
      }
      .flatMap { all =>
        val ordered = Bind.values.toVector.flatMap(action => all.get(action).map(action -> _))
        ordered.indices.iterator
          .flatMap { index =>
            val (action, binding) = ordered(index)
            problem(binding, ordered.take(index)).map(reason => s"${action.field}: $reason")
          }
          .nextOption()
          .toLeft(Bindings(all))
      }

  // An absent file is the defaults. A refused file stops Desk at start, like the settings.
  def load(path: Path): Either[String, Bindings] =
    Try(LocalFiles.read(path, maximumBytes)).toOption
      .toRight("the file must be private, UTF-8 and at most 16 KiB")
      .flatMap(_.fold[Either[String, Bindings]](Right(defaults))(parse))
      .left
      .map(reason => s"invalid key bindings: $reason (${path.getFileName})")

  def save(path: Path, bindings: Bindings): Unit =
    val entries = Bind.values.toVector.flatMap(action =>
      bindings.map.get(action).map { binding =>
        action.field -> ujson.Arr.from(binding.keys.map { key =>
          ujson.Str(new String(key.bytes.map(_.toByte).toArray, UTF_8))
        })
      }
    )
    val json = ujson.Obj("version" -> ujson.Num(1), "bindings" -> ujson.Obj.from(entries))
    LocalFiles.write(path, ujson.write(json, indent = 2) + "\n")
