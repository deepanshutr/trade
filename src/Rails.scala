package desk

// A marker on a rail: its value on the group's scale, its glyph and its ink.
final case class RailMark(value: Double, glyph: Char, ink: String)
// One window's row: its label, its marks and its value texts, each with an ink.
final case class RailRow(label: String, marks: Vector[RailMark], values: Vector[(String, String)])
// A group of five rows on one scale.
final case class RailGroup(
    title: Vector[(String, String)],
    labelWidth: Int,
    lo: Double,
    hi: Double,
    scale: Vector[String],
    valuesWidth: Int,
    rows: Vector[RailRow]
)

object Rails:
  val gap: Int = 3
  val narrowest: Int = 17
  val widest: Int = 41

  // The most groups, from the first, that leave each rail at least the narrowest odd width, with
  // that width. An odd width has a centre cell for the bar.
  def fit(groups: Vector[RailGroup], width: Int): (Vector[RailGroup], Int) =
    (groups.size to 1 by -1).iterator
      .map { count =>
        val kept = groups.take(count)
        val fixed = kept.map(g => g.labelWidth + 1 + g.valuesWidth).sum + (count - 1) * gap
        val w = widest.min(Math.floorDiv(width - fixed, count))
        (kept, if w % 2 == 0 then w - 1 else w)
      }
      .find(_._2 >= narrowest)
      .getOrElse((Vector.empty, 0))

  // Faint dots with a centre bar and each mark at its nearest cell. A mark past an end is drawn on
  // that end in inverse. Two or more marks in one cell are drawn as one glyph in the overlap ink.
  def rail(cells: Int, lo: Double, hi: Double, marks: Vector[RailMark], overlap: String): String =
    val at = marks.groupBy(m =>
      Screen.round((m.value.max(lo).min(hi) - lo) / (hi - lo) * (cells - 1)).toInt
    )
    Chart.styled(Vector.tabulate(cells) { i =>
      at.get(i) match
        case None       => (if i == (cells - 1) / 2 then '|' else '·', Screen.dim)
        case Some(here) =>
          val past = if here.exists(m => m.value < lo || m.value > hi) then Screen.inverse else ""
          if here.size == 1 then (here.head.glyph, past + here.head.ink) else ('◉', past + overlap)
    })

  // Twelve rows of the width: the title, scale and five rails of each kept group side by side,
  // four blank rows, then the key.
  def lines(groups: Vector[RailGroup], width: Int, key: String, overlap: String): Vector[String] =
    val (kept, w) = fit(groups, width)
    def piece(p: (String, String)): String =
      if p._2.isEmpty then p._1 else p._2 + p._1 + Screen.reset
    def column(g: RailGroup): Vector[String] =
      val title = Chart.fit(g.title.map(piece).mkString("  "), g.labelWidth + w + 1 + g.valuesWidth)
      val rails = g.rows.take(5).map { r =>
        Screen.dim + Chart.fit(r.label, g.labelWidth) + Screen.reset +
          rail(w, g.lo, g.hi, r.marks, overlap) + " " + r.values.map(piece).mkString(" ")
      }
      (Vector(title, " " * g.labelWidth + scale(g.scale, w)) ++ rails).padTo(11, "")
    val columns = kept.map(column)
    val x = kept.scanLeft(0)((at, g) => at + g.labelWidth + w + 1 + g.valuesWidth + gap)
    // The text before each group is cut or padded to the start column of that group. The last
    // group is fitted to the width with the row.
    Vector.tabulate(11) { r =>
      Chart.fit(
        kept.indices.foldLeft("")((line, k) => Chart.fit(line, x(k)) + columns(k)(r)),
        width
      )
    } :+ Chart.fit(key, width)

  // The three scale words, faint, at the start, the middle and the end of a rail of w cells.
  private def scale(words: Vector[String], w: Int): String =
    val starts = Vector(0, (w - 1) / 2 - words(1).length / 2, w - words(2).length)
    val at = words.zip(starts).flatMap((word, s) => word.indices.map(j => (s + j) -> word(j))).toMap
    Chart.styled(Vector.tabulate(w)(i => at.get(i).fold((' ', ""))(c => (c, Screen.dim))))
