package desk

class ScreenSpec extends munit.FunSuite:
  val t0 = 1_700_000_000_000L
  def lv(px: String, sz: String) = Level(px, sz, 1)
  def plain(x: String) =
    x.replace(Screen.bold, "").replace(Screen.dim, "").replace(Screen.reset, "")

  test("the escape sequences are built from the escape character") {
    assertEquals(Screen.esc, 27.toChar.toString)
    assertEquals(Screen.bold, Screen.esc + "[1m")
    assertEquals(Screen.dim, Screen.esc + "[2m")
    assertEquals(Screen.reset, Screen.esc + "[0m")
  }
  // the research snapshot at 09:00:41Z: u = 0.964, r = 1.696, bar width 20
  test("bar lengths reproduce the research numbers") {
    val (u, r) = (0.964, 1.696)
    assertEquals(Screen.barLen(0.547, u, r), 9L)
    assertEquals(Screen.barLen(0.740, u, r), 11L)
    assertEquals(Screen.barLen(0.964, u, r), 14L)
    assertEquals(Screen.barLen(1.221, u, r), 16L)
    assertEquals(Screen.barLen(1.696, u, r), 20L)
    assert(Screen.barLen(2.235, u, r) > 20L)
    assertEquals(Screen.barLen(0, u, r), 0L)
    assertEquals(Screen.barLen(1, 0, r), 0L)
  }
  test("rounding is half up") {
    assertEquals(Screen.round(0.5), 1L)
    assertEquals(Screen.round(-0.5), 0L)
    assertEquals(Screen.round(2.49), 2L)
  }
  test("lower median for even counts, plain median for odd") {
    assertEquals(Screen.lowerMedian(Vector(4, 1, 3, 2)), 2.0)
    assertEquals(Screen.lowerMedian(Vector(3, 1, 2)), 2.0)
    assertEquals(Screen.lowerMedian(Vector.empty), 0.0)
  }
  test("the reference is the second largest on screen until the window holds 40 sizes") {
    val onScreen = Vector(0.547, 0.740, 2.235, 1.696, 0.964)
    assertEquals(Screen.reference(Vector.empty, onScreen), 1.696)
    assertEquals(Screen.reference(Vector.empty, Vector(5.0)), 5.0)
    val window = Vector((t0, (1 to 40).map(_.toDouble).toVector))
    assertEquals(Screen.reference(window, onScreen), 38.0)
  }
  test("cumulative sums from the best level outward") {
    val ls = Vector(lv("1", "0.5"), lv("2", "1.5"), lv("3", "1"))
    assertEquals(Screen.cumulative(ls), Vector(0.5, 2.0, 3.0))
  }
  test("the gauge track places the mark by tenths and the centre bar survives") {
    assertEquals(Screen.track(0.0923), "..........|#.........")
    assertEquals(Screen.track(-0.049), "..........#..........")
    assertEquals(Screen.track(1.0), "..........|.........#")
    assertEquals(Screen.track(-1.0), "#.........|..........")
    assertEquals(Screen.track(0.0), "..........#..........")
  }
  test("queue and distance weighted imbalance reproduce the research numbers") {
    val asks = Vector(
      "518.40" -> "0.547",
      "518.43" -> "0.740",
      "518.44" -> "1.221",
      "518.47" -> "0.584",
      "518.49" -> "0.771",
      "518.50" -> "0.732",
      "518.53" -> "0.732",
      "518.56" -> "1.696",
      "518.58" -> "2.235",
      "518.59" -> "1.696"
    ).map(lv)
    val bids = (0 until 10).map(i => lv(f"${518.36 - 0.01 * i}%.2f", "0.964")).toVector
    val (q, d) = Screen.imbalance(Book(bids, asks, 0))
    assertEqualsDouble(q, 0.2760, 0.0005)
    assertEqualsDouble(d, 0.0923, 0.0005)
    assertEquals(Screen.imbalance(Book(Vector.empty, asks, 0)), (-1.0, -1.0))
  }
  test("trade flow over the last sixty seconds") {
    val tape = Vector(
      Trade(t0, Side.Buy, "1", "19.4"),
      Trade(t0 - 59_000, Side.Sell, "1", "21.4"),
      Trade(t0 - 61_000, Side.Buy, "1", "1000")
    )
    val (f, n) = Screen.flow(tape, t0)
    assertEqualsDouble(f, -0.049, 0.001)
    assertEquals(n, 2)
    assertEquals(Screen.flow(Vector.empty, t0), (0.0, 0))
  }
  test("the spread rule shows spread, basis points and book age, padded to 46") {
    val b = Book(Vector(lv("518.36", "1")), Vector(lv("518.40", "1")), t0 - 400)
    val r = Screen.rule(b, t0)
    assertEquals(r.length, 46)
    assert(r.startsWith("--------  spread 0.04  0.8bp  book 0.4s  ----"), r)
    val empty = Screen.rule(Book(Vector.empty, Vector.empty, t0), t0)
    assertEquals(empty.length, 46)
    assert(empty.contains("spread n/a"))
  }
  test("ladder rows are 46 wide, asks above the rule highest first, bids below best first") {
    val b = Book(Vector(lv("100", "1"), lv("99", "2")), Vector(lv("101", "3"), lv("102", "4")), t0)
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(b))
    val rows = Screen.ladder(s, 27, t0)
    assertEquals(rows.size, 27)
    assert(rows.forall(r => plain(r).length == 46), rows.map(plain).map(_.length))
    assert(plain(rows(11)).startsWith("     102"))
    assert(plain(rows(12)).startsWith("     101"))
    assert(rows(13).startsWith("--------"))
    assert(plain(rows(14)).startsWith("     100"))
    assert(plain(rows(15)).startsWith("      99"))
    assert(plain(rows(0)).trim.isEmpty)
    assert(plain(rows(26)).trim.isEmpty)
  }
  test("the best rows are bold, a clamped row carries the marker and bold, outer rows dim") {
    val asks = (0 until 13).map(i => lv((101 + i).toString, "1")).toVector
    val bids = (0 until 13).map(i => lv((100 - i).toString, if i == 5 then "40" else "1")).toVector
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(Book(bids, asks, t0)))
    val rows = Screen.ladder(s, 27, t0)
    assert(rows(12).startsWith(Screen.bold), "best ask bold")
    assert(rows(14).startsWith(Screen.bold), "best bid bold")
    assert(rows(19).startsWith(Screen.bold) && rows(19).contains("#>"), "clamped bid row 5")
    assert(rows(0).startsWith(Screen.dim), "outer ask dim")
    assert(rows(26).startsWith(Screen.dim), "outer bid dim")
    assert(!rows(7).startsWith(Screen.dim) && !rows(7).startsWith(Screen.bold), "middle ask plain")
  }
  test("an even ladder height leaves one blank row at the bottom") {
    val rows = Screen.ladder(Snapshot.initial("X", "5m", t0), 28, t0)
    assertEquals(rows.size, 28)
  }
  test("gauge lines are 46 wide and read the book and the tape") {
    val b = Book(Vector(lv("100", "3")), Vector(lv("101", "1")), t0)
    val tape = Vector(Trade(t0, Side.Buy, "1", "2"))
    val s = Snapshot.initial("X", "5m", t0).copy(book = Some(b), tape = tape)
    val g = Screen.gauges(s, t0)
    assertEquals(g.size, 2)
    assert(g.forall(_.length == 46))
    assert(g(0).startsWith("BOOK ..........|....#..... D+0.50 Q+0.50"), g(0))
    assert(g(1).startsWith("TAPE ..........|.........# F+1.00 60s n=1"), g(1))
    val none = Screen.gauges(Snapshot.initial("X", "5m", t0), t0)
    assert(none(0).startsWith("BOOK ..........|.........."), none(0))
    assert(none(1).startsWith("TAPE ..........|.......... n=0"), none(1))
  }
