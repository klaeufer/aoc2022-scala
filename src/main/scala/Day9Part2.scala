object Day9Part2 extends App:

  val example = """R 4
                  |U 4
                  |L 3
                  |D 1
                  |R 4
                  |D 1
                  |L 5
                  |R 2
                  |""".stripMargin('|').split('\n').toIndexedSeq

  println(example)

  type Move = (Char, Int)

  def lines2moves(lines: Iterable[String]): Iterable[Move] = lines.map { l => (l(0), l.drop(2).toInt) }

  println(lines2moves(example))

  type Pos = (Int, Int)

  val zero = (0, 0)

  def plus(p: Pos, q: Pos): Pos = (p._1 + q._1, p._2 + q._2)

  def minus(p: Pos, q: Pos): Pos = (p._1 - q._1, p._2 - q._2)

  def headMove(d: Char): Pos = d match
    case 'L' => (0, -1)
    case 'R' => (0, 1)
    case 'U' => (-1, 0)
    case 'D' => (1, 0)

  def tailMove(t: Pos, d: Char): Pos = (t, d) match
    case ((0, 1), 'L') => (0, -1)
    case ((0, -1), 'R') => (0, 1)
    case ((1, 0), 'U') => (-1, 0)
    case ((-1, 0), 'D') => (1, 0)
    case ((1, 1), 'L') => (-1, -1)
    case ((1, 1), 'U') => (-1, -1)
    case ((1, -1), 'R') => (-1, 1)
    case ((1, -1), 'U') => (-1, 1)
    case ((-1, 1), 'L') => (1, -1)
    case ((-1, 1), 'D') => (1, -1)
    case ((-1, -1), 'R') => (1, 1)
    case ((-1, -1), 'D') => (1, 1)
    case ((x, y), _) => (0, 0)

  type Rope = Seq[Pos]

  def abs2rel(r: Rope): Rope = zero +: r.sliding(2).map { case Seq(h, t) => minus(t, h) }.toSeq

  def rel2abs(r: Rope, h: Pos): Rope = r.map(p => plus(p, h))

//  def move(r: Rope, d: Char): Rope =
//    val q = abs2rel(r)
//    val r2: Rope = headMove(d) +: q.tail.scanLeft(tailMove(q.head, d)) { (s, p) =>
//      tailMove(p, d)
//    }
//    rel2abs(r2, r.head)

//    val ms = q.tail.scanLeft(headMove(d))
//
//
//      headMove(d) +: q.tail.map(p => tailMove(p, d))
//    val r2 = q.zip(ms).map(plus)
//    rel2abs(r2, r.head)

  def move(r: Rope, d: Char): Rope =
    val h = headMove(d)
    val t = tailMove(minus(r.last, r.head), d)
    Seq(plus(r.head, h), plus(r.last, t))

  // IDIOM apply function to its result n times

  def moveN(p: Rope, d: Char, n: Int) =
    Iterator.iterate(p)(r => move(r, d)).take(n + 1).toSeq

  val r2 = Seq.fill(2)(zero)

  val result0 = lines2moves(example).scanLeft(Seq(r2)) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten
  println(result0.map(_.last).toSet.size)

  val input = scala.io.Source.fromFile("AdventOfCodeDay9Input.txt").getLines.toSeq
  val result1 = lines2moves(input).scanLeft(Seq(r2)) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten
  println(result1.map(_.last).toSet.size)
