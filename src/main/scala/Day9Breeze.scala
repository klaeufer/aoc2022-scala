import breeze.linalg.*
import breeze.numerics.*

object Day9Breeze extends App:

  val example =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2
      |""".stripMargin('|').split('\n').toIndexedSeq

  type Move = DenseVector[Int]

  given CanEqual[DenseVector[Int], DenseVector[Int]] = CanEqual.derived

  def toMove(d: Char): Move = d match
    case 'L' => DenseVector(0, -1)
    case 'R' => DenseVector(0, 1)
    case 'U' => DenseVector(-1, 0)
    case 'D' => DenseVector(1, 0)

  val m00 = DenseVector.zeros[Int](2)

  // h = (0, 0), t = position relative to h, m = hori/vert/diag
  // TODO review and possibly simplify
  def tailMove(t: Move, m: Move): Move =
    if norm(m - t) < 1.5 then
      m00
    else if m(0) == t(0) || m(1) == t(1) then
      (m - t) / 2
    else if norm(t) > 1.1 then
      m00 - t
    else
      m

  val m01 = DenseVector(0, 1)
  val m10 = DenseVector(1, 0)
  val m11 = DenseVector(1, 1)

  norm(m11)

  assert { tailMove(m00, m00) == m00 }
  assert { tailMove(m00, m01) == m00 }
  assert { tailMove(m00, m10) == m00 }
  assert { tailMove(m00, m11) == m00 }

  assert { tailMove(m01, m00) == m00 }
  assert { tailMove(m01, m01) == m00 }
  assert { tailMove(m01, m11) == m00 }
  assert { tailMove(m01, m10) == m00 }

  assert { tailMove(m01, m00 - m01) == m00 - m01 }
  assert { tailMove(m01, m00 - m11) == m00 - m11 }
  assert { tailMove(m00 - m01, m01) == m01 }
  assert { tailMove(m00 - m01, m11) == m11 }

  assert { tailMove(m11, m00 - m01) == m00 - m11 }
  assert { tailMove(m11, m00 - m11) == m00 - m11 }

  println("sup")

  type Rope = Seq[Move]

  // this is the heart of the solution as well as the hardest part
  // need to pass original and resulting positions to be able to determine move for each segment
  def move(r: Rope, m: Move): Rope =
    r.tail.scanLeft(r.head, r.head + m) { case ((p, n), t) =>
      (t, t + tailMove(t - p, n - p))
    }.map(_._2)

  // special case for length 2 (head and tail)
  //    Seq(r.head + m, r.last + tailMove(r.last - r.head, m))

  // IDIOM results of applying a function to itself n times
  def moveN(p: Rope, m: Move, n: Int) =
    Iterator.iterate(p)(r => move(r, m)).slice(1, n + 1).toSeq

  def r(l: Int) = Seq.fill(l)(m00)

  type MoveN = (Move, Int)

  def lines2moves(lines: Iterable[String]): Iterable[MoveN] =
    lines.map { l => (toMove(l(0)), l.drop(2).toInt) }

  val result0 = lines2moves(example).scanLeft(Seq(r(2))) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten
  println(result0.map(_.last).toSet.size)

  // part 1

  val input = scala.io.Source.fromFile("AdventOfCodeDay9Input.txt").getLines.toSeq
  val result1 = lines2moves(input).scanLeft(Seq(r(2))) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten
  println(result1.map(_.last).toSet.size)

  // part 2

  val result2a = lines2moves(example).scanLeft(Seq(r(10))) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten
  println(result2a.last)
  println(result2a.map(_.last).toSet.size)

  val example2b = """R 5
                    |U 8
                    |L 8
                    |D 3
                    |R 17
                    |D 10
                    |L 25
                    |U 20
                    |""".stripMargin('|').split('\n').toIndexedSeq

  val result2b = lines2moves(example2b).scanLeft(Seq(r(10))) { case (ms, (d, n)) => moveN(ms.last, d, n) }
//  result2b.map(_.last.map(v => (v(0), v(1)))).foreach(println)
  println(result2b.flatten.map(_.last).toSet.size)

  val result2c = lines2moves(input).scanLeft(Seq(r(10))) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten
  println(result2c.map(_.last).toSet.size)
