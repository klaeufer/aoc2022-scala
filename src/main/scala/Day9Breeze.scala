import breeze.linalg.*
import breeze.numerics.*

object Day9Breeze:

  def main(args: Array[String]): Unit =

    val input = scala.io.Source.fromFile("AdventOfCodeDay9Input.txt").getLines.toSeq
    val moves = lines2moves(input)

    // part 1

    val ropeMoves1 = ropeMoves(createRope(2), moves)
    val visitedByTail1 = positionsVisitedByTail(ropeMoves1)
    println(s"Day 9 part 1: ${visitedByTail1.size}")

    // part 2

    val ropeMoves2 = ropeMoves(createRope(10), moves)
    val visitedByTail2 = positionsVisitedByTail(ropeMoves2)
    println(s"Day 9 part 2: ${visitedByTail2.size}")

  type Move = DenseVector[Int]
  type MoveN = (Move, Int)
  given CanEqual[DenseVector[Int], DenseVector[Int]] = CanEqual.derived

  val m00 = DenseVector.zeros[Int](2)
  val m01 = DenseVector(0, 1)
  val m10 = DenseVector(1, 0)
  val m11 = DenseVector(1, 1)

  type Rope = Seq[Move]
  def createRope(l: Int) = Seq.fill(l)(m00)

  def toMove(d: Char): Move = d match
    case 'L' => m00 - m01
    case 'R' => m01
    case 'U' => m00 - m10
    case 'D' => m10

  // h = (0, 0), t = position relative to h, m = hori/vert/diag
  def tailMove(t: Move, m: Move): Move =
    val diff = m - t
    if (diff dot diff) <= 2 then
      m00
    else
      convert(signum(diff), Int)

  // this is the heart of the solution as well as the hardest part:
  // need to pass original and resulting positions to be able to determine move for each segment
  // IDIOM
  def move(r: Rope, m: Move): Rope =
    r.tail.scanLeft(r.head, r.head + m) { case ((orig, updated), curr) =>
      val relativePos = curr - orig
      val headMove = updated - orig
      (curr, curr + tailMove(relativePos, headMove))
    }.map(_._2)

  // special case for length 2 (head and tail)
  //    Seq(r.head + m, r.last + tailMove(r.last - r.head, m))

  // IDIOM results of applying a function to itself n times
  def moveN(p: Rope, m: Move, n: Int) =
    Iterator.iterate(p)(r => move(r, m)).slice(1, n + 1).toSeq

  def lines2moves(lines: Iterable[String]): Iterable[MoveN] =
    lines.map { l => (toMove(l(0)), l.drop(2).toInt) }

  def ropeMoves(r: Rope, moves: Iterable[MoveN]): Iterable[Rope] =
    moves.scanLeft(Seq(r)) { case (ms, (d, n)) => moveN(ms.last, d, n) }.flatten

  def positionsVisitedByTail(moves: Iterable[Rope]): Set[Move] =
    moves.map(_.last).toSet
