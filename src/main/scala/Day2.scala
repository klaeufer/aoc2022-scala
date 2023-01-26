import scala.CanEqual

object Day2 extends App:

  // straightforward map/sum
  
  enum Move(val value: Int) derives CanEqual:
    case X extends Move(1)
    case Y extends Move(2)
    case Z extends Move(3)
    case A extends Move(1)
    case B extends Move(2)
    case C extends Move(3)

  val loss = 0
  val draw = 3
  val win = 6

  def play(a: Int, b: Int): Int = (a, b) match
    case (x, y) if x == y => y + draw
    case (1, 2) => 2 + win
    case (1, 3) => 3 + loss
    case (2, 1) => 1 + loss
    case (2, 3) => 3 + win
    case (3, 1) => 1 + win
    case (3, 2) => 2 + loss

  def move(a: Move, b: Move): Int = play(a.value, b.value)

  val input = scala.io.Source.fromFile("AdventOfCodeDay2Input.txt").getLines.toList

  val result1 = input.map(s => move(Move.valueOf(s.substring(0, 1).nn), Move.valueOf(s.substring(2, 3).nn))).sum

  println(result1)

  enum Result(val value: Int) derives CanEqual:
    case X extends Result(1)
    case Y extends Result(2)
    case Z extends Result(3)

  def chooseMove(theirs: Move, result: Result): Move = (theirs, result) match
    case (Move.A, Result.X) => Move.Z
    case (Move.B, Result.X) => Move.X
    case (Move.C, Result.X) => Move.Y
    case (Move.A, Result.Y) => Move.X
    case (Move.B, Result.Y) => Move.Y
    case (Move.C, Result.Y) => Move.Z
    case (Move.A, Result.Z) => Move.Y
    case (Move.B, Result.Z) => Move.Z
    case (Move.C, Result.Z) => Move.X

  val result2 = input
    .map(s => (Move.valueOf(s.substring(0, 1).nn), Result.valueOf(s.substring(2, 3).nn)))
    .map((m, r) => move(m, chooseMove(m, r)))
    .sum

  println(result2)
