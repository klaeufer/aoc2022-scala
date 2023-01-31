import org.scalatest.matchers.should
import org.scalatest.prop.*
import org.scalatest.propspec.AnyPropSpec

class Day9Test extends AnyPropSpec, TableDrivenPropertyChecks, should.Matchers:

  import Day9Breeze.*

  forAll(moves) { (p, m, t) =>
    tailMove(p, m) shouldEqual t
  }

  forAll(examples) { (input, ropeLength, numTailVisits) =>
    val moves = lines2moves(input)
    val rs = ropeMoves(createRope(ropeLength), moves)
    val vs = positionsVisitedByTail(rs)
    vs.size shouldEqual numTailVisits
  }

  def moves = Table(
    ("rel tail pos", "head move", "tail move"),
    (m00, m00, m00),
    (m00, m01, m00),
    (m00, m10, m00),
    (m00, m11, m00),
    (m01, m00, m00),
    (m01, m01, m00),
    (m01, m11, m00),
    (m01, m10, m00),
    (m01, m00 - m01, m00 - m01),
    (m01, m00 - m11, m00 - m11),
    (m00 - m01, m01, m01),
    (m00 - m01, m11, m11),
    (m11, m00 - m01, m00 - m11),
    (m11, m00 - m11, m00 - m11)
  )

  def example1 =
      """R 4
        |U 4
        |L 3
        |D 1
        |R 4
        |D 1
        |L 5
        |R 2
        |""".stripMargin('|').split('\n').toIndexedSeq

  def example2 =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20
      |""".stripMargin('|').split('\n').toIndexedSeq

  def examples = Table(
    ("moves", "rope length", "number of tail visits"),
    (example1, 2, 13),
    (example1, 10, 1),
    (example2, 10, 36),
  )
