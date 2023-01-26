object Day4 extends App:

  // straightforward map/count

  val input = scala.io.Source.fromFile("AdventOfCodeDay4Input.txt").getLines.toSeq

  val result1 = input
    .map(_.split(Array('-', ',')))
    .count { case Array(l1, u1, l2, u2) => l1.toInt <= l2.toInt && u2.toInt <= u1.toInt || l2.toInt <= l1.toInt && u1.toInt <= u2.toInt }

  println(result1)

  val result2 = input.length - input
    .map(_.split(Array('-', ',')))
    .count { case Array(l1, u1, l2, u2) => !(u1.toInt >= l2.toInt && u2.toInt >= l1.toInt) }

  println(result2)
