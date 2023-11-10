object Day4 extends App:

  // straightforward map/count

  val input = scala.io.Source.fromFile("data/AdventOfCodeDay4Input.txt").getLines.toSeq

  val result1 = input
    .map(_.split(Array('-', ',')).map(_.toInt))
    .count { case Array(l1, u1, l2, u2) => l1 <= l2 && u2 <= u1 || l2 <= l1 && u1 <= u2 }

  println(result1)

  val result2 = input.length - input
    .map(_.split(Array('-', ',')).map(_.toInt))
    .count { case Array(l1, u1, l2, u2) => !(u1 >= l2 && u2 >= l1) }

  println(result2)
