object Day6 extends App:

  // easy when using sliding and zipWithIndex

  val input = scala.io.Source.fromFile("data/AdventOfCodeDay6Input.txt").mkString

  def f(input: String) = input.sliding(4).indexWhere(_.toSet.size == 4) + 4

  def g(input: String) = input.sliding(14).indexWhere(_.toSet.size == 14) + 14

  println(f(input))

  println(g(input))
