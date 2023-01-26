object Day6 extends App:

  // easy when using sliding and zipWithIndex

  val input = scala.io.Source.fromFile("AdventOfCodeDay6Input.txt").mkString

  def f(input: String) = input.sliding(4).zipWithIndex.takeWhile((s, i) => s.toSet.size < 4).toSeq.last._2 + 5

  def g(input: String) = input.sliding(14).zipWithIndex.takeWhile((s, i) => s.toSet.size < 14).toSeq.last._2 + 15

  println(f(input))

  println(g(input))
