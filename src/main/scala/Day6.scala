val input = scala.io.Source.fromFile("AdventOfCodeDay6Input.txt").mkString

def f(input: String) = input.sliding(4).zipWithIndex.takeWhile((s, i) => s.toSet.size < 4).toList.last._2 + 5

def g(input: String) = input.sliding(14).zipWithIndex.takeWhile((s, i) => s.toSet.size < 14).toList.last._2 + 15
