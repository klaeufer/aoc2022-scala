object Day3 extends App:

  // relatively straightforward map/sum with a bit of string manipulation

  val input = scala.io.Source.fromFile("data/AdventOfCodeDay3Input.txt").getLines.toSeq

  def commonItem(s: String) =
    val (left, right) = s.splitAt(s.length / 2)
    left.toSet.intersect(right.toSet).head

  def priority(c: Char) =
    val p = c - 'a' + 1
    if p > 0 then p else c - 'A' + 27

  val result1 = input.map(commonItem).map(priority).sum
  println(result1)

  val result2 = input.sliding(3, 3).map{ case Seq(u, v, w) => u.intersect(v.intersect(w)) }.map(_(0)).map(priority).sum
  println(result2)
