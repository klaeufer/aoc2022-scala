val input = scala.io.Source.fromFile("AdventOfCodeDay3Input.txt").getLines.toList

def commonItem(s: String) =
  val (left, right) = s.splitAt(s.length / 2)
  left.toSet.intersect(right.toSet).head

def priority(c: Char) = 
  val p = c - 'a' + 1
  if p > 0 then p else c - 'A' + 27
  
input.map(commonItem).map(priority).sum

input.sliding(3, 3).map{ case List(u, v, w) => u.intersect(v.intersect(w)) }.map(_(0)).map(priority).sum
