val input = scala.io.Source.fromFile("AdventOfCodeDay1Input.txt").getLines

Iterator.continually { 
  input.takeWhile { line => 
    line.trim.nonEmpty 
  } 
}.takeWhile { _ => 
  input.hasNext
}.map { group => 
  group.map { line => line.toInt }.sum
}.max
