object Day1 extends App:

  val input = scala.io.Source.fromFile("AdventOfCodeDay1Input.txt").getLines

  // TODO continually/takeWhile combination refers to source iterator twice - is there a more straightforward way? 
  
  val result = Iterator.continually {
    input.takeWhile { line =>
      line.nonEmpty
    }
  }.takeWhile { _ =>
    input.hasNext
  }.map { group =>
    group.map { line => line.toInt }.sum
  }.toSeq.sorted

  println(result.last)

  println(result.takeRight(3).sum)
