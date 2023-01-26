import scala.collection.immutable.Queue

object Day8 extends App:

  val example = """30373
                  |25512
                  |65332
                  |33549
                  |35390""".stripMargin('|').split('\n').map(_.toList.toSeq)

  def transpose(arr: Seq[Seq[Char]]) = arr.map(_.toSeq).transpose

  println(example.toSeq)
  println(transpose(example))

  // TODO
  
  def visible(row: String) =
    row.zipWithIndex.foldLeft(Queue.empty[(Char, Int)]) { case (q, (t, i)) =>
      if q.isEmpty || q.last._1 < t then
        q :+ (t, i)
      else
        q
    }

  println(visible("12534176"))

//  def visible(row: String, pos: Int) =
//    row.toSeq.zipWithIndex.
