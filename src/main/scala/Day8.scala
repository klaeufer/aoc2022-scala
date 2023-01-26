import scala.collection.immutable.Queue

object Day8 extends App:

  val example = """30373
                  |25512
                  |65332
                  |33549
                  |35390""".stripMargin('|').split('\n')

  def transpose(arr: Seq[String]) = arr.map(_.toSeq).transpose.map(_.mkString)

  println(example.toSeq)
  println(transpose(example))

  // TODO

  def visibleInRow(row: String, indices: IterableOnce[Int]) =
    row.zip(indices).foldLeft(Queue.empty[(Char, Int)]) { case (q, (t, i)) =>
      if q.isEmpty || q.last._1 < t then
        q :+ (t, i)
      else
        q
    }.map(_._2).toSet

  def visibleFromLeft(row: String) = visibleInRow(row, row.indices)

  def visibleFromRight(row: String) = visibleInRow(row.reverse, row.indices.reverse)

  println(visibleFromLeft("12534176"))

  println(visibleFromRight("12534176"))

  def visible(input: Seq[String]) =
    val v1 = input.zipWithIndex.map((r, i) => visibleFromLeft(r).union(visibleFromRight(r)).map(j => (i, j))).reduce((u, v) => u.union(v))
    val v2 = transpose(input).zipWithIndex.map((r, i) => visibleFromLeft(r).union(visibleFromRight(r)).map(j => (j, i)))reduce((u, v) => u.union(v))
    v1.union(v2).size

  println(visible(example))

  val input = scala.io.Source.fromFile("AdventOfCodeDay8Input.txt").getLines.toSeq

  println(visible(input))

