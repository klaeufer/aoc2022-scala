import scala.collection.immutable.Queue

object Day8 extends App:

  val example = """30373
                  |25512
                  |65332
                  |33549
                  |35390""".stripMargin('|').split('\n').toIndexedSeq

  def transpose(arr: Seq[String]) = arr.map(_.toSeq).transpose.map(_.mkString)

  println(example)
  println(transpose(example))

  // TODO discuss which of these is more comprehensible

  def monotonicSubsequenceWithIndicesLinear[A](it: Iterable[A], indices: IterableOnce[Int] = Iterator.from(0))(implicit ord: Ordering[A]): Seq[(A, Int)] =
    it.zip(indices).foldLeft(Queue.empty[(A, Int)]) { case (q, (t, i)) =>
      if q.isEmpty || ord.lt(q.last._1,  t) then
        q :+ (t, i)
      else
        q
    }

  def monotonicSubsequenceWithIndices[A](it: Iterable[A], indices: IterableOnce[Int] = Iterator.from(0))(implicit ord: Ordering[A]): Seq[(A, Int)] =
    it.zip(indices).inits.toSeq.reverse.tail.map(_.max).distinctBy(_._1)

  val s = "12534176"
  println(monotonicSubsequenceWithIndices(s))

  def visibleFromLeft(row: String) = monotonicSubsequenceWithIndices(row, row.indices).map(_._2)

  def visibleFromRight(row: String) = monotonicSubsequenceWithIndices(row.reverse, row.indices.reverse).map(_._2)

  println(visibleFromLeft(s))

  println(visibleFromRight(s))

  for r <- example do
    println(s"$r: ${visibleFromLeft(r)} ${visibleFromRight(r)}")

  println(visibleFromLeft("30373"))
  println(visibleFromRight("30373"))
  println(visibleFromLeft("30373".reverse))
  println(visibleFromRight("30373".reverse))

  def visibleFromOutside(grid: Seq[String]) =
    val v1 = grid.zipWithIndex.map((r, i) => visibleFromLeft(r).toSet.union(visibleFromRight(r).toSet).map(j => (i, j))).reduce(_.union(_))
    val v2 = transpose(grid).zipWithIndex.map((r, i) => visibleFromLeft(r).toSet.union(visibleFromRight(r).toSet).map(j => (j, i))).reduce(_.union(_))
    v1.union(v2)

  println(visibleFromOutside(example).toSeq.sorted)

  val input = scala.io.Source.fromFile("AdventOfCodeDay8Input.txt").getLines.toSeq

  println(visibleFromOutside(input).size)

  // TODO quite inefficient - should be able to use memoization
  // separately for the viewing distance in each direction?

  def viewingDistance(t: Char, row: String) =
    row.zipWithIndex.find((c, i) => c >= t).map(_._2 + 1).getOrElse(row.length)

  def scenicScore(grid: Seq[String], gridT: Seq[String], i: Int, j: Int) =
    val t = grid(i)(j)
    viewingDistance(t, grid(i).take(j).reverse)
    * viewingDistance(t, grid(i).takeRight(grid(i).length - 1 - j))
    * viewingDistance(t, gridT(j).take(i).reverse)
    * viewingDistance(t, gridT(j).takeRight(grid(j).length - 1 - i))

  val exampleT = transpose(example)

  println(scenicScore(example, exampleT, 1, 2))
  println(scenicScore(example, exampleT, 3, 2))

  val inputT = transpose(input)
  val positions = for i <- 0 until input.length ; j <- 0 until input(0).length yield (i, j)
  val result2 = positions.map((i, j) => scenicScore(input, inputT, i, j)).max
  println(result2)
