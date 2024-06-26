object Day1 extends App:

  /** Partitions an iterator into chunks of consecutive elements for which the predicate holds. */
  extension [A](it: Iterator[A])
    def splitWhere(p: A => Boolean): Iterator[Iterator[A]] = Iterator
      .continually(it.takeWhile(!p(_)))
      .takeWhile(_ => it.hasNext)

  val input = scala.io.Source.fromFile("data/AdventOfCodeDay1Input.txt").getLines

  // iterate over inventories of consecutive nonempty lines
  // this stores only one Int per elf in memory
  val result = input
    .splitWhere(_.isEmpty)
    .map(_.map(_.toInt).sum)
    .foldLeft(Seq.empty[Int])(_.prepended(_).sorted.takeRight(3))

  println(s"Day 1 part 1: ${result.last}")
  println(s"Day 1 part 2: ${result.takeRight(3).sum}")

  println(Iterator(5, 2, 4, 3, 8, 10, 7, 7, 7, 12, 14, 20, 3).splitWhere(_ % 2 != 0).map(_.length).toSeq)
