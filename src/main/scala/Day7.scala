object Day7 extends App:

  // immutable file system traversal based on directory stack and result list

  // part 1

  val input = scala.io.Source.fromFile("data/AdventOfCodeDay7Input.txt").getLines.toSeq

  // regex patterns for matching input lines
  val cd = "\\$ cd (.+)".r
  val file = "(\\d+) (.+)".r

  // as a warmup, determine total sum of file sizes
  val inUse = input.foldLeft(0) {
    case (r, file(k, _)) => r + k.toInt
    case (r, _) => r
  }

  println(f"disk space in use = $inUse")

  // q: current dir stack
  // r: processed dirs with their total subtree sizes
  val (q, r) = input.foldLeft(Seq.empty[(String, Int)], Seq.empty[(String, Int)]) {
    case ((q, r), cd("..")) => ((q.tail.head._1 -> (q.tail.head._2 + q.head._2)) +: q.tail.tail, q.head +: r)
    case ((q, r), cd(d1)) => ((d1 -> 0) +: q, r)
    case ((q, r), file(k, _)) => ((q.head._1 -> (q.head._2 + k.toInt)) +: q.tail, r)
    case (s, _) => s
  }

  println(q)
  println(r)

  // process directories still left in dir stack (equivalent of successive cd .. back to /)
  val r1 = q.foldLeft(q :+ (".DUMMY" -> -1), r) { case ((q, r), _) =>
    ((q.tail.head._1 -> (q.tail.head._2 + q.head._2)) +: q.tail.tail, q.head +: r)
  }

  println(r1._2)

  println(r1._2.map(_._2).filter(_ <= 100_000).sum)

  // part 2

  val total = 70_000_000
  val need = 30_000_000
  val free = total - inUse
  println(f"free space = $free")
  val toDelete = need - free
  println(f"need to delete at least $toDelete")

  println(r1._2.filter(_._2 >= toDelete).minBy(_._2))
