object Day7 extends App {

  // part 1

  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  // regex patterns for matching input lines
  val cd = "\\$ cd (.+)".r
  val ls = "\\$ ls".r
  val dir = "dir (.+)".r
  val file = "(\\d+) (.+)".r

  // as a warmup, determine total sum of file sizes
  val inUse = input.foldLeft(0) { case (r, l) =>
    l match
      case file(k, _) => r + k.toInt
      case _ => r
  }

  println(f"disk space in use = $inUse")

  // q: current dir stack
  // r: processed dirs with their total subtree sizes
  val (q, r) = input.foldLeft(Seq.empty[(String, Int)], Seq.empty[(String, Int)]) { case ((q, r), l) =>
    l match
      case cd("..") => ((q.tail.head._1 -> (q.tail.head._2 + q.head._2)) +: q.tail.tail, q.head +: r)
      case cd(d1) => ((d1 -> 0) +: q, r)
      case file(k, _) => ((q.head._1 -> (q.head._2 + k.toInt)) +: q.tail, r)
      case _ => (q, r)
  }

  println(q)
  println(r)

  // process directories left in dir stack (equivalent of successive cd .. back to /)
  val r1 = q.foldLeft(q :+ (".DUMMY" -> -1), r) { case ((q, r), _) =>
    ((q.tail.head._1 -> (q.tail.head._2 + q.head._2)) +: q.tail.tail, q.head +: r)
  }

  println(r1._2)

  println(r1._2.map(_._2).filter(_ <= 100000).sum)

  // part 2

  val total = 70000000
  val need = 30000000
  val free = total - inUse
  println(f"free space = $free")
  val toDelete = need - free
  println(f"need to delete at least $toDelete")

  println(r1._2.filter(_._2 >= toDelete).minBy(_._2))
}
