import scala.util.matching.Regex

object Day7 extends App {

  // part 1

  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val cd = "\\$ cd (.+)".r
  val ls = "\\$ ls".r
  val dir = "dir (.+)".r
  val file = "(\\d+) (.+)".r

  val inUse = input.foldLeft(0) { case (r, l) =>
    l match
      case file(k, _) => r + k.toInt
      case _ => r
  }

  println(f"disk space in use = $inUse")

  val (q, r) = input.foldLeft(Seq.empty[(String, Int)], Seq.empty[(String, Int)]) { case ((q, r), l) =>
    l match
      case cd("/") => (("/" -> 0) +: q, r)
      case cd("..") => ((q.tail.head._1 -> (q.tail.head._2 + q.head._2)) +: q.tail.tail, q.head +: r)
      case cd(d1) => ((d1 -> 0) +: q, r)
      case ls() => (q, r)
      case dir(_) => (q, r)
      case file(k, _) => ((q.head._1 -> (q.head._2 + k.toInt)) +: q.tail, r)
  }

  println(q)
  println(r)

  val r1 = q.foldLeft(q :+ ("|" -> -1), r) { case ((q, r), _) =>
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
