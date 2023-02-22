object Day10:

  def main(args: Array[String]): Unit =

    val input = scala.io.Source.fromFile("AdventOfCodeDay10Input.txt").getLines

    val execution = Iterator
      .iterate((1, Option.empty[Int])) {
        case (x, None) =>
          input.next match
            case s"addx $v" => (x, Some(v.toInt))
            case "noop" => (x, None)
        case (x, Some(v)) =>
          (x + v, None)
      }
      .takeWhile(_ => input.hasNext)
      .toIndexedSeq

    // part 1

    val offset = 20
    val cycleLength = 40
    val length = execution.length
    val cycles = Iterator.range(offset, length, cycleLength)

    println(cycles.map(p => execution(p - 1)._1 * p).sum)

    // part 2

    val rowLength = 40
    val crt = execution.zipWithIndex.map { case ((x, _), p) =>
      ".#"((math.abs(x - (p % rowLength)) <= 1).compare(false).sign)
    }

    println(crt.grouped(rowLength).map(_.mkString).mkString("\n"))
