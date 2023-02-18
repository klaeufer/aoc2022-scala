object Day10:

  def main(args: Array[String]): Unit =

    val filename1 = "AdventOfCodeDay10Example.txt"
    val filename2 = "AdventOfCodeDay10Input.txt"

    val input = scala.io.Source.fromFile(filename2).getLines

    val addx = "addx (-?\\d+)".r

    val execution = Iterator
      .iterate((1, Option.empty[Int])) {
        case (x, None) =>
          input.next match
            case addx(v) => (x, Some(v.toInt))
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

    val cycles = Iterator.iterate(offset)(s => s + cycleLength).takeWhile(_ <= length)

    println(cycles.map(p => execution(p - 1)._1 * p).sum)

    // part 2

    val rowLength = 40

    val crt = execution.zipWithIndex.map { case ((x, _), p) =>
      if math.abs(x - (p % rowLength)) <= 1 then '#' else '.'
    }

    println(crt.grouped(rowLength).map(_.mkString).mkString("\n"))
