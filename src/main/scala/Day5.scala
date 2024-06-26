object Day5 extends App:

  // mutable stacks

  val input = scala.io.Source.fromFile("data/AdventOfCodeDay5Input.txt").getLines.toSeq

  val stackInput = input.takeWhile(_.nonEmpty)

  val numStacks = (stackInput.last.length + 1) / 4
  val stacks = Array.fill(numStacks)(collection.mutable.Stack.empty[Char])

  def initStacks() =
    stacks.foreach(_.clear())
    for s <- stackInput.init.reverse do
      for i <- 0 until stacks.length do
        val c = s(1 + 4 * i)
        if (c != ' ') stacks(i).push(s(1 + 4 * i))

  val movesInput = input.drop(stackInput.length + 1)

  val parsedMoves = movesInput.map(l => { val s = l.split(' ') ; (s(1).toInt, s(3).toInt, s(5).toInt) })

  // part 1

  initStacks()

  for (n, f, t) <- parsedMoves do
    for _ <- 0 until n do
      val c = stacks(f - 1).pop
      stacks(t - 1).push(c)

  println(stacks.map(_.top).mkString)

  // part 2

  initStacks()

  for (n, f, t) <- parsedMoves do
    val c = stacks(f - 1).take(n)
    stacks(f - 1).dropInPlace(n)
    stacks(t - 1).pushAll(c.reverse)

  println(stacks.map(_.top).mkString)
