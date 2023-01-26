val res0 = input.takeWhile(_.nonEmpty)

val stacks = Array.fill(numStacks)(collection.mutable.Stack.empty[Char])

for s <- res0.init.reverse do
  for i <- 0 until numStacks do
    val c = s(1 + 4 * i)
    if (c != ' ') stacks(i).push(s(1 + 4 * i))

val res6 = input.drop(res0.length + 1)

val res28 = res6.map(l => { val s = l.split(" ") ; (s(1).toInt, s(3).toInt, s(5).toInt) })

res28.foreach { case (n, f, t) =>
  for i <- 0 until n do
    val c = stacks(f - 1).pop
    stacks(t - 1).push(c)
    
    
for l <- res28 do
  val (n, f, t) = l
  val c = stacks(f - 1).take(n)
  stacks(f - 1).dropInPlace(n)
  stacks(t - 1).pushAll(c.reverse)
  
}

stacks.map(_.top).mkString
