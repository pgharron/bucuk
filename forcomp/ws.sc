
val l = List(('a', 2), ('b', 2))

val y = {
  for {
    (c, i) <- l
    j <- (i to 1 by -1)
  } yield {
    (c, j)
  }
}.sorted


val g: List[Seq[(Char, Int)]] = List() :: {
  for {
    c <- 1 to y.size
    pair <- y take c
    group <- for {
      n <- y drop c
      if pair._1 < n._1
    } yield {
      Seq(pair, n)
    }
  } yield group
}.toSet.toList


y.foldLeft(g)((a: List[Seq[(Char, Int)]], n) => List(n) :: a)
