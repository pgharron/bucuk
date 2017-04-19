val l = List[(Char,Int)](('a', 1), ('b',0), ('a',1), ('c',1))
val l2 = List[Char]('a', 'b', 'a')


l2.groupBy(c=>c).map{
  case (k, v) => (k, v.size)
}.toList

//l.map(l => l match {
//  case (c:Char, n) => (c, n+1)
//  case o => println(s"its a $o")
//})