val l = List(('a', 2), ('b', 2))

val m = l.toMap


val k = List[(Char,Int)](('b', 2), ('a',1))

k.foldLeft(m){(m, p) =>
  val (c, i) = p
  if (m.contains(c)) {
    val v = m.apply(c)
    val g = if (v - i <= 0) {
      m - c
    } else m.updated(c, v - i)
    g
  } else m
}

val ff = List(List(('y',1)), List(('v',1)), List(('u',1)))
ff drop 2