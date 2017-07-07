package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b.apply(), 2) - (4 * a.apply() * c.apply()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val topPos = -b.apply() + Math.sqrt(delta.apply())
    val topNeg = -b.apply() - Math.sqrt(delta.apply())
    val bottom = 2 * a.apply()

    Signal(Set(topPos / bottom, topNeg / bottom))
  }
}
