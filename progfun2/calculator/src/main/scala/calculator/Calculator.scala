package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map{
      case (s, e) => s -> Signal(eval(e.apply(), namedExpressions - s))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(x) => x
      case Plus(x, y) => eval(x, references) + eval(y, references)
      case Minus(x, y) => eval(x, references) - eval(y, references)
      case Times(x, y) => eval(x, references) * eval(y, references)
      case Divide(x, y) => eval(x, references) / eval(y, references)
      case Ref(name) =>
        eval(getReferenceExpr(name, references), references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]):Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
