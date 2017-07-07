package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("polynomial") {
    val result = Polynomial.computeDelta(Signal(2), Signal(5), Signal(-3))
    assert(result.apply() == 49D)
  }

  test("computeValues") {
    val expr = Signal[Expr](Plus(Literal(5), Literal(2)))
    val m = Map[String, Signal[Expr]]("a" -> expr)

    val result = Calculator.computeValues(m)
    assert(result.mapValues(_.apply) == Map(("a", 7)))
  }

  test("computeValues with Ref") {
    val a = Signal[Expr](Plus(Literal(5), Literal(2)))
    val b = Signal[Expr](Times(Literal(2), Ref("a")))

    val m = Map[String, Signal[Expr]]("a" -> a, "b" -> b)

    val result = Calculator.computeValues(m)
    assert(result.mapValues(_.apply) == Map(("a", 7), ("b", 14)))
  }

  test("computeValues with Cyclic") {
    val a = Signal[Expr](Plus(Ref("b"), Literal(1)))
    val b = Signal[Expr](Times(Literal(2), Ref("a")))

    val m = Map[String, Signal[Expr]]("a" -> a, "b" -> b)

    val result = Calculator.computeValues(m)
    assert(result.keySet === Set("a", "b"))
    assert(result.values.forall(s => java.lang.Double.isNaN(s.apply())))
  }

  test("solutions") {
    val delta = Polynomial.computeDelta(Signal(2), Signal(5), Signal(-3))
    val result = Polynomial.computeSolutions(Signal(2), Signal(5), Signal(-3), delta)
    assert(result.apply() == Set(0.5, -3.0))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(14))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

}
