package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      (c, r) match {
        case (0, 0) => 1
        case (_, 1) => 1
        case (0, _) => 1
        case _ if c == r => 1
        case _ => pascal(c -1 ,r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def search(rest: List[Char], lookingForPartner: Boolean, matched: Boolean): Boolean = {
        rest match {
          case Nil =>
            matched && !lookingForPartner

          case h :: t if !lookingForPartner =>
              search(t, h == '(', matched)

          case h :: t =>
            val m = h ==')'
            search(t, if (m) false else true, m)
        }
      }

      chars match {
        case Nil => false
        case _ => search(chars, false, false)
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
