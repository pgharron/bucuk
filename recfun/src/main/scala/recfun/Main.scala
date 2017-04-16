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
    **/ 
    def balance(chars: List[Char]): Boolean = {
       
	def doIt(rest: List[Char], on: Boolean, k: Int): Boolean = {
 
		if (rest.isEmpty) 
		  !on && k == 0		
		else {
			val h = rest.head 
			val (o, t) = if (h == '(' && !on) (true, k+1)
				else if (h == '(') (true, k+1)
				else if (h == ')' && on) (k-1 > 0, k-1)
                                else if (h == ')') (true, k+1)
				else (on, k)

			doIt(rest.tail, o, t)  	
		}
        }

	doIt(chars, chars.isEmpty, 0) 
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty)
        0
      else if (money == 0)
        1
      else
        countChange(money - coins.head, coins) +
          countChange(money, coins.tail)
    }
  }
