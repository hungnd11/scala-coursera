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
      // values at sides are equal to 0
      // values inside are the sum of 2 above elements
      if (c == 0 || c == r)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      // if we get an open parenthesis, we add it to a stack
      // then if we encounter a close parenthesis, we pop the opened one out from the stack
      // openedParentheses keeps track of the number of opened parentheses in stack
      @tailrec
      def balancing(chars: List[Char], openedParentheses: Int): Boolean = {
        if(chars.isEmpty)
          openedParentheses == 0
        else if (chars.head == '(')
          balancing(chars.tail, openedParentheses + 1)
        else if (chars.head == ')')
          (openedParentheses > 0) && balancing(chars.tail, openedParentheses - 1)
        else
          balancing(chars.tail, openedParentheses)
      }

      balancing(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0)
        1
      else if(money > 0 && coins.nonEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else
        0
    }
  }
