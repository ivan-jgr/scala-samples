package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
   if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r-1) + pascal(c, r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def countParenthesis(chars: List[Char], open: Boolean): Boolean = {
      if (chars.isEmpty)
        open
      else if (chars.head == '(')
        countParenthesis(chars.tail, open)
      else if (chars.head == ')')
        countParenthesis(chars.tail, !open)
      else
        countParenthesis(chars.tail, open)
      }

      !countParenthesis(chars, open = false)
    }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def verifySum(sum: Int, coins: List[Int]): Int = {
      if (sum == 0)
        1
      else if (sum < 0 || coins.isEmpty)
        0
      else
        verifySum(sum - coins.head, coins) + verifySum(sum, coins.tail)
    }

    verifySum(money, coins)
  }
}
