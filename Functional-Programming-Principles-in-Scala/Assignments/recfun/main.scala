package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def leftBra(l: Int, chars: List[Char]): Boolean =
      if (l < 0) false
      else if (chars.isEmpty && l == 0) true
      else if (chars.isEmpty && l > 0) false
      else {
        if (chars.head == '(') leftBra(l + 1, chars.tail)
        else if (chars.head == ')') leftBra(l - 1, chars.tail)
        else leftBra(l, chars.tail)
      }
    leftBra(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, coins: List[Int]): Int =
      if (m == 0) 1
      else if (m < 0) 0
      else if (coins.isEmpty && m >= 1) 0
      else count(m, coins.tail) + count(m - coins.head, coins)
      
      count(money, coins)
  }
}
