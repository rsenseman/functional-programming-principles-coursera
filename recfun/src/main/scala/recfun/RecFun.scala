package recfun

import scala.collection.mutable.{ListBuffer, Map}

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(s"${pascal(col, row)} ")
//      println()
//    }

//    println(balance("test(test)".toList))

    println(countChange(10, List(25,10,5,1)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r || c < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], numOpen:Int): Boolean = {
      if (numOpen < 0)
        false
      else if (chars.isEmpty)
        numOpen==0
      else {
        val firstChar = chars.head

        if (firstChar == '(')
          balanceIter(chars.tail, numOpen+1)
        else if (firstChar == ')')
          balanceIter(chars.tail, numOpen-1)
        else
          balanceIter(chars.tail, numOpen)
      }
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money==0)
      1
    else if (coins.isEmpty || money < 0)
      0
    else
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
}


//