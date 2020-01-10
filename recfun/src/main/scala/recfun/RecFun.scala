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
      if (numOpen < 0) return false
      else if (chars.isEmpty) numOpen==0
      else {
        val firstChar = chars.head

        if (firstChar == '(') balanceIter(chars.tail, numOpen+1)
        else if (firstChar == ')') balanceIter(chars.tail, numOpen-1)
        else balanceIter(chars.tail, numOpen)
      }
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val standardCoinMap = Map(coins.map((a) => (a -> 0)): _*)

//    println(standardCoinMap)

    def mergeMaps(aMap: Map[Int, Int], bMap: Map[Int, Int]): Map[Int,Int] = {
      var newMap: Map[Int, Int] = Map.empty[Int, Int]
      for (key <- aMap.keys) {
        newMap(key) = aMap.getOrElse(key, 0) + bMap.getOrElse(key, 0)
      }
      newMap
    }

    def mergeMapLists(aMaps: List[Map[Int,Int]], bMaps: List[Map[Int,Int]]): ListBuffer[Map[Int,Int]] = {
      var fullMaps: ListBuffer[Map[Int,Int]] = ListBuffer.empty[Map[Int, Int]]
      for (aMap <- aMaps) {
        for (bMap <- bMaps) {
          var newMap = mergeMaps(aMap, bMap)
          fullMaps += newMap
        }
      }
      fullMaps
    }

    def getChangePatterns(money: Int, coins: List[Int]): ListBuffer[Map[Int, Int]] = {
      var newMaps: ListBuffer[Map[Int, Int]] = ListBuffer.empty[Map[Int, Int]]

      if (money > 0) {
        for (coin <- coins) {
          var subMaps: ListBuffer[Map[Int, Int]] = ListBuffer.empty[Map[Int, Int]]
          var newMap: Map[Int, Int] = standardCoinMap.clone()

          if (coin < money) {
            newMap(coin) += 1
            var subMapList = getChangePatterns(money - coin, coins)
            for (subMap <- subMapList) {
              newMaps.append(mergeMaps(newMap, subMap))
            }
          } else if (coin == money) {
            newMap(coin) += 1
            newMaps.append(newMap)
          }
        }
      }
      newMaps
    }

    val allPatterns = getChangePatterns(money, coins)
    val allPatternsSet = allPatterns.map(_.toSet).toSet
    println(allPatternsSet)
    allPatternsSet.size
  }
}


//