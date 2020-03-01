val x = "hello world".toList

println(x)

val rawList: List[(Char, List[Char])] = x.groupBy((elem: Char) => elem).toList

rawList.foreach(x =>  println(x))

val occurrenceList = rawList.map((tup: (Char, List[Char])) => (tup._1, tup._2.length)).sorted
//val occurrenceList = rawList.map(tup => tup match {
//  case (c, cL) => (c, cL.length)
//})

//val emptyYield = for (i <- 1 until 1) yield i
val emptyYield = List(List())
for (x <- emptyYield) println("test")
println(emptyYield)

println(occurrenceList)

List(Nil) == List()

val y = List("hello", "world")
y.reduce((a, b) => a + b)
y.reduce((a, b) => a concat b)

(0 to 5).foreach(print)
(0 until 5).foreach(print)

val variations = for (
  occurrence <- occurrenceList.sorted;
  numOccurences <- (0 until occurrence._2)
) yield (occurrence._1, numOccurences)

println(variations)

def getSingleValSubsets(occurrence: (Char, Int)): List[(Char, Int)] = {
  val sequence = for (i <- 0 to occurrence._2) yield (occurrence._1, i)
  sequence.toList
}

println(getSingleValSubsets(('a', 5)))

val smallOccurrenceList = List(('a', 2), ('b', 2))

def getAllSubsets(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] = occurrences match {
  case List() => List(List())
  case x :: xs => {
    print("x: ");println(x)
    print("xs: ");println(xs)
    val tailSubsets = getAllSubsets(xs)
    val headSubsets = getSingleValSubsets(x)
    print("Head subsets: ");println(headSubsets)
    print("Tail subsets: ");println(tailSubsets)
    val iterable = for (
      headSubset <- headSubsets;
      tailSubsetList <- tailSubsets
    ) yield {
      if (headSubset._2 == 0) tailSubsetList else headSubset :: tailSubsetList
    }
    iterable
  }
}
getAllSubsets(smallOccurrenceList).foreach(println(_))
//getAllSubsets(occurrenceList).foreach(println(_))

def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = y match {
    // it follows from the preconditions that y will be empty before or
    // at the same time as x so the base case is when y is empty
    case List() => x
    case (yChar, yCount) :: ys => {
      print(yChar);print(", ");println(x.head._1)
      if (x.head._1 == yChar) (yChar, x.head._2 - yCount) :: subtract(x.tail, ys)

      else x.head :: subtract(x.tail, y)
    }
  }

subtract(smallOccurrenceList, List(('a', 1)))