def removeAt[T](n: Int, xs: List[T]): List[T] = {
//  if (n == 0) xs.tail
//  else xs.head :: removeAt(n-1, xs.tail)

  (xs take n) ::: (xs drop n+1)
}

removeAt(1, List('a', 'b', 'c', 'd'))

def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
  case (Nil, ys) => ys
  case (xs, Nil) => xs
  case (x :: xs, y :: ys) => {
    if (x < y) x :: merge(xs, right)
    else y :: merge(left, ys)
  }
}

def mergeSort(inList: List[Int]): List[Int] = {
  val n = inList.length/2
  if (n==0) inList
  else {
    val (left, right) = inList splitAt n
    merge(mergeSort(left), mergeSort(right))
  }
}

println(mergeSort(List(7, 2, -3, 9, 5)))

//abstract class List[T] {
//  def map[U](f: T => U): List[U] = this match {
//    case Nil => this
//    case x :: xs => f(x) :: xs.map(f)
//  }
//
//  def filter(p: T => Boolean): List[T] = {
//    // p for predicate
//    case Nil => this
//    case x :: xs => if (p(x)) x::xs.filter(p) else xs.filter(p)
//  }
//}
//
//def scaleList(xs: List[Double], factor: Double) = xs map (x => x * factor)
//def squareList(xs: List[Double]) = xs map (x => x * x)