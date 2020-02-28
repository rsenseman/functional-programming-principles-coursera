//val s = "Hello World"
//s filter (_.isUpper)
//s exists (_.isUpper)
//
//val z = List(1, 2, 3) zip List('a', 'b', 'c')
//val uz = z.unzip
//
//def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
//
//val n: Int = 7
//val allPairs = (1 until n) map (i =>
//  (1 until i) map (j => (i, j)))
//
//allPairs.flatten
//
//(1 until n) flatMap (i =>
//  (1 until i) map (j => (i, j))) filter (pair =>
//  isPrime(pair._1 + pair._2)
//  )
//
//for {
//  i <- 1 until n
//  j <- 1 until n
//  if isPrime(i + j)
//} yield (i, j)
//
//def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
////  val productList = for {
////    i <- xs
////    j <- ys
////  } yield i * j
//  val productList = for ((x, y) <- xs zip ys) yield x*y
//productList.sum
//}

class Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0

//  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

  def + (other: Poly) = {
    new Poly((other.terms foldLeft terms)(addTerm))
  }
  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (
      for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield coeff + "x^" + exp
    ) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2



























