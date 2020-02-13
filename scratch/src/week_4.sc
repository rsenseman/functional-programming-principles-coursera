//import week4.{Expr, Number, Sum}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("No natural number preceeds Zero")
  def successor = new Succ(this)
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("No negative natural numbers")
}

//def show(e: Expr): String = e match {
//    case Number(x) => x.toString()
//    case Sum(l, r) => show(l) + " + " + show(r)
//  }
//val a = new Number(5)
//println(show(a))

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => {
    if (x < y) x :: xs
    else y :: insert(x, ys)
  }
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

val l = List(7, 9, 5, 2)
println(isort(l))