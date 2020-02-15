package week4

import java.util.NoSuchElementException

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet) = {
    ((left union right) union other) incl elem
  }

  override def toString = "{" + left + elem + right + "}"
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

//object test {
//  val x: List[String] = Nil
//  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
//}

trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

case class Number(n: Int) extends Expr {
  def isNumber = true
  def isSum = false
  def numValue = n
  def leftOp = throw new Error("Number.leftOp")
  def rightOp = throw new Error("Number.rightOp")
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  def isNumber = false
  def isSum = true
  def numValue = throw new Error("Sum.numvalue")
  def leftOp = e1
  def rightOp = e2
}
