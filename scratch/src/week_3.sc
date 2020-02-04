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

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
t2 incl -1


def getValue[T](inList: List[T], index: Int): T = {
  if (index < 0 || inList.isEmpty) throw new IndexOutOfBoundsException("index out of range or empty list")
  else if (index==0) inList.head
  else getValue(inList.tail, index-1)
}
