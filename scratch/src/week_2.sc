def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1 else f(a) * product(f)(a + 1, b)
}

def product_expanded(f: Int => Int): (Int, Int) => Int  = {
  def product_inner(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }
  product_inner
}

product(x => x + 2)(2, 5)
product_expanded(x => x)(2, 5)


def factorial(x:Int) = product(x => x)(1, x)

factorial(5)


class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b)
  private val g = gcd(x, y)

  def numer = x
  def denom = y

  def neg = new Rational(-numer, denom)

  def add(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def less(that: Rational) = this.sub(that).denom < 0

  def max(that: Rational) = if(this.less(that)) that else this

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational): Rational = {
    new Rational(
      this.numer * that.numer,
      this.denom * that.denom
    )
  }

  override def toString = numer / g + "/" + denom / g
}


//val x = new Rational(1,2)
//x.numer
//x.denom
//
//val y = new Rational(2,3)
//x.add(y)
//
//x.neg()

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.add(y.neg).mul(z)

x.sub(z)
z.sub(x)

x.less(z)
z.less(x)

x.max(y)