package week3

class Rational(x: Int, y: Int) {
  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b)

  private val g = gcd(x, y)

  def numer: Int = x
  def denom: Int = y

  def neg = new Rational(-numer, denom)

  def + (that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def < (that: Rational) = this.sub(that).denom < 0

  def max(that: Rational) = if(this < that) that else this

  def sub(that: Rational) = this + that.neg

  def * (that: Rational): Rational = {
    new Rational(
      this.numer * that.numer,
      this.denom * that.denom
    )
  }

  override def toString = numer / g + "/" + denom / g
}


object Hello {
  def main(args: Array[String]) = println("Hello World!")

}
