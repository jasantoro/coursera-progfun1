package org.coursera.week3

/**
  * Created by jorge on 06/06/16.
  */
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def neg: Rational = new Rational(-this.numer, this.denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = {
    this.numer + "/" + this.denom
  }
}
