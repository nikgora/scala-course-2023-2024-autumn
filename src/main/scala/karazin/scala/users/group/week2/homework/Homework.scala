package karazin.scala.users.group.week2.homework

import scala.annotation.{tailrec, targetName}
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    private lazy val g = gcd(abs(x), y)
    val numer = x / g
    val denom = y / g

    def this(x: Int) = this(x, 1)

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("addition")
    infix def +(that: Rational): Rational =
      Rational(this.numer * that.denom + that.numer * this.denom, this.denom * that.denom)

    @targetName("negation")
    infix def unary_- : Rational = Rational(-this.numer, this.denom)

    @targetName("substraction")
    infix def -(that: Rational): Rational =
      Rational(this.numer * that.denom - that.numer * this.denom, this.denom * that.denom)

    @targetName("multiplication")
    infix def *(that: Rational): Rational = Rational(this.numer * that.numer, this.denom * that.denom)

    @targetName("devision")
    infix def /(that: Rational): Rational =
      require(that.numer != 0, "division by zero")
      Rational(this.numer * that.denom * Math.signum((this.denom * that.numer).toDouble).toInt, Math.abs(this.denom * that.numer))

    override def toString: String = s"${this.numer}/${this.denom}"

    override def equals(other: Any): Boolean = other match
      case that: Rational =>
        that.canEqual(this) &&
          g == that.g &&
          numer == that.numer &&
          denom == that.denom
      case _ => false

    private def canEqual(other: Any): Boolean = other.isInstanceOf[Rational]

    override def hashCode(): Int =
      val state = Seq(g, numer, denom)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    @tailrec
    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

  end Rational

end Homework