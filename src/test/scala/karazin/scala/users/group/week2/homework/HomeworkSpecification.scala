package karazin.scala.users.group.week2.homework

import karazin.scala.users.group.week2.arbitraries
import karazin.scala.users.group.week2.arbitraries.restricted.{Integer, NegativeInteger, PositiveInteger, Zero}
import karazin.scala.users.group.week2.homework.Homework.*
import karazin.scala.users.group.week2.homework.arbitraries
import karazin.scala.users.group.week2.homework.utils.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}

import scala.language.implicitConversions
import scala.math.*

object HomeworkSpecification extends Properties("Homework"):

  import karazin.scala.users.group.week2.arbitraries.{given Arbitrary[Integer], given Arbitrary[NegativeInteger], given Arbitrary[PositiveInteger], given Arbitrary[Zero]}
  import karazin.scala.users.group.week2.homework.arbitraries.{given Arbitrary[Int], given Arbitrary[Rational]}

  property("throw exception due to zero denominator") = forAll { (numer: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, -abs(kindaDenom))
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Int, int: Int) ⇒
    val denom = abs(int) + 1
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == (left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == (left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    val res = -rational
    res.numer * rational.denom == -rational.numer * res.denom
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    val res = left + right
    val exceptedNumer = left.numer * right.denom + right.numer * left.denom
    val exceptedDenom = right.denom * left.denom
    val exceptedRes = Rational(exceptedNumer, exceptedDenom)
    res.numer * exceptedRes.denom == exceptedRes.numer * res.denom

  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    val res = left - right
    val exceptedNumer = left.numer * right.denom - right.numer * left.denom
    val exceptedDenom = right.denom * left.denom
    val exceptedRes = Rational(exceptedNumer, exceptedDenom)
    res.numer * exceptedRes.denom == exceptedRes.numer * res.denom
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    val res = left * right
    val exceptedNumer = left.numer * right.numer
    val exceptedDenom = right.denom * left.denom
    val exceptedRes = Rational(exceptedNumer, exceptedDenom)
    res.numer * exceptedRes.denom == exceptedRes.numer * res.denom
  }

  property("division for positive") = forAll { (left: Rational, numer: PositiveInteger, denom: PositiveInteger) =>
    val right = Rational(numer, denom)
    val res = left / right
    val exceptedNumer = (left.numer * denom) * Math.signum((numer * left.denom).toDouble).toInt
    val exceptedDenom = Math.abs(numer * left.denom)
    val exceptedRes = Rational(exceptedNumer, exceptedDenom)
    res.numer * exceptedRes.denom == exceptedRes.numer * res.denom
  }

  property("division for negative") = forAll { (left: Rational, numer: NegativeInteger, denom: PositiveInteger) =>
    val right = Rational(numer, denom)
    val res = left / right
    val exceptedNumer = (left.numer * denom) * Math.signum((numer * left.denom).toDouble).toInt
    val exceptedDenom = Math.abs(numer * left.denom)
    val exceptedRes = Rational(exceptedNumer, exceptedDenom)
    res.numer * exceptedRes.denom == exceptedRes.numer * res.denom
  }
  
  property("division by zero") = forAll { (left: Rational, z: Zero) =>
    throws(classOf[IllegalArgumentException]) {
      val res = left / Rational(z)
    }
  }

end HomeworkSpecification