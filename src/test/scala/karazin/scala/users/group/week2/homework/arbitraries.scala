package karazin.scala.users.group.week2.homework

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck._
import Homework._

object arbitraries:

  given Arbitrary[Int] = Arbitrary(Gen.choose[Int](min = -1000, max = 1000))

  given Arbitrary[Rational] =
    Arbitrary(
      for
        num ← Arbitrary.arbitrary[Int]
        denom ← Arbitrary.arbitrary[Int].suchThat(_ > 0)
      yield Rational(num, denom)
    )

  def validValuesForRational: Gen[(Int, Int)] = for {
    numer ← Arbitrary.arbitrary[Int]
    denom ← Arbitrary.arbitrary[Int].suchThat(_ > 0)
  } yield (numer, denom)


  given Arbitrary[(Int, Int)] = Arbitrary(validValuesForRational)
