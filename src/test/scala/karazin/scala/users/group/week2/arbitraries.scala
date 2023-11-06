package karazin.scala.users.group.week2

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck._
import scala.language.implicitConversions

import Topic._

object arbitraries:

  object restricted:
    opaque type Integerable                     = Int
    opaque type Zero <: Integerable             = Int
    opaque type PositiveInteger <: Integerable  = Int
    opaque type NegativeInteger <: Integerable  = Int
    type Integer                                = PositiveInteger | Zero | NegativeInteger

    object Zero:
      def apply(v: Int): Zero = 
        require(v == 0, s"Cannot build Zero from [$v]"); v

    object PositiveInteger:
      def apply(v: Int): PositiveInteger = 
        require(v > 0, s"Cannot build positive integer from [$v]"); v

    object NegativeInteger:
      def apply(v: Int): NegativeInteger = 
        require(v < 0, s"Cannot build negative integer from [$v]"); v

    object Integer:
      def apply(v: PositiveInteger | Zero | NegativeInteger): Integer = v

    implicit val integerableToInt: Conversion[Integerable, Int] = _.toInt 

  import restricted._

  val zeroGen: Gen[Zero] = Gen.const(Zero(0))
  val positiveIntegerGen: Gen[PositiveInteger] = Gen.choose(min = 1, max = 1000) map { PositiveInteger(_) }
  val negativeIntegerGen: Gen[NegativeInteger] = Gen.choose(min = -1000, max = -1) map { NegativeInteger(_) }
  val integerGen: Gen[Integer] = Gen.frequency((2, negativeIntegerGen), (1, zeroGen), (2, positiveIntegerGen))

  given Arbitrary[Zero] = Arbitrary(zeroGen)
  given Arbitrary[PositiveInteger] = Arbitrary(positiveIntegerGen)
  given Arbitrary[NegativeInteger] = Arbitrary(negativeIntegerGen)
  given Arbitrary[Integer] = Arbitrary(integerGen)

  given Arbitrary[Rational] =
    Arbitrary(
      for {
        num   ← Arbitrary.arbitrary[Integer]
        denom ← Arbitrary.arbitrary[PositiveInteger]
      } yield Rational(num, denom)
    )