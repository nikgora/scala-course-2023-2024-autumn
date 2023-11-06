package karazin.scala.users.group.week1

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Topic._

object arbitraries:

  val smallInteger: Gen[Int] = Gen.choose[Int](min = 0, max = 10)

  val smallIntegerPair: Gen[(Int, Int)] = for {
    left <- Gen.choose(min = 0, max = 9)
    right <- (Gen.choose(min = 1, max = 10) suchThat (_ > left))
  } yield (left, right)
  
  val higherOrderFunction: Gen[BigInt => BigInt] =
    Gen.oneOf(
      `Higher-Order Functions`.id,
      `Higher-Order Functions`.cube,
      `Higher-Order Functions`.factorial
    )  

  given Arbitrary[Int] = Arbitrary(smallInteger)

  given Arbitrary[(Int, Int)] = Arbitrary(smallIntegerPair)

  given Arbitrary[BigInt => BigInt] = Arbitrary(higherOrderFunction)