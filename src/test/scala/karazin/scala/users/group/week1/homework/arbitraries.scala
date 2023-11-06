package karazin.scala.users.group.week1.homework

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck._

// Implement additional arbitraries if needed
object arbitraries:

  val smallInteger: Gen[Int] = Gen.choose[Int](min = 0, max = 10)

  val smallIntegerPair: Gen[(Int, Int)] = for {
    left  <- Gen.choose[Int](min = 0, max = 9)
    right <- Gen.choose[Int](min = left + 1, max = 10)
  } yield (left, right)

  given Arbitrary[Int] = Arbitrary(smallInteger)

  given Arbitrary[(Int, Int)] = Arbitrary(smallIntegerPair)

  
  
