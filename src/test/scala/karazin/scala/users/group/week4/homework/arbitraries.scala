package karazin.scala.users.group.week4.homework

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck._
import Homework._

object arbitraries:

  val smallInteger: Gen[Int] = Gen.choose[Int](min = -1000, max = 1000)

  def nonEmptySortedList: Gen[List[Int]] = for {
      n     ← Gen.choose[Int](min = 1, max = 128)
      list  ← (Gen.listOfN[Int](n, smallInteger) map { _.distinct.sorted[Int] })
    } yield list  
  
  val empty: Gen[Empty] = Gen.const(Empty)
  
  def nonEmpty(list: List[Int]): Gen[NonEmpty] = 
    for {
      n                             ← Gen.choose[Int](min = 1, max = list.size)
      (leftList, elem :: rightList) = list.splitAt(n - 1)
      left                          ← intSet(leftList)
      right                         ← intSet(rightList)
    } yield NonEmpty(elem, left, right)
  
  def intSet(list: List[Int] = Nil): Gen[IntSet] = 
    list match
      case Nil  ⇒ empty
      case list ⇒ Gen.frequency((1, nonEmpty(list)), (1, empty))

  given Arbitrary[NonEmpty] = Arbitrary( nonEmptySortedList flatMap { nonEmpty(_) } )
  
  given Arbitrary[IntSet] = Arbitrary(nonEmptySortedList flatMap { intSet(_) } )
