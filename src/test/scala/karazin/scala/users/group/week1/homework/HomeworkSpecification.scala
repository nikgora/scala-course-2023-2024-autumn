package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndSaySequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == (!b)
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    or(left, right) == (left || right)
  }   

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    if (n<0) try {
      fermatNumber(n)
      false
    } catch {
      case e: IllegalArgumentException => true
    }
    else fermatNumber(n) == BigInt(2).pow(BigInt(2).pow(n).toInt)+1
  }  

end FermatNumbersSpecification

object LookAndSaySequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    val expectedResults = Map(
      1 -> BigInt("1"),
      2 -> BigInt("11"),
      3 -> BigInt("21"),
      4 -> BigInt("1211"),
      5 -> BigInt("111221"),
      6 -> BigInt("312211"),
      7 -> BigInt("13112221"),
      8 -> BigInt("1113213211"),
      9 -> BigInt("31131211131221"),
      10 -> BigInt("13211311123113112211"),
      11 -> BigInt("11131221133112132113212221"),
      12 -> BigInt("3113112221232112111312211312113211"),
      13 -> BigInt("1321132132111213122112311311222113111221131221"),
      14 -> BigInt("11131221131211131231121113112221121321132132211331222113112211"),
      15 -> BigInt("311311222113111231131112132112311321322112111312211312111322212311322113212221"),
    )

    if (n <= 0) try {
      lookAndSaySequenceElement(n)
      false
    } catch {
      case e: IllegalArgumentException => true
    }
    else lookAndSaySequenceElement(n)==expectedResults.getOrElse(n, BigInt(0))
  }  

end LookAndSaySequenceSpecification
