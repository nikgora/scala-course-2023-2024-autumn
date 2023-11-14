package karazin.scala.users.group.week1.homework

import karazin.scala.users.group.week1.homework.Homework.*
import karazin.scala.users.group.week1.homework.arbitraries
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndSaySequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):

  import `Boolean Operators`.*

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

  import `Fermat Numbers`.*
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    if (n < 0) try {
      fermatNumber(n)
      false
    } catch {
      case e: IllegalArgumentException => true
    }
    else fermatNumber(n) == BigInt(2).pow(BigInt(2).pow(n).toInt) + 1
  }

end FermatNumbersSpecification

object LookAndSaySequenceSpecification extends Properties("Look-and-say Sequence"):

  import `Look-and-say Sequence`.*
  import arbitraries.given Arbitrary[Int]

  import scala.annotation.tailrec


  @tailrec
  private def loop(n: Int, num: String): String = {
    if (n <= 0) num else loop(n - 1, lookandsay(num))
  }

  private def lookandsay(number: String): String = {
    val result = new StringBuilder

    @tailrec
    def loop(numberString: String, repeat: Char, times: Int): String =
      if (numberString.isEmpty) result.toString()
      else if (numberString.head != repeat) {
        result.append(times).append(repeat)
        loop(numberString.tail, numberString.head, 1)
      } else loop(numberString.tail, numberString.head, times + 1)

    loop(number.tail + " ", number.head, 1)
  }


  property("lookAndSaySequenceElement") = forAll { (n: Int) =>

    if (n <= 0) try {
      lookAndSaySequenceElement(n)
      false
    } catch {
      case e: IllegalArgumentException => true
    }
    else lookAndSaySequenceElement(n).toString() == loop(n - 1, "1")
  }

end LookAndSaySequenceSpecification
