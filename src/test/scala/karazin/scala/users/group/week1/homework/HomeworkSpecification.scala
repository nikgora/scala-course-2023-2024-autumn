package karazin.scala.users.group.week1.homework

import karazin.scala.users.group.week1.homework.Homework.*
import karazin.scala.users.group.week1.homework.arbitraries
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}

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
    val newN = Math.abs(n)
    fermatNumber(newN) == BigInt(2).pow(BigInt(2).pow(newN).toInt) + 1
  }

  property("fermatNumber - Argument n should be non negative") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newN = if (n > 0) -Math.abs(n) else -1
      fermatNumber(newN)
    }
  }

end FermatNumbersSpecification

object LookAndSaySequenceSpecification extends Properties("Look-and-say Sequence"):

  import `Look-and-say Sequence`.*
  import arbitraries.given Arbitrary[Int]

  import scala.annotation.tailrec

  // https://rosettacode.org/wiki/Look-and-say_sequence#Scala
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
    val newN = if (n < 0) Math.abs(n) else 1
    lookAndSaySequenceElement(newN).toString() == loop(newN - 1, "1")
  }

  property("lookAndSaySequenceElement - Argument n should be greater then zero") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newN = -Math.abs(n)
      lookAndSaySequenceElement(newN)
    }
  }

end LookAndSaySequenceSpecification