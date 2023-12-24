package karazin.scala.users.group.week3

import karazin.scala.users.group.week2.arbitraries.restricted.{NegativeInteger, PositiveInteger}
import karazin.scala.users.group.week3.Homework.*
import karazin.scala.users.group.week3.SuccSpecification.property
import karazin.scala.users.group.week3.ZeroSpecification.property
import karazin.scala.users.group.week3.arbitraries.{given_Arbitrary_Int, given_Arbitrary_Nat, given_Arbitrary_Succ, given_Arbitrary_Zero}
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math.*

val findNumber: Nat => Int = (num: Nat) =>
  @tailrec
  def findNumberReq(num: Nat, acc: Int): Int =
    if num.isZero then acc
    else findNumberReq(num.predecessor, acc + 1)

  findNumberReq(num, 0)

@tailrec
def addition(left: Nat, right: Nat): Nat =
  right match {
    case Zero => left
    case _ => addition(left.successor, right.predecessor)
  }

@tailrec
def subtraction(left: Nat, right: Nat): Nat =
  require(findNumber(left) >= findNumber(right), "Negative result in Peano numbers")
  right match {
    case Zero => left
    case _ => subtraction(left.predecessor, right.predecessor)
  }

def equality(left: Nat, right: Nat): Boolean =
  findNumber(left) == findNumber(right)


val findStringForm: Nat => String = (num: Nat) =>
  @tailrec
  def findStringFormReq(num: Nat, acc: String): String =
    if num.isZero then acc
    else findStringFormReq(num.predecessor, acc = s"Succ($acc)")

  findStringFormReq(num, "Zero")

val findPeanoNumberFromInt: (Nat, Int) => Nat = (startNat: Nat, num: Int) =>
  require(num >= 0, s"Argument can`t be negative, actual [$num]")

  @tailrec
  def findPeanoNumberFromIntReq(num: Int, acc: Nat): Nat =
    if num == 0 then acc
    else findPeanoNumberFromIntReq(num - 1, Succ(acc))

  findPeanoNumberFromIntReq(num, startNat)


object HomeworkSpecification extends Properties("Homework"):

  include(ZeroSpecification)
  include(SuccSpecification)
  include(NatSpecification)

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):

  property("testing of Zero.isZero") = forAll { (zero: Zero) =>
    zero.isZero
  }

  property("Zero equality") = forAll { (zero: Zero, num2: Nat) =>
    zero.equals(num2) == equality(zero, num2)
  }

  property("minus of two Zero") = forAll { (left: Zero, right: Zero) =>
    (left - right) == subtraction(left, right)
  }

  property("Zero minus Peano number") = forAll { (left: Zero, right: Succ) =>
    throws(classOf[Exception]) {
      left - right
    }
  }

  property("zero plus Peano number is Peano number") = forAll { (zero: Zero, num: Nat) =>
    (zero + num) == num
  }

  property("successor of zero is one") = forAll { (zero: Zero) =>
    zero.successor == Succ(zero)
  }

  property("throwing exception due zero.predecessor") = forAll { (zero: Zero) =>
    throws(classOf[Exception]) {
      zero.predecessor
    }
  }

  property("Zero.toInt is 0") = forAll { (zero: Zero) =>
    zero.toInt == findNumber(zero)
  }

  property("Zero to String") = forAll { (zero: Zero) =>
    zero.toString == findStringForm(zero)
  }

  property("Zero.fromInt") = forAll { (num: PositiveInteger) =>
    Zero.fromInt(num) == findPeanoNumberFromInt(Zero, num)
  }

  property("Zero.fromInt with negative argument") = forAll { (num: NegativeInteger) =>
    throws(classOf[IllegalArgumentException]) {
      Zero.fromInt(num)
    }
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):

  property("Succ.toInt testing") = forAll { (succ: Succ) =>
    succ.toInt == findNumber(succ)
  }

  property("testing of Succ.isZero") = forAll { (succ: Succ) =>
    !succ.isZero
  }
  property("Succ equality") = forAll { (succ: Succ, num2: Nat) =>
    succ.equals(num2) == equality(succ, num2)
  }

  property("commutative of addition testing") = forAll { (succ: Succ, num: Nat) =>
    (succ + num)==addition(succ, num) && (num + succ)==addition(succ, num)
  }

  property("Succ addition is the same as successor") = forAll { (num1: Succ, num2: Succ) =>
    num1.successor==num1 + Succ(Zero) && num2.successor==num2 + Succ(Zero)
  }

  property("Succ subtraction") = forAll { (succ: Succ, num: Nat) =>
    if succ.toInt < num.toInt then (num - succ)==subtraction(num, succ)
    else (succ - num)==subtraction(succ, num)
  }

  property("Succ subtraction Zero") = forAll { (succ: Succ, num: Zero) =>
    (succ - num)==succ
  }

  property("Succ subtraction Succ") = forAll { (succ: Succ, num: Succ) =>
    if succ.toInt < num.toInt then (num - succ)==subtraction(num, succ)
    else (succ - num)==subtraction(succ, num)
  }

  property("Succ subtraction and getting negative result") = forAll { (succ: Succ, num: Nat) =>
    if succ==num then true
    else if succ.toInt < num.toInt
    then throws(classOf[Exception]) {
        succ - num
      }
    else
      throws(classOf[Exception]) {
        num - succ
      }
  }

  property("Succ(Nat).toInt == Nat.toInt + 1") = forAll { (num: Succ) =>
    Succ(num).toInt == num.toInt + 1
  }

  property("Succ to String") = forAll { (succ: Succ) =>
    succ.toString == findStringForm(succ)
  }

  property("Succ.fromInt") = forAll { (succ: Succ, num: PositiveInteger) =>
    succ.fromInt(num) == findPeanoNumberFromInt(succ, num)
  }

  property("Succ.fromInt with negative argument") = forAll { (succ: Succ, num: NegativeInteger) =>
    throws(classOf[IllegalArgumentException]) {
      succ.fromInt(num)
    }
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  property("testing of Nat.toInt") = forAll { (num: Nat) =>
    num.toInt == findNumber(num)
  }

  property("Nat equality") = forAll { (num1: Nat, num2: Nat) =>
    num1.equals(num2) == equality(num1, num2)
  }

  property("Nat,isZero testing") = forAll { (num: Nat) =>
    num match {
      case n: Zero => n.isZero
      case _ => !num.isZero
    }
  }

  property("addition is commutative") = forAll { (a: Nat, b: Nat) =>
    ((a + b)==(b + a)) == equality(addition(a, b), addition(b, a))
  }

  property("addition is associative") = forAll { (a: Nat, b: Nat, c: Nat) =>
    ((a + b) + c)==(a + (b + c)) == equality(addition(addition(a, b), c), addition(addition(c, b), a))
  }

  property("subtraction undoes addition") = forAll { (a: Nat, b: Nat) =>
    (a + b - b)==a && (b + a - a)==b == equality(subtraction(addition(a, b), b), a) && equality(subtraction(addition(a, b), a), b)
  }

  property("Nat subtraction") = forAll { (num1: Nat, num2: Nat) =>
    if num1.toInt < num2.toInt then (num2 - num1)==subtraction(num2, num1)
    else (num1 - num2)==subtraction(num1, num2)
  }

  property("Nat subtraction and getting negative result") = forAll { (num1: Nat, num2: Nat) =>
    if num1==num2 then true
    else if num1.toInt < num2.toInt
    then throws(classOf[Exception]) {
        num1 - num2
      }
    else
      throws(classOf[Exception]) {
        num2 - num1
      }
  }

  property("Nat to String") = forAll { (num: Nat) =>
    num.toString == findStringForm(num)
  }

  property("Nat.fromInt") = forAll { (nat: Nat, num: PositiveInteger) =>
    nat.fromInt(num) == findPeanoNumberFromInt(nat, num)
  }

  property("Nat.fromInt with negative argument") = forAll { (nat: Nat, num: NegativeInteger) =>
    throws(classOf[IllegalArgumentException]) {
      nat.fromInt(num)
    }
  }


end NatSpecification
  