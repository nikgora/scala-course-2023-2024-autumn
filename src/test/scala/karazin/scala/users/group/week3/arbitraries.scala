package karazin.scala.users.group.week3

import org.scalacheck._
import Gen.lzy
import Homework.{Nat, Succ, Zero}

object arbitraries:
  
  val zero: Gen[Zero] = Gen.const(Zero)

  def succ(nat: Nat = Zero): Gen[Succ] = 
    Gen.frequency((1, Gen.const(Succ(nat))), (3, lzy(succ(Succ(nat)))))

  val nat: Gen[Nat] = Gen.frequency((1, zero), (4, succ()))
  
  val integer: Gen[Int] = Gen.choose[Int](min = 0, max = 100)

  given Arbitrary[Int] = Arbitrary(integer)
  given Arbitrary[Zero] = Arbitrary(zero)
  given Arbitrary[Succ] = Arbitrary(succ())
  given Arbitrary[Nat] = Arbitrary(nat)
  