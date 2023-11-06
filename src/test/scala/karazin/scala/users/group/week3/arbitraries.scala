package karazin.scala.users.group.week3

import org.scalacheck._
import Gen.lzy
import Homework.{Nat, Succ, Zero}

object arbitraries:
  
  val zero: Gen[Zero] = Gen.const(Zero)

  def succ(nat: Nat = Zero): Gen[Succ] = 
    Gen.frequency((1, Gen.const(Succ(nat))), (3, lzy(succ(Succ(nat)))))

  val nat: Gen[Nat] = Gen.frequency((1, zero), (4, succ()))

  given Arbitrary[Zero] = Arbitrary(zero)
  given Arbitrary[Succ] = Arbitrary(succ())
  given Arbitrary[Nat] = Arbitrary(nat)
  