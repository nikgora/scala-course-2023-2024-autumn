package karazin.scala.users.group.week3

import karazin.scala.users.group.week3.Homework.{Nat, Succ, Zero}
import org.scalacheck.*
import org.scalacheck.Gen.lzy

object arbitraries:

  val zero: Gen[Zero] = Gen.const(Zero)
  val nat: Gen[Nat] = Gen.frequency((1, zero), (4, succ()))

  def succ(nat: Nat = Zero): Gen[Succ] =
    Gen.frequency((1, Gen.const(Succ(nat))), (3, lzy(succ(Succ(nat)))))

  given Arbitrary[Zero] = Arbitrary(zero)

  given Arbitrary[Succ] = Arbitrary(succ())

  given Arbitrary[Nat] = Arbitrary(nat)
