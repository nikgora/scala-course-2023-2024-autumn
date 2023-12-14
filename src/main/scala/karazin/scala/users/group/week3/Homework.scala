package karazin.scala.users.group.week3

object Homework:

  type Zero = Zero.type

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat = new Succ(this)

    infix def +(that: Nat): Nat

    infix def -(that: Nat): Nat

    // Optional task
    def toInt: Int

    // Optional task
    def fromInt(int: Int) = ???

    override def toString: String = s"Nat($predecessor)"

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false

    def predecessor: Nat = ???

    infix def +(that: Nat): Nat = ???

    infix def -(that: Nat): Nat = ???

    // Optional task
    def toInt: Int = ???

    override def toString = s"Succ()"

    override def equals(obj: Any): Boolean = ???

  object Zero extends Nat:
    def isZero: Boolean = true

    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat = throw new IllegalArgumentException("A NAT can't be negative")

    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = ???

