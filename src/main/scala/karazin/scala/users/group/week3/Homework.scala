package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:

  type Zero = Zero.type

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean

    def predecessor: Nat

    infix def +(that: Nat): Nat

    infix def -(that: Nat): Nat

    // Optional task
    def toInt: Int

    // Optional task
    def fromInt(int: Int): Nat =
      require(int >= 0, s"Argument can`t be negative, actual [$int]")

      @tailrec
      def fromIntRec(int: Int, acc: Nat): Nat =
        if int == 0 then acc
        else fromIntRec(int - 1, acc.successor)

      fromIntRec(int, acc = this)

    def successor: Nat = new Succ(this)

    override def toString: String = s"Nat($predecessor)"

  class Succ(n: Nat) extends Nat:
    infix def +(that: Nat): Nat =
      if that.isZero then this
      else n + that.successor

    infix def -(that: Nat): Nat =
      if that.toInt > this.toInt then throw new Exception("Negative result in Peano numbers")
      else if that.isZero then this
      else n - that.predecessor

    // Optional task
    def toInt: Int =
      @tailrec
      def toIntRec(nat: Nat, acc: Int): Int =
        if nat.isZero then acc
        else toIntRec(nat.predecessor, acc + 1)

      toIntRec(this, acc = 0)

    override def toString = s"Succ()"

    override def equals(other: Any): Boolean = other match
      case that: Succ =>
        that.canEqual(this) &&
          hashCode() == that.hashCode() &&
          isZero == that.isZero &&
          predecessor == that.predecessor
      case _ => false

    private def canEqual(other: Any): Boolean = other.isInstanceOf[Succ]

    override def hashCode(): Int =
      val state = Seq(isZero, predecessor)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    def predecessor: Nat = n

    def isZero: Boolean = false

  object Zero extends Nat:
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat = throw new IllegalArgumentException("A NAT can't be negative")

    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(other: Any): Boolean = other match
      case that: Zero =>
        that.canEqual(this) &&
          hashCode() == that.hashCode() &&
          isZero == that.isZero
      case _ => false

    private def canEqual(other: Any): Boolean = other.isInstanceOf[Zero]

    override def hashCode(): Int =
      val state = Seq(isZero)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    def isZero: Boolean = true