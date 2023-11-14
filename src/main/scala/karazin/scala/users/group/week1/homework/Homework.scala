package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    val int = 42

    def not(b: Boolean): Boolean =
      if (b) false
      else true // here is my greatest solution

    def and(left: Boolean, right: Boolean): Boolean =
      if (left) right
      else false

    def or(left: Boolean, right: Boolean): Boolean =
      if (left)  true
      else right

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a,b) =>
      @tailrec
      def multiplicationReq(a: BigInt, b: BigInt, res: BigInt): BigInt =
        if (b == 0) res
        else  multiplicationReq (a, b-1, a+res)

      multiplicationReq (a, b, res = 0)

    val power: (BigInt, BigInt) => BigInt = (a, b) =>
      @tailrec
      def  powerReq(a: BigInt, b: BigInt, res: BigInt): BigInt =
        if (b == 0) res
        else powerReq(a, b-1, multiplication(res, a))

      powerReq (a, b, res = 1)

    val fermatNumber: Int => BigInt = n =>
      if (n<0) throw IllegalArgumentException(s"Expected non-negative value, actual[$n]")
      else power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = n =>

      @tailrec
      def reverseWithTwoNumbers(number: BigInt, result : BigInt): BigInt=
      {
        if (number == 0) result
        else reverseWithTwoNumbers(number/100, result*100+number.mod(100))
      }

      @tailrec
      def countDigites(number: BigInt, prev: Int, count: Int): Int={
        if (number == 0)count
        else if (prev != number.mod(10).toInt) count
        else countDigites(number/10, number.mod(10).toInt, count+1)
      }

      @tailrec
      def generateNextNumber (number: BigInt, result : BigInt): BigInt =
        {
          if (number == 0) result
          else
            val c = countDigites(number,number.mod(10).toInt,count = 0)
            generateNextNumber(number / BigInt(10).pow(c), (c*10+number.mod(10)*1)+result*100)
        }

      @tailrec
      def lookAndSaySequenceElementReq(n:Int, now: BigInt): BigInt = {
        if (n == 1) now
        else lookAndSaySequenceElementReq(n-1, reverseWithTwoNumbers(generateNextNumber(now,result = 0),result = 0))
      }

      if (n <= 0) throw IllegalArgumentException(s"Expected non-negative value, actual[$n]")
      else
      {
        lookAndSaySequenceElementReq(n, now = 1)
      }

  end `Look-and-say Sequence`

end Homework