package karazin.scala.users.group.week2

import scala.math._
import scala.annotation.targetName

object Topic:

  class Rational(x: Int, y: Int):
    require(y > 0, "denominator must be positive")
  
    def this(x: Int) = this(x, 1)
  
    lazy val numer = x / g
    lazy val denom = y / g
    
    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)
  
    private lazy val g = gcd(abs(x), y)
  
  end Rational

end Topic
