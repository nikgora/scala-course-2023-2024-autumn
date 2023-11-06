package karazin.scala.users.group.week2.homework

object utils:

  def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)