package karazin.scala.users.group.week1

object utils:
  val modelFactorial: Int => BigInt =
    n => (0 to n).foldLeft(BigInt(1)) { (acc, n) =>
      if n == 0 then acc * 1 else acc * n
    }

  val modelSum: (BigInt => BigInt) => (Int, Int) => BigInt =
    f => (left, right) => (left to right).foldLeft(BigInt(0)) { (acc, v) =>
      acc + f(v)
    }  
