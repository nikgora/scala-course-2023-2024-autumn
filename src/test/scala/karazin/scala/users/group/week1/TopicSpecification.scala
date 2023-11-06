package karazin.scala.users.group.week1

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Topic._

object TopicSpecification extends Properties("Topic"):

  include(RecursionSpecification)
  include(TailRecursionSpecification)
  include(HigherOrderFunctionsSpecification)
  
end TopicSpecification

object RecursionSpecification extends Properties("Recursion"):
  // Importing arbitraies only
  import arbitraries.{given Arbitrary[Int]} 
  import utils._
  
  property("factorial") = forAll { (n: Int) =>
    Recursion.factorial(n) == modelFactorial(n)
  }

end RecursionSpecification

object TailRecursionSpecification extends Properties("Tail Recursion"):
  // Importing arbitraies only
  import arbitraries.{given Arbitrary[Int]}
  import utils._

  property("factorial") = forAll { (n: Int) =>
    `Tail Recursion`.factorial(n) == modelFactorial(n)
  }

end TailRecursionSpecification

object HigherOrderFunctionsSpecification extends Properties("Higher-Order Functions"):
  // Importing arbitraies only
  import arbitraries.{given Arbitrary[Int], given Arbitrary[(Int, Int)], given Arbitrary[BigInt => BigInt]}
  import `Higher-Order Functions`._
  import utils._

  property("id") = forAll { (n: Int) =>
    id(n) == n
  }

  property("cube") = forAll { (n: Int) =>
    cube(n) == n * n * n
  }

  property("factorial") = forAll { (n: Int) =>
    factorial(n) == modelFactorial(n)
  }

  property("sum") = forAll { (pair: (Int, Int), f: BigInt => BigInt) =>
    val (left, right) = pair

    sum(f)(left, right) == modelSum(f)(left, right)
  }

end HigherOrderFunctionsSpecification
