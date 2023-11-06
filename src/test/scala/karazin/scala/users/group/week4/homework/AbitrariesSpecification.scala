package karazin.scala.users.group.week4.homework

import org.scalacheck._
import Prop.forAll

import karazin.scala.users.group.week4.homework.arbitraries
import Homework._

object AbitrariesSpecification extends Properties("Abitraries"):
  import arbitraries.given Arbitrary[IntSet]

  def validate(set: IntSet): Boolean =

    def validateReq(value: Int, predicate: (Int, Int) => Boolean, set: IntSet): Boolean =
      set match
        case Empty                        ⇒ true
        case NonEmpty(elem, left, right)  ⇒
          predicate(elem, value) &&
            validateReq(elem, (l, b) ⇒ l < b, left) &&
            validateReq(elem, (r, b) ⇒ r > b, right)

    set match
      case Empty                       ⇒ true
      case NonEmpty(elem, left, right) ⇒
        validateReq(elem, (l, b) ⇒ l < b, left) && validateReq(elem, (r, b) ⇒ r > b, right)

  end validate

  property("validate set structure") = forAll { (set: IntSet) ⇒
    validate(set)
  }

end AbitrariesSpecification

