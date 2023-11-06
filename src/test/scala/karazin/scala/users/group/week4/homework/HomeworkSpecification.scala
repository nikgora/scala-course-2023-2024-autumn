package karazin.scala.users.group.week4.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import arbitraries.given
import Homework._

object HomeworkSpecification extends Properties("Homework"):

  include(EmptySpecification)
  include(NonEmptySpecification)
  include(IntSetSpecification)

end HomeworkSpecification

// Add additional cases if needed
object EmptySpecification extends Properties("Empty"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[NonEmpty], given Arbitrary[IntSet]}

  property("equals to Empty") = propBoolean {
    Empty == Empty
  }

  property("not equal to NonEmpty") = forAll { (nonEmpty: NonEmpty) ⇒
    Empty != nonEmpty
  }

  property("include") = forAll { (element: Int) ⇒
    (Empty include element) == NonEmpty(element, Empty, Empty)
  }

  property("contains") = forAll { (element: Int) ⇒
    !(Empty contains element)
  }

  property("remove") = forAll { (element: Int) ⇒
    (Empty remove element) == Empty
  }

  property("union") = forAll { (set: IntSet) ⇒
    (Empty ∪ set) == set
  }

  property("intersection") = forAll { (set: IntSet) ⇒
    (Empty ∩ set) == Empty
  }

  property("complement of Empty") = forAll { (set: IntSet) ⇒
    (set ∖ Empty) == set
  }

  property("complement of set") = forAll { (set: IntSet) ⇒
    (Empty ∖ set) == Empty
  }

  property("left disjunctive union") = forAll { (set: IntSet) ⇒
    (Empty ∆ set) == set
  }

  property("right disjunctive union") = forAll { (set: IntSet) ⇒
    (set ∆ Empty) == set
  }

end EmptySpecification

// Add additional cases if needed
object NonEmptySpecification extends Properties("NonEmpty"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[NonEmpty], given Arbitrary[IntSet]}

  property("not equals to Empty") = forAll { (nonEmpty: NonEmpty) ⇒
    nonEmpty != Empty
  }

  property("equal") = forAll { (nonEmpty: NonEmpty) ⇒
    nonEmpty == nonEmpty
  }

  property("include") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    false
  }

  property("contains") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    false
  }

  property("remove") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    false
  }

  property("union") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    false
  }

  property("intersection") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    false
  }

  property("complement") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    false
  }

  property("disjunctive") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    false
  }

end NonEmptySpecification

// Add additional cases if needed
object IntSetSpecification extends Properties("IntSet"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[IntSet]}

  property("equals") = forAll { (set: IntSet) ⇒
    set == set
  }

end IntSetSpecification