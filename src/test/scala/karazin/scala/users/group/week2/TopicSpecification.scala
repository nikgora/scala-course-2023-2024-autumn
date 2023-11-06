package karazin.scala.users.group.week2

import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck._

import scala.language.implicitConversions
import scala.math._

import Topic._
import utils._
import arbitraries.restricted._

object TopicSpecification extends Properties("Topic"):
  import arbitraries.{given Arbitrary[NegativeInteger], 
                      given Arbitrary[Integer], 
                      given Arbitrary[Rational]}

  property("throw exception due to zero denominator") = forAll { (numer: Integer) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Integer, denom: NegativeInteger) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, denom)
    }
  }

end TopicSpecification