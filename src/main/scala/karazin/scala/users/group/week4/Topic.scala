package karazin.scala.users.group.week4

import scala.annotation.targetName
import utils._

object Topic:

  /*
   * +X (and -X) represents so called `variant` and `contravariant` types.
   * Generally speaking omiting the detais it means that if `<:` and `>:` is a relation
   * of order on types means that if type `Dog` implements type `Pet` then `Dog <: Pet` and `Pet :> Dog`,
   * then we can consider 3 cases:
   * 1. if a container `Box` defined as `trait Box[V]`, then there is no any order relations between
   * `Box[Dog]` and `Box[Pet]`;
   * 2. if a container `Box` defined as `trait Box[+V]` (covariant), then there is an order relations between
   * `Box[Dog]` and `Box[Pet]` such that `Box[Dog] <: Box[Pet]` 
   * 3. if a container `Box` defined as `trait Box[-V]` (contravariant), then there is an order relations between
   * `Box[Dog]` and `Box[Pet]` such that `Box[Dog] >: Box[Pet]`
   * 
   * Unfortunately due to Liskov Substitution Principle we have to take care about covariant/contravariant
   * parameters, that's why we needed such a weird `AbstractSet`.
   * 
   * For more details see https://docs.scala-lang.org/tour/variances.html and material of next lectures.
   * 
   */
  sealed trait AbstractSet[+U, Repr[+X] <: AbstractSet[X, Repr]]:
    this: Repr[U] ⇒

    infix def include[V >: U](x: V)(using ordering: ItemOrdering[V]): Repr[V]

    infix def contains[V >: U](x: V)(using ordering: ItemOrdering[V]): Boolean

  end AbstractSet
  
  trait Set[+U] extends AbstractSet[U, Set] 

  type Empty = Empty.type
  
  case object Empty extends Set[Nothing]:
    
    infix def include[V](x: V)(using ordering: ItemOrdering[V]) = NonEmpty(x, Empty, Empty)

    infix def contains[V](x: V)(using ordering: ItemOrdering[V]): Boolean = false

  end Empty
    
  case class NonEmpty[+V](elem: V, left: Set[V], right: Set[V]) extends Set[V]:

    infix def include[U >: V](x: U)(using ordering: ItemOrdering[U]): Set[U] =
      if ordering.lt(x, elem)       then NonEmpty(elem, left include x, right)
      else if ordering.gt(x, elem)  then NonEmpty(elem, left, right include x)
      else                          this

    infix def contains[U >: V](x: U)(using ordering: ItemOrdering[U]): Boolean =
      if ordering.lt(x, elem)        then left contains x
      else if ordering.gt(x, elem)   then right contains x
      else                           true

  end NonEmpty

end Topic

