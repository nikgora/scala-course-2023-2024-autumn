package karazin.scala.users.group.week4.homework

import scala.annotation.targetName
import karazin.scala.users.group.week4.utils.ItemOrdering

object Homework:

  abstract class IntSet:

    infix def include(x: Int): IntSet

    infix def remove(x: Int): IntSet

    infix def contains(x: Int): Boolean

    @targetName("union")
    infix def ∪(that: IntSet): IntSet

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet

  end IntSet

  type Empty = Empty.type
  
  case object Empty extends IntSet:
    
    infix def include(x: Int): IntSet = NonEmpty(x, Empty, Empty)

    infix def contains(x: Int): Boolean = false

    infix def remove(x: Int): IntSet = ???
    
    @targetName("union")
    infix def ∪(that: IntSet): IntSet = ???

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = ???

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet = ???

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet = ???
    
    override def toString: String = "[*]"    
    
    override def equals(other: Any): Boolean = ???    

  end Empty
    
  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:

    infix def include(x: Int): IntSet = 
      if x < elem       then NonEmpty(elem, left include x, right)
      else if x > elem  then NonEmpty(elem, left, right include x)
      else              this

    infix def contains(x: Int): Boolean = 
      if x < elem       then left contains x
      else if x > elem  then right contains x
      else              true

    // Optional task
    infix def remove(x: Int): IntSet = ???

    @targetName("union")
    infix def ∪(that: IntSet): IntSet = ???

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = ???

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet = ???

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet = ???
    
    override def toString: String = s"[$left - [$elem] - $right]"    
    
    override def equals(other: Any): Boolean = ???

  end NonEmpty

end Homework
