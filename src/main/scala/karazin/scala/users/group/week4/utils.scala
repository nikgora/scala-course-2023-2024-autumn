package karazin.scala.users.group.week4

object utils:

  trait ItemOrdering[-V]:
    infix def lt(v1: V, v2: V): Boolean
    infix def gt(v1: V, v2: V): Boolean
    infix def eq(v1: V, v2: V): Boolean
