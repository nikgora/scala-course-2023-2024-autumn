package karazin.scala.users.group.week1

import scala.annotation.tailrec

object Topic: 
  
  // General recursion
  object Recursion: 
    /*
        For recursive functions explicit 
        result type is required
      */
    def factorial(n: BigInt): BigInt = 
      if n == 0 then 1
      else n * factorial(n - 1) 
      
  /* 
    Optional construction.
    It's allowed to use common java-style braces. 
   */
  end Recursion
  
  /* 
    Tail recursion
    
    In most project `Some name` is depricated and
    it's forced camel case SomeName to be used.
    
    But I generally like it.
  */
  object `Tail Recursion`:
    def factorial(n: BigInt): BigInt =
      /*
        For tail-recursive functions explicit 
        result type is required
      */
      @tailrec // Optional annotation, should be imported (review imports at the beggining of the file)
      def factorialReq(n: BigInt, acc: BigInt): BigInt =
        if n == 0 /* Automatically casted to Int */ then acc /* Automatically casted to BigInt */
        else factorialReq(n - 1, acc * n)
        
      factorialReq(n, acc = 1)
  
  // Optional construction    
  end `Tail Recursion`
  
  object `Higher-Order Functions`:
    /* 
      It's possible to omit type of `v` because 
      explicit type `Int => Int` is forced. 
     */
    val id: BigInt => BigInt = v => v
    val cube: BigInt => BigInt = v => v * v * v
    
    /*
      `def` implicitly transformed to functional value with
      the type `BigInt => BigInt`
     */
    val factorial: BigInt => BigInt = `Tail Recursion`.factorial
    
    /*
      The function for summing elements in a range [a, b]
      Terrific signature
        `(BigInt => BigInt)` is for element-transfroming function `f`
        `(BigInt, BigInt)` is for arguments
        the last `=> BigInt` is for result
     */
    val sum: (BigInt => BigInt) => (BigInt, BigInt) => BigInt =
      /* 
        Explicit types for `f`, `a` and `b` could be omited
        because they are inforced by the explicit type of `sum`.
        If it's hard to guess the types feel free to use 
        explicit types. 
      */ 
      f => (a, b) => 
        // It's allowed to use common java-style `if`s
        if a < 0 || b < 0 || a > b then /* Use then to avoid braces */ 0
        else f(a) + sum(f)(a + 1, b)
    
    /* 
      Optional construction for a long `val` definitions 
      without braces.
      
      It's allowed to use common java-style braces.
     */
    end sum
    
    val /* or `def` */ sumInts: (BigInt, BigInt) => BigInt = sum(id)   
    val /* or `def` */ sumCubes: (BigInt, BigInt) => BigInt = sum(cube)   
    
    /* 
      Compile error
      `def sumFactorials(a: BigInt, b: BigInt): BigInt = sum(id)`
     */
    
    // Works
    def sumFactorials(a: BigInt, b: BigInt): BigInt = sum(factorial)(a, b)
    
    // Also works but the type is undesired
    def anotherSumFactorials(a: BigInt, b: BigInt) /*: (BigInt, BigInt) => BigInt */ = sum(factorial)
    
  // Optional construction
  end `Higher-Order Functions`

end Topic
