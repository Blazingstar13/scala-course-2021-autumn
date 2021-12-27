package karazin.scala.users.group.week1.homework

import karazin.scala.users.group.week1.Topic.`Higher-Order Functions`.sum
import karazin.scala.users.group.week1.homework.Homework.`Boolean Operators`.not

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative *
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  //solution for booleans
  object `Boolean Operators` :

    def not(b: Boolean): Boolean =
      if (b)
        false
      else
        true

    def and(left: Boolean, right: => Boolean): Boolean =
      if (left)
        right
      else
        false

    def or(left: Boolean, right: => Boolean): Boolean =
      if (left)
          true
      else
          right


  end `Boolean Operators`

  //solution for Fermat numbers
  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) =>    {
      def mult1: (BigInt, BigInt) => BigInt = (x, y) =>      {
        if (y > 1)
          x + mult1(x, y - 1)
        else
          x
      }
      if ((a == 0) || (b == 0))
        0
      else
        mult1(a, b)
    }

    val power: (BigInt, BigInt) => BigInt = (a, b) =>   {

        def pow2: (BigInt, BigInt) => BigInt = (x, y) =>    {
          if(y == 1) x
          else multiplication(x,pow2(x,y-1))
        }

        //любое число в 0 степени = 1, 1 в любой степени = 1
        if ((a == 0) && (b != 0))
          BigInt(0)                              //число 0 в любой степени кроме 0 равна 0
        if (b == 0)
          BigInt(1)
        if (b < 0)
          1 / pow2(a, b)                    //a^-b = 1/a^b
        else
          pow2(a,b)
    }


    val fermatNumber: Int => BigInt = (n) =>      {
        power(2, power(2, n)) + 1
      }

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = ???

  end `Look-and-say Sequence`

end Homework