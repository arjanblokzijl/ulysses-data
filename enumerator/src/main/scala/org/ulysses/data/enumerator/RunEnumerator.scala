package org.ulysses.data
package enumerator

import Internal._
import Enumerator._

/**
 * User: arjan
 */

object RunEnumerator {


//  -- | Run an iteratee until it finishes, and return either the final value
//-- (if it succeeded) or the error (if it failed).
//--
//-- > import Data.Enumerator
//-- > import Data.Enumerator.List as EL
//-- >
//-- > main = do
//-- >     result <- run (EL.iterate succ 'A' $$ EL.take 5)
//-- >     case result of
//-- >         Left exc -> putStrLn ("Got an exception: " ++ show exc)
//-- >         Right chars -> putStrLn ("Got characters: " ++ show chars)


  def main(args: Array[String]) = {
    import scalaz.std.stream._
    val take5: Iteratee[Int, Stream, Stream[Int]] = EL.take[Stream, Int](5)
    val iteratee: Iteratee[Int, Stream, Stream[Int]] = take5 >>== EL.iterate[Stream, Int, Stream[Int]](s => s + 1)(5)

    val res = run(iteratee).map(result => result.toList)
    println("result is " + res)
  }
}