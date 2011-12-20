package org.ulysses.data
package enumerator

import Internal._

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
    val iterate: (Step[Int, Stream, Int]) => Iteratee[Int, Stream, Int] = EL.iterate[Stream, Int, Int]((i: Int) => i + 1)(0)
//    $$(iterate)

  }
}