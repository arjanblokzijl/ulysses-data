package org.ulysses.data
package enumerator


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
    println("TODO")
  }
}