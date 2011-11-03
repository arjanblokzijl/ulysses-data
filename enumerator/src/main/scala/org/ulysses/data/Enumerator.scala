package org.ulysses.data

import scalaz.Monad

/**
 * A port of haskell's enumerator library.
 * This is in no way an attempt to improve over Scalaz's implementation, but just for the learning experience.
 */

trait Enumerator {

  import org.ulysses.data.enumerator.Internal._
  import org.ulysses.data.enumerator.Iteratees._

//  def run[A, M[_], B](i: Iteratee[A, M[_], B])(implicit M: Monad[M]) : M[Either[Throwable, B]]= {
//    for (mStep <- i)
//  }

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
//run :: Monad m => Iteratee a m b
//    -> m (Either Exc.SomeException b)
//run i = do
//	mStep <- runIteratee $ enumEOF ==<< i
//	case mStep of
//		Error err -> return $ Left err
//		Yield x _ -> return $ Right x
//		Continue _ -> error "run: divergent iteratee"

}