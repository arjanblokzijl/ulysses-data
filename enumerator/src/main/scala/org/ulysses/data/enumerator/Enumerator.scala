package org.ulysses.data
package enumerator

import scalaz.Monad

/**
 * A port of haskell's enumerator library.
 * This is in no way an attempt to improve over Scalaz's implementation, but just for the learning experience.
 */

object Enumerator {

  import org.ulysses.data.enumerator.Internal._
  import org.ulysses.data.enumerator.Iteratees._
//
//  def run[A, M[_], B](i: Iteratee[A, M, B])(implicit m: Monad[M]) : M[Either[Throwable, B]] = {
//    val enum: Iteratee[A, M, B] = ==<<(enumEOF[A, M, B](m))(i)
//    m.map(enum.runI)(mStep => mStep match {
//        case ErrorS(t) => Left(t)
//        case Yield(x, _) => Right(x)
//      })
//  }
}