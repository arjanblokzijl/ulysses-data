package org.ulysses.data
package enumerator

import scalaz.Monad

/**
 * A direct port of haskell's enumerator library.
 */
object Enumerator {

  import org.ulysses.data.enumerator.Internal._
  import org.ulysses.data.enumerator.Iteratees._

  //https://issues.scala-lang.org/browse/SI-5152 seems to popup a bit too much in this code...
  def run[M[_], A, B](i: Iteratee[A, M, B])(implicit m: Monad[M]) : M[Either[Throwable, B]] = {
//    val f: (Step[A, M, B]) => Iteratee[A, M, B] = enumEOF[A, M, B](m)
//    val iter: Iteratee[A, M, B] = ==<<(f)(i)
//    m.map(iter.runI)(mStep => mStep match {
//       case ErrorS(t) => Left(t)
//       case Yield(x, _) => Right(x)
//    })
    sys.error("todo")
  }
}