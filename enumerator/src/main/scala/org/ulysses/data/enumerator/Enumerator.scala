package org.ulysses.data
package enumerator

import scalaz.Monad

/**
 * A direct port of haskell's enumerator library.
 */
object Enumerator {

  import org.ulysses.data.enumerator.Internal._
  import org.ulysses.data.enumerator.Iteratees._

  //figure out https://issues.scala-lang.org/browse/SI-5152 ....
  def run[M[_], A, B](i: Iteratee[A, M, B])(implicit m: Monad[M]): M[B] = {
    val iter: Iteratee[A, M, B] = ==<<(enumEOF[A, M, B](m))(i)
    m.map(iter.runI)((mStep: Step[A, M, B]) => mStep match {
      case Yield(b, s) => b
      case ErrorS(t) => throw t
    })
  }
}