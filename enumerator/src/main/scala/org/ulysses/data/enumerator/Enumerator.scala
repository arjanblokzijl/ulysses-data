package org.ulysses.data
package enumerator

import scalaz.Monad

/**
 * A direct port of haskell's enumerator library.
 */

import org.ulysses.data.enumerator.Internal._
import org.ulysses.data.enumerator.Iteratees._

object Enumerator extends EnumeratorFunctions {

  //figure out https://issues.scala-lang.org/browse/SI-5152 .... It pops up every now and again...
  def run[A, F[_], B](i: Iteratee[A, F, B])(implicit M: Monad[F]): F[B] = {
    val iter: Iteratee[A, F, B] = i ==<< enumEOF[A, F, B](M)
    M.map(iter.value)((s: Step[A, F, B]) => s match {
      case Yield(b, s) => b
      case ErrorS(t) => throw t
      case Continue(k) => sys.error("divergent iteratee")
    })
  }
}

trait EnumeratorFunctions {
  implicit def enumStream[A, F[_], B](as: Stream[A])(implicit M: Monad[F]): Enumerator[A, F, B] = s => {
    def loop(str: Stream[A])(s: Step[A, F, B]): Iteratee[A, F, B] = str match {
      case Stream.Empty => returnI(s)
      case x #:: xs => s match {
        case Continue(k) => k(Chunks(Stream(x))) >>== loop(xs)
        case step => returnI(step)
      }
    }
    loop(as)(s)
  }
}