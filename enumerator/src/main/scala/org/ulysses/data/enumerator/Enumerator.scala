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
    val iter: Iteratee[A, F, B] = i ==<<(enumEOF[A, F, B](M))
    M.map(iter.value)((s: Step[A, F, B]) => s match {
      case Yield(b, s) => b
      case ErrorS(t) => throw t
      case Continue(k) => sys.error("divergent iteratee")
    })
//    sys.error("b")
  }
}

trait EnumeratorFunctions {
//  implicit def enumStream[A, F[_] : Monad, B](n: Int)(str: Stream[A]): Enumerator[A, F, B] = {
//    def loop(xs: Stream[A])(s: Step[A, F, B]): Iteratee[A, F, B] = {
//      s match {
//        case Continue(k) if (!xs.isEmpty) => {
//          val (s1, s2) = xs.splitAt(n)
//          k(Chunks(s1)) >>== loop(s2)
//        }
//        case step => returnI(step)
//      }
//    }
//    loop(str)
//  }
}