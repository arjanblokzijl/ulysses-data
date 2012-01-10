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
  def run[A, M[_], B](i: Iteratee[A, M, B])(implicit m: Monad[M]): M[B] = {
    val iter: Iteratee[A, M, B] = ==<<[A, M, B, A, B](enumEOF[A, M, B](m))(i)
    m.map(iter.value)((s: Step[A, M, B]) => s match {
      case Yield(b, s) => b
      case ErrorS(t) => throw t
    })
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