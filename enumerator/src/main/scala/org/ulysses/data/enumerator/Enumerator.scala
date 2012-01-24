package org.ulysses.data
package enumerator

import scalaz.Monad

/**
 * A direct port of haskell's enumerator library.
 */

import org.ulysses.data.enumerator.Internal._
import org.ulysses.data.enumerator.IterateeInstances._

object Enumerator extends EnumeratorFunctions {

  //figure out https://issues.scala-lang.org/browse/SI-5152 .... It pops up every now and again...
  def run[A, F[_], B](i: Iteratee[A, F, B])(implicit M: Monad[F]): F[B] = {
    M.map((i ==<< enumEOF[A, F, B]).value)((s: Step[A, F, B]) => s match {
      case Yield(b, s) => b
      case ErrorS(t) => throw t
      case Continue(k) => sys.error("divergent iteratee")
    })
  }
}

trait EnumeratorFunctions {
//  def enumStream[A, F[_], B](as: Stream[A])(implicit M: Monad[F]): Enumerator[A, F, B] = step => {
//    def loop(str: Stream[A])(s: Step[A, F, B]): Iteratee[A, F, B] = str match {
//      case Stream.Empty => returnI(s)
//      case x #:: xs => s match {
//        case Continue(k) => k(Chunks(Stream(x))) >>== loop(xs)
//        case s1 => returnI(s1)
//      }
//    }
//    loop(as)(step)
//  }
}