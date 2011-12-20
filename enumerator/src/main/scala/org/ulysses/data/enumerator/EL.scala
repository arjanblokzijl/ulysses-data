package org.ulysses
package data
package enumerator

import Internal._
import scalaz.Monad


object EL {

  def iterate[M[_], A, B](f: A => A)(a: => A)(implicit m: Monad[M]): Enumerator[A, M, B] = {
    checkcontinue1[A, M, B, A](a)(loop => s => k => k(Chunks(List(s))) >>== loop(f(s)))
  }

  import Iteratees._

  def take[M[_], A, B](n: Int)(implicit m: Monad[M]): Iteratee[A, M, List[A]] = {
    val ITP = Iteratees.IterateeMonad[A, M]
    def plus(l1: List[A])(l2: List[A]): List[A] = l1 ::: l2
    def loop(acc: List[A])(nn: Int)(s: StreamI[A]): Iteratee[A, M, List[A]] = s match {
      case Chunks(xs) => {
        if (xs.length < nn) continue(loop(plus(acc)(xs))((nn - xs.length)))
        else {
          val (xss, extra) = xs.splitAt(nn)
          yieldI(plus(acc)(xss), Chunks(extra))
        }
      }
      case EOF() => yieldI(acc, EOF())
    }
    if (n <= 0) ITP.point[List[A]](List())
    else continue(loop(List())(n))
  }
}