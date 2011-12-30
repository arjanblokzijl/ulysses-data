package org.ulysses
package data
package enumerator

import Internal._
import scalaz.{Monoid, Monad}


object EL {

  def iterate[M[_], A, B](f: A => A)(a: => A)(implicit m: Monad[M]): Enumerator[A, M, B] = {
    checkcontinue1[A, M, B, A](a)(loop => s => k => k(Chunks(Stream(s))) >>== loop(f(s)))
  }

  import Iteratees._

  def take[M[_], A](n: Int)(implicit m: Monad[M]): Iteratee[A, M, Stream[A]] = {
    val ITP = Iteratees.IterateeMonad[A, M]
    def plus(l1: Stream[A])(l2: Stream[A]): Stream[A] = l1 append l2
    def loop(acc: Stream[A])(nn: Int)(s: StreamI[A]): Iteratee[A, M, Stream[A]] = s match {
      case Chunks(xs) => {
        if (xs.length < nn) continue(loop(plus(acc)(xs))((nn - xs.length)))
        else {
          val (xss, extra) = xs.splitAt(nn)
          yieldI(plus(acc)(xss), Chunks(extra))
        }
      }
      case EOF() => yieldI(acc, EOF())
    }
    if (n <= 0) ITP.point[Stream[A]](Stream.empty[A])
    else continue(loop(Stream.empty[A])(n))
  }

//  def take2[X, M[_], A](n: Int)(implicit m: Monad[M]): Iteratee[X, M, A] = {
//    val ITP = Iteratees.IterateeMonad[A, M]
//    def plus(l1: Stream[A])(l2: Stream[A]): Stream[A] = l1 append l2
//    def loop(acc: Stream[A])(nn: Int)(s: StreamI[A]): Iteratee[A, M, Stream[A]] = s match {
//      case Chunks(xs) => {
//        if (xs.length < nn) continue(loop(plus(acc)(xs))((nn - xs.length)))
//        else {
//          val (xss, extra) = xs.splitAt(nn)
//          yieldI(plus(acc)(xss), Chunks(extra))
//        }
//      }
//      case EOF() => yieldI(acc, EOF())
//    }
//    if (n <= 0) ITP.point[Stream[A]](Stream.empty[A])
//    else continue(loop(Stream.empty[A])(n))
//  }

//  def take2[X, M[_], A](n: Int)(implicit m: Monoid[M[A]]): Iteratee[X, M, M[A]] = {
//    val ITP = Iteratees.IterateeMonad[A, M]
//    def plus(l1: Stream[A])(l2: Stream[A]): Stream[A] = l1 append l2
//    def loop(acc: M[A])(nn: Int)(s: StreamI[A]): Iteratee[X, M, M[A]] = s match {
//      case Chunks(xs) => {
//        if (xs.length < nn) continue(loop(m.append(acc)(xs))((nn - xs.length)))
//        else {
//          val (xss, extra) = xs.splitAt(nn)
//          yieldI(plus(acc)(xss), Chunks(extra))
//        }
//      }
//      case EOF() => yieldI(acc, EOF())
//    }
//    if (n <= 0) ITP.point[Stream[A]](Stream.empty[A])
//    else continue(loop(Stream.empty[A])(n))
//  }
}