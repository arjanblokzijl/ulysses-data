package org.ulysses
package data
package enumerator

import Internal._
import scalaz.{MonadTrans, Monoid, Monad}


object EL {

  def iterate[M[_], A, B](f: A => A)(a: => A)(implicit m: Monad[M]): Enumerator[A, M, B] =
      checkcontinue1[A, M, B, A](a)(loop => s => k => k(Chunks(Stream(s))) >>== loop(f(s)))

  import Iteratees._

  def take[M[_], A](n: Int)(implicit m: Monad[M]): Iteratee[A, M, Stream[A]] = {
    val ITP = Iteratees.IterateeMonad[A, M]
    def plus(l1: Stream[A])(l2: Stream[A]): Stream[A] = l1 append l2
    def loop(acc: Stream[A])(nn: Int)(s: StreamI[A]): Iteratee[A, M, Stream[A]] = {
      s match {
        case Chunks(xs) => {
          if (xs.length < nn) continue(loop(plus(acc)(xs))((nn - xs.length)))
          else {
            val (xss, extra) = xs.splitAt(nn)
            yieldI(plus(acc)(xss), Chunks(extra))
          }
        }
        case EOF() => {
          yieldI[A, M, Stream[A]](acc, EOF())
        }
      }
    }
    if (n <= 0) ITP.point[Stream[A]](Stream.empty[A])
    else continue(loop(Stream.empty[A])(n))
  }

  private def streams[A](l1: Stream[A])(l2: Stream[A]): Stream[A] = l1 append l2

  def takeWhile[M[_], A](p: A => Boolean)(implicit m: Monad[M]): Iteratee[A, M, Stream[A]] = {
    def loop(acc: Stream[A])(s: StreamI[A]): Iteratee[A, M, Stream[A]] = s match {
      case Chunks(Stream.Empty) => continue(loop(acc))
      case Chunks(xs) => {
        xs.span(p) match {
          case (_, Stream.Empty) => continue(loop(streams(acc)(xs)))
          case (xss, extra) => yieldI(streams(acc)(xss), Chunks(extra))
        }
      }
      case EOF() => yieldI[A, M, Stream[A]](acc, EOF())
    }
    continue(loop(Stream.Empty))
  }

  /**
   * Applies a predicate to the stream. The inner iteratee only receives elements for which the predicate is true.
   */
  def filter[A, M[_], B](p: A => Boolean)(implicit M: Monad[M]): Enumeratee[A, A, M, B] =
    concatMap(x => Stream(x).filter(p))

  def concatMap[AO, AI, F[_], B](f: AO => Stream[AI])(implicit M : Monad[F]): Enumeratee[AO, AI, F, B] = {
    def pointf(ao: AO): F[Stream[AI]] = M.point(f(ao))
    concatMapM[AO, AI, F, B](pointf)
  }

  def concatMapM[AO, AI, M[_], B](f: AO => M[Stream[AI]])(implicit M : Monad[M]): Enumeratee[AO, AI, M, B] = {
    def step: (StreamI[AI] => Iteratee[AI, M, B]) => StreamI[AO] => Iteratee[AO, M, Step[AI, M, B]] = si => so => (si, so) match {
      case (k, EOF()) => yieldI[AO, M, Step[AI, M, B]](Continue(k), EOF())
      case (k, Chunks(xs)) => loop(xs)(k)
    }

    def loop: Stream[AO] => (StreamI[AI] => Iteratee[AI, M, B]) =>  Iteratee[AO, M, Step[AI, M, B]] = str => stri => (stri, str) match {
      case (k, Stream.Empty) => continue(step(k))
      case (k, x #:: xs) => {
        MonadTrans[({type λ[α[_], β] = Iteratee[AO, α, β]})#λ].liftM(f(x)).flatMap(fx =>
          k(Chunks(fx)) >>== checkDoneEx(Chunks(xs))(loop(xs)))
      }
    }
    def cs(k: StreamI[AI] => Iteratee[AI, M, B]): Iteratee[AO, M, Step[AI, M, B]] = continue(step(k))
    checkDone(cs)
  }

}