package org.ulysses
package data
package enumerator

import scalaz.Monad


/**
 * User: arjan
 */

import scalaz._
object Internal {

  sealed trait StreamI[A]
  case class Chunks[A](l: Stream[A]) extends StreamI[A]
  case class EOF[A]() extends StreamI[A]

  sealed trait Step[A, F[_], B]
  case class Continue[A, F[_], B](f: StreamI[A] => Iteratee[A, F, B]) extends Step[A, F, B]
  case class Yield[A, F[_], B](b: B, s: StreamI[A]) extends Step[A, F, B]
  case class ErrorS[A, F[_], B](t: Throwable) extends Step[A, F, B]

  case object Error {
    def unapply[A, F[_], B](s: Step[A, F, B]): Boolean = s match {
      case Continue(_) | Yield(_, _) => false
      case _ => true
    }
  }

  def err[A, F[_], B](t: Throwable) = ErrorS[A, F, B](t)

  case class Iteratee[A, F[_], B](value: F[Step[A, F, B]]) {
    def apply(f: (=> B) => F[B])(implicit m: Monad[F]): F[B] = {
      m.bind(value)((mStep: Step[A, F, B]) => mStep match {
        case Yield(b, s) => m.point(b)
        case ErrorS(t) => throw t
      })
    }

    def flatMap[C](f: (B) => Iteratee[A, F, C])(implicit m: Monad[F]): Iteratee[A, F, C] = {
      Iteratee(m.bind(value)((mStep: Step[A, F, B]) => mStep match {
        case Yield(x, Chunks(Stream.Empty)) => f(x).value
        case Yield(x, chunk) => {
          m.bind(f(x).value)(r => r match {
            case Continue(k) => k(chunk).value
            case ErrorS(t) => m.pure(err(t))
          })
        }
        case Continue(k) => m.pure(Continue(str => k(str).flatMap(f)))
        case ErrorS(t) => m.pure(err(t))
      }
      ))
    }

    def map[C](f: (B) => C)(implicit F: Monad[F]): Iteratee[A, F, C] = {
      flatMap(a => Iteratee[A, F, C](F.pure(Yield(f(a), Chunks(Stream.empty))))) //compiles as well without type annotation, but intellij gives annoying red lines
    }

    //just shamelessly stealing the scalaz implementation seems easiest here
    def mapI[G[_]](f: F ~> G)(implicit F: Functor[F]): Iteratee[A, G, B] = {
        def step: Step[A, F, B] => Step[A, G, B] = s => s match {
          case Yield(x, Chunks(Stream.Empty)) => Yield[A, G, B](x, EOF())
          case Yield(x, chunks) => Yield[A, G, B](x, chunks)
          case Continue(k) => Continue[A, G, B](k andThen loop)
          case ErrorS(t) => ErrorS[A, G, B](t)
        }

      def loop: Iteratee[A, F, B] => Iteratee[A, G, B] = i => Iteratee[A, G, B](f(F.map(i.value)(step)))
      loop(this)
    }

    def mapI[C](f: (B) => C)(implicit F: Monad[F]): Iteratee[A, F, C] = {
      flatMap(a => Iteratee[A, F, C](F.pure(Yield(f(a), Chunks(Stream.empty)))))
    }

    def >>==[AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
      Internal.>>==(this)(f)

    def ==<<[AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
      Internal.==<<(f)(this)
  }

  def >>==[A, F[_], B, AA, BB](i: Iteratee[A, F, B])(f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit m: Monad[F]): Iteratee[AA, F, BB] =
    Iteratee[AA, F, BB](m.bind(i.value)(step => f(step).value))

  def ==<<[A, F[_], B, AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(i: Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
    >>==[A, F, B, AA, BB](i)(f)

//  def $$[A, F[_], B, AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(i: Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[AA, F, BB] = ==<<(f)(i)

  type Enumerator[A, F[_], B] = Step[A, F, B] => Iteratee[A, F, B]

  type Enumeratee[AO, AI, M[_], B] = Step[AI, M, B] => Iteratee[AO, M, Step[AI, M, B]]
  def returnI[A, F[_], B](step: Step[A, F, B])(implicit m: Monad[F]): Iteratee[A, F, B] = Iteratee[A, F, B](m.pure(step))
  def yieldI[A, F[_], B](x: B, extra: StreamI[A])(implicit m: Monad[F]): Iteratee[A, F, B] = returnI[A, F, B](Yield(x, extra))
  def continue[A, F[_], B](k: StreamI[A] => Iteratee[A, F, B])(implicit m: Monad[F]): Iteratee[A, F, B] = returnI[A, F, B](Continue(k))

  /**
   * Sends EOF to its iteratee.
   */
  def enumEOF[A, F[_], B](implicit m: Monad[F]): Enumerator[A, F, B] = s => s match {
    case Yield(x, _) => yieldI[A, F, B](x, EOF())
    case ErrorS(s) => returnI[A, F, B](err(s))
    case Continue(k) => >>==(k(EOF()))(check => check match {
      case Continue(_) => sys.error("enumEOF: divergent iteratee")
      case s => enumEOF[A, F, B](m)(s)
    })
  }

  /**
   * Checks whether the inner Iteratee has finished and, if so, returns its output.
   * checkDone passes its parameter a continuation if the Iteratee can still consume input, or yields otherwise.
   */
  def checkDoneEx[AA, A, M[_], B](st: StreamI[AA])(ff: (StreamI[A] => Iteratee[A, M, B]) => Iteratee[AA, M, Step[A, M, B]])(implicit m: Monad[M]): Enumeratee[AA, A, M, B] = f => f match {
    case Continue(k) => ff(k)
    case step => yieldI[AA, M, Step[A, M, B]](step, st)
  }

  def checkDone[AA, A, M[_], B](ff: (StreamI[A] => Iteratee[A, M, B]) => Iteratee[AA, M, Step[A, M, B]])(implicit m: Monad[M]): Enumeratee[AA, A, M, B] =
     checkDoneEx[AA, A, M, B](Chunks(Stream.empty))(ff)

  def checkContinue0[A, F[_], B](inner: Enumerator[A, F, B] => (StreamI[A] => Iteratee[A, F, B]) => Iteratee[A, F, B])(implicit m: Monad[F]): Enumerator[A, F, B] = {
    def loop(s: Step[A, F, B])(implicit m: Monad[F]): Iteratee[A, F, B] = s match {
      case Continue(k) => inner(loop)(k)
      case _ => returnI(s)
    }
    loop
  }

  def checkcontinue1[A, F[_], B, S1](s1: S1)(inner: ((S1 => Enumerator[A, F, B]) => (=> S1) => (StreamI[A] => Iteratee[A, F, B]) => Iteratee[A, F, B]))(implicit m: Monad[F]): Enumerator[A, F, B] = {
    def loop(s1: S1)(step: Step[A, F, B])(implicit m: Monad[F]): Iteratee[A, F, B] = step match {
      case Continue(k) => inner(loop)(s1)(k)
      case _ => returnI(step)
    }
    loop(s1)
  }
}

import Internal._
import scalaz.{MonadTrans, Functor, Monad, MonadPlus}
import scalaz._

trait Iteratees {

  implicit def IterateeMonad[X, F[_]](implicit F0: Monad[F]) = new IterateeMonad[X, F] {
    implicit def F = F0
  }

  implicit def IterateeMonadTrans[A]: MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ] {
    def hoist[F[_]: Monad, G[_]](f: F ~> G) = new (({type f[x] = Iteratee[A, F, x]})#f ~> ({type f[x] = Iteratee[A, G, x]})#f) {
      def apply[B](fa: Iteratee[A, F, B]): Iteratee[A, G, B] = fa mapI f
    }

    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]) =  Iteratee(Monad[G].map(ga)((x: A) => Yield(x, EOF())))
  }
}

object Iteratees extends Iteratees


private[enumerator] trait IterateeMonad[X, F[_]] extends Monad[({type l[a] = Iteratee[X, F, a]})#l] {
  implicit def F: Monad[F]

  def bind[A, B](fa: Iteratee[X, F, A])(f: (A) => Iteratee[X, F, B]) = {
    Iteratee(F.bind(fa.value)(mStep => mStep match {
      case Yield(x, Chunks(Stream.Empty)) => f(x).value
      case Yield(x, chunk) => {
        F.bind(f(x).value)(r => r match {
          case Continue(k) => k(chunk).value
          case ErrorS(t) => F.pure(err[X, F, B](t))
        })
      }
      case Continue(k) => F.pure(Continue(str => bind(k(str))(f)))
      case ErrorS(t) => F.pure(err[X, F, B](t))
    }
    ))
  }

  override def map[A, B](fa: Iteratee[X, F, A])(f: (A) => B): Iteratee[X, F, B] = fa map f

  def point[A](a: => A) = yieldI(a, Chunks(Stream.empty[X]))
}
