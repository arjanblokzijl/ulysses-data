package org.ulysses
package data
package enumerator

import scalaz.{Functor, Monad, MonadPlus}

/**
 * User: arjan
 */

object Internal {

  sealed trait StreamI[A]
  case class Chunks[A](l: Stream[A]) extends StreamI[A]
  case class EOF[A]() extends StreamI[A]

  sealed trait Step[A, F[_], B]
  case class Continue[A, F[_], B](f: StreamI[A] => Iteratee[A, F, B]) extends Step[A, F, B]
  case class Yield[A, F[_], B](b: B, s: StreamI[A]) extends Step[A, F, B]
  case class ErrorS[A, F[_], B](t: Throwable) extends Step[A, F, B]

  case object Error {
    //    def apply[A, F[_], B](t: Throwable):Step[A, F, B] = ErrorS[A, F, B](t)

    def unapply[A, F[_], B](s: Step[A, F, B]): Boolean = s match {
      case Continue(_) | Yield(_, _) => false
      case _ => true
    }
  }

  def err[A, F[_], B](t: Throwable) = ErrorS[A, F, B](t)

  case class Iteratee[A, F[_], B](runI: F[Step[A, F, B]]) {
    def flatMap[C](f: (B) => Iteratee[A, F, C])(implicit m: Monad[F]): Iteratee[A, F, C] = {
      Iteratee(m.bind(runI)((mStep: Step[A, F, B]) => mStep match {
        case Yield(x, Chunks(Stream.Empty)) => f(x).runI
        case Yield(x, chunk) => {
          m.bind(f(x).runI)(r => r match {
            case Continue(k) => k(chunk).runI
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

    def >>==[AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
      Internal.>>==(this)(f)

    def ==<<[AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
      Internal.==<<(f)(this)
  }

  def >>==[A, F[_], B, AA, BB](i: Iteratee[A, F, B])(f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit m: Monad[F]): Iteratee[AA, F, BB] =
    Iteratee(m.bind(i.runI)(step => f(step).runI))

  def ==<<[A, F[_], B, AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(i: Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
    >>==(i)(f)

  def $$[A, F[_], B, AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(i: Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[AA, F, BB] = ==<<(f)(i)

  type Enumerator[A, F[_], B] = Step[A, F, B] => Iteratee[A, F, B]

  type Enumeratee[AO, AI, F[_], B] = Step[AI, F, B] => Iteratee[AO, F, Step[AI, F, B]]
  def returnI[A, F[_], B](step: Step[A, F, B])(implicit F: Monad[F]): Iteratee[A, F, B] = Iteratee(F.pure(step))
  def yieldI[A, F[_], B](x: B, extra: StreamI[A])(implicit F: Monad[F]): Iteratee[A, F, B] = returnI(Yield(x, extra))
  def continue[A, F[_], B](k: StreamI[A] => Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[A, F, B] = returnI(Continue(k))

  /**
   * Sends EOF to its iteratee.
   */
  def enumEOF[A, F[_], B](implicit m: Monad[F]): Enumerator[A, F, B] = s => s match {
    case Yield(x, _) => yieldI(x, EOF())
    case ErrorS(s) => returnI(err(s))
    case Continue(k) => >>==(k(EOF()))(check => check match {
      case Continue(_) => sys.error("enumEOF: divergent iteratee")
      case s => enumEOF(m)(s)
    })
  }

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
trait Iteratees {

  implicit def IterateeMonad[X, F[_]](implicit F0: Monad[F]) = new IterateeMonad[X, F] {
    implicit def F = F0
  }
//  implicit def iterateeMonad[X, F[_]](implicit F: Monad[F]) = new Monad[({type l[a] = Iteratee[X, F, a]})#l] {
//    def bind[A, B](fa: Iteratee[X, F, A])(f: (A) => Iteratee[X, F, B]) = {
//      Iteratee(F.bind(fa.runI)(mStep => mStep match {
//        case Yield(x, Chunks(List())) => f(x).runI
//        case Yield(x, chunk) => {
//          F.bind(f(x).runI)(r => r match {
//            case Continue(k) => k(chunk).runI
//            case ErrorS(t) => F.pure(err[X, F, B](t))
//          })
//        }
//        case Continue(k) => F.pure(Continue(str => bind(k(str))(f)))
//        case ErrorS(t) => F.pure(err[X, F, B](t))
//      }
//      ))
//    }
//
//    def point[A](a: => A) = yieldI(a, Chunks(List()))
//  }
}

object Iteratees extends Iteratees


private[enumerator] trait IterateeMonad[X, F[_]] extends Monad[({type l[a] = Iteratee[X, F, a]})#l] {
  implicit def F: Monad[F]

  def bind[A, B](fa: Iteratee[X, F, A])(f: (A) => Iteratee[X, F, B]) = {
    Iteratee(F.bind(fa.runI)(mStep => mStep match {
      case Yield(x, Chunks(Stream.Empty)) => f(x).runI
      case Yield(x, chunk) => {
        F.bind(f(x).runI)(r => r match {
          case Continue(k) => k(chunk).runI
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
