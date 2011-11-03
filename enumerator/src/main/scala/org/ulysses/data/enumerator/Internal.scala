package org.ulysses.data.enumerator

import scalaz.{Functor, Monad, MonadPlus}

/**
 * User: arjan
 */

object Internal {

  sealed trait StreamI[A]
  case class Chunks[A](l: List[A]) extends StreamI[A]
  case object EOF extends StreamI[Nothing]

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
      Iteratee(m.bind(runI)(mStep => mStep match {
        case Yield(x, Chunks(List())) => f(x).runI
        case Yield(x, chunk) =>  {
          m.bind(f(x).runI)(r => r match {
            case Continue(k) => k(chunk).runI
            case ErrorS(t) => m.pure(err(t))
          })
        }
        case Continue(k) => {
          m.pure(Continue(str => k(str).flatMap(f)))
        }
        case ErrorS(t) => m.pure(err(t))
      }
      ))
    }

    def map[C](f: (B) => C)(implicit F: Monad[F]): Iteratee[A, F, C] = {
      flatMap(a => Iteratee[A, F, C](F.pure(Yield(f(a), Chunks(List()))))) //compiles as well without type annotation, but intellij gives annoying red lines
    }
  }

  type Enumerator[A, F[_], B] = Step[A, F, B] => Iteratee[A, F, B]

  type Enumeratee[AO, AI, F[_], B] = Step[AI, F, B] => Iteratee[AO, F, Step[AI, F, B]]

  def returnI[A, F[_], B](step: Step[A, F, B])(implicit F: Monad[F]): Iteratee[A, F, B] = Iteratee(F.pure(step))

  def yieldI[A, F[_], B](x: B, extra: StreamI[A])(implicit F: Monad[F]): Iteratee[A, F, B] = returnI(Yield(x, extra))

  //crazy signature, and the way is done in haskell doesn't exactly translate that well, it seems. TODO therefore
//  def checkcontinue1[A, F[_], B, Step[A, F, B]](inner: Step[A, F, B] => (=> Enumerator[A, F, B]) => Step[A, F, B] => (Stream[A] => Iteratee[A, F, B]) => Iteratee[A, F, B], s1: Step[A, F, B])(implicit F: Monad[F]): Enumerator[A, F, B] =  {
//    def loop(inn: Step[A, F, B] => (=> Enumerator[A, F, B]) => Step[A, F, B] => (Stream[A] => Iteratee[A, F, B]) => Iteratee[A, F, B], s11: Step[A, F, B]) = (inn, s11) match {
//      case (s, (Continue(k))) => inner(loop(s, _: Step[A, F, B]), k)
//    }
//  }


}

trait Iteratees {
  import Internal._
  
  implicit def iterateeMonad[X, F[_]](implicit F: Monad[F]) = new Monad[({type l[a]=Iteratee[X, F, a]})#l] {
    def bind[A, B](fa: Iteratee[X, F, A])(f: (A) => Iteratee[X, F, B]) = {
      Iteratee(F.bind(fa.runI)(mStep => mStep match {
        case Yield(x, Chunks(List())) => f(x).runI
        case Yield(x, chunk) =>  {
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
    def pure[A](a: => A) = yieldI(a, Chunks(List()))
  }
}

object Iteratees extends Iteratees