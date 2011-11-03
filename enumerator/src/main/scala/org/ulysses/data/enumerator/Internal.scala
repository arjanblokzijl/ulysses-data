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
//  case class Error[A, F[_], B]() extends Step[A, F, B]

  case object Error {
    def apply[A, F[_], B]:Step[A, F, B] = new Step[A, F, B]{} //TODO define fold on step

    def unapply[A, F[_], B](s: Step[A, F, B]): Boolean = s match {
      case Continue(_) | Yield(_, _) => false
      case _ => true
    }
  }

  def err[A, F[_], B] = Error[A, F, B]

  case class Iteratee[A, F[_], B](runI: F[Step[A, F, B]])

  type Enumerator[A, F[_], B] = Step[A, F, B] => Iteratee[A, F, B]

  type Enumeratee[AO, AI, F[_], B] = Step[AI, F, B] => Iteratee[AO, F, Step[AI, F, B]]

  def returnI[A, F[_], B](step: Step[A, F, B])(implicit F: Monad[F]): Iteratee[A, F, B] = Iteratee(F.pure(step))

  def yieldI[A, F[_], B](x: B, extra: StreamI[A])(implicit F: Monad[F]): Iteratee[A, F, B] = returnI(Yield(x, extra))

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
            case Error() => F.pure(err[X, F, B])
          })
        }
        case Continue(k) => F.pure(Continue(str => bind(k(str))(f)))
        case Error() => F.pure(err[X, F, B])
      }
      ))
    }
    def pure[A](a: => A) = yieldI(a, Chunks(List()))
  }
}