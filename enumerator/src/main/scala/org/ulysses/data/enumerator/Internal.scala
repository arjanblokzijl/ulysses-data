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
  case object Error extends Step[Nothing, Nothing, Nothing] {
    def apply(): Nothing = sys.error("bzzt")
  }

//  trait IterateeI[A, F[_], B] {
//    def runIteratee[A, F[_], B](iteratee: IterateeI): Step[A, F, B]
//  }

  case class Iteratee[A, F[_], B](runI: F[Step[A, F, B]])

//  def runIteratee[A, F[_], B](f: Iteratee[A, F, B]) : F[Step[A, F, B]] = F.pure

//  newtype Iteratee a m b = Iteratee
//	{ runIteratee :: m (Step a m b)
//	}
//  runIteratee: Iteratee a m b -> m (Step[A, M, B])
//>|  newtype Age = Age { unAge :: Int }
//>|brings into scope both a constructor and a de-constructor:
// >|  Age   :: Int -> Age
//>| unAge :: Age -> Int

//  def runIteratee[A, F[_], B]

//  case class Iteratee[A, F[_], B](value: M[IterVM[M, E, A]])

  type Enumerator[A, F[_], B] = Step[A, F, B] => Iteratee[A, F, B]

  type Enumeratee[AO, AI, F[_], B] = Step[AI, F, B] => Iteratee[AO, F, Step[AI, F, B]]

  def returnI[A, F[_], B](step: Step[A, F, B])(implicit F: Monad[F]): Iteratee[A, F, B] = {
    Iteratee(F.pure(step))
  }

  def yieldI[A, F[_], B](x: B, extra: StreamI[A])(implicit F: Monad[F]): Iteratee[A, F, B] = {
    returnI(Yield(x, extra))
  }
}

trait Iteratees {
  import Internal._
  
  implicit def iterateeMonad[X, F[_]](implicit F: Monad[F]) = new Monad[({type l[a]=Iteratee[X, F, a]})#l] {
    def bind[A, B](fa: Iteratee[X, F, A])(f: (A) => Iteratee[X, F, B]) = {
      null
    }

    def pure[A](a: => A) = yieldI(a, Chunks(List()))
  }
}