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

  sealed trait Step[A, F[_], B] {
    def fold[R](continue: (StreamI[A] => Iteratee[A, F, B]) => R, yieldI: (=> B) => StreamI[A] => R, error: Throwable => R): R
  }
  case class Continue[A, F[_], B](f: (StreamI[A]) => Iteratee[A, F, B]) extends Step[A, F, B] {
    def fold[R](continue: (StreamI[A] => (Iteratee[A, F, B])) => R, yieldI: (=> B) => (StreamI[A]) => R, error: (Throwable) => R) = continue(f)
  }
  case class Yield[A, F[_], B](b: B, s: StreamI[A]) extends Step[A, F, B] {
    def fold[R](continue: (StreamI[A] => Iteratee[A, F, B]) => R, yieldI: (=> B) => (StreamI[A]) => R, error: (Throwable) => R) = yieldI(b)(s)
  }
  case class ErrorS[A, F[_], B](t: Throwable) extends Step[A, F, B] {
    def fold[R](continue: (StreamI[A] => Iteratee[A, F, B]) => R, yieldI: (=> B) => (StreamI[A]) => R, error: (Throwable) => R) = error(t)
  }

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

    def flatMap[C](f: (B) => Iteratee[A, F, C])(implicit M: Monad[F]): Iteratee[A, F, C] = {
      Iteratee(M.bind(value)((mStep: Step[A, F, B]) => mStep match {
        case Yield(x, Chunks(Stream.Empty)) => f(x).value
        case Yield(x, extra) => {
          M.bind(f(x).value)(r => r match {
            case Yield(xx, _) => M.pure(Yield(xx, extra))
            case Continue(k) => k(extra).value
            case ErrorS(t) => M.pure(err(t))
          })
        }
        case Continue(k) => M.pure(Continue(str => k(str).flatMap(f)))
        case ErrorS(t) => M.pure(err(t))
      }
      ))
    }

    def map[C](f: (B) => C)(implicit F: Monad[F]): Iteratee[A, F, C] =
      flatMap(a => Iteratee[A, F, C](F.pure(Yield(f(a), Chunks(Stream.empty))))) //compiles as well without type annotation, but intellij gives annoying red lines


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

    def >>==[AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit M: Monad[F]): Iteratee[AA, F, BB] =
      Iteratee[AA, F, BB](M.bind(value)(step => f(step).value))


    def ==<<[AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit M: Monad[F]): Iteratee[AA, F, BB] = >>==(f)

    /**
     * Wraps an inner iteratee in an enumeratee wrapper. The resulting iteratee will consume the
     * wrapper's input type and yield inner's output type.
     */
     def joinI[I](outer: Iteratee[A, F, Step[I, F, B]])(implicit M: Monad[F]): Iteratee[A, F, B] = {
      val ITP = IterateeInstances.IterateeMonad[A, F]
       def check: Step[I, F, B] => Iteratee[A, F, B] = step => step match {
         case Continue(k) => k(EOF()) >>== (s => s match {
             case Continue(_) => sys.error("joinI: divergent iteratee")
             case _ => check(s)
           })
         case Yield(x, _) => ITP.point(x)
         case ErrorS(t) => throw t
       }
      outer flatMap check
     }

     def zip[C](that: Iteratee[A, F, C])(implicit M: Monad[F]): Iteratee[A, F, (B, C)] = {
       def enumStream[D](stream: StreamI[A])(s: Step[A, F, D]): Iteratee[A, F, D] = s match {
         case Continue(k) => k(stream)
         case Yield(b, extra) => yieldI(b, streamI.streamIMonoid[A].append(extra, stream))
         case ErrorS(t) => throw t
       }

       def shorter(s1: StreamI[A])(s2: StreamI[A]): StreamI[A] = (s1, s2) match {
         case (c1@Chunks(xs), c2@Chunks(ys)) => if (xs.length < ys.length) c1 else c2
         case (_, _) => EOF()
       }

       import IterateeInstances._
       def step: StreamI[A] => Iteratee[A, F, (B, C)] = (s: StreamI[A]) => s match {
         case Chunks(Stream.Empty) => continue(step)
         case stream@Chunks(_) => {
           MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ].liftM((this ==<< enumStream(stream)).value).flatMap(s1 => {
             MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ].liftM((that ==<< enumStream(stream)).value).flatMap((s2: Step[A, F, C]) => (s1, s2) match {
               case (Continue(k1), Continue(k2)) => continue(k1).zip(continue(k2))
               case (Yield(b1, _), Continue(k2)) => yieldI[A, F, B](b1, Chunks(Stream.Empty)).zip(continue(k2))
               case (Continue(k1), Yield(b2, _)) => continue(k1).zip(yieldI(b2, Chunks(Stream.Empty)))
               case (Yield(b1, ex1), Yield(b2, ex2)) => yieldI((b1, b2), shorter(ex1)(ex2))
               case (_, _) => sys.error("bzzt")
             })
           })
         }
         case EOF() => {
           val ITP = IterateeInstances.IterateeMonad[A, F]
           val i1: Iteratee[A, F, B] = MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ].liftM(this.value).flatMap(enumEOF)
           val i2: Iteratee[A, F, C] = MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ].liftM(that.value).flatMap(enumEOF)
           i1.flatMap(b1 => i2.flatMap(b2 => ITP.point(b1, b2)))
         }
       }
     continue(step)
    }
  }

  //    def joinE[AI, A, F[_], B](enum: Enumerator[A, F, Step[AI, F, B]])(enee: Enumeratee[A, AI, F, B])(implicit M: Monad[F]): Enumerator[AI, F, B] = s => {
  def joinE[AI, AO, F[_], B](enum: Enumerator[AO, F, Step[AI, F, B]])(enee: Enumeratee[AO, AI, F, B])(implicit M: Monad[F]): Enumerator[AI, F, B] = s => {
     val ens = enee(s)
     val enumens = enee(s) ==<< enum
     Iteratee[AI, F, B](M.bind(enumens.value)(step => step match {
       case Yield(x, si) => M.point(x)
       case Continue(_) => sys.error("joinE: divergent iteratee")
       case ErrorS(t) => M.point(ErrorS(t))
     }))
  }

  def >==>[A, B, F[_], AA, BB](e1: Enumerator[A, F, B])(e2: Step[A, F, B] => Iteratee[AA, F, BB])(implicit F: Monad[F]): Step[A, F, B] => Iteratee[AA, F, BB] = s =>
    e1(s) >>== e2

  def <==<[A, B, F[_], AA, BB](e1: Step[A, F, B] => Iteratee[AA, F, BB])(e2: Enumerator[A, F, B])(implicit F: Monad[F]): Step[A, F, B] => Iteratee[AA, F, BB] = s =>
    e2(s) >>== e1

  def >>==[A, F[_], B, AA, BB](i: Iteratee[A, F, B])(f: Step[A, F, B] => Iteratee[AA, F, BB])(implicit m: Monad[F]): Iteratee[AA, F, BB] =
     i >>== f

  def ==<<[A, F[_], B, AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(i: Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[AA, F, BB] =
     i ==<< f

//  def $$[A, F[_], B, AA, BB](f: Step[A, F, B] => Iteratee[AA, F, BB])(i: Iteratee[A, F, B])(implicit F: Monad[F]): Iteratee[AA, F, BB] = ==<<(f)(i)

  type Enumerator[A, F[_], B] = Step[A, F, B] => Iteratee[A, F, B]

  type Enumeratee[AO, AI, M[_], B] = Step[AI, M, B] => Iteratee[AO, M, Step[AI, M, B]]
  def returnI[A, F[_], B](step: Step[A, F, B])(implicit m: Monad[F]): Iteratee[A, F, B] = Iteratee[A, F, B](m.pure(step))
  def yieldI[A, F[_], B](x: B, extra: StreamI[A])(implicit m: Monad[F]): Iteratee[A, F, B] = returnI[A, F, B](Yield(x, extra))
  def continue[A, F[_], B](k: StreamI[A] => Iteratee[A, F, B])(implicit m: Monad[F]): Iteratee[A, F, B] = returnI[A, F, B](Continue(k))


  /**
   * Sends EOF to its iteratee.
   */
  def enumEOF[A, F[_], B](implicit M: Monad[F]): Enumerator[A, F, B] = s => s match {
      case Yield(x, _) => yieldI[A, F, B](x, EOF())
      case ErrorS(s) => returnI[A, F, B](err(s))
      case Continue(k) => {
        k(EOF()) >>== (check => check match {
          case Continue(_) => sys.error("enumEOF: divergent iteratee")
          case s => enumEOF[A, F, B](M)(s)
     })}
  }


  /**
   * Checks whether the inner Iteratee has finished and, if so, returns its output.
   * checkDone passes its parameter a continuation if the Iteratee can still consume input, or yields otherwise.
   */
  def checkDoneEx[AA, A, M[_], B](extra: StreamI[AA])(ff: (StreamI[A] => Iteratee[A, M, B]) => Iteratee[AA, M, Step[A, M, B]])(implicit m: Monad[M]): Enumeratee[AA, A, M, B] = f => f match {
      case Continue(k) => ff(k)
      case step => yieldI[AA, M, Step[A, M, B]](step, extra)
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

trait IterateeInstances {

  implicit def IterateeMonad[X, F[_]](implicit F0: Monad[F]) = new IterateeMonad[X, F] {
    implicit def F = F0
  }

  implicit def IterateeMonadTrans[A]: MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = Iteratee[A, α, β]})#λ] {
    def hoist[F[_]: Monad, G[_]](f: F ~> G) = new (({type f[x] = Iteratee[A, F, x]})#f ~> ({type f[x] = Iteratee[A, G, x]})#f) {
      def apply[B](fa: Iteratee[A, F, B]): Iteratee[A, G, B] = fa mapI f
    }
    def runIteratee[A, F[_], B](i: Iteratee[A, F, B]): F[Step[A, F, B]] = i.value

    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]) =  Iteratee(Monad[G].map(ga)((x: A) => Yield(x, Chunks(Stream.Empty))))
  }
}

object IterateeInstances extends IterateeInstances


private[enumerator] trait IterateeMonad[X, F[_]] extends Monad[({type l[a] = Iteratee[X, F, a]})#l] {
  implicit def F: Monad[F]

  def bind[A, B](fa: Iteratee[X, F, A])(f: (A) => Iteratee[X, F, B]) =
    fa flatMap f

  override def map[A, B](fa: Iteratee[X, F, A])(f: (A) => B): Iteratee[X, F, B] = fa map f

  def point[A](a: => A) = yieldI(a, Chunks(Stream.empty[X]))
}

object streamI extends StreamIInstances
trait StreamIInstances {
  implicit def streamIMonoid[A] = new Monoid[StreamI[A]] {
    def append(f1: StreamI[A], f2: => StreamI[A]) = (f1, f2) match {
      case (Chunks(xs), Chunks(ys)) =>  Chunks(xs #::: ys)
      case (_, _) =>  EOF()
    }

    def zero = Chunks(Stream.Empty)
  }
}