package org.ulysses
package data
package enumerator

import Internal._
import scalaz.Monad


class EL {

  def iterate[M[_], A, B, S1](f: A => A)(a: => A)(implicit m : Monad[M]): Enumerator[A, M, B] = {
    checkcontinue1[A, M, B, A](a)(loop => s => k => k(Chunks(List(s))) >>== loop(f(s)))
  }
}