package org.ulysses.data.enumerator

/**
 * User: arjan
 */

object Internal {

  sealed trait Stream[A]
  case class Chunks[A](l: List[A]) extends Stream[A]
  case object EOF extends Stream[Nothing]

  sealed trait Step[A, M, B]
  case class Continue[A, M, B](f: Stream[A] => Iteratee[A, M, B]) extends Step[A, M, B]

  case class Iteratee[A, M, B]()
}