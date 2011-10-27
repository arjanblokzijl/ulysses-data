package org.ulysses.data.enumerator

/**
 * User: arjan
 */

object Internal {

  sealed trait Stream[A]
  case class Chunks[A](l: List[A]) extends Stream[A]
  case object EOF extends Stream[Nothing]
}