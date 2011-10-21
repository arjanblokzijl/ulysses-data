package test

/**
 * User: arjan
 */

object Test {
  import scalaz._
  import Scalaz._

//  val s = some(3)
//  optionT(List(some(3)))
//
//  type StateTIntOption[A] = StateT[Int, Option, A]
//
////  val a1: OptionT[Option, String] = optionT(some("a1"))
////  val a2: OptionT[Option, String] = optionT(some(some("a2")))
////  val a3: OptionT[Option, String] = optionT(some(none))
////
////  for (s <- stateT[Int, Option, String](i => Some(("aaa", i + 1)))) yield(s + "bbb")
////  for (s <- st) yield(s + "bbb")
//
////  (a1 ∘ (_ + "x")).runT assert_=== Some(Some("a1x"))
////  (a2 ∘ (_ + "x")).runT assert_=== Some(Some("a2x"))
////  (a3 ∘ (_ + "x")).runT assert_=== Some(None)
//
//  case class AA(i: Int, s: String)
//
//  val a = AA.curried
}