package org.ulysses.data


import org.specs2.mutable._

/**
 * User: arjan
 */

class BMapExamples extends Specification {
  "A BMap" should {
    import BMap._
    import scalaz._
    import std.Int._
    import scalaz.Equal._
    import Ident._
    import scalaz.syntax.applicative._
    "create BMap from list" in {
      val t1 = fromList(List((5, "a"), (3, "b"), (7, "C")))

//      (singleton(5, "a") insert(3, "b") insert(7, "C")).equal(t1) must beTrue
      bmap.equal(t1, (singleton(5, "a") insert(3, "b") insert(7, "C"))) must beTrue
    }
    "union maps in left-biased manner" in {
      val t1 = fromList(List((5, "a"), (3, "b")))
      val t2 = fromList(List((5, "A"), (7, "C")))

      bmap.equal(t1.union(t2), fromList(List((3, "b"), (5, "a"), (7, "C")))) must beTrue
    }

    "transform to stream" in {
      val s1 = fromList(List((5, "a"), (3, "b"), (7, "C"))).toStream
      s1 must haveSize(3)
      s1.head mustEqual ((3, "b"))
    }

    "be traversable" in {

      val s1 = fromList(List((5, "a"), (3, "b"), (7, "C")))
      val s2 = s1.traverse[Id, String](_  + "abc")
      s2.toList must haveSize(3)
      bmap.equal(fromList(List((3, "babc"), (5, "aabc"), (7, "Cabc"))), s2) must beTrue
    }
  }
}
