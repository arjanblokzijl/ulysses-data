package org.ulysses.data

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Arbitrary}
import Gen.{sized, value}
import org.specs2.mutable._

/**
 * User: arjan
 */

class BMapSpec extends Specification {
  import scalaz._
  import Scalaz._
  import BMap._

  "A BMap" should {
    "create BMap from list" in {
      val t1 = fromList(List((5, "a"), (3, "b"), (7, "C")))

      (t1 === (singleton(5, "a") insert(3, "b") insert(7, "C"))) must beTrue
    }
    
    "union maps in left-biased manner" in {
      val t1 = fromList(List((5, "a"), (3, "b")))
      val t2 = fromList(List((5, "A"), (7, "C")))

      (t1.union(t2) === (fromList(List((3, "b"), (5, "a"), (7, "C"))))) must beTrue
    }

    "be a pointed instance" in {
      type IntMap[X] = BMap[Int, X]
      val t1 = "a".point[IntMap]
      (t1 === singleton(0, "a")) must beTrue
    }
  }
}
