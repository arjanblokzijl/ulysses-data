package org.ulysses.data

import scalaz._

import org.specs2.mutable.Specification
import org.scalacheck._
import Gen._
import Prop._
import org.specs2.specification.gen._
import org.specs2.ScalaCheck

/**
 * User: arjan
 */

import BMap._
import std.anyVal._

object BMapUtil {

}

class BMapSpec extends Specification with ScalaCheck {
    "must insert or update" ! check {
      var bMap = BMap.empty[Int, String]
      check {
        (i: Int) => {
          bMap = bMap.insert(i, "a")
          bMap.find(i) mustEqual("a")
        }
      }
    }
    "maintain balance invariance" ! check {
      var bMap = BMap.empty[Int, String]
      check {
        (i: Int) => {
          bMap = bMap.insert(i, "a")
          bMap.balanced must beTrue
        }
      }
    }

  "update value if key already exists" ! check {
    val emptyMap = BMap.empty[Int, String]
    check {
      (i: Int) => emptyMap.insert(i, "a") === emptyMap.insert(i, "b").insert(i, "a")
    }
  }

  "union must preserve balance" ! check {
    var m1 = BMap.empty[Int, String]
    var m2 = BMap.empty[Int, String]
    check {
      (i: Int, j: Int) =>
        m1 = m1.insert(i, "a")
        m2 = m2.insert(j, "b")
        m1.union(m2).balanced must beTrue
    }
  }
}