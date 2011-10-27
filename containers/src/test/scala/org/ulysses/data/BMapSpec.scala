package org.ulysses.data

import scalaz.std

import org.specs2._
import org.scalacheck._
import Gen._
import Prop._
import specification.gen._

/**
 * User: arjan
 */

import BMap._
import std.Int._

object BMapUtil {

}

class BMapSpec extends Specification with ScalaCheck {
  import scalaz.syntax.Syntax.equal._
  def is =
    "must insert or update" ! {
      var bMap = BMap.empty[Int, String]
      check {
        (i: Int) => {
          bMap = bMap.insert(i, "a")
          bMap.find(i) mustEqual("a")
        }
      }
    }
    "maintain balance invariance" ! {
      var bMap = BMap.empty[Int, String]
      check {
        (i: Int) => {
          bMap = bMap.insert(i, "a")
          bMap.balanced must beTrue
        }
      }
    }

  "update value if key already exists" ! {
    val emptyMap = BMap.empty[Int, String]
    check {
      (i: Int) => emptyMap.insert(i, "a") === emptyMap.insert(i, "b").insert(i, "a")
    }
  }

  "union must preserve balance" ! {
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