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
  def is =
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
}