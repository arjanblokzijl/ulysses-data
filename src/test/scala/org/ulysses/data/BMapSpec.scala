package org.ulysses.data

import org.specs2._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._
import org.scalacheck._
import scalaz.std

/**
 * User: arjan
 */

import BMap._
import std.Int._

class BMapSpec extends Specification with ScalaCheck { override def is =
  "update value if key already exists" ! {
    val emptyMap = BMap.empty[Int, String]
    check{(i: Int) => emptyMap.insert(i, "a") === emptyMap.insert(i, "b").insert(i, "a")}
  }
}