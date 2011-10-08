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
  import BMap._

  "A BMap" should {
    "return the inserted value" in {
      "Hello world" must startWith("Hello")
    }

  }
}
//
//  private def checkComposibility = {
//    def associative[F[_], X, Y, Z](cat: GenericCategory[F],
//                          axy: Arbitrary[(X => Y)],
//                          ayz: Arbitrary[(Y => Z)],
//                          azx: Arbitrary[(X => Y)]) =
//      forAll((a1: F[X], f1: (X => Y), f2: (Y => Z), f3: (Z => X)) =>
//        ((cat.compose(f1, cat.compose(f2, f3))  == cat.compose(cat.compose(f1, f2), f3)).label("composition"))
//  }