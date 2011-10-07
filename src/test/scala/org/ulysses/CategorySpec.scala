package org.ulysses

import cat.{GenericCategory, Category}
import org.specs.{ScalaCheck, Specification}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.{Gen, Arbitrary}
import Gen.{sized, value}

/**
 * User: arjan
 */

class CategorySpec extends Specification with ScalaCheck {
  import Category._

  "A Category" should {
    "satisfy composibility" in {
     checkComposibility
    }

  }

  private def checkComposibility = {
    def associative[F[_], X, Y, Z](cat: GenericCategory[F],
                          axy: Arbitrary[(X => Y)],
                          ayz: Arbitrary[(Y => Z)],
                          azx: Arbitrary[(X => Y)]) =
      forAll((a1: F[X], f1: (X => Y), f2: (Y => Z), f3: (Z => X)) =>
        ((cat.compose(f1, cat.compose(f2, f3))  == cat.compose(cat.compose(f1, f2), f3)).label("composition"))
  }
}