package data

import org.specs2.mutable.Specification

import BSet._
import scalaz._
import std.anyVal._
import org.specs2.ScalaCheck
import org.scalacheck.{Prop, Gen}

class BSetSpec extends Specification with ScalaCheck {
  val nel: Gen[List[Int]] = Gen.listOf1(Gen.choose(Int.MinValue, Int.MaxValue))

  "maintain balance when inserting" ! check {(l: List[Int]) =>
    val s = fromList(l)
    s.balanced must beTrue
    s.size mustEqual(l.toSet.size)
    s.toList must containAllOf(l.distinct)
  }

  "delete" ! Prop.forAll(nel) { (l: List[Int]) => {
    val s1 = fromList(l)
    val s2 = s1.delete(l.head)
    s2.balanced must beTrue
    s2.size mustEqual(s1.size - 1)
    s1.toList must containAllOf(s2.toList)
   }
  }

  "findMin" ! Prop.forAll(nel) { (l: List[Int]) => {
    val a = fromList(l).findMin
    a must be_==(l.sorted.head)
   }
  }

  "findMax" ! Prop.forAll(nel) { (l: List[Int]) => {
    val a = fromList(l).findMax
    a must be_==(l.sortWith((a, b) => b < a).head)
   }
  }

  "contain unique elements" ! check {(l1: List[Int]) =>
    val l2 = l1.distinct
    val s1 = fromList(l1)
    val s2 = fromList(l2)
    s1.size must be_==(s2.size)
    s2.toList must containAllOf(s1.toList)
  }

  "union same sets is equal" ! check {(l: List[Int]) =>
    val s1 = fromList(l)
    val s2 = s1 union s1
    s1.size must be_==(s2.size)
    s1.toList must containAllOf(s2.toList)
  }

  "union" ! check {(l1: List[Int], l2: List[Int]) =>
    val s = fromList(l1) union fromList(l2)
    val l = (l1.toSet union l2.toSet).toList
    s.size must be_==(l.size)
    s.toList must containAllOf(l)
  }

  "union maintains balance" ! check {(l1: List[Int], l2: List[Int]) =>
    (fromList(l1) union fromList(l2) balanced) must beTrue
  }

  "foldRight" ! check {(l: List[Int]) =>
    val s = fromList(l).foldRight(0)(_ + _)
    s must be_==(l.distinct.foldRight(0)(_+_))
  }

  "toAscList" ! check {(l: List[Int]) =>
    fromList(l).toAscList must be_==(l.distinct.sorted)
  }

  "bset" should {
    "toAscList does not blow the stack" in {
      val s = fromList(List.range(0, 1000000))
      s.toAscList.take(3) must be_==(List(0, 1, 2))
    }
  }
}
