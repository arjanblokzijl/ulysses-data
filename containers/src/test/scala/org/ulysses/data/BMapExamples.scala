package org.ulysses.data


import org.specs2.mutable._

/**
 * User: arjan
 */

class BMapExamples extends Specification {
  "A BMap" should {
    import BMap._
    import scalaz._
    import std.anyVal._
    import scalaz.Equal._
    import Id._

    "create BMap from list" in {
      val t1 = fromList(List((5, "a"), (3, "b"), (7, "C")))

//      (singleton(5, "a") insert(3, "b") insert(7, "C")).equal(t1) must beTrue
      t1 === (singleton(5, "a") insert(3, "b") insert(7, "C")) must beTrue
    }
    "union maps in left-biased manner" in {
      val t1 = fromList(List((5, "a"), (3, "b")))
      val t2 = fromList(List((5, "A"), (7, "C")))

      t1.union(t2) === fromList(List((3, "b"), (5, "a"), (7, "C"))) must beTrue
    }

    "transform to stream" in {
      val s1 = fromList(List((5, "a"), (3, "b"), (7, "C"))).toStream
      s1 must haveSize(3)
      s1.head mustEqual ((3, "b"))
    }

    "be traversable" in {
      import std.option._
      import syntax.applicative._
      import syntax.traverse._
      val s1 = fromList(List((5, "a"), (3, "b"), (7, "C")))
      println("s1 is %s".format(s1))
      val s2 = s1.traverse[Id, String](_  + "abc")
      val expected: BMap[Int, String] = fromList(List((3, "babc"), (5, "aabc"), (7, "Cabc")))
      println("s2 is %s".format(s2))
      expected === s2 must beTrue
      
      val s3: Option[BMap[Int, String]] = s1.traverse[Option, String](x => Some(x + "abc"))
      println("s3: " + s3)
      s3.map(_ === expected).getOrElse(false) must beTrue
    }

    "be traversable for large lists" in {
      import std.option._
      import syntax.applicative._
      import syntax.traverse._
      var bMap = BMap.empty[Int, String]
      val sample = 100000
      (1 to sample).foreach(i => bMap = bMap.insert(i, "aaa"))

      println("bMap " + bMap)
      val s3: Option[BMap[Int, String]] = bMap.traverse[Option, String](x => Some(x + "abc"))
      val result: BMap[Int, String] = s3.getOrElse(BMap.empty[Int, String])
      println("result: " + result)
      result.size mustEqual(sample)
    }
  }
}
