package org.ulysses.data

/**
 * User: arjan
 */

object ShowBmap {

  import BMap._

  def main(args: Array[String]) = {
    import scalaz._
    import std.anyVal._

    val res = singleton(1, "a").insert(2, "b").insert(3, "c").insert(4, "d").insert(5, "e").insert(6, "f").insert(7, "g").insert(8, "h").insert(9, "j")
    println("got result " + res)
    println(res.showTree)
  }
}