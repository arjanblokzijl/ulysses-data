package org.ulysses.data
package enumerator

import Internal._
import Enumerator._

/**
 * User: arjan
 */

object RunEnumerator extends App {

    import scalaz.std.stream._
    import scalaz.Id._
    import scalaz.Id
    val iterate = EL.iterate[Id, Int, Stream[Int]]((s: Int) => s + 1)(5)
    def take10 = EL.take[scalaz.Id, Int](10) >>== iterate
    def take5 = EL.take[scalaz.Id, Int](5) >>== iterate
    def takeWhile = EL.takeWhile[scalaz.Id, Int](i => i <= 10) >>== iterate

    println("take result is " + run(take10).take(10).force)
    println("takeWhile result is " + run(takeWhile).take(10).force)

   val filtered = run(EL.take[scalaz.Id, Int](10) >>== joinE[Int, Int, Id, Stream[Int]](EL.iterate((s: Int) => s + 1)(20))(EL.filter((x: Int) => x % 2 == 0)))
   println("filter = %s " format filtered.take(10).force)


   val zip = run((take5 >>== iterate).zip(take10 >>== iterate))
   println("zip = (%s, %s) " format (zip._1.take(10).force, zip._2.take(10).force))

}