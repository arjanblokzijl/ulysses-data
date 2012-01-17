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
    def iteratee = EL.take[scalaz.Id, Int](10) >>== EL.iterate((s: Int) => s + 1)(5)

    val res: scalaz.Id[Stream[Int]] = run(iteratee)
    println("iterate result is " + res.take(10).force)

   val filtered = run(EL.take[scalaz.Id, Int](10) >>== joinE[Int, Int, Id, Stream[Int]](EL.iterate((s: Int) => s + 1)(20))(EL.filter((x: Int) => x % 2 == 0)))
   println("res = %s " format filtered.take(10).force)

}