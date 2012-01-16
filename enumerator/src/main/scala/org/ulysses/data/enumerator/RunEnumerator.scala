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
    def iterate: (Step[Int, scalaz.Id, Stream[Int]]) => Iteratee[Int, scalaz.Id, Stream[Int]] = EL.iterate((s: Int) => s + 1)(5)
    def iteratee = EL.take[scalaz.Id, Int](10) >>== iterate

    val res: scalaz.Id[Stream[Int]] = run(iteratee)
    println("iterate result is " + res.take(10).force)
    println("")
    println("")
    println("----------------------------------------------------")
    println("")
    println("")

   val join = joinE[Int, Int, Id, Stream[Int]](EL.iterate((s: Int) => s + 1)(20))(EL.filter((x: Int) => x % 2 == 0))
   val joinRes = EL.take[scalaz.Id, Int](10) >>== join
   val filtered = run(joinRes)
   println("joinRes = %s " format joinRes)
   println("res = %s " format filtered.take(10).force)

//   val filter = EL.take[scalaz.Id, Int](4) >>== join
//    val filter: Iteratee[Int, scalaz.Id, Stream[Int]] =  EL.take[scalaz.Id, Int](4) ==<< join
//    val fres = run(filter)
//    println("filter result " + fres)
}