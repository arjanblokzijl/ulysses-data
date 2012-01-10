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
    val iteratee = EL.take[scalaz.Id, Int](10) >>== EL.iterate((s: Int) => s + 1)(5)
    val res: scalaz.Id[Stream[Int]] = run(iteratee)
    println("result is " + res.take(10).force)

    val filter =  EL.take[scalaz.Id, Int](10) >>== EL.iterate((s: Int) => s + 1)(5)
    val filterRes: scalaz.Id[Step[Int, scalaz.Id, Stream[Int]]] = run(filter.==<<(EL.filter((x: Int) => (x % 2 == 0))))
//    val filterRes = run(filter)
    filterRes match {
      case Yield(s, rest) => println("filter yield " + s.take(10).force)
      case _ => println("no yield!")
    }
}