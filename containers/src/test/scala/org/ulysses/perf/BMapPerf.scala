package org.ulysses.perf

import org.ulysses.data.BMap
import java.util.Date
import scala.collection.Map
import scalaz.std

/**
 * User: arjan
 */

object BMapPerf {
  import Perflib._
  import BMap._
  import std.String._

   def main(args: Array[String]) {
     val sample = 100000
     var bMap = empty[String, String]
     var scalaMap: Map[String, String] = Map[String, String]()
     val indexes = new Array[String](sample)
     for (i <- 0 until indexes.length) {
       indexes(i) = scala.math.random.toString
     }
     println("start inserting %s elements in the bmap at %s".format(sample, new Date))
     val bmapOp = "BMap insert" -> time {

      var i = 0
      while (i < indexes.length) {
        bMap = bMap.insert(i.toString, "the data")
        i += 1
      }
     }
     println("Finished inserting %s elements in the bmap at %s".format(sample, new Date))
     val scalaMapOp = "ScalaMap insert" -> time {
       var j = 0

        while (j < indexes.length) {
          scalaMap = scalaMap + (j.toString -> "the data")
          j += 1
        }
     }
     println("Finished inserting %s elements in the scala map at %s".format(sample, new Date))

     bmapOp compare scalaMapOp
     var total = ""
     val bmapLookup = "BMap lookup" -> time {
       var i = 0
       while (i < indexes.length) {
         val result = bMap.lookup(i.toString)
         total = result.getOrElse("")
         i += 1
       }
     }
     val scalaMapLookup = "ScalaMap lookup" -> time {
       var i = 0
       while (i < indexes.length) {
         val result = scalaMap.get(i.toString)
         total = result.getOrElse("")
         i += 1
       }
     }
     println(total)
     bmapLookup compare scalaMapLookup
   }
}